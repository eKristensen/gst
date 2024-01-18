// This file takes care of checking Session Types against an environment

use std::{collections::HashMap, fmt::format};

use crate::{
    cerl_parser::ast::{Atom, Expr, Exprs, Lit, Var},
    st_parser::ast::{SessionMode, SessionType},
};

use super::{
    analyze_fun::fun_name_extractor,
    analyze_var::{chk_st_exprs, VarType},
};

// Check if call relates to a session type.
pub fn try_st_env_update(
    env: HashMap<Var, VarType>,
    mod_name: &Exprs,
    call_name: &Exprs,
    args: &Vec<Exprs>,
) -> Result<(VarType, HashMap<Var, VarType>), String> {
    let (mod_name, call_name) = fun_name_extractor(mod_name, call_name);
    match (mod_name.as_str(), call_name.as_str()) {
        ("gen_server_plus", "call") => {
            // Sanity check: There MUST be 3 args
            if args.len() != 3 {
                return Err("gen_server_plus:call is used with a wrong number of arguments. There must be three arguments.".to_owned());
            };

            // 2: If yes: Then (this is the case right here)
            //     2a: match the ST on the argument args[0] and args[1]
            // If args[1] == Atom('new'), then args[0] must be fresh(false,...) <-- The easiest case, let us start here
            if *args.get(1).unwrap()
                == Exprs::Single(Box::new(Expr::Lit(Lit::Atom(Atom("new".to_owned())))))
            {
                // In this case only env update is required, Session Type is "activated" on new variable.
                // Check variable name is available etc etc
                let Exprs::Single(st_binder_var_name) = args.get(0).unwrap() else {
                    return Err("Invalid argument for gst+ call".to_owned());
                };
                let Expr::Var(st_binder_var_name) = *st_binder_var_name.clone() else {
                    return Err("Invalid argument for gst+ call".to_owned());
                };

                match env.get(&st_binder_var_name) {
                    Some(st) => {
                        let VarType::ST(st) = st else {
                            return Err(format!("Expected session type"));
                        };
                        let SessionMode::Fresh(false, st) = st else {
                            return Err(format!(
                                "Fresh session is no longer fresh, cannot start new session again!"
                            ));
                        };
                        let mut env = env.clone();
                        env.remove(&st_binder_var_name);
                        return Ok((VarType::ST(SessionMode::Fresh(true, st.clone())), env));
                    }
                    None => {
                        return Err(format!(
                            "Tried to use non-existing session type {:?}",
                            st_binder_var_name
                        ))
                    }
                }
            };

            // If args[1] != Atom('new') then args[0] must be ongoing or fresh(true,...)
            // Use args[1] to lookup session by the session id.
            let Exprs::Single(session_id) = args.get(1).unwrap() else {
                return Err(format!(
                    "Session identifier for session type must be singular"
                ));
            };
            let Expr::Var(session_id) = *session_id.clone() else {
                return Err(format!("Session identifier for session type must be var"));
            };
            // TODO: OK to basically ignore the ServerPid , i.e. args[0] ??? That is what I do right now.
            match env.get(&session_id) {
                Some(sid_type) => {
                    // In this case the content of the session type must be checked

                    // First the type must be a session id
                    let VarType::ST(sid_type) = sid_type else {
                        return Err(format!("Expected a session type, found {:?}", sid_type));
                    };

                    match sid_type {
                        SessionMode::NotST => todo!("NotST not used"),
                        SessionMode::Fresh(true, sid_cnt) => {
                            if sid_cnt.len() < 1 {
                                return Err(format!(
                                    "Session Type is empty, cannot continue, no send?"
                                ));
                            };

                            // Send: match the sending argument type from env, args[2]
                            // Lookup type in environment or check literal type match the ST.
                            let dummy_m = HashMap::new();
                            let sending_type =
                                chk_st_exprs(&dummy_m, env.clone(), (args.get(2)).unwrap())?;
                            let (VarType::Base(sending_type), _) = sending_type.first().unwrap()
                            else {
                                return Err(format!("Must send base type."));
                            };
                            // The Send(sending_type) === sid_cnt[0]

                            let SessionType::Send(must_send_type) = sid_cnt.first().unwrap() else {
                                return Err(format!("Next ST must be send"));
                            };

                            if *sending_type != *must_send_type {
                                return Err(format!("Session type does not match. Must send {:?}, but call is sending {:?}", must_send_type, sending_type));
                            };
                            // If so, then remove this element form st and update env.
                            // TODO: Optimize maybe find a way to avoid a bunch of clone() here?
                            let mut env = env.clone();
                            let mut sid_cnt = sid_cnt.clone();
                            sid_cnt.remove(0);

                            // Receive: Get receive return type for return
                            // Receive type is used for return
                            if sid_cnt.len() < 1 {
                                return Err(format!(
                                    "Session Type is empty, cannot continue, no receive?"
                                ));
                            };
                            let SessionType::Receive(returned_type) = sid_cnt.first().unwrap()
                            else {
                                return Err(format!("Must receive in session"));
                            };

                            // Remove first TODO: Maybe possible to get and remove in one step?
                            let mut sid_cnt = sid_cnt.clone(); // TODO: At least one more clone than strictly needed
                            sid_cnt.remove(0);

                            env.insert(
                                session_id.clone(),
                                VarType::ST(SessionMode::Fresh(true, sid_cnt.clone())),
                            );

                            return Ok((VarType::Base(returned_type.clone()), env));
                        }
                        SessionMode::Fresh(false, _) => {
                            return Err(format!("fresh session must be initialized before use."))
                        }
                        SessionMode::Ongoing(_, _) => todo!("ongoing not yet implemented"),
                    }
                }
                None => Err(format!("Using non-existing session!")),
            }

            // What to do with both SessionID and ServerPid? SessionID is not given in argument to client function!

            // Maybe fresh requires or assumes argument to be PiD, and
            // only allow ST to be evaluated once the ST has been moved
            // to a "SessionID" variable?
        }
        _ => Err(format!("no st match on {:?}:{:?}", mod_name, call_name)),
    }
}

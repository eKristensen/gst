// This file takes care of checking Session Types against an environment

use std::collections::HashMap;

use crate::{
    cerl_parser::ast::{Atom, Expr, Exprs, FunCall, FunKind, Lit, Var},
    st_parser::ast::{Label, SessionElement, SessionType, Types},
};

use super::analyze_var::{chk_st_exprs, VarType};

// Check if call relates to a session type.
pub fn try_st_env_update(
    env: &HashMap<Var, VarType>,
    call: &FunCall,
    args: &Vec<Exprs>,
) -> Result<(VarType, HashMap<Var, VarType>), String> {
    let FunCall {
        kind: FunKind::Call(kind),
        name: Atom(name),
    } = call
    else {
        return Err(format!(
            "Only call supported so far st check but found {:?}",
            call
        ));
    };
    match (kind.as_str(), name.as_str()) {
        ("gen_server_plus", "call") => {
            // Process differently depending on the number of arguments
            match args.len() {
                2 => {
                    // If args[1] == Atom('new'), then args[0] must be fresh(false,...) <-- The easiest case, let us start here
                    if *args.get(1).unwrap()
                            == Exprs(vec!(Expr::Lit(Lit::Atom(Atom("new".to_owned())))))
                        {
                            // In this case only env update is required, Session Type is "activated" on new variable.
                            // Check variable name is available etc etc
                            let Exprs(st_binder_var_name) = args.get(0).unwrap();
                            if st_binder_var_name.len() != 1 {
                                return Err(format!("Expected one argument in gs+call"));
                            }
                            let Expr::Var(st_binder_var_name) = st_binder_var_name.first().unwrap().clone() else {
                                return Err("Invalid argument for gst+ call".to_owned());
                            };

                            match env.get(&st_binder_var_name) {
                                Some(st) => {
                                    let VarType::ST(st) = st else {
                                        return Err(format!("Expected session type"));
                                    };
                                    let SessionType::New(st) = st else {
                                        return Err(format!(
                                            "Session has already been started, cannot start new session again!"
                                        ));
                                    };
                                    //let mut env = env.clone();
                                    // TODO: Ask Marco: Should constructor be preserved or removed?
                                    //env.remove(&st_binder_var_name);
                                    return Ok((VarType::ST(SessionType::Ongoing(st.clone(),None)), env.clone()));
                                }
                                None => {
                                    return Err(format!(
                                        "Tried to use non-existing session type {:?}",
                                        st_binder_var_name
                                    ))
                                }
                            }
                        }
                        else {
                            return Err(format!("Two argument non-new call not supported."))
                        }
                },
                3 => {
                    // If args[1] != Atom('new') then args[0] must be ongoing or fresh(true,...)
                    // Use args[1] to lookup session by the session id.
                    let Exprs(session_id) = args.get(1).unwrap();
                    if session_id.len() != 1 {
                        return Err(format!("Session ID cannot be composed of more than one thing."));
                    }
                    let Expr::Var(session_id) = session_id.first().unwrap().clone() else {
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
                                SessionType::NotST => todo!("NotST not used"),
                                SessionType::Ongoing(sid_cnt, local_binder_res) => {
                                    if sid_cnt.len() < 1 {
                                        return Err(format!(
                                            "Session Type is empty, cannot continue, no send?"
                                        ));
                                    };

                                    // Send: match the sending argument type from env, args[2]
                                    // Lookup type in environment or check literal type match the ST.
                                    let dummy_m = HashMap::new();
                                    let sending_type =
                                        chk_st_exprs(&dummy_m, env, (args.get(2)).unwrap())?;
                                    let (VarType::Base(sending_type), _) = sending_type.first().unwrap()
                                    else {
                                        return Err(format!("Must send base type."));
                                    };

                                    match sid_cnt.first().unwrap() {
                                        SessionElement::Send(must_send_type) => {
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
                                            let SessionElement::Receive(returned_type) = sid_cnt.first().unwrap()
                                            else {
                                                return Err(format!("Must receive in session"));
                                            };

                                            // Remove first TODO: Maybe possible to get and remove in one step?
                                            let mut sid_cnt = sid_cnt.clone(); // TODO: At least one more clone than strictly needed
                                            sid_cnt.remove(0);

                                            env.insert(
                                                session_id.clone(),
                                                VarType::ST(SessionType::Ongoing(sid_cnt.clone(),local_binder_res.clone())),
                                            );

                                            return Ok((VarType::Base(returned_type.clone()), env));
                                        },
                                        SessionElement::Receive(_) => return Err(format!("Receive not supported when sending")),
                                        SessionElement::MakeChoice(_,_) => todo!("make choice when sending impl"),
                                        SessionElement::OfferChoice(choices) => {
                                            // When the session-type offers a choice, the action must be something that makes a choice. In this case a choice is sending an atom, which must correspond to one of the choices available.

                                            let Exprs(choice_label) = args.get(2).unwrap().clone();
                                            if choice_label.len() != 1 { return Err(format!("Choice label must be just a single expr.")) }
                                            let Expr::Lit(Lit::Atom(Atom(choice_label))) = choice_label.first().unwrap() else { return Err(format!("Officer choice labe must be an atom"))};
                                            let choice_label = Label(choice_label.clone());

                                            match choices.get(&choice_label) {
                                                Some(inner_st) => {
                                                    // Choice found, return now
                                                    let mut env = env.clone();
                                                    env.insert(
                                                        session_id.clone(),
                                                        VarType::ST(SessionType::Ongoing(inner_st.clone(),local_binder_res.clone())),
                                                    );
                                                    return Ok((VarType::Base(Types::Tuple(vec![])),env)); // TODO: ugly return type, this will properly give problems later
                                                },
                                                None => return Err(format!("Requested choice {:?} not found in {:?}", choice_label, choices)),
                                            }
                                        },
                                        SessionElement::End => return Err(format!("Session type is finished with .end and cannot be used for further communication.")),
                                    }
                                }
                                SessionType::New(_) => {
                                    return Err(format!("session must be initialized before use."))
                                }
                            }
                        }
                        None => Err(format!("Using non-existing session!")),
                    }
                },
                _ => return Err("gen_server_plus:call is used with a wrong number of arguments. There must be two of three arguments.".to_owned()),
            }

            // What to do with both SessionID and ServerPid? SessionID is not given in argument to client function!

            // Maybe fresh requires or assumes argument to be PiD, and
            // only allow ST to be evaluated once the ST has been moved
            // to a "SessionID" variable?
        }
        _ => Err(format!("no st match on {:?}", call)),
    }
}

// Type information is not merged into one place. Maybe that is a mistake on me? TODO: Should they be?
pub fn extract_var_type(session_type: &SessionType, spec_type: &Types) -> VarType {
    match session_type {
        SessionType::NotST => {
            // If not session type use the type from -spec
            return VarType::Base(spec_type.clone());
        }
        SessionType::New(_) => {
            // Check that -spec type matches (consistency)
            if *spec_type == Types::Single("new".to_owned()) {
                // Get session type and insert
                return VarType::ST(session_type.clone());
            } else {
                // Should move that part to well formed instead of potentially running check twice
                panic!("-session does not match -spec!! Issue is: {:?} according to -spec, but should be server() to match -session: {:?}", spec_type, session_type);
            };
        }
        SessionType::Ongoing(_, _) => {
            // Check that -spec type matches (consistency)
            if *spec_type == Types::Single("ongoing".to_owned()) {
                // Get session type and insert
                return VarType::ST(session_type.clone());
            } else {
                // Should move that part to well formed instead of potentially running check twice
                panic!("-session does not match -spec!! Issue is: {:?} according to -spec, but should be session() to match -session: {:?}", spec_type, session_type);
            };
        }
    }
}

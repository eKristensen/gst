// Purpose of this file is to check for elements that are mostly syntax, but seems to fit better at a slightly higher abstraction level.
// They could for the most case be included when parsing with nom, but splitting responsibilities makes it easer to read, write and maintain the source code at the expense that well formed checks could be "forgotten".
// However the data-structure does allows to store data that makes no sense so using this for sanity check makes sense in any case.

use std::collections::HashMap;

use crate::{
    cerl_parser::ast::{FunHead, Module},
    st_parser::ast::{SessionElement, SessionType},
};

use super::compute_init_env::FunEnv;

// TODO high and low level test of check_wf_st_t

// cerl parsed module
// init_env includes parsed session types
pub fn check_wf(m: Module, env: &HashMap<FunHead, FunEnv>) -> Result<(), String> {
    // Check no ST has elements after "end."
    for (_, elm) in env {
        match &elm.session {
            Some(session) => check_wf_st_t(&session.st)?,
            None => (),
        }
    }

    // Check all binders refer to variables that are defined in the function body.
    // Remember to exclude function argument variables... (those names are automatically generated and should not be used)

    // TODO: keywords check? Maybe it would sense if labels could not be named "new" or "ongoing"

    Ok(())
}

fn check_wf_st_t(st: &Vec<SessionType>) -> Result<(), String> {
    for elm in st {
        match elm {
            SessionType::NotST => (),
            SessionType::New(inner) => check_wf_st_elm(inner)?,
            SessionType::Ongoing(_, _) => todo!(), // something like: Check first is ok, if last is .end, then the 2nd must be empty. Check 2nd one is ok.
        }
    }

    Ok(())
}

fn check_wf_st_elm(st: &Vec<SessionElement>) -> Result<(), String> {
    let mut end_seen = false;
    for elm in st {
        if end_seen {
            return Err(format!("Saw end before {:?}", elm));
        }
        match elm {
            SessionElement::Send(_) => (),
            SessionElement::Receive(_) => (),
            SessionElement::MakeChoice(_, mc_st) => check_wf_st_elm(mc_st)?,
            SessionElement::OfferChoice(oc) => {
                for (_, oc_em) in oc {
                    check_wf_st_elm(oc_em)?
                }
            }
            SessionElement::End => {
                end_seen = true;
            }
        }
    }

    Ok(())
}

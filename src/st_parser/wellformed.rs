use std::{collections::HashSet, rc::Rc};

use crate::{
    cerl_parser::ast::{Atom, Var},
    contract_cerl::types::{SessionType, SessionTypesList},
};

// TODO: Wellformed add negative tests that ensure the well-formed function is captable of catching
// the types of wellformedness that is targeted. They should be negativ tests that can detect type
// of error precisely.

// Check session type is well-formed. The rust code does allow for invalid sessions to be stored
// Why? To make it easier to process. A precise AST representing the session type cannot use as
// many build in constructs in rust.
pub fn wellformed(mspec: bool, consume: bool, st: &SessionTypesList) -> Result<(), String> {
    // Rules to implement:
    // 1) Only send-receive (if not mspec) or receive-send (if mspec)
    // 2a) If mspec require state and no duplicates
    // 2b) If not mspec ensure no state is included
    // 3) All sessions must end with end or cut (only ok if consume=true)
    // 4) Correct use of recursion:
    // 4a) Recursion variable must exist when used
    // 5) Proper ending of session: Session type list ends after recursion var, end, cut or choices
    let mut must_see_rec_var: HashSet<Rc<Var>> = HashSet::new();
    let mut seen_state_labels: HashSet<Rc<Atom>> = HashSet::new();
    wellformed_aux(
        &mut must_see_rec_var,
        &mut seen_state_labels,
        true,
        mspec,
        consume,
        st.0.as_slice(),
    )?;
    if !must_see_rec_var.is_empty() {
        return Err(format!("Unbalanced rec var #1: {:?}", must_see_rec_var));
    }
    Ok(())
}

fn wellformed_aux(
    must_see_rec_var: &mut HashSet<Rc<Var>>,
    seen_state_labels: &mut HashSet<Rc<Atom>>,
    first: bool,
    mspec: bool,
    consume: bool,
    st: &[SessionType],
) -> Result<(), String> {
    let st = match st {
        [SessionType::End] => return Ok(()),
        [SessionType::Cut] if consume => return Ok(()),
        [SessionType::Var(var)] => {
            if !must_see_rec_var.contains(var) {
                return Err(format!(
                    "Rec var used without rec definition, var name is: {}",
                    var
                ));
            }
            must_see_rec_var.remove(var);
            return Ok(());
        }
        st => st,
    };
    let st = match st {
        [SessionType::State(state), st @ ..] if !first && mspec && !st.is_empty() => {
            if seen_state_labels.contains(state) {
                return Err(format!("Duplicate state label: {}", state));
            }
            seen_state_labels.insert(state.clone());
            st
        }
        _ if !first && mspec => return Err(format!("Expected state here: {:?}", st)),
        _ => st,
    };
    match st {
        [SessionType::Send(_), SessionType::Receive(_), tail @ ..]
            if !mspec && !tail.is_empty() =>
        {
            wellformed_aux(
                must_see_rec_var,
                seen_state_labels,
                false,
                mspec,
                consume,
                tail,
            )
        }
        [SessionType::Receive(_), SessionType::Send(_), tail @ ..] if mspec && !tail.is_empty() => {
            wellformed_aux(
                must_see_rec_var,
                seen_state_labels,
                false,
                mspec,
                consume,
                tail,
            )
        }
        [SessionType::Choice(_, offers)] => {
            // Save the starting point for rec vars
            let branch_must_see_rec_var_start = must_see_rec_var.clone();
            for offer in offers.values() {
                // Rec var must be seen in branch, clone here:
                let mut branch_must_see_rec_var = branch_must_see_rec_var_start.clone();
                wellformed_aux(
                    &mut branch_must_see_rec_var,
                    seen_state_labels,
                    false,
                    mspec,
                    consume,
                    offer.0.as_slice(),
                )?;
                // Find new rec var definitions
                let must_see_rec_var_diff: HashSet<Rc<Var>> = branch_must_see_rec_var
                    .difference(&branch_must_see_rec_var_start)
                    .cloned()
                    .collect();
                // There must not be any
                if !must_see_rec_var_diff.is_empty() {
                    return Err("Unbalanced rec var #2".to_string());
                }
                // Be sure to remove seen vars from the master set
                let seen_rec_vars: HashSet<Rc<Var>> = branch_must_see_rec_var_start
                    .difference(&branch_must_see_rec_var)
                    .cloned()
                    .collect();
                // Remove these from the master set
                for seen_rec_var in seen_rec_vars {
                    must_see_rec_var.remove(&seen_rec_var);
                }
            }
            Ok(())
        }
        [SessionType::Rec(var), tail @ ..] if !tail.is_empty() => {
            if must_see_rec_var.contains(var) {
                return Err(format!("Shadowing var {} in recursion not supported", var));
            }
            must_see_rec_var.insert(var.clone());
            wellformed_aux(
                must_see_rec_var,
                seen_state_labels,
                true,
                mspec,
                consume,
                tail,
            )
        }
        _ => Err(format!("Not wellformed session: {:?}", st)),
    }
}

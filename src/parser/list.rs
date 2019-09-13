use super::util;
use super::util::GatherMode;

pub fn build_list<'a, E, F>(
    slice: &'a [u8],
    includes_brackets: bool,
    mut with_each: F,
) -> Result<(), E>
where
    F: FnMut(&'a [u8]) -> Result<(), E>,
{
    let mut i = if includes_brackets { 1 } else { 0 };
    loop {
        if i >= slice.len() {
            if includes_brackets {
                panic!("ERROR_TODO: Missing ending of list");
            } else {
                return Ok(());
            }
        }
        let (raw_entity, was_on) =
            util::gather_to(GatherMode::NonBreaking, &slice[i..], &[b',', b']']);

        // This is the last element of the list
        if was_on == b']' || was_on == 0 {
            let trimmed = util::trim(&raw_entity[0..raw_entity.len() - 1]);
            with_each(trimmed)?;
            break;

        // Skip spaces and empty
        } else if raw_entity.is_empty() {
            i += 1;
            continue;

        // Unexpected end on gather, must be something wrong. Could be EOF
        } else if ![b'\'', b'"', b','].contains(&was_on) {
            panic!(
                "ERROR_TODO: Invalid char in list: {:?} as char: {:?}",
                was_on, was_on as char
            );

        // Alright all seems okay lets continue like normal
        } else {
            i += raw_entity.len();
        }

        /*
        // Dirty hack for fixing strings breaking in lists
        // TODO: Figure out a better way
        if was_on != b',' {
            i += util::count_to(&slice[i..], b',');
        }
        */

        let trimmed = util::trim(raw_entity);
        with_each(trimmed)?;
    }
    Ok(())
}

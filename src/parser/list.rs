use super::util;

pub fn build_list<'a, E, F>(slice: &'a [u8], mut with_each: F) -> Result<(), E>
where
    F: FnMut(&'a [u8]) -> Result<(), E>,
{
    let mut i = 1;
    loop {
        if i > slice.len() {
            panic!("ERROR_TODO: Missing ending of list");
        }
        let (raw_entity, was_on) = util::gather_to(&slice[i..], &[b',', b']']);
        if was_on == b']' {
            let trimmed = util::trim(&raw_entity[0..raw_entity.len() - 1]);
            with_each(trimmed)?;
            break;
        } else if raw_entity.is_empty() {
            i += 1;
            continue;
        } else if ![b'\'', b'"', b',', b']'].contains(&was_on) {
            panic!("ERROR_TODO: Invalid char in list: {:?}", was_on as char);
        } else {
            i += raw_entity.len();
        }

        let trimmed = util::trim(raw_entity);
        with_each(trimmed)?;
    }
    Ok(())
}

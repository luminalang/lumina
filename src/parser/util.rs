pub fn gather_to<'a>(buf: &'a [u8], mut to: &[u8]) -> (&'a [u8], u8) {
    let mut append = false;
    let mut index = 0;
    loop {
        if index >= buf.len() {
            return (trim(&buf[0..index]), 0);
        }
        let c = buf[index];

        // We aren't supposed to stop at the ordinary stoppers if it's a string, list or byte literal
        {
            if c == b'\'' && !append {
                to = &[b'\''];
                append = true;
                index += 1;
                continue;
            }
            if c == b'"' && !append {
                to = &[b'"'];
                append = true;
                index += 1;
                continue;
            }
            if c == b'[' && !append {
                to = &[b']'];
                append = true;
                index += 1;
                continue;
            }
        }

        // Edge-case for `a` in `(a b c)`
        if c == b'(' && index == 0 {
            return (b"(", b'(');
        }
        if to.contains(&c) {
            if c == b']' {
                index += 1;
            }
            if c == b'"' {
                index += 1;
            }
            return (&buf[0..index], c);
        }
        index += 1;
    }
}

pub fn count_to(buf: &[u8], to: u8) -> usize {
    let mut count = 0;
    loop {
        let c = buf.get(count);
        if c == Some(&to) || c == None {
            return count;
        }
        count += 1;
    }
}

pub fn trim(buf: &[u8]) -> &[u8] {
    let mut left = 0;
    let mut right = 0;
    if buf.is_empty() {
        return buf;
    }

    let mut i = 0;
    loop {
        if buf[i] == b' ' {
            left += 1;
            i += 1;
        } else {
            break;
        }
    }
    let mut i = buf.len() - 1;
    loop {
        if buf[i] == b' ' {
            right += 1;
            i -= 1;
        } else {
            break;
        }
    }
    &buf[left..buf.len() - right]
}

pub fn prt(buf: &[u8]) {
    println!(
        "{:?}",
        buf.iter().map(|a| *a as char).collect::<Vec<char>>(),
    )
}

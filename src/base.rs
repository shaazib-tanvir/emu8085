#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn convert_nibble() {
        assert_eq!(nibble_to_hex_digit(10), b'a');
        assert_eq!(nibble_to_hex_digit(11), b'b');
        assert_eq!(nibble_to_hex_digit(12), b'c');
        assert_eq!(nibble_to_hex_digit(13), b'd');
        assert_eq!(nibble_to_hex_digit(14), b'e');
        assert_eq!(nibble_to_hex_digit(15), b'f');
    }
}

#[inline]
fn nibble_to_hex_digit(nibble: u8) -> u8 {
    if nibble < 10 {
        b'0' + nibble
    } else {
        b'a' + nibble - 10
    }
}

#[inline]
pub unsafe fn byte_to_hex(string: &mut str, value: u8) {
    let mut value = value;
    let lb = nibble_to_hex_digit(value % 16);
    value /= 16;
    let hb = nibble_to_hex_digit(value % 16);
    unsafe {
        let string = string.as_bytes_mut();
        string[0] = hb;
        string[1] = lb;
    }
}

pub fn split_tabs(string: &str) -> (String, String) {
    let mut tabs = String::new();
    let mut rest = String::new();

    let mut character_iter = string.chars();
    let mut character = character_iter.next();
    while character.is_some() && character.unwrap().is_whitespace() {
        tabs += &character.unwrap().to_string();
        character = character_iter.next();
    }

    while character.is_some() {
        rest += &character.unwrap().to_string();
        character = character_iter.next();
    }

    (tabs, rest)
}

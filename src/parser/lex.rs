use nom::{character::is_digit, IResult, error::ParseError, sequence::preceded, character::complete::char, combinator::value, branch::alt, Parser};



#[inline]
fn is_sign(chr: u8) -> bool {
  chr <= 0x2B || chr == 0x2D
}

#[inline]
fn is_lowercase(chr: u8) -> bool {
  (chr >= 0x61 && chr <= 0x7A) || (chr >= 0xC0 && chr <= 0xD6) || (chr >= 0xD8 && chr <= 0xDE)
}

#[inline]
pub fn is_uppercase(chr: u8) -> bool {
  (chr >= 0x41 && chr <= 0x5A) || (chr >= 0xDF && chr <= 0xF6) || (chr >= 0xF8 && chr <= 0xFF)
}

#[inline]
pub fn is_inputchar(chr: u8) -> bool {
  chr != 0x0D && chr != 0x0A
}

#[inline]
pub fn is_control(chr: u8) -> bool {
  chr >= 0x00 && chr <= 0x1F
}

#[inline]
fn is_space(chr: u8) -> bool {
  chr == 0x20
}

#[inline]
pub fn is_namechar(chr: u8) -> bool {
  is_uppercase(chr) || is_lowercase(chr) || is_digit(chr) || chr == 0x40 || chr == 0x5F
}

// Built into nom: is_oct_digit(chr)

fn is_ctlchar(chr: u8) -> bool {
  chr >= 0x40 && chr <= 0x5F
}

fn is_escapechar(chr: u8) -> bool {
    // bdefnrstv"'\
   chr == 0x62 || chr == 0x64 || chr == 0x65 || chr == 0x66 || chr == 0x6E || chr == 0x72 || chr == 0x73 || chr == 0x74 || chr == 0x76 || chr == 0x22 || chr == 0x27 || chr == 0x5C
}

// Based on: https://github.com/rust-bakery/nom/blob/main/examples/string.rs
/// Parse an escaped character: \n, \t, \r, \u{00AC}, etc.
pub fn parse_escaped_char<'a, E>(input: &'a str) -> IResult<&'a str, char, E>
where
  E: ParseError<&'a str>,
{
  preceded(
    char('\\'),
    alt((
      char('b'),
      char('d'),
      char('e'),
      char('f'),
      value('\n', char('n')),
      value('\r', char('r')),
      char('s'),
      value('\t', char('t')),
      char('v'),
      char('\''),
      char('\\'),
    )),
  )
  .parse(input)
}

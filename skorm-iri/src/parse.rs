use super::ty::{IntoIri, IriRanges};
use locate::InputLocate;
use locate::WithPos;
use nom::{
  branch::alt,
  bytes::complete::tag,
  character::complete::{alpha1, char, digit0, digit1, hex_digit1},
  combinator::{map, opt},
  error::{convert_error, ErrorKind, ParseError, VerboseError},
  multi::{count, many0, many1, many_m_n},
  sequence::{preceded, terminated, tuple},
  AsChar, Compare, Err, IResult, InputIter, InputLength, InputTake, InputTakeAtPosition, Slice,
};
use std::ops::{Range, RangeFrom};

pub mod error;
mod locate;

struct Spanned<T> {
  start: usize,
  end: usize,
  value: T,
}

impl<T> Spanned<T> {
  #[inline]
  fn len(&self) -> usize {
    self.end - self.start
  }

  #[inline]
  fn with_value<U>(&self, value: U) -> Spanned<U> {
    Spanned {
      start: self.start,
      end: self.end,
      value,
    }
  }

  #[inline]
  fn input<U: InputTake>(&self, input: &U) -> Spanned<U> {
    Spanned {
      start: self.start,
      end: self.end,
      value: input.take(self.len()),
    }
  }

  fn into_range(self) -> Range<usize> {
    self.start..self.end
  }
}

trait IResultExt<I, T, E: ParseError<I>>: Sized {
  fn add_err_context(self, input: I, context: &'static str) -> Self;
}

impl<I, T, E: ParseError<I>> IResultExt<I, T, E> for IResult<I, T, E> {
  #[inline]
  fn add_err_context(self, input: I, context: &'static str) -> Self {
    match self {
      Ok(o) => Ok(o),
      Err(Err::Incomplete(i)) => Err(Err::Incomplete(i)),
      Err(Err::Error(e)) => Err(Err::Error(E::add_context(input, context, e))),
      Err(Err::Failure(e)) => Err(Err::Failure(E::add_context(input, context, e))),
    }
  }
}

// fn position<T, E: ParseError<T>>(input: T) -> IResult<T, usize, E>
// where
//   T: InputLocate,
// {
//   let offset = input.offset();
//   Ok((input, offset))
// }

fn spanned<I, T, E: ParseError<I>>(
  parser: impl Fn(I) -> IResult<I, T, E>,
) -> impl Fn(I) -> IResult<I, Spanned<T>, E>
where
  I: InputLocate,
{
  move |input| {
    let start = input.offset();
    match parser(input) {
      Ok((i, value)) => {
        let end = i.offset();
        Ok((i, Spanned { start, end, value }))
      }
      Err(e) => Err(e),
    }
  }
}

fn ignore<I, T, E: ParseError<I>>(
  parser: impl Fn(I) -> IResult<I, T, E>,
) -> impl Fn(I) -> IResult<I, (), E> {
  move |input| match parser(input) {
    Ok((i, _)) => Ok((i, ())),
    Err(e) => Err(e),
  }
}

fn exact<I: InputLength + Clone, T, E: ParseError<I>>(
  parser: impl Fn(I) -> IResult<I, T, E>,
) -> impl Fn(I) -> IResult<I, T, E> {
  move |input| {
    let cloned = input.clone();
    match parser(input) {
      Ok((i, v)) => {
        if i.input_len() == 0 {
          Ok((i, v))
        } else {
          Err(Err::Error(E::from_error_kind(i, ErrorKind::NonEmpty)))
        }
      }
      Err(e) => Err(e).add_err_context(cloned, "exact"),
    }
  }
}

macro_rules! between {
  ($chr:expr, $low:expr => $hi:expr,) => {
    if $chr >= $low && $chr <= $hi {
      true
    } else {
      false
    }
  };
  ($chr:expr, $low:expr => $hi:expr, $($restlow:expr => $resthi:expr,)+) => {
    if $chr >= $low && $chr <= $hi {
      true
    } else { between!($chr, $($restlow => $resthi,)+) }
  };
}

macro_rules! char_parsers {
  (mod $name:ident($item:ident: impl AsChar) -> bool $body:block) => {
    #[allow(dead_code)]
    mod $name {
      use super::*;

      pub(super) fn is($item: impl AsChar) -> bool $body
      #[inline]
      pub(super) fn is_not(item: impl AsChar) -> bool {
        !is(item)
      }

      pub(super) fn single<T, E: ParseError<T>>(input: T) -> IResult<T, char, E>
      where
        T: Slice<RangeFrom<usize>> + InputIter + Clone,
        <T as InputIter>::Item: AsChar,
      {
        match input.iter_elements().next().map(|t| {
          let c = t.as_char();
          let b = is(c);
          (c, b)
        }) {
          Some((c, true)) => Ok((input.slice(c.len()..), c)),
          _ => Err(
            Err::Error(
              E::add_context(
                input.clone(),
                stringify!($name::single),
                E::from_error_kind(input, ErrorKind::NoneOf))
              )
            )
        }
      }

      pub(super) fn many0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
      where
        T: InputTakeAtPosition + InputLength,
        <T as InputTakeAtPosition>::Item: AsChar,
      {
        input.split_at_position_complete(is_not)
          .add_err_context(input, stringify!($name::many0))
      }

      pub(super) fn many1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
      where
        T: InputTakeAtPosition + InputLength,
        <T as InputTakeAtPosition>::Item: AsChar,
      {
        input.split_at_position1_complete(is_not, ErrorKind::NoneOf)
          .add_err_context(input, stringify!($name::many1))
      }
    }
  };
}

macro_rules! parse_between {
  ($name:ident, $($restlow:expr => $resthi:expr,)+) => {
    char_parsers! {
      mod $name(item: impl AsChar) -> bool {
        let chr = item.as_char();
        between!(chr, $($restlow => $resthi,)+)
      }
    }
  };
}

parse_between!(iprivate,
  '\u{E000}' => '\u{F8FF}',
  '\u{F0000}' => '\u{FFFFD}',
  '\u{100000}' => '\u{10FFFD}',
);

parse_between!(ucschar,
  '\u{A0}' => '\u{D7FF}',
  '\u{F900}' => '\u{FDCF}',
  '\u{FDF0}' => '\u{FFEF}',
  '\u{10000}' => '\u{1FFFD}',
  '\u{20000}' => '\u{2FFFD}',
  '\u{30000}' => '\u{3FFFD}',
  '\u{40000}' => '\u{4FFFD}',
  '\u{50000}' => '\u{5FFFD}',
  '\u{60000}' => '\u{6FFFD}',
  '\u{70000}' => '\u{7FFFD}',
  '\u{80000}' => '\u{8FFFD}',
  '\u{90000}' => '\u{9FFFD}',
  '\u{A0000}' => '\u{AFFFD}',
  '\u{B0000}' => '\u{BFFFD}',
  '\u{C0000}' => '\u{CFFFD}',
  '\u{D0000}' => '\u{DFFFD}',
  '\u{E1000}' => '\u{EFFFD}',
);

char_parsers! {
  mod iunreserved(item: impl AsChar) -> bool {
    let c = item.as_char();
    if c.is_alphanum() {
      true
    } else {
      match c {
        '-' | '.' | '_' | '~' => true,
        c => ucschar::is(c)
      }
    }
  }
}

fn pct_encoded<T, E: ParseError<T>>(input: T) -> IResult<T, u8, E>
where
  T: Slice<RangeFrom<usize>> + InputIter + Clone,
  <T as InputIter>::Item: AsChar,
{
  #[inline]
  fn hex_digit(c: char) -> u8 {
    c.to_digit(16).unwrap() as u8
  }

  let mut iter = input
    .iter_indices()
    .map(|(pos, c)| (pos, AsChar::as_char(c)));
  let result = match iter.next() {
    Some((_, '%')) => match iter.next() {
      Some((_, c1)) if c1.is_hex_digit() => match iter.next() {
        Some((pos, c2)) if c2.is_hex_digit() => {
          let pos = pos + c2.len();
          let n1 = hex_digit(c1);
          let n2 = hex_digit(c2);
          Some((pos, (n1 << 4) + n2))
        }
        _ => None,
      },
      _ => None,
    },
    _ => None,
  };

  match result {
    Some((pos, num)) => Ok((input.slice(pos..), num)),
    None => Err(Err::Error(E::from_error_kind(
      input.clone(),
      ErrorKind::Tag,
    )))
    .add_err_context(input, "pct_encoded"),
  }
}

char_parsers! {
  mod sub_delims(item: impl AsChar) -> bool {
    let c = item.as_char();
    match c {
      '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | ';' | '=' => true,
      _ => false,
    }
  }
}

char_parsers! {
  mod gen_delims(item: impl AsChar) -> bool {
    let c = item.as_char();
    match c {
      ':' | '/' | '?' | '#' | '[' | ']' | '@' => true,
      _ => false,
    }
  }
}

char_parsers! {
  // sub_delims | gen_delims
  mod reserved(item: impl AsChar) -> bool {
    let c = item.as_char();
    match c {
      '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | ';'
      | '=' | ':' | '/' | '?' | '#' | '[' | ']' | '@' => true,
      _ => false,
    }
  }
}

char_parsers! {
  mod unreserved(item: impl AsChar) -> bool {
    let c = item.as_char();
    if c.is_alphanum() {
      true
    } else {
      match c {
        '-' | '.' | '_' | '~' => true,
        _ => false,
      }
    }
  }
}

fn dec_octet<T, E: ParseError<T>>(input: T) -> IResult<T, u8, E>
where
  T: Slice<RangeFrom<usize>> + InputIter + Clone,
  <T as InputIter>::Item: AsChar,
{
  let mut iter = input.iter_indices().map(|(i, c)| (i, c.as_char()));
  let mut pos = 0;
  let mut num = 0;
  loop {
    if let Some((i, c)) = iter.next() {
      if c.is_dec_digit() {
        let digit = c.to_digit(10).unwrap();
        let new_num = num * 10 + digit;
        if new_num < 256 {
          num = new_num;
          pos = i + c.len();
          if new_num == 0 {
            break;
          }
        } else {
          break;
        }
      } else {
        break;
      }
    } else {
      break;
    }
  }

  if pos > 0 {
    Ok((input.slice(pos..), num as u8))
  } else {
    Err(Err::Error(E::from_error_kind(
      input.clone(),
      ErrorKind::Digit,
    )))
    .add_err_context(input, "dec_octet")
  }
}

fn ip4_address<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
  T: Slice<RangeFrom<usize>> + InputTake + InputIter + InputLocate + Clone,
  <T as InputIter>::Item: AsChar,
{
  let cloned = input.clone();
  let dot = |i| char('.')(i);
  match spanned(tuple((
    dec_octet, dot, dec_octet, dot, dec_octet, dot, dec_octet,
  )))(input)
  {
    Ok((rest, spanned)) => Ok((rest, cloned.take(spanned.len()))),
    Err(e) => Err(e).add_err_context(cloned, "ip4_address"),
  }
}

fn h16<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
  T: InputTake + InputIter + Clone,
  <T as InputIter>::Item: AsChar,
{
  let cloned = input.clone();
  match input
    .iter_indices()
    .map(|(i, c)| (i, c.as_char()))
    .take_while(|(_, c)| c.is_hex_digit())
    .take(4)
    .last()
  {
    Some((i, c)) => {
      let pos = i + c.len();
      Ok(cloned.take_split(pos))
    }
    None => Err(Err::Error(E::from_error_kind(
      input.clone(),
      ErrorKind::HexDigit,
    )))
    .add_err_context(input, "h16"),
  }
}

fn ls32<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
  T: Slice<RangeFrom<usize>> + InputIter + InputLocate + InputTake + Clone,
  <T as InputIter>::Item: AsChar,
{
  fn h32<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
  where
    T: Slice<RangeFrom<usize>> + InputIter + InputLocate + InputTake + Clone,
    <T as InputIter>::Item: AsChar,
  {
    let cloned = input.clone();
    match spanned(tuple((h16, char(':'), h16)))(input) {
      Ok((rest, spanned)) => Ok((rest, cloned.take(spanned.len()))),
      Err(e) => Err(e).add_err_context(cloned, "ls32::h32"),
    }
  }

  let cloned = input.clone();
  alt((h32, ip4_address))(input).add_err_context(cloned, "ls32")
}

//   IPv6address    =                            6( h16 ":" ) ls32
//                  /                       "::" 5( h16 ":" ) ls32
//                  / [               h16 ] "::" 4( h16 ":" ) ls32
//                  / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
//                  / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
//                  / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
//                  / [ *4( h16 ":" ) h16 ] "::"              ls32
//                  / [ *5( h16 ":" ) h16 ] "::"              h16
//                  / [ *6( h16 ":" ) h16 ] "::"
fn ip6_address<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
  T: Slice<RangeFrom<usize>>
    + InputIter
    + InputLocate
    + InputTake
    + Clone
    + PartialEq
    + Compare<&'static str>,
  <T as InputIter>::Item: AsChar,
{
  // helpers
  let h16colon = |input| ignore(preceded(h16, char(':')))(input);
  let dbl_col = |input| tag("::")(input);
  let max = |f, n| many_m_n(0, n, f);

  #[rustfmt::skip]
  let addr = {
    // lines in spec
    let l1 = ignore(tuple((count(h16colon, 6), ls32)));
    let l2 = ignore(tuple((dbl_col, count(h16colon, 5), ls32)));
    let l3 = ignore(tuple((opt(h16), dbl_col, count(h16colon, 4), ls32)));
    let l4 = ignore(tuple((opt(tuple((max(h16colon, 1), h16))), dbl_col, count(h16colon, 3), ls32)));
    let l5 = ignore(tuple((opt(tuple((max(h16colon, 2), h16))), dbl_col, count(h16colon, 2), ls32)));
    let l6 = ignore(tuple((opt(tuple((max(h16colon, 3), h16))), dbl_col, h16colon, ls32)));
    let l7 = ignore(tuple((opt(tuple((max(h16colon, 4), h16))), dbl_col, ls32)));
    let l8 = ignore(tuple((opt(tuple((max(h16colon, 5), h16))), dbl_col, h16)));
    let l9 = ignore(tuple((opt(tuple((max(h16colon, 6), h16))), dbl_col)));

    // ipv6 address
    alt((l1, l2, l3, l4, l5, l6, l7, l8, l9))
  };

  let cloned = input.clone();
  match spanned(addr)(input) {
    Ok((i, spanned)) => Ok((i, cloned.take(spanned.len()))),
    Err(e) => Err(e).add_err_context(cloned, "ip6_address"),
  }
}

fn ipvfuture_address<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
  T: Slice<RangeFrom<usize>>
    + InputIter
    + InputLocate
    + InputLength
    + InputTake
    + InputTakeAtPosition
    + Clone
    + PartialEq
    + Compare<&'static str>,
  <T as InputIter>::Item: AsChar,
  <T as InputTakeAtPosition>::Item: AsChar,
{
  let p1 = char('v');
  let p2 = hex_digit1;
  let p3 = char('.');
  let p4_alt = alt((
    ignore(unreserved::many1),
    ignore(sub_delims::many1),
    ignore(char(':')),
  ));
  let p4 = many_m_n(1, std::usize::MAX, p4_alt);
  let full = ignore(tuple((p1, p2, p3, p4)));
  let cloned = input.clone();
  match spanned(full)(input) {
    Ok((i, spanned)) => Ok((i, cloned.take(spanned.len()))),
    Err(e) => Err(e).add_err_context(cloned, "ipvfuture_address"),
  }
}

fn ip_literal<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
  T: Slice<RangeFrom<usize>>
    + InputIter
    + InputLocate
    + InputLength
    + InputTake
    + InputTakeAtPosition
    + Clone
    + PartialEq
    + Compare<&'static str>,
  <T as InputIter>::Item: AsChar,
  <T as InputTakeAtPosition>::Item: AsChar,
{
  let inner = alt((ip6_address, ipvfuture_address));
  let outer = tuple((char('['), inner, char(']')));

  let cloned = input.clone();
  match spanned(outer)(input) {
    Ok((i, spanned)) => Ok((i, cloned.take(spanned.len()))),
    Err(e) => Err(e).add_err_context(cloned, "ip_literal"),
  }
}

fn port<T, E: ParseError<T>>(input: T) -> IResult<T, Spanned<T>, E>
where
  T: InputTakeAtPosition + InputLocate + Clone,
  <T as InputTakeAtPosition>::Item: AsChar,
{
  let cloned = input.clone();
  spanned(digit0)(input).add_err_context(cloned, "port")
}

fn scheme<T, E: ParseError<T>>(input: T) -> IResult<T, Spanned<T>, E>
where
  T: Slice<RangeFrom<usize>>
    + InputIter
    + InputLocate
    + InputLength
    + InputTake
    + InputTakeAtPosition
    + Clone
    + PartialEq
    + std::fmt::Debug
    + Compare<&'static str>,
  <T as InputIter>::Item: AsChar,
  <T as InputTakeAtPosition>::Item: AsChar,
{
  let c2 = alt((
    ignore(alpha1),
    ignore(digit1),
    ignore(char('+')),
    ignore(char('-')),
    ignore(char('.')),
  ));
  let mult = ignore(many0(c2));
  let full = tuple((alpha1, mult));

  let cloned = input.clone();
  match spanned(full)(input) {
    Ok((i, spanned)) => Ok((i, spanned.with_value(cloned.take(spanned.len())))),
    Err(e) => Err(e).add_err_context(cloned, "scheme"),
  }
}

#[allow(dead_code)]
mod ipchar {
  use super::*;

  char_parsers! {
    mod ipchar_chars(item: impl AsChar) -> bool {
      let c = item.as_char();

      iunreserved::is(c) || sub_delims::is(c) || c == ':' || c == '@'
    }
  }

  fn pct<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
  where
    T: Slice<RangeFrom<usize>> + InputIter + InputTake + InputLocate + Clone,
    <T as InputIter>::Item: AsChar,
  {
    let cloned = input.clone();
    match spanned(pct_encoded)(input) {
      Ok((i, spanned)) => Ok((i, cloned.take(spanned.len()))),
      Err(e) => Err(e),
    }
  }

  pub(super) fn single<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
  where
    T: Slice<RangeFrom<usize>> + InputIter + InputTake + InputLocate + Clone,
    <T as InputIter>::Item: AsChar,
  {
    let cloned = input.clone();
    let ip_char = move |input| match ipchar_chars::single(input) {
      Ok((i, _)) => Ok((i, cloned.take(1))),
      Err(e) => Err(e),
    };

    let cloned = input.clone();
    alt((ip_char, pct))(input).add_err_context(cloned, "ipchar::single")
  }

  pub(super) fn many0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
  where
    T: Slice<RangeFrom<usize>>
      + InputTakeAtPosition
      + InputIter
      + InputLength
      + InputTake
      + InputLocate
      + Clone
      + PartialEq
      + std::fmt::Debug,
    <T as InputTakeAtPosition>::Item: AsChar,
    <T as InputIter>::Item: AsChar,
  {
    let inner = alt((ipchar_chars::many1, pct));
    let cloned = input.clone();
    match spanned(super::many0(inner))(input) {
      Ok((i, spanned)) => Ok((i, cloned.take(spanned.len()))),
      Err(e) => Err(e).add_err_context(cloned, "ipchar::many0"),
    }
  }

  pub(super) fn many1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
  where
    T: Slice<RangeFrom<usize>>
      + InputTakeAtPosition
      + InputIter
      + InputLength
      + InputTake
      + InputLocate
      + Clone
      + PartialEq,
    <T as InputTakeAtPosition>::Item: AsChar,
    <T as InputIter>::Item: AsChar,
  {
    let inner = alt((ipchar_chars::many1, pct));
    let cloned = input.clone();
    match spanned(super::many1(inner))(input) {
      Ok((i, spanned)) => Ok((i, cloned.take(spanned.len()))),
      Err(e) => Err(e).add_err_context(cloned, "ipchar::many1"),
    }
  }
}

fn ifragment<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
  T: Slice<RangeFrom<usize>>
    + InputTakeAtPosition
    + InputIter
    + InputLength
    + InputTake
    + InputLocate
    + Clone
    + PartialEq
    + std::fmt::Debug,
  <T as InputTakeAtPosition>::Item: AsChar,
  <T as InputIter>::Item: AsChar,
{
  let inner = alt((ignore(ipchar::many1), ignore(char('/')), ignore(char('?'))));

  let cloned = input.clone();
  match spanned(many0(inner))(input) {
    Ok((i, spanned)) => Ok((i, cloned.take(spanned.len()))),
    Err(e) => Err(e).add_err_context(cloned, "ifragment"),
  }
}

fn iquery<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
  T: Slice<RangeFrom<usize>>
    + InputTakeAtPosition
    + InputIter
    + InputLength
    + InputTake
    + InputLocate
    + Clone
    + PartialEq
    + std::fmt::Debug,
  <T as InputTakeAtPosition>::Item: AsChar,
  <T as InputIter>::Item: AsChar,
{
  let inner = alt((
    ignore(ipchar::many1),
    ignore(iprivate::many1),
    ignore(char('/')),
    ignore(char('?')),
  ));

  let cloned = input.clone();
  match spanned(many0(inner))(input) {
    Ok((i, spanned)) => Ok((i, cloned.take(spanned.len()))),
    Err(e) => Err(e).add_err_context(cloned, "iquery"),
  }
}

// // non-zero-length segment without any colon ":"
// fn isegment_nz_nc<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
// where
//   T: Slice<RangeFrom<usize>>
//     + InputTakeAtPosition
//     + InputIter
//     + InputLength
//     + InputTake
//     + InputLocate
//     + Clone
//     + PartialEq,
//   <T as InputTakeAtPosition>::Item: AsChar,
//   <T as InputIter>::Item: AsChar,
// {
//   let inner = alt((
//     ignore(iunreserved::many1),
//     ignore(pct_encoded),
//     ignore(sub_delims::many1),
//     ignore(char('@')),
//   ));

//   let cloned = input.clone();
//   match spanned(many1(inner))(input) {
//     Ok((i, spanned)) => Ok((i, cloned.take(spanned.len()))),
//     Err(e) => Err(e),
//   }
// }

fn isegment_nz<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
  T: Slice<RangeFrom<usize>>
    + InputTakeAtPosition
    + InputIter
    + InputLength
    + InputTake
    + InputLocate
    + Clone
    + PartialEq,
  <T as InputTakeAtPosition>::Item: AsChar,
  <T as InputIter>::Item: AsChar,
{
  let cloned = input.clone();
  ipchar::many1(input).add_err_context(cloned, "isegment_nz")
}

fn isegment<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
  T: Slice<RangeFrom<usize>>
    + InputTakeAtPosition
    + InputIter
    + InputLength
    + InputTake
    + InputLocate
    + Clone
    + PartialEq
    + std::fmt::Debug,
  <T as InputTakeAtPosition>::Item: AsChar,
  <T as InputIter>::Item: AsChar,
{
  let cloned = input.clone();
  ipchar::many0(input).add_err_context(cloned, "isegment")
}

fn ipath_empty<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
  T: InputTake,
{
  let result = input.take(0);
  Ok((input, result))
}

fn ipath_rootless<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
  T: Slice<RangeFrom<usize>>
    + InputTakeAtPosition
    + InputIter
    + InputLength
    + InputTake
    + InputLocate
    + Clone
    + PartialEq
    + std::fmt::Debug,
  <T as InputTakeAtPosition>::Item: AsChar,
  <T as InputIter>::Item: AsChar,
{
  let inner = tuple((char('/'), isegment));

  let full = tuple((isegment_nz, many0(inner)));
  let cloned = input.clone();
  match spanned(full)(input) {
    Ok((i, spanned)) => Ok((i, cloned.take(spanned.len()))),
    Err(e) => Err(e).add_err_context(cloned, "ipath_rootless"),
  }
}

// fn ipath_noscheme<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
// where
//   T: Slice<RangeFrom<usize>>
//     + InputTakeAtPosition
//     + InputIter
//     + InputLength
//     + InputTake
//     + InputLocate
//     + Clone
//     + PartialEq
//     + std::fmt::Debug,
//   <T as InputTakeAtPosition>::Item: AsChar,
//   <T as InputIter>::Item: AsChar,
// {
//   let inner = tuple((char('/'), isegment));

//   let full = tuple((isegment_nz_nc, many0(inner)));
//   let cloned = input.clone();
//   match spanned(full)(input) {
//     Ok((i, spanned)) => Ok((i, cloned.take(spanned.len()))),
//     Err(e) => Err(e),
//   }
// }

fn ipath_absolute<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
  T: Slice<RangeFrom<usize>>
    + InputTakeAtPosition
    + InputIter
    + InputLength
    + InputTake
    + InputLocate
    + Clone
    + PartialEq
    + std::fmt::Debug,
  <T as InputTakeAtPosition>::Item: AsChar,
  <T as InputIter>::Item: AsChar,
{
  // "/" [ isegment-nz *( "/" isegment ) ]
  let inner = tuple((char('/'), isegment));
  let inner = tuple((isegment_nz, many0(inner)));
  let full = tuple((char('/'), opt(inner)));
  let cloned = input.clone();
  match spanned(full)(input) {
    Ok((i, spanned)) => Ok((i, cloned.take(spanned.len()))),
    Err(e) => Err(e).add_err_context(cloned, "ipath_absolute"),
  }
}

fn ipath_abempty<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
  T: Slice<RangeFrom<usize>>
    + InputTakeAtPosition
    + InputIter
    + InputLength
    + InputTake
    + InputLocate
    + Clone
    + PartialEq
    + std::fmt::Debug,
  <T as InputTakeAtPosition>::Item: AsChar,
  <T as InputIter>::Item: AsChar,
{
  // *( "/" isegment )
  let inner = tuple((char('/'), isegment));
  let cloned = input.clone();
  match spanned(many0(inner))(input) {
    Ok((i, spanned)) => Ok((i, cloned.take(spanned.len()))),
    Err(e) => Err(e).add_err_context(cloned, "ipath_abempty"),
  }
}

// fn ipath<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
// where
//   T: Slice<RangeFrom<usize>>
//     + InputTakeAtPosition
//     + InputIter
//     + InputLength
//     + InputTake
//     + InputLocate
//     + Clone
//     + PartialEq
//     + std::fmt::Debug,
//   <T as InputTakeAtPosition>::Item: AsChar,
//   <T as InputIter>::Item: AsChar,
// {
//   alt((
//     ipath_abempty,  // begins with "/" or is empty
//     ipath_absolute, // begins with "/" but not "//"
//     ipath_noscheme, // begins with a non-colon segment
//     ipath_rootless, // begins with a segment
//     ipath_empty,    // zero characters
//   ))(input)
// }

fn ireg_name<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
  T: Slice<RangeFrom<usize>>
    + InputTakeAtPosition
    + InputIter
    + InputLength
    + InputTake
    + InputLocate
    + Clone
    + PartialEq,
  <T as InputTakeAtPosition>::Item: AsChar,
  <T as InputIter>::Item: AsChar,
{
  let inner = alt((
    ignore(iunreserved::many1),
    ignore(pct_encoded),
    ignore(sub_delims::many1),
  ));

  let cloned = input.clone();
  match spanned(many0(inner))(input) {
    Ok((i, spanned)) => Ok((i, cloned.take(spanned.len()))),
    Err(e) => Err(e).add_err_context(cloned, "ireg_name"),
  }
}

fn ihost<T, E: ParseError<T>>(input: T) -> IResult<T, Spanned<T>, E>
where
  T: Slice<RangeFrom<usize>>
    + InputTakeAtPosition
    + InputIter
    + InputLength
    + InputTake
    + InputLocate
    + Clone
    + PartialEq
    + Compare<&'static str>,
  <T as InputTakeAtPosition>::Item: AsChar,
  <T as InputIter>::Item: AsChar,
{
  let cloned = input.clone();
  spanned(alt((ip_literal, ip4_address, ireg_name)))(input).add_err_context(cloned, "ihost")
}

fn iuserinfo<T, E: ParseError<T>>(input: T) -> IResult<T, Spanned<T>, E>
where
  T: Slice<RangeFrom<usize>>
    + InputTakeAtPosition
    + InputIter
    + InputLength
    + InputTake
    + InputLocate
    + Clone
    + PartialEq,
  <T as InputTakeAtPosition>::Item: AsChar,
  <T as InputIter>::Item: AsChar,
{
  let inner = alt((
    ignore(iunreserved::many1),
    ignore(pct_encoded),
    ignore(sub_delims::many1),
    ignore(char(':')),
  ));

  let cloned = input.clone();
  match spanned(many0(inner))(input) {
    Ok((i, spanned)) => Ok((i, spanned.input(&cloned))),
    Err(e) => Err(e).add_err_context(cloned, "iuserinfo"),
  }
}

struct Authority<T> {
  userinfo: Option<Spanned<T>>,
  host: Spanned<T>,
  port: Option<Spanned<T>>,
  value: Spanned<T>,
}

fn iauthority<T, E: ParseError<T>>(input: T) -> IResult<T, Authority<T>, E>
where
  T: Slice<RangeFrom<usize>>
    + InputTakeAtPosition
    + InputIter
    + InputLength
    + InputTake
    + InputLocate
    + Compare<&'static str>
    + Clone
    + PartialEq
    + std::fmt::Debug,
  <T as InputTakeAtPosition>::Item: AsChar,
  <T as InputIter>::Item: AsChar,
{
  // [ iuserinfo "@" ] ihost [ ":" port ]
  let userinfo = opt(terminated(iuserinfo, char('@')));
  let host = ihost;
  let port = opt(preceded(char(':'), port));
  let full = tuple((userinfo, host, port));

  let cloned = input.clone();
  match spanned(full)(input) {
    Ok((i, spanned)) => {
      let value = spanned.input(&cloned);
      let (userinfo, host, port) = spanned.value;
      let authority = Authority {
        userinfo,
        host,
        port,
        value,
      };
      Ok((i, authority))
    }

    Err(e) => Err(e).add_err_context(cloned, "iauthority"),
  }
}

// fn irelative_part<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
// where
//   T: Slice<RangeFrom<usize>>
//     + InputTakeAtPosition
//     + InputIter
//     + InputLength
//     + InputTake
//     + InputLocate
//     + Compare<&'static str>
//     + Clone
//     + PartialEq
//     + std::fmt::Debug,
//   <T as InputTakeAtPosition>::Item: AsChar,
//   <T as InputIter>::Item: AsChar,
// {
//   // "//" iauthority ipath-abempty
//   //    / ipath-absolute
//   let alt1 = ignore(tuple((tag("//"), iauthority, ipath_abempty)));
//   let alt2 = ignore(ipath_absolute);
//   let cloned = input.clone();

//   match spanned(alt((alt1, alt2)))(input) {
//     Ok((i, spanned)) => Ok((i, cloned.take(spanned.len()))),
//     Err(e) => Err(e),
//   }
// }

// fn irelative_ref<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
// where
//   T: Slice<RangeFrom<usize>>
//     + InputTakeAtPosition
//     + InputIter
//     + InputLength
//     + InputTake
//     + InputLocate
//     + Compare<&'static str>
//     + Clone
//     + PartialEq
//     + std::fmt::Debug,
//   <T as InputTakeAtPosition>::Item: AsChar,
//   <T as InputIter>::Item: AsChar,
// {
//   // irelative-part [ "?" iquery ] [ "#" ifragment ]
//   let query = opt(preceded(char('?'), spanned(iquery)));
//   let fragment = opt(preceded(char('#'), spanned(ifragment)));
//   let parser = tuple((irelative_part, query, fragment));

//   let cloned = input.clone();
//   match spanned(parser)(input) {
//     Ok((i, spanned)) => Ok((i, cloned.take(spanned.len()))),
//     Err(e) => Err(e),
//   }
// }

struct HierPart<T> {
  authority: Option<Authority<T>>,
  path: Spanned<T>,
  // value: Spanned<T>,
}

fn ihier_part<T, E: ParseError<T>>(input: T) -> IResult<T, HierPart<T>, E>
where
  T: Slice<RangeFrom<usize>>
    + InputTakeAtPosition
    + InputIter
    + InputLength
    + InputTake
    + InputLocate
    + Compare<&'static str>
    + Clone
    + PartialEq
    + std::fmt::Debug,
  <T as InputTakeAtPosition>::Item: AsChar,
  <T as InputIter>::Item: AsChar,
{
  // "//" iauthority ipath-abempty
  // / ipath-absolute
  // / ipath-rootless
  // / ipath-empty
  let alt1 = map(
    tuple((tag("//"), iauthority, spanned(ipath_abempty))),
    |(_, auth, path)| (Some(auth), path),
  );
  let alt2 = map(spanned(ipath_absolute), |path| (None, path));
  let alt3 = map(spanned(ipath_rootless), |path| (None, path));
  let alt4 = map(spanned(ipath_empty), |path| (None, path));
  let parser = alt((alt1, alt2, alt3, alt4));

  let cloned = input.clone();
  match spanned(parser)(input) {
    Ok((i, spanned)) => {
      // let value = spanned.input(&cloned);
      let (authority, path) = spanned.value;

      Ok((
        i,
        HierPart {
          authority,
          path,
          // value,
        },
      ))
    }
    Err(e) => Err(e).add_err_context(cloned, "ihier_part"),
  }
}

// fn absolute_iri<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
// where
//   T: Slice<RangeFrom<usize>>
//     + InputTakeAtPosition
//     + InputIter
//     + InputLength
//     + InputTake
//     + InputLocate
//     + Compare<&'static str>
//     + Clone
//     + PartialEq
//     + std::fmt::Debug,
//   <T as InputTakeAtPosition>::Item: AsChar,
//   <T as InputIter>::Item: AsChar,
// {
//   // scheme ":" ihier-part [ "?" iquery ]
//   let query = opt(preceded(char('?'), spanned(iquery)));
//   let parser = tuple((scheme, char(':'), ihier_part, query));

//   let cloned = input.clone();
//   match spanned(parser)(input) {
//     Ok((i, spanned)) => Ok((i, cloned.take(spanned.len()))),
//     Err(e) => Err(e),
//   }
// }

// fn iri_reference<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
// where
//   T: Slice<RangeFrom<usize>>
//     + InputTakeAtPosition
//     + InputIter
//     + InputLength
//     + InputTake
//     + InputLocate
//     + UnspecializedInput
//     + Compare<&'static str>
//     + Clone
//     + PartialEq,
//   <T as InputTakeAtPosition>::Item: AsChar,
//   <T as InputIter>::Item: AsChar,
// {
//   // IRI / irelative-ref
// }

struct IriInfo<T> {
  scheme: Spanned<T>,
  authority: Option<Authority<T>>,
  path: Spanned<T>,
  query: Option<Spanned<T>>,
  fragment: Option<Spanned<T>>,
}

fn iri<T, E: ParseError<T>>(input: T) -> IResult<T, IriInfo<T>, E>
where
  T: Slice<RangeFrom<usize>>
    + InputTakeAtPosition
    + InputIter
    + InputLength
    + InputTake
    + InputLocate
    + Compare<&'static str>
    + Clone
    + PartialEq
    + std::fmt::Debug,
  <T as InputTakeAtPosition>::Item: AsChar,
  <T as InputIter>::Item: AsChar,
{
  let query = opt(preceded(char('?'), spanned(iquery)));
  let fragment = opt(preceded(char('#'), spanned(ifragment)));
  let parser = tuple((scheme, char(':'), ihier_part, query, fragment));

  let cloned = input.clone();
  match parser(input) {
    Ok((i, value)) => {
      let (scheme, _, hier_part, query, fragment) = value;
      let authority = hier_part.authority;
      let path = hier_part.path;

      let info = IriInfo {
        scheme,
        authority,
        path,
        query,
        fragment,
      };
      Ok((i, info))
    }

    Err(e) => Err(e).add_err_context(cloned, "iri"),
  }
}

pub fn try_parse<I: IntoIri>(s: I) -> Result<<I as IntoIri>::Iri, error::Error> {
  match _try_parse(s.borrow()) {
    Ok(frames) => Ok(s.into_iri(frames)),
    Err(error) => Err(
      error::Context {
        source: s.borrow().into(),
        error: error,
      }
      .into(),
    ),
  }
}

fn _try_parse(iri_str: &str) -> Result<IriRanges, String> {
  let parsed = {
    let parser = exact(iri);
    let input = WithPos::new(iri_str);
    parser(input)
  };

  match parsed {
    Ok((_, info)) => {
      let scheme = info.scheme.into_range();
      let (authority, userinfo, host, port) = match info.authority {
        None => (None, None, None, None),
        Some(a) => (
          Some(a.value.into_range()),
          a.userinfo.map(Spanned::into_range),
          Some(a.host.into_range()),
          a.port.map(Spanned::into_range),
        ),
      };
      let path = info.path.into_range();
      let query = info.query.map(Spanned::into_range);
      let fragment = info.fragment.map(Spanned::into_range);

      let ranges = IriRanges {
        scheme,
        authority,
        userinfo,
        host,
        port,
        path,
        query,
        fragment,
      };

      Ok(ranges)
    }

    Err(Err::Incomplete(_)) => unreachable!(),
    Err(Err::Error(e)) => {
      let e: VerboseError<_> = e;
      let errors = e
        .errors
        .into_iter()
        .map(|(i, kind)| (i.into_inner(), kind))
        .collect();
      let e = VerboseError { errors };
      Err(convert_error(iri_str, e))
    }
    Err(Err::Failure(_)) => panic!("parser failure."),
  }
}

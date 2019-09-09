use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::{multispace0, multispace1};
use nom::sequence::preceded;
use nom::sequence::terminated;
use nom::sequence::tuple;
use nom::IResult;
use std::path::Path;

fn prefix(input: &str) -> IResult<&str, (&str, &str)> {
  let header = tag("@prefix");
  let lt = tag("<");
  let gt = tag(">");
  let period = tag(".");
  let colon = tag(":");
  let prefix = take_while1(|c| c != ':');
  let url = take_while1(|c| c != '>');

  let enclosed_url = terminated(preceded(lt, url), gt);

  let prefix_line = tuple((
    header,
    multispace1,
    prefix,
    multispace0,
    colon,
    multispace0,
    enclosed_url,
    multispace0,
    period,
  ));

  let (input, (_, _, prefix, _, _, _, url, _, _)) = prefix_line(input)?;
  Ok((input, (prefix, url)))
}

pub fn parse_prefix<'a>(
  input: &'a str,
  line_no: usize,
  file: &Path,
) -> std::io::Result<(&'a str, &'a str)> {
  match prefix(input) {
    Ok((_, result)) => Ok(result),
    Err(e) => Err(std::io::Error::new(
      std::io::ErrorKind::Other,
      format!(
        "Error parsing line {} of file {}: {:?}",
        line_no,
        file.display(),
        e
      ),
    )),
  }
}

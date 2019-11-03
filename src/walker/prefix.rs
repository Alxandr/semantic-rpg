use super::error::Result;
use super::{error, Walker};
use itertools::Itertools;
use skorm_curie::CurieStore;
use std::collections::HashMap;
use std::fs::OpenOptions;
use std::io;
use std::io::{BufRead, BufReader, Read, Seek, SeekFrom, Write};
use tracing::{debug, span, Level};

pub fn build_prefix_store(walker: &Walker) -> Result<CurieStore> {
  let mut store = CurieStore::new();
  for doc in walker.prefix_docs()? {
    let _span = span!(Level::DEBUG, "walker.prefix", doc = %doc.doc.rel);
    let _enter = _span.enter();

    {
      debug!("Open file");
      let mut fs = OpenOptions::new()
        .read(true)
        .write(true)
        .open(&doc.doc.path)?;
      let mut file_prefixes = HashMap::new();

      // read all prefixes from prefix document.
      {
        let reader = BufReader::new(&fs);

        for (no, line) in reader.lines().enumerate() {
          let line = line?;
          if line.is_empty() {
            debug!(line = no, "Skip line {} - empty.", no);
            continue;
          }

          debug!(line = no, "Parse line {} - {}.", no, line);
          let (prefix, url) = parser::parse_prefix(&line, no, &doc.doc.path)?;
          if file_prefixes.contains_key(prefix) {
            return Err(error::Context::prefix_exists(prefix).into());
          }

          file_prefixes.insert(prefix.to_owned(), url.to_owned());
          //let iri = skorm_iri::try_parse(url)?;
        }
      }

      // sort and write back prefixes
      fs.set_len(0)?;
      fs.seek(SeekFrom::Start(0))?;

      debug!("Writing sorted prefixes.");
      for (prefix, url) in file_prefixes.iter().sorted_by_key(|(prefix, _)| *prefix) {
        write!(fs, "@prefix {}: <{}> .\n", prefix, url)?;
      }

      // resolve and parse iris
      for (prefix, mut url) in file_prefixes {
        if url.starts_with(".") {
          let copy = url;
          url = String::with_capacity(copy.len() - 1 + walker.root.len() + doc.dir.rel.len());
          url.push_str(&walker.root);
          url.push_str(&doc.dir.rel);
          url.push_str(&copy[1..]);
          debug!(relative = %copy, absolute = %url, "Relative prefix {} resolved to {}", copy, url);
        }

        store.insert_prefix(prefix, url)?;
      }
    }
  }

  Ok(store)
}

struct Cursor<'a> {
  pos: usize,
  bytes: &'a mut [u8],
}

impl<'a> Cursor<'a> {
  fn new(bytes: &'a mut [u8]) -> Self {
    Self { pos: 0, bytes }
  }

  fn write<B: AsRef<[u8]>>(&mut self, data: B) -> bool {
    let bin = data.as_ref();
    if bin.len() > self.left() {
      false
    } else {
      self.bytes[self.pos..self.pos + bin.len()].copy_from_slice(bin);
      self.pos += bin.len();
      true
    }
  }

  #[inline]
  fn left(&self) -> usize {
    self.bytes.len() - self.pos
  }

  #[inline]
  fn delegate<R: Read>(self, reader: &mut R) -> io::Result<usize> {
    if self.pos == 0 {
      reader.read(self.bytes)
    } else {
      let len = reader.read(&mut self.bytes[self.pos..])?;
      Ok(self.pos + len)
    }
  }
}

pub fn prefixed<'a, R: Read>(
  reader: &'a mut R,
  base: &'a str,
  store: &'a CurieStore,
) -> impl Read + 'a {
  PrefixRead {
    inner: reader,
    base,
    state: PrefixReadState::Uninitializd(store),
  }
}

struct PrefixRead<'a, R: Read> {
  inner: &'a mut R,
  base: &'a str,
  state: PrefixReadState<'a>,
}

enum PrefixReadState<'a> {
  Uninitializd(&'a CurieStore),
  Prefix(std::iter::Peekable<skorm_curie::Iter<'a>>),
  EmptyPrefix,
  Base,
  Inner,
}

impl<'a, R: Read> Read for PrefixRead<'a, R> {
  fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
    let mut writer = Cursor::new(buf);
    loop {
      match &mut self.state {
        PrefixReadState::Uninitializd(s) => {
          self.state = PrefixReadState::Prefix(s.prefixes().peekable());
        }

        PrefixReadState::Prefix(s) => {
          while let Some((p, v)) = s.peek() {
            let line = format!("@prefix {}: <{}> .\n", p, v);
            if !writer.write(line) {
              return Ok(writer.pos);
            }

            // discarded (after peek).
            s.next();
          }
          self.state = PrefixReadState::EmptyPrefix;
        }

        PrefixReadState::EmptyPrefix => {
          let line = format!("@prefix : <{}> .\n", &self.base);
          if !writer.write(line) {
            return Ok(writer.pos);
          }

          self.state = PrefixReadState::Base;
        }

        PrefixReadState::Base => {
          let line = format!("@base <{}> .\n", &self.base);
          if !writer.write(line) {
            return Ok(writer.pos);
          }

          self.state = PrefixReadState::Inner;
        }

        PrefixReadState::Inner => return writer.delegate(self.inner),
      }
    }
  }
}

mod parser {
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
}

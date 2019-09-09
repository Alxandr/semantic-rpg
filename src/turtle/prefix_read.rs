use super::*;
use std::borrow::Cow;
use std::io::{Read, Result};
use std::mem::replace;

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
  fn delegate<R: Read>(self, reader: &mut R) -> Result<usize> {
    if self.pos == 0 {
      reader.read(self.bytes)
    } else {
      let len = reader.read(&mut self.bytes[self.pos..])?;
      Ok(self.pos + len)
    }
  }
}

enum ReadState<'a> {
  Uninitialized,
  Prefix(
    Id<String>,
    std::collections::hash_set::Iter<'a, Id<String>>,
    &'a Vec<String>,
  ),
  EmptyPrefix,
  Base(String),
  Content,
}

pub struct PrefixRead<'a, R: Read> {
  content: R,
  strings: &'a Arena<String>,
  prefixes: &'a HashMap<String, PrefixRef>,
  prefix_docs: &'a Arena<PrefixFile>,
  prefix_doc: Option<Id<PrefixFile>>,
  root: &'a str,
  path: &'a Vec<String>,
  state: ReadState<'a>,
}

impl<'a, R: Read> PrefixRead<'a, R> {
  pub fn new(
    content: R,
    strings: &'a Arena<String>,
    prefixes: &'a HashMap<String, PrefixRef>,
    prefix_docs: &'a Arena<PrefixFile>,
    prefix_doc: Id<PrefixFile>,
    root: &'a str,
    path: &'a Vec<String>,
  ) -> Self {
    Self {
      content,
      strings,
      prefixes,
      prefix_docs,
      prefix_doc: prefix_doc.into(),
      root,
      path,
      state: ReadState::Uninitialized,
    }
  }
}

impl<'a, R: Read> Read for PrefixRead<'a, R> {
  fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
    let mut writer = Cursor::new(buf);
    loop {
      match &self.state {
        ReadState::Uninitialized => match self.prefix_doc {
          None => self.state = ReadState::EmptyPrefix,
          Some(id) => {
            let doc = &self.prefix_docs[id];
            self.prefix_doc = doc.parent;
            let mut iter = doc.prefixes.iter();
            match iter.next() {
              None => self.state = ReadState::Uninitialized,
              Some(id) => self.state = ReadState::Prefix(*id, iter, &doc.path),
            }
          }
        },

        ReadState::Prefix(..) => {
          if let ReadState::Prefix(id, mut iter, path) =
            replace(&mut self.state, ReadState::Uninitialized)
          {
            let prefix = &self.strings[id];
            let iri = resolve_iri(&self.strings[self.prefixes[prefix].id], path, &self.root);
            let line = format!("@prefix {}: <{}> .\n", prefix, iri);
            if !writer.write(line) {
              return Ok(writer.pos);
            }

            match iter.next() {
              None => self.state = ReadState::Uninitialized,
              Some(id) => self.state = ReadState::Prefix(*id, iter, path),
            }
          } else {
            unreachable!()
          }
        }

        ReadState::EmptyPrefix => {
          let mut base = self.root.to_owned();
          for segment in self.path.iter() {
            base.push_str(segment);
            base.push('/');
          }

          let line = format!("@prefix : <{}> .\n", base);
          if !writer.write(line) {
            return Ok(writer.pos);
          }

          self.state = ReadState::Base(base);
        }

        ReadState::Base(base) => {
          let line = format!("@base <{}> .\n", base);

          if !writer.write(line) {
            return Ok(writer.pos);
          }

          self.state = ReadState::Content;
        }

        ReadState::Content => {
          return writer.delegate(&mut self.content);
        }
      }
    }
  }
}

pub fn resolve_iri<'a>(mut iri: &'a str, path: &Vec<String>, root: &str) -> Cow<'a, str> {
  if !iri.starts_with('.') {
    iri.into()
  } else {
    let mut ret = root.to_owned();
    let mut include_count = path.len();
    while iri.starts_with('.') {
      if iri.starts_with("./") {
        iri = &iri[2..];
      } else if iri.starts_with("../") {
        iri = &iri[3..];
        include_count = include_count
          .checked_sub(1)
          .expect("Invalid relative path.");
      }
    }

    if include_count > 0 {
      for segment in path.iter().take(include_count) {
        ret.push_str(segment);
        ret.push('/');
      }
    }

    ret.push_str(iri);
    ret.into()
  }
}

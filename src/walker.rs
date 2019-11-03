//mod prefix_read;
mod doc;
pub mod error;
mod prefix;

use error::*;
use skorm_curie::CurieStore;
use skorm_store::RdfStore;
use std::ffi::OsStr;
use std::fs::File;
use std::path::Path;
use std::path::PathBuf;
use tracing::debug;

const PREFIX_FILE_NAME: &'static str = "_prefix.ttl";

pub struct Walker<'a> {
  path: &'a Path,
  root: &'a str,
  dirs: &'a [&'a str],
}

#[derive(Clone)]
struct WalkerPath {
  path: PathBuf,
  rel: String,
}

struct WalkerDoc {
  dir: WalkerPath,
  doc: WalkerPath,
}

impl WalkerPath {
  fn new(path: PathBuf, rel: String) -> Self {
    Self { path, rel }
  }

  fn join(&self, path: impl AsRef<Path>) -> Self {
    let path = path.as_ref();
    Self {
      path: self.path.join(path),
      rel: if self.rel.len() == 0 {
        path.display().to_string()
      } else {
        format!("{}/{}", self.rel, path.display())
      },
    }
  }
}

impl<'a> Walker<'a> {
  pub fn new(path: &'a Path, root: &'a str, dirs: &'a [&'a str]) -> Self {
    Self { path, root, dirs }
  }

  fn dirs(&self) -> Result<impl Iterator<Item = WalkerPath>> {
    let mut result = Vec::new();
    result.push(WalkerPath::new(self.path.to_owned(), String::new()));

    for dir_name in self.dirs {
      let dir = result[0].join(dir_name);
      if dir.path.is_dir() {
        recursive_add_dirs(&mut result, dir)?;
      }
    }

    Ok(result.into_iter())
  }

  fn prefix_docs(&self) -> Result<impl Iterator<Item = WalkerDoc>> {
    let mut result = Vec::new();
    for dir in self.dirs()? {
      let prefix_file = dir.join(PREFIX_FILE_NAME);
      if !prefix_file.path.is_file() {
        debug!(
          file = %prefix_file.path.display(),
          "Creating file {}.", prefix_file.path.display());
        File::create(&prefix_file.path)?;
      }

      result.push(WalkerDoc {
        dir,
        doc: prefix_file,
      });
    }

    Ok(result.into_iter())
  }

  fn docs(&self) -> Result<impl Iterator<Item = WalkerDoc>> {
    let mut result = Vec::new();
    for dir in self.dirs()? {
      for entry in dir.path.read_dir()? {
        let entry = entry?;

        if entry.file_name() == PREFIX_FILE_NAME {
          continue;
        }

        let sub_path = dir.join(entry.file_name());
        if !sub_path.path.is_file() {
          continue;
        }

        match entry.path().extension().and_then(OsStr::to_str) {
          Some("ttl") => (),
          _ => continue,
        };

        result.push(WalkerDoc {
          dir: dir.clone(),
          doc: sub_path,
        });
      }
    }

    Ok(result.into_iter())
  }

  fn build_prefix_store(&self) -> Result<CurieStore> {
    prefix::build_prefix_store(self)
  }

  pub fn build_rdf_store(&self) -> Result<RdfStore> {
    doc::build_rdf_store(self)
  }
}

fn recursive_add_dirs(vec: &mut Vec<WalkerPath>, dir: WalkerPath) -> Result<()> {
  vec.push(dir);
  for entry in vec[vec.len() - 1].path.read_dir()? {
    let entry = entry?;
    let sub_path = vec[vec.len() - 1].join(entry.file_name());
    if sub_path.path.is_dir() {
      recursive_add_dirs(vec, sub_path)?;
    }
  }

  Ok(())
}

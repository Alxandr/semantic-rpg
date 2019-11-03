use crate::rdf::{RdfClass, RdfProp};
use skorm_store::NamedOrBlankNode;
use std::collections::HashSet;
use std::ffi::OsStr;
use std::fs;
use std::path::Path;
use tracing::{debug, span, Level};

pub mod error;
mod rdf;

pub struct Writer<'a> {
  out_dir: &'a Path,
  root: &'a str,
}

impl<'a> Writer<'a> {
  pub fn new(path: &'a Path, root: &'a str) -> Self {
    Self {
      out_dir: path,
      root,
    }
  }

  pub fn write_class<'b>(&self, class: RdfClass<'b>) -> error::Result<()> {
    let class_full_name = class.name();
    let rel_path: &Path = class_full_name[self.root.len()..].as_ref();
    let class_name = rel_path.file_name().and_then(OsStr::to_str).unwrap();
    let abs_path = self.out_dir.join(rel_path);
    let dir = abs_path.parent().unwrap();

    let span = span!(Level::DEBUG, "writer.class",
      class = %class_name,
      dir = %rel_path.parent().unwrap().display());
    let _enter = span.enter();
    fs::create_dir_all(dir)?;

    let mut writer = rdf::RdfMultiWriter::new(&abs_path)?;
    let mut seen = HashSet::new();
    self.write_class_impl(&mut writer, &mut seen, class)?;

    Ok(())
  }

  fn write_class_impl<'b>(
    &self,
    writer: &mut rdf::RdfMultiWriter,
    seen: &mut HashSet<NamedOrBlankNode<'b>>,
    class: RdfClass<'b>,
  ) -> error::Result<()> {
    if !seen.insert(class.node().into()) {
      return Ok(());
    }

    debug!(class = %class.name(), "Writing class.");

    for triple in class.triples() {
      writer.write(triple)?;
    }

    for prop in class.own_props() {
      self.write_prop_impl(writer, seen, prop)?;
    }

    for base in class.base_classes() {
      self.write_class_impl(writer, seen, base)?;
    }

    Ok(())
  }

  fn write_prop_impl<'b>(
    &self,
    writer: &mut rdf::RdfMultiWriter,
    seen: &mut HashSet<NamedOrBlankNode<'b>>,
    prop: RdfProp<'b>,
  ) -> error::Result<()> {
    if !seen.insert(prop.node().into()) {
      return Ok(());
    }

    debug!(prop = %prop.name(), "Writing prop.");

    for triple in prop.triples() {
      writer.write(triple)?;
    }

    for base in prop.base_props() {
      self.write_prop_impl(writer, seen, base)?;
    }

    Ok(())
  }
}

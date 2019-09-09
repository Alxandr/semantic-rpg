use class::Class;
use graph::Graph;
use id_arena::{Arena, Id};
use itertools::Itertools;
use rio_api::parser::TriplesParser;
use rio_turtle::{TurtleError, TurtleParser};
use std::borrow::Cow;
use std::cmp::Reverse;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::{File, OpenOptions};
use std::io::{BufRead, BufReader, Error, ErrorKind, Result, Seek, SeekFrom, Write};
use std::path::Path;
use std::rc::Rc;

mod class;
mod graph;
mod parser;
mod prefix_read;

pub struct Tree {
  schema: Graph,
  data: Graph,
}

impl Tree {
  pub fn classes<'a>(&'a self) -> impl Iterator<Item = Class<'a>> {
    self
      .schema
      .subjects_where_object("rdf:type", "rdfs:Class")
      .filter_map(move |s| match s {
        graph::NamedOrBlankNode::Named(id) => Some(Class::new(&self.schema, *id)),
        _ => None,
      })
  }
}

pub struct PrefixRef {
  id: Id<String>,
  path: Rc<Vec<String>>,
}

pub struct PrefixFile {
  path: Rc<Vec<String>>,
  prefixes: HashSet<Id<String>>,
  parent: Option<Id<PrefixFile>>,
}

pub fn build_turtle_tree(path: &Path, root: &str) -> Result<Tree> {
  let mut strings = Arena::new();
  let mut prefixes = HashMap::new();
  let mut prefix_docs = Arena::new();
  let mut schemas = Graph::new(&format!("{}schemas/", root));
  let mut docs = Graph::new(root);

  let prefix_doc = add_prefix_doc(
    Vec::with_capacity(0),
    &path.join("_prefix.ttl"),
    &mut strings,
    &mut prefixes,
    None,
  )?;

  let root_id = prefix_docs.alloc(prefix_doc);

  walk_dir(
    vec!["schema".to_owned()],
    &path.join("schema"),
    &mut strings,
    &mut prefixes,
    &mut prefix_docs,
    root_id,
    &mut schemas,
    root,
  )?;

  walk_dir(
    Vec::with_capacity(0),
    &path.join("data"),
    &mut strings,
    &mut prefixes,
    &mut prefix_docs,
    root_id,
    &mut docs,
    root,
  )?;

  let prefix_map = build_prefix_map(&prefixes, &strings, root);
  add_prefixes(&mut schemas, &prefix_map)?;
  add_prefixes(&mut docs, &prefix_map)?;

  Ok(Tree {
    schema: schemas,
    data: docs,
  })
}

fn add_prefix_doc(
  path: Vec<String>,
  file: &Path,
  strings: &mut Arena<String>,
  prefixes: &mut HashMap<String, PrefixRef>,
  parent: Option<Id<PrefixFile>>,
) -> Result<PrefixFile> {
  println!("Add prefix doc: {}", file.display());

  let path = Rc::new(path);
  let mut fs = OpenOptions::new().read(true).write(true).open(file)?;
  let mut file_prefixes = HashSet::new();
  {
    let reader = BufReader::new(&fs);

    for (no, line) in reader.lines().enumerate() {
      let line = line?;
      if line.is_empty() {
        continue;
      }

      let (prefix, url) = parser::parse_prefix(&line, no, file)?;
      if prefixes.contains_key(prefix) {
        return Err(Error::new(
          ErrorKind::Other,
          format!(
            "Prefix {} already defined ({}:{})",
            prefix,
            file.display(),
            no
          ),
        ));
      }

      let prefix_id = strings.alloc(prefix.to_owned());
      let url_id = strings.alloc(url.to_owned());
      file_prefixes.insert(prefix_id);
      prefixes.insert(
        prefix.to_owned(),
        PrefixRef {
          id: url_id,
          path: path.clone(),
        },
      );
    }
  }

  fs.set_len(0)?;
  fs.seek(SeekFrom::Start(0))?;

  for prefix in file_prefixes.iter().map(|id| &strings[*id]).sorted() {
    let url_id = prefixes[prefix].id;
    let url = &strings[url_id];
    // println!("@prefix {}: <{}> .", prefix, url);
    write!(fs, "@prefix {}: <{}> .\n", prefix, url)?;
  }

  fs.flush()?;

  Ok(PrefixFile {
    path,
    prefixes: file_prefixes,
    parent,
  })
}

fn is_ttl_file<P: AsRef<Path>>(path: P) -> bool {
  let path = path.as_ref();

  if !path.is_file() {
    return false;
  }

  match path.extension() {
    None => false,
    Some(e) => e == "ttl",
  }
}

fn walk_dir(
  path: Vec<String>,
  dir: &Path,
  strings: &mut Arena<String>,
  prefixes: &mut HashMap<String, PrefixRef>,
  prefix_docs: &mut Arena<PrefixFile>,
  parent: Id<PrefixFile>,
  graph: &mut Graph,
  root: &str,
) -> Result<()> {
  let prefix_doc_path = dir.join("_prefix.ttl");
  if !prefix_doc_path.is_file() {
    File::create(&prefix_doc_path)?;
  }

  let doc = add_prefix_doc(
    path.clone(),
    &prefix_doc_path,
    strings,
    prefixes,
    Some(parent),
  )?;

  let prefix_doc_id = prefix_docs.alloc(doc);
  for entry in dir.read_dir()? {
    let entry = entry?;
    let sub_path = entry.path();
    if sub_path.is_dir() {
      let name = sub_path
        .file_name()
        .unwrap()
        .to_owned()
        .into_string()
        .unwrap();
      let mut path = path.clone();
      path.push(name);

      walk_dir(
        path,
        &sub_path,
        strings,
        prefixes,
        prefix_docs,
        prefix_doc_id,
        graph,
        root,
      )?;
    } else if is_ttl_file(&sub_path) {
      if sub_path.file_name().unwrap() == "_prefix.ttl" {
        continue;
      }

      let name = sub_path
        .file_name()
        .unwrap()
        .to_owned()
        .into_string()
        .unwrap();

      let mut path = path.clone();
      path.push(name);
      read_doc(
        &sub_path,
        strings,
        prefixes,
        prefix_docs,
        prefix_doc_id,
        graph,
        root,
      )?;
    }
  }

  Ok(())
}

fn read_doc(
  file: &Path,
  strings: &Arena<String>,
  prefixes: &HashMap<String, PrefixRef>,
  prefix_docs: &Arena<PrefixFile>,
  prefix_doc: Id<PrefixFile>,
  graph: &mut Graph,
  root: &str,
) -> Result<()> {
  println!("Read doc: {}", file.display());

  let fs = File::open(file)?;
  let mut base = root.to_owned();
  let pfx_doc = &prefix_docs[prefix_doc];
  for segment in pfx_doc.path.iter() {
    base.push_str(segment);
    base.push('/');
  }

  let reader = prefix_read::PrefixRead::new(
    fs,
    strings,
    prefixes,
    prefix_docs,
    prefix_doc,
    root,
    &pfx_doc.path,
  );
  let buf_reader = std::io::BufReader::new(reader);
  let mut parser =
    TurtleParser::new(buf_reader, "").map_err(|e| Error::new(ErrorKind::Other, e))?;
  parser
    .parse_all(&mut |t| {
      graph.insert(t);
      std::result::Result::<(), TurtleError>::Ok(())
    })
    .map_err(|e| Error::new(ErrorKind::Other, e))?;

  Ok(())
}

fn add_prefixes(graph: &mut Graph, prefixes: &Vec<(&str, Cow<'_, str>)>) -> Result<()> {
  for (prefix, full) in prefixes {
    graph.add_prefix(&prefix, full)?;
  }

  Ok(())
}

fn build_prefix_map<'a>(
  prefixes: &'a HashMap<String, PrefixRef>,
  strings: &'a Arena<String>,
  root: &str,
) -> Vec<(&'a str, Cow<'a, str>)> {
  let mut ret = Vec::<(&'a str, Cow<'a, str>)>::new();
  for (prefix, prefix_ref) in prefixes {
    let full = &strings[prefix_ref.id];
    let full = prefix_read::resolve_iri(full, &prefix_ref.path, root);
    ret.push((&prefix, full));
  }

  ret.sort_by_key(|(_, v)| {
    let s: &'_ str = &v;
    Reverse(s.len())
  });

  ret
}

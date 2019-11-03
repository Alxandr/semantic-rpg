#![feature(proc_macro_hygiene)]
#![feature(stmt_expr_attributes)]

const ROOT: &'static str = "https://yolodev.github.io/semantic-rpg/";
const DIRS: &'static [&'static str] = &["data", "schema"];
const OUT_DIR: &'static str = "out";

use std::path::Path;
use tracing::info;

mod rdf;
mod walker;
mod writer;
// mod turtle;

use rdf::*;
use walker::Walker;
use writer::Writer;

fn main() -> walker::error::Result<()> {
  tracing_subscriber::fmt::init();

  let cwd: &Path = &std::env::current_dir()?;
  info!(cwd = %cwd.display(), root = %ROOT);

  let walker = Walker::new(cwd, ROOT, DIRS);
  let store = walker.build_rdf_store()?;
  let out_dir = cwd.join(OUT_DIR);
  let writer = Writer::new(&out_dir, ROOT);

  println!("Classes: ");
  for class in store.classes() {
    writer.write_class(class).unwrap();
    // println!(
    //   "- {}: {:?}",
    //   class.name(),
    //   class.base_class().map(|c| c.name())
    // );
  }
  // for triple in store.iter() {
  //   println!("{}", triple);
  // }

  // let tree = turtle::build_turtle_tree(&std::env::current_dir()?, ROOT)?;

  // let out_dir = cwd.join("out");

  // for class in tree.classes().filter(|class| class.is_local(ROOT)) {
  //   let mut classes = Vec::new();
  //   classes.push(class);
  //   classes.extend(class.super_classes());
  //   classes.extend(class.sub_classes());

  //   let local_path = out_dir
  //     .join(class.local_path(ROOT).unwrap())
  //     .with_extension("ttl");

  //   std::fs::create_dir_all(local_path.parent().unwrap())?;
  //   println!("Create: {}", local_path.display());
  //   let file = File::create(local_path)?;
  //   let mut formatter = TurtleFormatter::new(file);
  //   for class in classes {
  //     for triple in class.triples() {
  //       formatter.format(&triple)?;
  //     }
  //   }

  //   formatter.finish()?;

  // println!(
  //   "Class: {} (super: {:?}, sub: {:?})",
  //   class.local_path(ROOT).unwrap(),
  //   super_classes,
  //   sub_classes
  // );
  // }

  Ok(())
}

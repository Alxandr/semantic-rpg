#![feature(proc_macro_hygiene)]
#![feature(stmt_expr_attributes)]

const ROOT: &'static str = "https://yolodev.github.io/semantic-rpg/";

use rio_api::formatter::TriplesFormatter;
use rio_turtle::TurtleFormatter;
use std::fs::File;
use std::path::Path;

mod turtle;

fn main() -> std::io::Result<()> {
  let cwd: &Path = &std::env::current_dir()?;
  let tree = turtle::build_turtle_tree(&std::env::current_dir()?, ROOT)?;

  let out_dir = cwd.join("out");

  for class in tree.classes().filter(|class| class.is_local(ROOT)) {
    let mut classes = Vec::new();
    classes.push(class);
    classes.extend(class.super_classes());
    classes.extend(class.sub_classes());

    let local_path = out_dir
      .join(class.local_path(ROOT).unwrap())
      .with_extension("ttl");

    std::fs::create_dir_all(local_path.parent().unwrap())?;
    println!("Create: {}", local_path.display());
    let file = File::create(local_path)?;
    let mut formatter = TurtleFormatter::new(file);
    for class in classes {
      for triple in class.triples() {
        formatter.format(&triple)?;
      }
    }

    formatter.finish()?;

    // println!(
    //   "Class: {} (super: {:?}, sub: {:?})",
    //   class.local_path(ROOT).unwrap(),
    //   super_classes,
    //   sub_classes
    // );
  }

  Ok(())
}

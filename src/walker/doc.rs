use super::error::Result;
use super::{error, Walker};
use rio_api::parser::TriplesParser;
use rio_turtle::TurtleParser;
use skorm_store::RdfStore;
use std::fs::OpenOptions;
use std::io::BufReader;
use tracing::{debug, span, Level};

pub fn build_rdf_store(walker: &Walker) -> Result<RdfStore> {
  let curie = walker.build_prefix_store()?;
  let mut store = RdfStore::with_mapper(curie.clone());

  for doc in walker.docs()? {
    let _span = span!(Level::DEBUG, "walker.doc", doc = %doc.doc.rel);
    let _enter = _span.enter();

    let mut base = String::with_capacity(walker.root.len() + doc.dir.rel.len() + 1);
    base.push_str(&walker.root);
    base.push_str(&doc.dir.rel);
    base.push('/');
    debug!(%base);

    {
      let mut fs = OpenOptions::new()
        .read(true)
        .write(false)
        .open(&doc.doc.path)?;
      let prefixed = super::prefix::prefixed(&mut fs, &base, &curie);
      let reader = BufReader::new(prefixed);
      let mut parser = TurtleParser::new(reader, "")?;

      debug!("Parse file");
      parser.parse_all(&mut |triple| {
        debug!(%triple.subject, %triple.predicate, %triple.object);
        store.insert(triple).map_err(error::Error::from)
      })?;
    }
  }

  Ok(store)
}

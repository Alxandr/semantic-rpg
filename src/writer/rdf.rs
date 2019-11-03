use super::error;
use rio_turtle::TurtleFormatter;
use skorm_store::{BlankNode, Literal, NamedNode, NamedOrBlankNode, Term, Triple};
use std::fs::File;
use std::io;
use std::path::{Path, PathBuf};

trait RioWriter {
  fn write<'a>(&mut self, triple: &rio_api::model::Triple<'a>) -> error::Result<()>;
  fn close(&mut self) -> error::Result<()>;
}

fn to_rio<'a>(triple: Triple<'a>) -> rio_api::model::Triple<'a> {
  use rio_api::model;
  fn map_named<'c>(named: NamedNode<'c>) -> model::NamedNode<'c> {
    let iri: &'c str = named.iri().as_ref();
    model::NamedNode { iri }
  }

  fn map_blank<'c>(blank: BlankNode<'c>) -> model::BlankNode<'c> {
    model::BlankNode { id: blank.name() }
  }

  fn map_named_or_blank<'c>(node: NamedOrBlankNode<'c>) -> model::NamedOrBlankNode<'c> {
    match node {
      NamedOrBlankNode::Named(n) => model::NamedOrBlankNode::NamedNode(map_named(n)),
      NamedOrBlankNode::Blank(n) => model::NamedOrBlankNode::BlankNode(map_blank(n)),
    }
  }

  fn map_literal<'c>(literal: Literal<'c>) -> model::Literal<'c> {
    let value = literal.value();
    if let Some(datatype) = literal.datatype() {
      model::Literal::Typed {
        value,
        datatype: map_named(datatype),
      }
    } else if let Some(language) = literal.language() {
      model::Literal::LanguageTaggedString { value, language }
    } else {
      model::Literal::Simple { value }
    }
  }

  fn map_term<'c>(term: Term<'c>) -> model::Term<'c> {
    match term {
      Term::Named(n) => model::Term::NamedNode(map_named(n)),
      Term::Blank(n) => model::Term::BlankNode(map_blank(n)),
      Term::Literal(n) => model::Term::Literal(map_literal(n)),
    }
  }

  let subject = map_named_or_blank(triple.subject());
  let predicate = map_named(triple.predicate());
  let object = map_term(triple.object());

  model::Triple {
    subject,
    predicate,
    object,
  }
}

macro_rules! rio_formatter_wrapper {
  ($name:ident, $formatter:ident) => {
    struct $name<W: io::Write>(Option<$formatter<W>>);
    impl<W: io::Write> RioWriter for $name<W> {
      fn write<'a>(&mut self, triple: &rio_api::model::Triple<'a>) -> error::Result<()> {
        use rio_api::formatter::TriplesFormatter;

        match &mut self.0 {
          None => Err(error::Context::WriterClosed.into()),
          Some(w) => {
            w.format(triple)?;

            Ok(())
          }
        }
      }

      fn close(&mut self) -> error::Result<()> {
        match self.0.take() {
          None => Err(error::Context::WriterClosed.into()),
          Some(w) => w.finish().map(|_| ()).map_err(Into::into),
        }
      }
    }

    impl<W: io::Write> From<$formatter<W>> for $name<W> {
      fn from(formatter: $formatter<W>) -> Self {
        Self(Some(formatter))
      }
    }
  };
}

rio_formatter_wrapper!(RioTurtle, TurtleFormatter);

pub struct RdfMultiWriter {
  writers: Vec<Box<dyn RioWriter>>,
}

impl Drop for RdfMultiWriter {
  fn drop(&mut self) {
    let mut writers = Vec::with_capacity(0);
    std::mem::swap(&mut self.writers, &mut writers);

    for mut writer in writers {
      writer.close().unwrap();
    }
  }
}

impl RdfMultiWriter {
  pub fn write<'a>(&mut self, triple: Triple<'a>) -> error::Result<()> {
    let rio = to_rio(triple);

    for writer in self.writers.iter_mut() {
      writer.write(&rio)?;
    }

    Ok(())
  }
}

impl RdfMultiWriter {
  pub fn new(root: &Path) -> error::Result<Self> {
    let writers = vec![RdfMultiWriter::turtle(root.with_extension("ttl"))?];

    Ok(Self { writers })
  }

  fn turtle(file: PathBuf) -> error::Result<Box<dyn RioWriter>> {
    let file = File::create(file)?;
    let formatter = TurtleFormatter::new(file);
    Ok(Box::new(RioTurtle::from(formatter)))
  }
}

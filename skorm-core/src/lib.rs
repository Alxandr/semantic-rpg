#![feature(never_type)]

use std::borrow::Cow;

mod ty;

pub use ty::{BlankNode, Literal, NamedNode, NamedOrBlankNode, Term, Triple};

pub trait IntoTriple {
  type Error: std::error::Error;

  fn into_triple<'a>(&'a self) -> Result<Triple<'a>, Self::Error>;
}

impl<'a> IntoTriple for Triple<'a> {
  type Error = !;

  #[inline]
  fn into_triple<'b>(&'b self) -> Result<Triple<'b>, Self::Error> {
    Ok(self.clone())
  }
}

impl<'a> IntoTriple for rio_api::model::Triple<'a> {
  type Error = skorm_iri::Error;

  fn into_triple<'b>(&'b self) -> Result<Triple<'b>, Self::Error> {
    use rio_api::model;
    fn map_named<'c>(named: model::NamedNode<'c>) -> Result<NamedNode<'c>, skorm_iri::Error> {
      let iri = Cow::Owned(skorm_iri::try_parse(named.iri)?.to_owned());
      Ok(NamedNode { iri })
    }

    fn map_blank<'c>(blank: model::BlankNode<'c>) -> BlankNode<'c> {
      BlankNode { id: blank.id }
    }

    fn map_named_or_blank<'c>(
      node: model::NamedOrBlankNode<'c>,
    ) -> Result<NamedOrBlankNode<'c>, skorm_iri::Error> {
      Ok(match node {
        model::NamedOrBlankNode::NamedNode(n) => NamedOrBlankNode::NamedNode(map_named(n)?),
        model::NamedOrBlankNode::BlankNode(n) => NamedOrBlankNode::BlankNode(map_blank(n)),
      })
    }

    fn map_literal<'c>(literal: model::Literal<'c>) -> Result<Literal<'c>, skorm_iri::Error> {
      Ok(match literal {
        model::Literal::Simple { value } => Literal::Simple { value },
        model::Literal::LanguageTaggedString { value, language } => {
          Literal::LanguageTaggedString { value, language }
        }
        model::Literal::Typed { value, datatype } => Literal::Typed {
          value,
          datatype: map_named(datatype)?,
        },
      })
    }

    fn map_term<'c>(term: model::Term<'c>) -> Result<Term<'c>, skorm_iri::Error> {
      Ok(match term {
        model::Term::NamedNode(n) => Term::NamedNode(map_named(n)?),
        model::Term::BlankNode(n) => Term::BlankNode(map_blank(n)),
        model::Term::Literal(n) => Term::Literal(map_literal(n)?),
      })
    }

    let subject = map_named_or_blank(self.subject)?;
    let predicate = map_named(self.predicate)?;
    let object = map_term(self.object)?;

    Ok(Triple {
      subject,
      predicate,
      object,
    })
  }
}

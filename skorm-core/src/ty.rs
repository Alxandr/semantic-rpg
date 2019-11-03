use skorm_iri::Iri;
use std::borrow::Cow;
use std::fmt;
use std::fmt::Write;

/// A RDF [IRI](https://www.w3.org/TR/rdf11-concepts/#dfn-iri).
#[derive(Eq, PartialEq, Debug, Clone, Hash)]
pub struct NamedNode<'a> {
  /// The [IRI](https://www.w3.org/TR/rdf11-concepts/#dfn-iri) itself.
  pub iri: Cow<'a, Iri>,
}

impl<'a> fmt::Display for NamedNode<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<{}>", self.iri)
  }
}

/// A RDF [blank node](https://www.w3.org/TR/rdf11-concepts/#dfn-blank-node).
#[derive(Eq, PartialEq, Debug, Clone, Copy, Hash)]
pub struct BlankNode<'a> {
  /// The [blank node identifier](https://www.w3.org/TR/rdf11-concepts/#dfn-blank-node-identifier).
  pub id: &'a str,
}

impl<'a> fmt::Display for BlankNode<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "_:{}", self.id)
  }
}

/// A RDF [literal](https://www.w3.org/TR/rdf11-concepts/#dfn-literal).
#[derive(Eq, PartialEq, Debug, Clone, Hash)]
pub enum Literal<'a> {
  /// A [simple literal](https://www.w3.org/TR/rdf11-concepts/#dfn-simple-literal) without datatype or language form.
  Simple {
    /// The [lexical form](https://www.w3.org/TR/rdf11-concepts/#dfn-lexical-form).
    value: &'a str,
  },

  /// A [language-tagged string](https://www.w3.org/TR/rdf11-concepts/#dfn-language-tagged-string)
  LanguageTaggedString {
    /// The [lexical form](https://www.w3.org/TR/rdf11-concepts/#dfn-lexical-form).
    value: &'a str,
    /// The [language tag](https://www.w3.org/TR/rdf11-concepts/#dfn-datatype-iri).
    language: &'a str,
  },

  /// A literal with an explicit datatype
  Typed {
    /// The [lexical form](https://www.w3.org/TR/rdf11-concepts/#dfn-lexical-form).
    value: &'a str,
    /// The [datatype IRI](https://www.w3.org/TR/rdf11-concepts/#dfn-language-tag).
    datatype: NamedNode<'a>,
  },
}

impl<'a> Literal<'a> {
  pub fn value(&self) -> &str {
    match self {
      Literal::Simple { value } => value,
      Literal::LanguageTaggedString { value, .. } => value,
      Literal::Typed { value, .. } => value,
    }
  }
}

impl<'a> fmt::Display for Literal<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Literal::Simple { value } => {
        f.write_char('"')?;
        escape(value).try_for_each(|c| f.write_char(c))?;
        f.write_char('"')
      }

      Literal::LanguageTaggedString { value, language } => {
        f.write_char('"')?;
        escape(value).try_for_each(|c| f.write_char(c))?;
        f.write_char('"')?;
        write!(f, "@{}", language)
      }

      Literal::Typed { value, datatype } => {
        f.write_char('"')?;
        escape(value).try_for_each(|c| f.write_char(c))?;
        f.write_char('"')?;
        write!(f, "^^{}", datatype)
      }
    }
  }
}

/// The union of [IRIs](https://www.w3.org/TR/rdf11-concepts/#dfn-iri) and [blank nodes](https://www.w3.org/TR/rdf11-concepts/#dfn-blank-node).
#[derive(Eq, PartialEq, Debug, Clone, Hash)]
pub enum NamedOrBlankNode<'a> {
  NamedNode(NamedNode<'a>),
  BlankNode(BlankNode<'a>),
}

impl<'a> fmt::Display for NamedOrBlankNode<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      NamedOrBlankNode::NamedNode(node) => node.fmt(f),
      NamedOrBlankNode::BlankNode(node) => node.fmt(f),
    }
  }
}

impl<'a> From<NamedNode<'a>> for NamedOrBlankNode<'a> {
  #[inline]
  fn from(node: NamedNode<'a>) -> Self {
    NamedOrBlankNode::NamedNode(node)
  }
}

impl<'a> From<BlankNode<'a>> for NamedOrBlankNode<'a> {
  #[inline]
  fn from(node: BlankNode<'a>) -> Self {
    NamedOrBlankNode::BlankNode(node)
  }
}

/// A RDF [term](https://www.w3.org/TR/rdf11-concepts/#dfn-rdf-term).
#[derive(Eq, PartialEq, Debug, Clone, Hash)]
pub enum Term<'a> {
  NamedNode(NamedNode<'a>),
  BlankNode(BlankNode<'a>),
  Literal(Literal<'a>),
}

impl<'a> fmt::Display for Term<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Term::NamedNode(node) => node.fmt(f),
      Term::BlankNode(node) => node.fmt(f),
      Term::Literal(literal) => literal.fmt(f),
    }
  }
}

impl<'a> From<NamedNode<'a>> for Term<'a> {
  #[inline]
  fn from(node: NamedNode<'a>) -> Self {
    Term::NamedNode(node)
  }
}

impl<'a> From<BlankNode<'a>> for Term<'a> {
  #[inline]
  fn from(node: BlankNode<'a>) -> Self {
    Term::BlankNode(node)
  }
}

impl<'a> From<Literal<'a>> for Term<'a> {
  #[inline]
  fn from(literal: Literal<'a>) -> Self {
    Term::Literal(literal)
  }
}

impl<'a> From<NamedOrBlankNode<'a>> for Term<'a> {
  #[inline]
  fn from(resource: NamedOrBlankNode<'a>) -> Self {
    match resource {
      NamedOrBlankNode::NamedNode(node) => Term::NamedNode(node),
      NamedOrBlankNode::BlankNode(node) => Term::BlankNode(node),
    }
  }
}

/// A [RDF triple](https://www.w3.org/TR/rdf11-concepts/#dfn-rdf-triple).
#[derive(Eq, PartialEq, Debug, Clone, Hash)]
pub struct Triple<'a> {
  pub subject: NamedOrBlankNode<'a>,
  pub predicate: NamedNode<'a>,
  pub object: Term<'a>,
}

impl<'a> fmt::Display for Triple<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{} {} {} .", self.subject, self.predicate, self.object)
  }
}

fn escape<'a>(s: &'a str) -> impl Iterator<Item = char> + 'a {
  s.chars().flat_map(EscapeRDF::new)
}

/// Customized version of EscapeDefault of the Rust standard library
struct EscapeRDF {
  state: EscapeRdfState,
}

enum EscapeRdfState {
  Done,
  Char(char),
  Backslash(char),
}

impl EscapeRDF {
  fn new(c: char) -> Self {
    Self {
      state: match c {
        '\n' => EscapeRdfState::Backslash('n'),
        '\r' => EscapeRdfState::Backslash('r'),
        '"' => EscapeRdfState::Backslash('"'),
        '\\' => EscapeRdfState::Backslash('\\'),
        c => EscapeRdfState::Char(c),
      },
    }
  }
}

impl Iterator for EscapeRDF {
  type Item = char;

  fn next(&mut self) -> Option<char> {
    match self.state {
      EscapeRdfState::Backslash(c) => {
        self.state = EscapeRdfState::Char(c);
        Some('\\')
      }
      EscapeRdfState::Char(c) => {
        self.state = EscapeRdfState::Done;
        Some(c)
      }
      EscapeRdfState::Done => None,
    }
  }

  fn size_hint(&self) -> (usize, Option<usize>) {
    let n = self.len();
    (n, Some(n))
  }

  fn count(self) -> usize {
    self.len()
  }
}

impl ExactSizeIterator for EscapeRDF {
  fn len(&self) -> usize {
    match self.state {
      EscapeRdfState::Done => 0,
      EscapeRdfState::Char(_) => 1,
      EscapeRdfState::Backslash(_) => 2,
    }
  }
}

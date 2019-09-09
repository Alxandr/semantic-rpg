use auto_enums::auto_enum;
use curie::PrefixMapping;
use id_arena::{Arena, Id};
use nom::lib::std::collections::HashMap;
use std::ops::Index;

#[derive(Debug)]
pub struct NamedNode {
  pub(super) iri: String,
}

#[derive(Debug)]
pub struct BlankNode {
  id: String,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub enum NamedOrBlankNode {
  Named(Id<NamedNode>),
  Blank(Id<BlankNode>),
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub enum Literal {
  Simple(String),
  LanguageTaggedString(String, String),
  Typed(String, Id<NamedNode>),
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub enum Term {
  Named(Id<NamedNode>),
  Blank(Id<BlankNode>),
  Literal(Id<Literal>),
}

#[derive(Debug)]
pub struct Triple {
  pub subject: NamedOrBlankNode,
  pub predicate: Id<NamedNode>,
  pub object: Term,
}

pub trait IntoTriple {
  fn into_triple(self, graph: &mut Graph) -> Triple;
}

#[derive(Debug)]
pub struct Graph {
  named_lookup: HashMap<String, Id<NamedNode>>,
  blank_lookup: HashMap<String, Id<BlankNode>>,

  named: Arena<NamedNode>,
  blank: Arena<BlankNode>,
  literals: Arena<Literal>,
  triples: Arena<Triple>,

  by_subject: HashMap<NamedOrBlankNode, Vec<Id<Triple>>>,
  by_predicate: HashMap<Id<NamedNode>, Vec<Id<Triple>>>,
  by_object: HashMap<Term, Vec<Id<Triple>>>,

  mapper: PrefixMapping,
}

impl Graph {
  pub fn new(default_prefix: &str) -> Self {
    let mut mapper = PrefixMapping::default();
    mapper.set_default(default_prefix);

    Graph {
      named_lookup: HashMap::new(),
      blank_lookup: HashMap::new(),

      named: Arena::new(),
      blank: Arena::new(),
      literals: Arena::new(),
      triples: Arena::new(),

      by_subject: HashMap::new(),
      by_predicate: HashMap::new(),
      by_object: HashMap::new(),

      mapper,
    }
  }

  pub fn insert<T: IntoTriple>(&mut self, triple: T) -> Id<Triple> {
    let triple = triple.into_triple(self);
    let id = self.triples.alloc(triple);
    let triple = &self.triples[id];

    self
      .by_subject
      .entry(triple.subject)
      .or_insert_with(Default::default)
      .push(id);

    self
      .by_predicate
      .entry(triple.predicate)
      .or_insert_with(Default::default)
      .push(id);

    self
      .by_object
      .entry(triple.object)
      .or_insert_with(Default::default)
      .push(id);

    id
  }

  pub fn named_node(&mut self, iri: &str) -> Id<NamedNode> {
    match self.named_lookup.get(iri) {
      Some(id) => *id,
      None => {
        let node = NamedNode {
          iri: iri.to_owned(),
        };
        let id = self.named.alloc(node);
        self.named_lookup.insert(iri.to_owned(), id);
        id
      }
    }
  }

  pub fn blank_node(&mut self, id: &str) -> Id<BlankNode> {
    match self.blank_lookup.get(id) {
      Some(id) => *id,
      None => {
        let node = BlankNode { id: id.to_owned() };
        let lookup = id.to_owned();
        let id = self.blank.alloc(node);
        self.blank_lookup.insert(lookup, id);
        id
      }
    }
  }

  pub fn literal(&mut self, literal: Literal) -> Id<Literal> {
    self.literals.alloc(literal)
  }

  pub(super) fn add_prefix(&mut self, prefix: &str, value: &str) -> std::io::Result<()> {
    self
      .mapper
      .add_prefix(prefix, value)
      .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, format!("{:?}", e)))?;

    Ok(())
  }

  pub fn named(&self, predicate: &str) -> Option<Id<NamedNode>> {
    let predicate = self.expand_curie_string(predicate);

    self.named_lookup.get(&predicate).map(|id| *id)
  }

  pub fn subjects_where_object<'a>(
    &'a self,
    predicate: &str,
    value: &str,
  ) -> impl Iterator<Item = &'a NamedOrBlankNode> {
    let predicate = self.expand_curie_string(predicate);
    let object = self.expand_curie_string(value);

    let ret = match self.named_lookup.get(&predicate) {
      None => SubjectIter::Empty,
      Some(predicate) => match self.named_lookup.get(&object) {
        None => SubjectIter::Empty,
        Some(object) => match self.by_predicate.get(predicate) {
          None => SubjectIter::Empty,
          Some(triples) => SubjectIter::ObjectMatches(self, triples.iter(), Term::Named(*object)),
        },
      },
    };

    ret
  }

  pub fn triples_for_subject(&self, node: &NamedOrBlankNode) -> impl Iterator<Item = &'_ Triple> {
    #[auto_enum(Iterator)]
    match self.by_subject.get(&node) {
      None => [].iter(),
      Some(ids) => ids.iter().map(move |id| &self[*id]),
    }
  }

  pub(super) fn curie(&self, iri: &str) -> String {
    match self.mapper.shrink_iri(iri) {
      Ok(curie) => format!("{}", curie),
      _ => iri.to_owned(),
    }
  }

  fn expand_curie_string<'a>(&self, s: &'a str) -> String {
    if s.starts_with("http://") || s.starts_with("https://") {
      s.into()
    } else {
      self.mapper.expand_curie_string(s).unwrap().into()
    }
  }
}

impl Index<Id<NamedNode>> for Graph {
  type Output = NamedNode;

  #[inline]
  fn index(&self, id: Id<NamedNode>) -> &NamedNode {
    &self.named[id]
  }
}

impl Index<Id<BlankNode>> for Graph {
  type Output = BlankNode;

  #[inline]
  fn index(&self, id: Id<BlankNode>) -> &BlankNode {
    &self.blank[id]
  }
}

impl Index<Id<Literal>> for Graph {
  type Output = Literal;

  #[inline]
  fn index(&self, id: Id<Literal>) -> &Literal {
    &self.literals[id]
  }
}

impl Index<Id<Triple>> for Graph {
  type Output = Triple;

  #[inline]
  fn index(&self, id: Id<Triple>) -> &Triple {
    &self.triples[id]
  }
}

pub enum SubjectIter<'a> {
  Empty,
  ObjectMatches(&'a Graph, std::slice::Iter<'a, Id<Triple>>, Term),
}

impl<'a> Iterator for SubjectIter<'a> {
  type Item = &'a NamedOrBlankNode;

  fn next(&mut self) -> Option<Self::Item> {
    match self {
      SubjectIter::Empty => None,
      SubjectIter::ObjectMatches(graph, iter, object) => loop {
        match iter.next() {
          None => return None,
          Some(id) => {
            let triple = &graph.triples[*id];
            if &triple.object == object {
              return Some(&triple.subject);
            }
          }
        }
      },
    }
  }
}

impl<'a> IntoTriple for rio_api::model::Triple<'a> {
  fn into_triple(self, graph: &mut Graph) -> Triple {
    let subject = match self.subject {
      rio_api::model::NamedOrBlankNode::NamedNode(n) => {
        let id = graph.named_node(n.iri);
        NamedOrBlankNode::Named(id)
      }

      rio_api::model::NamedOrBlankNode::BlankNode(n) => {
        let id = graph.blank_node(n.id);
        NamedOrBlankNode::Blank(id)
      }
    };

    let predicate = graph.named_node(self.predicate.iri);
    let object = match self.object {
      rio_api::model::Term::NamedNode(n) => {
        let id = graph.named_node(n.iri);
        Term::Named(id)
      }

      rio_api::model::Term::BlankNode(n) => {
        let id = graph.blank_node(n.id);
        Term::Blank(id)
      }

      rio_api::model::Term::Literal(l) => {
        let literal = match l {
          rio_api::model::Literal::Simple { value } => Literal::Simple(value.to_owned()),
          rio_api::model::Literal::LanguageTaggedString { value, language } => {
            Literal::LanguageTaggedString(value.to_owned(), language.to_owned())
          }
          rio_api::model::Literal::Typed { value, datatype } => {
            let id = graph.named_node(datatype.iri);
            Literal::Typed(value.to_owned(), id)
          }
        };

        let id = graph.literal(literal);
        Term::Literal(id)
      }
    };

    Triple {
      subject,
      predicate,
      object,
    }
  }
}

impl Triple {
  pub fn as_rio<'a>(&'a self, graph: &'a Graph) -> rio_api::model::Triple<'a> {
    let subject = match &self.subject {
      NamedOrBlankNode::Named(n) => {
        let node = &graph.named[*n];
        rio_api::model::NamedOrBlankNode::NamedNode(rio_api::model::NamedNode { iri: &node.iri })
      }

      NamedOrBlankNode::Blank(n) => {
        let node = &graph.blank[*n];
        rio_api::model::NamedOrBlankNode::BlankNode(rio_api::model::BlankNode { id: &node.id })
      }
    };

    let predicate = {
      let node = &graph.named[self.predicate];
      rio_api::model::NamedNode { iri: &node.iri }
    };

    let object = match &self.object {
      Term::Named(n) => {
        let node = &graph.named[*n];
        rio_api::model::Term::NamedNode(rio_api::model::NamedNode { iri: &node.iri })
      }

      Term::Blank(n) => {
        let node = &graph.blank[*n];
        rio_api::model::Term::BlankNode(rio_api::model::BlankNode { id: &node.id })
      }

      Term::Literal(l) => rio_api::model::Term::Literal(match &graph.literals[*l] {
        Literal::Simple(value) => rio_api::model::Literal::Simple { value },
        Literal::LanguageTaggedString(value, language) => {
          rio_api::model::Literal::LanguageTaggedString { value, language }
        }
        Literal::Typed(value, n) => {
          let node = &graph.named[*n];
          rio_api::model::Literal::Typed {
            value,
            datatype: rio_api::model::NamedNode { iri: &node.iri },
          }
        }
      }),
    };

    rio_api::model::Triple {
      subject,
      predicate,
      object,
    }
  }
}

use super::*;

pub mod into_node;

pub use into_node::{IntoNamedNode, IntoNode};

pub struct Triples<'a> {
  iter: Box<dyn Iterator<Item = &'a Id<RdfStoreTriple>> + 'a>,
  store: &'a RdfStore,
}

impl<'a> Triples<'a> {
  pub(super) fn new<T: Iterator<Item = &'a Id<RdfStoreTriple>> + 'a>(
    store: &'a RdfStore,
    iter: T,
  ) -> Self {
    Self {
      iter: Box::new(iter),
      store,
    }
  }
}

impl<'a> Iterator for Triples<'a> {
  type Item = Triple<'a>;

  fn next(&mut self) -> Option<Self::Item> {
    self.iter.next().map(|id| Triple::new(self.store, *id))
  }

  fn size_hint(&self) -> (usize, Option<usize>) {
    self.iter.size_hint()
  }
}

pub struct Terms<'a> {
  iter: Box<dyn Iterator<Item = RdfStoreTerm> + 'a>,
  store: &'a RdfStore,
}

impl<'a> Terms<'a> {
  pub(super) fn new<T: Iterator<Item = RdfStoreTerm> + 'a>(store: &'a RdfStore, iter: T) -> Self {
    Self {
      iter: Box::new(iter),
      store,
    }
  }

  pub(super) fn empty(store: &'a RdfStore) -> Self {
    Self {
      iter: Box::new(std::iter::empty()),
      store,
    }
  }
}

impl<'a> Iterator for Terms<'a> {
  type Item = Term<'a>;

  fn next(&mut self) -> Option<Self::Item> {
    self.iter.next().map(|id| Term::new(self.store, id))
  }

  fn size_hint(&self) -> (usize, Option<usize>) {
    self.iter.size_hint()
  }
}

pub struct SubjectsPredMatching<'a> {
  store: &'a RdfStore,
  iter: Option<std::slice::Iter<'a, Id<RdfStoreTriple>>>,
  filter: Box<dyn Fn(RdfStoreTerm) -> bool + 'a>,
}

impl<'a> SubjectsPredMatching<'a> {
  fn empty(store: &'a RdfStore) -> Self {
    Self {
      store,
      iter: None,
      filter: Box::new(|_| false),
    }
  }
}

impl<'a> Iterator for SubjectsPredMatching<'a> {
  type Item = NamedOrBlankNode<'a>;

  fn next(&mut self) -> Option<Self::Item> {
    match &mut self.iter {
      None => None,
      Some(iter) => {
        while let Some(id) = iter.next() {
          let triple = &self.store.triples[*id];
          let filter = &self.filter;
          if filter(triple.object) {
            return Some(NamedOrBlankNode::new(self.store, triple.subject));
          }
        }

        None
      }
    }
  }
}

pub(crate) fn subjects_matching<'a>(
  store: &'a RdfStore,
  pred: impl IntoNamedNode<'a> + 'a,
  term: impl std::cmp::PartialEq<RdfStoreTerm> + 'a,
) -> Result<SubjectsPredMatching<'a>, into_node::Error> {
  let pred = pred.into_named_node(store)?;
  Ok(match pred {
    None => SubjectsPredMatching::empty(store),

    Some(pred) => SubjectsPredMatching {
      store,
      iter: store.by_predicate.get(&pred.id).map(|v| v.iter()),
      filter: Box::new(move |t| term == t),
    },
  })
}

pub fn subjects_of_type<'a, T: IntoNode<'a>>(
  store: &'a RdfStore,
  ty: T,
) -> Result<SubjectsPredMatching<'a>, into_node::Error> {
  let ty = ty
    .into_node(store)
    .map_err(Into::<into_node::Error>::into)?
    .map(|t| t.id());

  match ty {
    None => Ok(SubjectsPredMatching::empty(store)),
    Some(ty) => subjects_matching(store, "rdf:type", ty),
  }
}

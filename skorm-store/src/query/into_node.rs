use super::*;
use evitable::*;

#[evitable]
pub enum Context {
  #[evitable(description = "Failed to parse iri.", from = skorm_iri::Error)]
  IriParse,

  #[evitable(description = "Failed to expand.", from = skorm_curie::Error)]
  Expand,
}

pub trait IntoNode<'a> {
  fn into_node(self, store: &'a RdfStore) -> Result<Option<NamedOrBlankNode<'a>>>;
}

pub trait IntoNamedNode<'a> {
  fn into_named_node(self, store: &'a RdfStore) -> Result<Option<NamedNode<'a>>>;
}

impl<'a> IntoNode<'a> for NamedOrBlankNode<'a> {
  #[inline]
  fn into_node(self, _store: &'a RdfStore) -> Result<Option<NamedOrBlankNode<'a>>> {
    Ok(Some(self))
  }
}

impl<'a> IntoNode<'a> for NamedNode<'a> {
  #[inline]
  fn into_node(self, _store: &'a RdfStore) -> Result<Option<NamedOrBlankNode<'a>>> {
    Ok(Some(NamedOrBlankNode::Named(self)))
  }
}

impl<'a> IntoNamedNode<'a> for NamedNode<'a> {
  #[inline]
  fn into_named_node(self, _store: &'a RdfStore) -> Result<Option<NamedNode<'a>>> {
    Ok(Some(self))
  }
}

impl<'a> IntoNode<'a> for BlankNode<'a> {
  #[inline]
  fn into_node(self, _store: &'a RdfStore) -> Result<Option<NamedOrBlankNode<'a>>> {
    Ok(Some(NamedOrBlankNode::Blank(self)))
  }
}

impl<'a, 'b> IntoNode<'a> for &'b Iri {
  fn into_node(self, store: &'a RdfStore) -> Result<Option<NamedOrBlankNode<'a>>> {
    let id = store.get_named_node_id(self);
    match id {
      None => Ok(None),
      Some(id) => NamedNode::new(store, id).into_node(store),
    }
  }
}

impl<'a, 'b> IntoNamedNode<'a> for &'b Iri {
  #[inline]
  fn into_named_node(self, store: &'a RdfStore) -> Result<Option<NamedNode<'a>>> {
    let id = store.get_named_node_id(self);
    match id {
      None => Ok(None),
      Some(id) => NamedNode::new(store, id).into_named_node(store),
    }
  }
}

impl<'a, 'b> IntoNode<'a> for &'b str {
  fn into_node(self, store: &'a RdfStore) -> Result<Option<NamedOrBlankNode<'a>>> {
    let iri = store.mapper.expand(self)?;
    let iri: &Iri = &iri;
    let result = iri.into_node(store);
    Ok(result.unwrap())
  }
}

impl<'a, 'b> IntoNamedNode<'a> for &'b str {
  fn into_named_node(self, store: &'a RdfStore) -> Result<Option<NamedNode<'a>>> {
    let iri = store.mapper.expand(self)?;
    let iri: &Iri = &iri;
    let result = iri.into_named_node(store);
    Ok(result.unwrap())
  }
}

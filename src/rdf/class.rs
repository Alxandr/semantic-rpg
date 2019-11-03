use super::{RdfProp, RdfStorePropExt};
use extend::ext;
use skorm_store::{NamedNode, NamedOrBlankNode, RdfStore, SubjectExt};

#[derive(Debug, Clone, Copy)]
pub struct RdfClass<'a> {
  store: &'a RdfStore,
  name: NamedNode<'a>,
}

impl<'a> RdfClass<'a> {
  pub fn name(&self) -> &'a str {
    self.name.iri().as_ref()
  }

  #[inline]
  pub fn node(&self) -> NamedNode<'a> {
    self.name
  }

  pub fn base_classes(&self) -> impl Iterator<Item = RdfClass<'a>> {
    let store = self.store;
    self
      .name
      .term("rdfs:subClassOf")
      .unwrap()
      .filter_map(move |term| term.as_named().map(|name| RdfClass { name, store }))
  }

  pub fn triples(&self) -> skorm_store::Triples<'a> {
    self.name.triples()
  }

  pub fn own_props(&self) -> impl Iterator<Item = RdfProp<'a>> + 'a {
    let self_name = self.name;
    let all_props = self.store.props();
    let with_self_as_domain =
      all_props.filter(move |prop| prop.domains().any(|domain| domain == self_name));
    with_self_as_domain
  }
}

#[ext(pub, name = RdfStoreClassExt)]
impl RdfStore {
  fn classes<'a>(&'a self) -> Box<dyn Iterator<Item = RdfClass<'a>> + 'a> {
    let iter = self
      .subjects_of_type("rdfs:Class")
      .unwrap()
      .filter_map(move |sub| match sub {
        NamedOrBlankNode::Named(name) => Some(RdfClass { store: self, name }),
        NamedOrBlankNode::Blank(_) => None,
      });

    Box::new(iter)
  }
}

use extend::ext;
use skorm_store::{NamedNode, NamedOrBlankNode, RdfStore, SubjectExt};

#[derive(Debug, Clone, Copy)]
pub struct RdfProp<'a> {
  store: &'a RdfStore,
  name: NamedNode<'a>,
}

impl<'a> RdfProp<'a> {
  pub fn name(&self) -> &'a str {
    self.name.iri().as_ref()
  }

  #[inline]
  pub fn node(&self) -> NamedNode<'a> {
    self.name
  }

  pub fn base_props(&self) -> impl Iterator<Item = RdfProp<'a>> {
    let store = self.store;
    self
      .name
      .term("rdfs:subPropertyOf")
      .unwrap()
      .filter_map(move |term| term.as_named().map(|name| RdfProp { name, store }))
  }

  pub fn domains(&self) -> impl Iterator<Item = NamedNode<'a>> {
    self
      .name
      .term("rdfs:domain")
      .unwrap()
      .filter_map(|term| term.as_named())
  }

  pub fn triples(&self) -> skorm_store::Triples<'a> {
    self.name.triples()
  }
}

#[ext(pub, name = RdfStorePropExt)]
impl RdfStore {
  fn props<'a>(&'a self) -> Box<dyn Iterator<Item = RdfProp<'a>> + 'a> {
    let iter = self
      .subjects_of_type("rdf:Property")
      .unwrap()
      .filter_map(move |sub| match sub {
        NamedOrBlankNode::Named(name) => Some(RdfProp { store: self, name }),
        NamedOrBlankNode::Blank(_) => None,
      });

    Box::new(iter)
  }
}

use id_arena::{Arena, Id};
use skorm_core::IntoTriple;
use skorm_curie::CurieStore;
use skorm_iri::{Iri, IriBuf};
use std::collections::HashMap;
use std::fmt;
use std::ops::Index;

trait RdfDisplay {
  fn fmt(&self, store: &RdfStore, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

#[derive(Debug)]
struct RdfStoreNamedNode {
  iri: IriBuf,
}

impl RdfDisplay for RdfStoreNamedNode {
  fn fmt(&self, _store: &RdfStore, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<{}>", self.iri)
  }
}

#[derive(Debug)]
struct RdfStoreBlankNode {
  id: String,
}

impl RdfDisplay for RdfStoreBlankNode {
  fn fmt(&self, _store: &RdfStore, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "_:{}", self.id)
  }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
enum RdfStoreNamedOrBlankNode {
  Named(Id<RdfStoreNamedNode>),
  Blank(Id<RdfStoreBlankNode>),
}

impl RdfDisplay for RdfStoreNamedOrBlankNode {
  fn fmt(&self, store: &RdfStore, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      RdfStoreNamedOrBlankNode::Named(id) => store[*id].fmt(store, f),
      RdfStoreNamedOrBlankNode::Blank(id) => store[*id].fmt(store, f),
    }
  }
}

#[derive(Debug, Hash, PartialEq, Eq)]
enum RdfStoreLiteral {
  Simple(String),
  LanguageTaggedString(String, String),
  Typed(String, Id<RdfStoreNamedNode>),
}

impl RdfStoreLiteral {
  fn value(&self) -> &str {
    match self {
      RdfStoreLiteral::Simple(s) => &s,
      RdfStoreLiteral::LanguageTaggedString(s, _) => &s,
      RdfStoreLiteral::Typed(s, _) => &s,
    }
  }
}

impl RdfDisplay for RdfStoreLiteral {
  fn fmt(&self, store: &RdfStore, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      RdfStoreLiteral::Simple(s) => write!(f, "\"{}\"", s),
      RdfStoreLiteral::LanguageTaggedString(s, l) => write!(f, "\"{}\"@{}", s, l),
      RdfStoreLiteral::Typed(s, t) => {
        write!(f, "\"{}\"^^", s)?;
        store[*t].fmt(store, f)?;
        Ok(())
      }
    }
  }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
enum RdfStoreTerm {
  Named(Id<RdfStoreNamedNode>),
  Blank(Id<RdfStoreBlankNode>),
  Literal(Id<RdfStoreLiteral>),
}

impl RdfDisplay for RdfStoreTerm {
  fn fmt(&self, store: &RdfStore, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      RdfStoreTerm::Named(id) => store[*id].fmt(store, f),
      RdfStoreTerm::Blank(id) => store[*id].fmt(store, f),
      RdfStoreTerm::Literal(id) => store[*id].fmt(store, f),
    }
  }
}

macro_rules! custom_eq {
  ($(impl PartialEq<$right:ty> for $left:ty {
    fn eq(&$self:ident, &$other:ident) -> bool $i:block
  })+) => {
    $(
      impl std::cmp::PartialEq<$right> for $left {
        fn eq(&$self, $other: &$right) -> bool $i
      }

      impl std::cmp::PartialEq<$left> for $right {
        #[inline]
        fn eq(&self, other: &$left) -> bool {
          other == self
        }
      }
    )+
  };
}

custom_eq! {
  impl PartialEq<Id<RdfStoreNamedNode>> for RdfStoreNamedOrBlankNode {
    fn eq(&self, &other) -> bool {
      match self {
        RdfStoreNamedOrBlankNode::Named(id) => id == other,
        RdfStoreNamedOrBlankNode::Blank(_) => false,
      }
    }
  }

  impl PartialEq<Id<RdfStoreBlankNode>> for RdfStoreNamedOrBlankNode {
    fn eq(&self, &other) -> bool {
      match self {
        RdfStoreNamedOrBlankNode::Named(_) => false,
        RdfStoreNamedOrBlankNode::Blank(id) => id == other,
      }
    }
  }

  impl PartialEq<Id<RdfStoreNamedNode>> for RdfStoreTerm {
    fn eq(&self, &other) -> bool {
      match self {
        RdfStoreTerm::Named(id) => id == other,
        RdfStoreTerm::Blank(_) => false,
        RdfStoreTerm::Literal(_) => false,
      }
    }
  }

  impl PartialEq<Id<RdfStoreBlankNode>> for RdfStoreTerm {
    fn eq(&self, &other) -> bool {
      match self {
        RdfStoreTerm::Named(_) => false,
        RdfStoreTerm::Blank(id) => id == other,
        RdfStoreTerm::Literal(_) => false,
      }
    }
  }

  impl PartialEq<Id<RdfStoreLiteral>> for RdfStoreTerm {
    fn eq(&self, &other) -> bool {
      match self {
        RdfStoreTerm::Named(_) => false,
        RdfStoreTerm::Blank(_) => false,
        RdfStoreTerm::Literal(id) => id == other,
      }
    }
  }

  impl PartialEq<RdfStoreNamedOrBlankNode> for RdfStoreTerm {
    fn eq(&self, &other) -> bool {
      match self {
        RdfStoreTerm::Named(id) => id == other,
        RdfStoreTerm::Blank(id) => id == other,
        RdfStoreTerm::Literal(_) => false,
      }
    }
  }
}

// impl std::cmp::PartialEq<RdfStoreTerm> for RdfStoreNamedOrBlankNode {
//   fn eq(&self, other: &RdfStoreTerm) -> bool {
//     other == self
//   }
// }

// impl std::cmp::PartialEq<RdfStoreNamedOrBlankNode> for RdfStoreTerm {
//   fn eq(&self, other: &RdfStoreNamedOrBlankNode) -> bool {
//     match self {
//       RdfStoreTerm::Named(id) => id == other,
//       RdfStoreTerm::Blank(id) => id == other,
//       RdfStoreTerm::Literal(_) => false,
//     }
//   }
// }

#[derive(Debug)]
struct RdfStoreTriple {
  pub subject: RdfStoreNamedOrBlankNode,
  pub predicate: Id<RdfStoreNamedNode>,
  pub object: RdfStoreTerm,
}

impl RdfDisplay for RdfStoreTriple {
  fn fmt(&self, store: &RdfStore, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.subject.fmt(store, f)?;
    write!(f, " ")?;
    store[self.predicate].fmt(store, f)?;
    write!(f, " ")?;
    self.object.fmt(store, f)?;
    write!(f, " .")?;
    Ok(())
  }
}

#[derive(Debug, Default)]
pub struct RdfStore {
  named_lookup: HashMap<IriBuf, Id<RdfStoreNamedNode>>,
  blank_lookup: HashMap<String, Id<RdfStoreBlankNode>>,

  named: Arena<RdfStoreNamedNode>,
  blank: Arena<RdfStoreBlankNode>,
  literals: Arena<RdfStoreLiteral>,
  triples: Arena<RdfStoreTriple>,

  by_subject: HashMap<RdfStoreNamedOrBlankNode, Vec<Id<RdfStoreTriple>>>,
  by_predicate: HashMap<Id<RdfStoreNamedNode>, Vec<Id<RdfStoreTriple>>>,
  by_object: HashMap<RdfStoreTerm, Vec<Id<RdfStoreTriple>>>,

  mapper: CurieStore,
}

impl RdfStore {
  #[inline]
  pub fn new() -> Self {
    Default::default()
  }

  pub fn with_mapper(mapper: CurieStore) -> Self {
    let mut inst = RdfStore::new();
    inst.mapper = mapper;
    inst
  }

  pub fn mapper(&self) -> &CurieStore {
    &self.mapper
  }

  pub fn insert<E: std::error::Error>(
    &mut self,
    triple: impl IntoTriple<Error = E>,
  ) -> Result<(), E> {
    let triple = triple.into_triple()?;

    let subject = self.insert_named_or_blank_node(triple.subject);
    let predicate = self.insert_named_node(triple.predicate);
    let object = self.insert_term(triple.object);
    let triple = RdfStoreTriple {
      subject,
      predicate,
      object,
    };

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

    Ok(())
  }

  pub fn insert_prefix(&mut self, prefix: &str, value: &str) -> skorm_curie::Result<()> {
    self.mapper.insert_prefix(prefix, value)
  }

  fn insert_named_node(&mut self, node: skorm_core::NamedNode<'_>) -> Id<RdfStoreNamedNode> {
    match self.named_lookup.get(node.iri.as_ref()) {
      Some(id) => *id,
      None => {
        let iri = node.iri.into_owned();
        let id = self.named.alloc(RdfStoreNamedNode { iri: iri.clone() });
        self.named_lookup.insert(iri, id);
        id
      }
    }
  }

  fn insert_blank_node(&mut self, node: skorm_core::BlankNode<'_>) -> Id<RdfStoreBlankNode> {
    match self.blank_lookup.get(node.id) {
      Some(id) => *id,
      None => {
        let id = self.blank.alloc(RdfStoreBlankNode {
          id: node.id.to_owned(),
        });
        self.blank_lookup.insert(node.id.to_owned(), id);
        id
      }
    }
  }

  fn insert_named_or_blank_node(
    &mut self,
    subject: skorm_core::NamedOrBlankNode<'_>,
  ) -> RdfStoreNamedOrBlankNode {
    use skorm_core::NamedOrBlankNode;

    match subject {
      NamedOrBlankNode::BlankNode(n) => RdfStoreNamedOrBlankNode::Blank(self.insert_blank_node(n)),

      NamedOrBlankNode::NamedNode(n) => RdfStoreNamedOrBlankNode::Named(self.insert_named_node(n)),
    }
  }

  fn insert_term(&mut self, subject: skorm_core::Term<'_>) -> RdfStoreTerm {
    use skorm_core::Term;

    match subject {
      Term::BlankNode(n) => RdfStoreTerm::Blank(self.insert_blank_node(n)),

      Term::NamedNode(n) => RdfStoreTerm::Named(self.insert_named_node(n)),

      Term::Literal(n) => RdfStoreTerm::Literal(self.insert_literal(n)),
    }
  }

  fn insert_literal(&mut self, literal: skorm_core::Literal<'_>) -> Id<RdfStoreLiteral> {
    use skorm_core::Literal;

    let literal = match literal {
      Literal::Simple { value } => RdfStoreLiteral::Simple(value.to_owned()),
      Literal::LanguageTaggedString { value, language } => {
        RdfStoreLiteral::LanguageTaggedString(value.to_owned(), language.to_owned())
      }
      Literal::Typed { value, datatype } => {
        RdfStoreLiteral::Typed(value.to_owned(), self.insert_named_node(datatype))
      }
    };

    self.literals.alloc(literal)
  }

  fn get_named_node_id(&self, iri: &Iri) -> Option<Id<RdfStoreNamedNode>> {
    match self.named_lookup.get(iri) {
      None => None,
      Some(id) => Some(*id),
    }
  }

  fn get_blank_node_id(&self, name: &str) -> Option<Id<RdfStoreBlankNode>> {
    match self.blank_lookup.get(name) {
      None => None,
      Some(id) => Some(*id),
    }
  }
}

macro_rules! impl_store_index {
  ($ty:ident, $arena:ident) => {
    impl Index<Id<$ty>> for RdfStore {
      type Output = $ty;

      #[inline]
      fn index(&self, index: Id<$ty>) -> &Self::Output {
        &self.$arena[index]
      }
    }
  };
}

impl_store_index!(RdfStoreNamedNode, named);
impl_store_index!(RdfStoreBlankNode, blank);
impl_store_index!(RdfStoreLiteral, literals);
impl_store_index!(RdfStoreTriple, triples);

trait FromRdfStoreRef<'a, T> {
  fn from_store_ref(store: &'a RdfStore, value: T) -> Self;
}

macro_rules! store_api_type {
  ($name:ident, $inner:ident) => {
    #[derive(Clone, Copy, Debug)]
    pub struct $name<'a> {
      store: &'a RdfStore,
      id: Id<$inner>,
    }

    impl<'a> $name<'a> {
      fn new(store: &'a RdfStore, id: Id<$inner>) -> Self {
        Self { store, id }
      }

      #[inline]
      fn id(&self) -> Id<$inner> {
        self.id
      }

      #[inline]
      fn as_inner(&self) -> &'a $inner {
        &self.store[self.id]
      }
    }

    impl<'a> FromRdfStoreRef<'a, Id<$inner>> for $name<'a> {
      #[inline]
      fn from_store_ref(store: &'a RdfStore, value: Id<$inner>) -> Self {
        Self::new(store, value)
      }
    }

    impl<'a> fmt::Display for $name<'a> {
      fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.as_inner().fmt(self.store, f)
      }
    }

    impl<'a> std::cmp::PartialEq for $name<'a> {
      #[inline]
      fn eq(&self, other: &Self) -> bool {
        self.id == other.id
      }
    }

    impl<'a> std::cmp::Eq for $name<'a> {}

    impl<'a> std::hash::Hash for $name<'a> {
      #[inline]
      fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state)
      }
    }
  };

  ($name:ident, $inner:ident {$($case:ident($ty:ident),)+}) => {
    #[derive(Clone, Copy, Debug)]
    pub enum $name<'a> {
      $($case($ty<'a>),)+
    }

    impl<'a> $name<'a> {
      #[inline]
      fn new(store: &'a RdfStore, id: $inner) -> Self {
        Self::from_store_ref(store, id)
      }

      fn id(&self) -> $inner {
        match self {
          $(
            $name::$case(v) => $inner::$case(v.id()),
          )+
        }
      }
    }

    $(
      impl<'a> From<$ty<'a>> for $name<'a> {
        #[inline]
        fn from(case: $ty<'a>) -> Self {
          $name::$case(case)
        }
      }
    )+

    impl<'a> FromRdfStoreRef<'a, $inner> for $name<'a> {
      fn from_store_ref(store: &'a RdfStore, value: $inner) -> Self {
        match value {
          $(
            $inner::$case(v) => Self::from($ty::from_store_ref(store, v)),
          )+
        }
      }
    }

    impl<'a> fmt::Display for $name<'a> {
      fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
          $($name::$case(n) => fmt::Display::fmt(n, f),)+
        }
      }
    }

    impl<'a> std::cmp::PartialEq for $name<'a> {
      fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
      }
    }

    impl<'a> std::cmp::Eq for $name<'a> {}

    impl<'a> std::hash::Hash for $name<'a> {
      #[inline]
      fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id().hash(state)
      }
    }
  };
}

store_api_type!(NamedNode, RdfStoreNamedNode);
store_api_type!(BlankNode, RdfStoreBlankNode);
store_api_type!(Literal, RdfStoreLiteral);
store_api_type!(Triple, RdfStoreTriple);
store_api_type!(NamedOrBlankNode, RdfStoreNamedOrBlankNode {
  Named(NamedNode),
  Blank(BlankNode),
});
store_api_type!(Term, RdfStoreTerm {
  Named(NamedNode),
  Blank(BlankNode),
  Literal(Literal),
});

impl<'a> NamedOrBlankNode<'a> {
  pub fn as_named(&self) -> Option<NamedNode<'a>> {
    match self {
      NamedOrBlankNode::Named(n) => Some(*n),
      _ => None,
    }
  }

  pub fn as_blank(&self) -> Option<BlankNode<'a>> {
    match self {
      NamedOrBlankNode::Blank(n) => Some(*n),
      _ => None,
    }
  }
}

impl<'a> Term<'a> {
  pub fn as_named(&self) -> Option<NamedNode<'a>> {
    match self {
      Term::Named(n) => Some(*n),
      _ => None,
    }
  }

  pub fn as_blank(&self) -> Option<BlankNode<'a>> {
    match self {
      Term::Blank(n) => Some(*n),
      _ => None,
    }
  }
}

pub trait Subject<'a> {
  fn store(&self) -> &'a RdfStore;
  fn node(&self) -> NamedOrBlankNode<'a>;
}

impl<'a> Subject<'a> for NamedOrBlankNode<'a> {
  fn store(&self) -> &'a RdfStore {
    match self {
      NamedOrBlankNode::Named(n) => n.store(),
      NamedOrBlankNode::Blank(n) => n.store(),
    }
  }

  #[inline]
  fn node(&self) -> NamedOrBlankNode<'a> {
    *self
  }
}

impl<'a> NamedNode<'a> {
  pub fn iri(&self) -> &'a Iri {
    &self.as_inner().iri
  }
}

impl<'a> Subject<'a> for NamedNode<'a> {
  #[inline]
  fn store(&self) -> &'a RdfStore {
    self.store
  }

  #[inline]
  fn node(&self) -> NamedOrBlankNode<'a> {
    NamedOrBlankNode::Named(*self)
  }
}

impl<'a> BlankNode<'a> {
  pub fn name(&self) -> &'a str {
    &self.as_inner().id
  }
}

impl<'a> Subject<'a> for BlankNode<'a> {
  #[inline]
  fn store(&self) -> &'a RdfStore {
    self.store
  }

  #[inline]
  fn node(&self) -> NamedOrBlankNode<'a> {
    NamedOrBlankNode::Blank(*self)
  }
}

impl<'a> Literal<'a> {
  pub fn value(&self) -> &'a str {
    self.as_inner().value()
  }

  pub fn datatype(&self) -> Option<NamedNode<'a>> {
    match self.as_inner() {
      RdfStoreLiteral::Typed(_, id) => Some(NamedNode::new(self.store, *id)),
      _ => None,
    }
  }

  pub fn language(&self) -> Option<&'a str> {
    match self.as_inner() {
      RdfStoreLiteral::LanguageTaggedString(_, lang) => Some(lang),
      _ => None,
    }
  }
}

impl<'a> Triple<'a> {
  pub fn subject(&self) -> NamedOrBlankNode<'a> {
    NamedOrBlankNode::from_store_ref(self.store, self.as_inner().subject)
  }

  pub fn predicate(&self) -> NamedNode<'a> {
    NamedNode::from_store_ref(self.store, self.as_inner().predicate)
  }

  pub fn object(&self) -> Term<'a> {
    Term::from_store_ref(self.store, self.as_inner().object)
  }
}

impl RdfStore {
  pub fn triples(&self) -> impl Iterator<Item = Triple> {
    self
      .triples
      .iter()
      .map(move |(id, _)| Triple::new(self, id))
  }

  pub fn subjects(&self) -> impl Iterator<Item = NamedOrBlankNode> {
    self.triples().map(|triple| triple.subject())
  }

  pub fn predicates(&self) -> impl Iterator<Item = NamedNode> {
    self.triples().map(|triple| triple.predicate())
  }

  pub fn objects(&self) -> impl Iterator<Item = Term> {
    self.triples().map(|triple| triple.object())
  }

  #[inline]
  pub fn iter(&self) -> impl Iterator<Item = Triple> {
    self.triples()
  }
}

mod query;
pub use query::*;

impl RdfStore {
  pub fn subjects_of_type<'a, T: IntoNode<'a>>(
    &'a self,
    ty: T,
  ) -> Result<impl Iterator<Item = NamedOrBlankNode<'a>>, into_node::Error> {
    subjects_of_type(self, ty)
  }
}

pub trait SubjectExt<'a>: Subject<'a> {
  fn triples(&self) -> query::Triples<'a>;
  fn term(&self, pred: impl IntoNamedNode<'a>) -> Result<query::Terms<'a>, into_node::Error>;
}

const EMPTY_IDS: &'static [Id<RdfStoreTriple>] = &[];

impl<'a, S: Subject<'a>> SubjectExt<'a> for S {
  fn triples(&self) -> query::Triples<'a> {
    let iter = self
      .store()
      .by_subject
      .get(&self.node().id())
      .map(|v| v.iter())
      .unwrap_or_else(|| EMPTY_IDS.iter());

    query::Triples::new(self.store(), iter)
  }

  fn term(&self, pred: impl IntoNamedNode<'a>) -> Result<query::Terms<'a>, into_node::Error> {
    let node = pred.into_named_node(self.store())?;
    Ok(match node {
      None => query::Terms::empty(self.store()),
      Some(node) => {
        let iter = self
          .triples()
          .filter(move |triple| triple.predicate() == node)
          .map(|triple| triple.object().id());

        query::Terms::new(self.store(), iter)
      }
    })
  }
}

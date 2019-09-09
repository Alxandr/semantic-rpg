use super::*;

#[derive(Clone, Copy)]
pub struct Class<'a> {
  graph: &'a Graph,
  node: &'a graph::NamedNode,
  id: Id<graph::NamedNode>,
}

impl<'a> Class<'a> {
  pub(super) fn new(graph: &'a Graph, id: Id<graph::NamedNode>) -> Self {
    let node = &graph[id];

    Self { graph, node, id }
  }

  pub fn iri(&self) -> &'a str {
    &self.node.iri
  }

  pub fn curie(&self) -> String {
    self.graph.curie(&self.node.iri)
  }

  pub fn is_local(&self, root: &str) -> bool {
    self.iri().starts_with(root)
  }

  pub fn local_path(&self, root: &str) -> Option<&'a str> {
    if !self.is_local(root) {
      None
    } else {
      Some(&self.iri()[root.len()..])
    }
  }

  pub fn sub_classes(&self) -> impl Iterator<Item = Class<'_>> {
    let mut set = HashSet::new();
    let mut ret = Vec::new();

    find_sub_classes(&mut set, &mut ret, self.to_owned());

    ret.into_iter()
  }

  pub fn super_classes(&self) -> impl Iterator<Item = Class<'_>> {
    let mut set = HashSet::new();
    let mut ret = Vec::new();
    let pred = self.graph.named("rdfs:subClassOf").unwrap();

    find_super_classes(&mut set, &mut ret, pred, self.to_owned());

    ret.into_iter()
  }

  pub fn triples(&self) -> impl Iterator<Item = rio_api::model::Triple<'_>> {
    self
      .graph
      .triples_for_subject(&graph::NamedOrBlankNode::Named(self.id))
      .map(move |t| t.as_rio(&self.graph))
  }
}

fn find_sub_classes<'a>(
  set: &mut HashSet<Id<graph::NamedNode>>,
  ret: &mut Vec<Class<'a>>,
  class: Class<'a>,
) {
  if !set.contains(&class.id) {
    if !set.is_empty() {
      ret.push(class);
    }
    set.insert(class.id);

    for sub_id in class
      .graph
      .subjects_where_object("rdfs:subClassOf", &class.node.iri)
      .filter_map(|node| match node {
        graph::NamedOrBlankNode::Named(n) => Some(*n),
        _ => None,
      })
    {
      let sub = Class::new(class.graph, sub_id);
      find_sub_classes(set, ret, sub);
    }
  }
}

fn find_super_classes<'a>(
  set: &mut HashSet<Id<graph::NamedNode>>,
  ret: &mut Vec<Class<'a>>,
  pred: Id<graph::NamedNode>,
  class: Class<'a>,
) {
  if !set.contains(&class.id) {
    if !set.is_empty() {
      ret.push(class);
    }
    set.insert(class.id);

    for super_id in class
      .graph
      .triples_for_subject(&graph::NamedOrBlankNode::Named(class.id))
      .filter_map(|triple| {
        if triple.predicate != pred {
          None
        } else if let graph::Term::Named(n) = &triple.object {
          Some(*n)
        } else {
          None
        }
      })
    {
      let super_class = Class::new(class.graph, super_id);
      find_super_classes(set, ret, pred, super_class);
    }
  }
}

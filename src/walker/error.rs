use evitable::*;

#[evitable]
pub enum Context {
  #[evitable(description("IO error"), from = std::io::Error)]
  Io,

  #[evitable(description("Curie error"), from = skorm_curie::Error)]
  Curie,

  #[evitable(description("Failed to parse IRI"), from = skorm_iri::Error)]
  IriParseError,

  #[evitable(description("Prefix {} already exists.", prefix))]
  PrefixExists { prefix: String },

  #[evitable(description("Failed to parse turtle document."), from = rio_turtle::TurtleError)]
  TurtleParseError,
}

impl Context {
  pub fn prefix_exists<S: Into<String>>(prefix: S) -> Self {
    Self::PrefixExists {
      prefix: prefix.into(),
    }
  }
}

use evitable::*;
use indexmap::IndexMap;
use iri_string::types::{IriStr, IriString};
use std::borrow::Cow;

#[evitable]
pub enum Context {
  #[evitable(description = "The prefix \"_\" is reserved.")]
  ReservedPrefix,

  #[evitable(description("The prefix \"{}\" is already in use.", prefix))]
  PrefixInUse { prefix: String },

  #[evitable(description = "The prefix on the CURIE has no valid mapping.")]
  InvalidMapping,

  #[evitable(description("Failed to parse IRI: \"{}\".", input))]
  ParseError { input: String },
}

pub trait IntoIri {
  fn into_iri(self) -> Result<IriString>;
}

pub trait IntoCurie<'a, 'b> {
  fn into_curie(self) -> Result<Curie<'a, 'b>>;
}

impl<'a, 'b, T: Into<Curie<'a, 'b>>> IntoCurie<'a, 'b> for T {
  #[inline]
  fn into_curie(self) -> Result<Curie<'a, 'b>> {
    Ok(self.into())
  }
}

impl<'a> IntoCurie<'a, 'a> for &'a IriStr {
  #[inline]
  fn into_curie(self) -> Result<Curie<'a, 'a>> {
    Ok(Curie::LongForm(Cow::Borrowed(self)))
  }
}

impl<'a, 'b> IntoCurie<'a, 'b> for IriString {
  #[inline]
  fn into_curie(self) -> Result<Curie<'a, 'b>> {
    Ok(Curie::LongForm(Cow::Owned(self)))
  }
}

impl<'a> IntoCurie<'a, 'a> for &'a str {
  fn into_curie(self) -> Result<Curie<'a, 'a>> {
    if self.contains("://") {
      self
        .parse::<IriString>()
        .context(|| Context::ParseError {
          input: self.to_owned(),
        })
        .map(|iri| Curie::LongForm(Cow::Owned(iri)))
    } else {
      if let Some(colon) = self.find(':') {
        let (prefix, reference) = self.split_at(colon);
        let (_, reference) = reference.split_at(1);
        Ok(Curie::ShortForm {
          prefix: Some(Cow::Borrowed(prefix)),
          reference: Cow::Borrowed(reference),
        })
      } else {
        Ok(Curie::ShortForm {
          prefix: None,
          reference: Cow::Borrowed(self),
        })
      }
    }
  }
}

impl<'a> IntoCurie<'a, 'a> for String {
  #[inline]
  fn into_curie(self) -> Result<Curie<'a, 'a>> {
    Ok(<&str as IntoCurie<'_, '_>>::into_curie(&self)?.to_owned())
  }
}

/// Maps prefixes to base URIs and allows for the expansion of
/// CURIEs (Compact URIs).
///
/// # Examples
///
/// ```
/// use skorm_curie::CurieStore;
///
/// let mut store = CurieStore::new();
/// ```
#[derive(Debug, Default)]
pub struct CurieStore {
  base: Option<String>,
  mappings: IndexMap<String, String>,
}

impl CurieStore {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn with_base(base: impl Into<String>) -> Self {
    Self {
      base: Some(base.into()),
      mappings: IndexMap::default(),
    }
  }

  /// Set a default prefix.
  ///
  /// This is used during CURIE expansion when there is no
  /// prefix, just a reference value.
  ///
  /// # Example:
  ///
  /// ```
  /// # use evitable::EvitableError;
  /// use skorm_curie::{CurieStore, ErrorKind};
  ///
  /// let mut store = CurieStore::new();
  ///
  /// // No base has been configured, so an error will be returned.
  /// assert_eq!(store.expand("Book").unwrap_err().kind(), ErrorKind::InvalidMapping);
  ///
  /// store.set_base("http://schema.org/");
  /// assert_eq!(store.expand("Book").unwrap(), "http://schema.org/Book");
  pub fn set_base(&mut self, base: impl Into<String>) {
    self.base = Some(base.into());
  }

  /// Add a prefix to the mapping.
  pub fn add_prefix(&mut self, prefix: impl Into<String>, value: impl Into<String>) -> Result<()> {
    let prefix = prefix.into();
    ensure!(prefix != "_", Context::ReservedPrefix);
    ensure!(
      !self.mappings.contains_key(&prefix),
      Context::PrefixInUse { prefix }
    );

    self.mappings.insert(prefix, value.into());
    Ok(())
  }

  /// Expand a CURIE, returning a complete IRI.
  pub fn expand<'b, 'c>(&self, curie: impl IntoCurie<'b, 'c>) -> Result<IriString> {
    match curie.into_curie()? {
      Curie::LongForm(cow) => Ok(cow.into_owned()),
      Curie::ShortForm { prefix, reference } => {
        if let Some(prefix) = prefix {
          if let Some(base) = self.mappings.get(prefix.as_ref()) {
            let mut string = String::with_capacity(base.len() + reference.len());
            string.push_str(base);
            string.push_str(&reference);
            let resolved = string
              .parse::<IriString>()
              .context(move || Context::ParseError { input: string })?;
            Ok(resolved)
          } else {
            Err(Context::InvalidMapping.into())
          }
        } else if let Some(base) = &self.base {
          let mut string = String::with_capacity(base.len() + reference.len());
          string.push_str(base);
          string.push_str(&reference);
          let resolved = string
            .parse::<IriString>()
            .context(move || Context::ParseError { input: string })?;
          Ok(resolved)
        } else {
          Err(Context::InvalidMapping.into())
        }
      }
    }
  }

  pub fn shrink<'a, 'b>(&'a self, iri: &'b IriStr) -> Result<Curie<'a, 'b>> {
    if let Some(base) = &self.base {
      if iri.as_str().starts_with(base.as_str()) {
        let reference = &iri.as_str()[base.as_str().len()..];
        return Ok(Curie::ShortForm {
          prefix: None,
          reference: Cow::Borrowed(reference),
        });
      }
    }

    for mp in &self.mappings {
      if iri.as_str().starts_with(mp.1.as_str()) {
        let reference = &iri.as_str()[mp.1.as_str().len()..];
        return Ok(Curie::ShortForm {
          prefix: Some(Cow::Borrowed(mp.0)),
          reference: Cow::Borrowed(reference),
        });
      }
    }

    Ok(Curie::LongForm(Cow::Borrowed(iri)))
  }
}

#[derive(Debug)]
pub enum Curie<'a, 'b> {
  ShortForm {
    prefix: Option<Cow<'a, str>>,
    reference: Cow<'b, str>,
  },

  LongForm(Cow<'b, IriStr>),
}

impl<'a, 'b> Curie<'a, 'b> {
  pub fn to_owned(self) -> Curie<'static, 'static> {
    match self {
      Curie::ShortForm { prefix, reference } => Curie::ShortForm {
        prefix: prefix.map(|prefix| Cow::Owned(prefix.into_owned())),
        reference: Cow::Owned(reference.into_owned()),
      },

      Curie::LongForm(iri) => Curie::LongForm(Cow::Owned(iri.into_owned())),
    }
  }

  pub fn prefix(&self) -> Option<&str> {
    match self {
      Curie::ShortForm { prefix, .. } => match prefix {
        None => None,
        Some(cow) => Some(cow.as_ref()),
      },
      Curie::LongForm(..) => None,
    }
  }

  pub fn reference(&self) -> Option<&str> {
    match self {
      Curie::ShortForm { reference, .. } => Some(reference.as_ref()),
      Curie::LongForm(..) => None,
    }
  }

  pub fn iri(&self) -> Option<&IriStr> {
    match self {
      Curie::ShortForm { .. } => None,
      Curie::LongForm(cow) => Some(cow.as_ref()),
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  const SCHEMA_VOCAB: &'static str = "http://schema.org/";
  const OWL_VOCAB: &'static str = "http://www.w3.org/2002/07/owl#";

  #[test]
  fn kitchen_sink() -> Result<()> {
    let mut store = CurieStore::new();
    store.add_prefix("schema", SCHEMA_VOCAB)?;
    store.add_prefix("owl", OWL_VOCAB)?;

    let person = store.expand("schema:Person").unwrap();
    let named_individual = store.expand("owl:NamedIndividual").unwrap();
    let unknown = store.expand("http://example.com/Unknown").unwrap();

    assert_eq!(person, "http://schema.org/Person");
    assert_eq!(
      named_individual,
      "http://www.w3.org/2002/07/owl#NamedIndividual"
    );

    let s_person = store.shrink(&person)?;
    let s_named_individual = store.shrink(&named_individual)?;
    let s_unknown = store.shrink(&unknown)?;

    assert_eq!(s_person.prefix(), Some("schema"));
    assert_eq!(s_named_individual.prefix(), Some("owl"));
    assert_eq!(s_unknown.iri(), Some(unknown.as_ref()));

    Ok(())
  }
}

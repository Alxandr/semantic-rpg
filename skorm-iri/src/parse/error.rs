use evitable::*;

#[evitable(description("Parse error: {}\nsource: {}", error, source))]
pub struct Context {
  pub(super) source: String,
  pub(super) error: String,
}

use evitable::*;

#[evitable]
pub enum Context {
  #[evitable(description("IO error"), from = std::io::Error)]
  Io,

  #[evitable(description("Writer closed"))]
  WriterClosed,
}

#![feature(extern_types)]

mod parse;
mod ty;

pub use parse::try_parse;
pub use ty::{IriBorrowed, IriStr, IriString};

#[cfg(test)]
mod tests {
  use super::*;

  const VALID_IRIS: &[&str] = &[
    // RFC 3987 itself.
    "https://tools.ietf.org/html/rfc3987",
    // RFC 3987 section 3.1.
    "http://r\u{E9}sum\u{E9}.example.org",
    "http://xn--rsum-bpad.example.org",
    "http://r%C3%A9sum%C3%A9.example.org",
    "http://www.example.org/red%09ros\u{E9}#red",
    "http://www.example.org/red%09ros%C3%A9#red",
    // RFC 3987 section 3.2.
    "http://example.com/\u{10300}\u{10301}\u{10302}",
    "http://example.com/%F0%90%8C%80%F0%90%8C%81%F0%90%8C%82",
    // RFC 3987 section 3.2.1.
    "http://www.example.org/r%C3%A9sum%C3%A9.html",
    "http://www.example.org/r%E9sum%E9.html",
    "http://www.example.org/D%C3%BCrst",
    "http://www.example.org/D%FCrst",
    "http://www.example.org/D\u{FC}rst",
    "http://xn--99zt52a.example.org/%e2%80%ae",
    "http://xn--99zt52a.example.org/%E2%80%AE",
    "http://\u{7D0D}\u{8C46}.example.org/%E2%80%AE",
    // RFC 3987 section 4.4.
    "http://ab.CDEFGH.ij/kl/mn/op.html",
    "http://ab.CDE.FGH/ij/kl/mn/op.html",
    "http://AB.CD.EF/GH/IJ/KL?MN=OP;QR=ST#UV",
    "http://AB.CD.ef/gh/IJ/KL.html",
    "http://ab.cd.EF/GH/ij/kl.html",
    "http://ab.CD.EF/GH/IJ/kl.html",
    "http://ab.CDE123FGH.ij/kl/mn/op.html",
    "http://ab.cd.ef/GH1/2IJ/KL.html",
    "http://ab.cd.ef/GH%31/%32IJ/KL.html",
    "http://ab.CDEFGH.123/kl/mn/op.html",
    // RFC 3987 section 5.2.
    "http://example.org/ros\u{E9}",
    // RFC 3987 section 5.3.2.
    "example://a/b/c/%7Bfoo%7D/ros\u{E9}",
    "eXAMPLE://a/./b/../b/%63/%7bfoo%7d/ros%C3%A9",
    // RFC 3987 section 5.3.2.1.
    "HTTP://www.EXAMPLE.com/",
    "http://www.example.com/",
    // RFC 3987 section 5.3.2.2.
    "http://www.example.org/r\u{E9}sum\u{E9}.html",
    "http://www.example.org/re\u{301}sume\u{301}.html",
    // RFC 3987 section 5.3.2.3.
    "http://example.org/~user",
    "http://example.org/%7euser",
    "http://example.org/%7Euser",
    // RFC 3987 section 5.3.3.
    "http://example.com",
    "http://example.com/",
    "http://example.com:/",
    "http://example.com:80/",
    "http://r\u{E9}sum\u{E9}.example.org", // duplicate
    "http://xn--rsum-bpad.example.org",    // duplicate
    // RFC 3987 section 5.3.4.
    "http://example.com/data",
    "http://example.com/data/",
    // RFC 3987 section 6.4.
    "http://www.example.org/r%C3%A9sum%C3%A9.html", // duplicate
    "http://www.example.org/r\u{E9}sum\u{E9}.html", // duplicate
    "http://www.example.org/r%E9sum%E9.html",       // duplicate
    "http://www.example.org/r%E9sum%E9.xml#r\u{E9}sum\u{E9}",
  ];

  #[test]
  fn valid_iris() {
    for s in VALID_IRIS {
      try_parse(*s).unwrap();
    }
  }

  #[test]
  fn simple() {
    let iri = try_parse("http://example.com/").unwrap();
    assert_eq!(iri.scheme(), "http");
    assert_eq!(iri.authority(), Some("example.com"));
    assert_eq!(iri.userinfo(), None);
    assert_eq!(iri.host(), Some("example.com"));
    assert_eq!(iri.port(), None);
    assert_eq!(iri.path(), "/");
    assert_eq!(iri.query(), None);
    assert_eq!(iri.fragment(), None);
  }

  #[test]
  fn complex() {
    let iri = try_parse("https://user:pass@a.example.com:8080/b/c/d/?123&aa=1&aa=2#455").unwrap();
    assert_eq!(iri.scheme(), "https");
    assert_eq!(iri.authority(), Some("user:pass@a.example.com:8080"));
    assert_eq!(iri.userinfo(), Some("user:pass"));
    assert_eq!(iri.host(), Some("a.example.com"));
    assert_eq!(iri.port(), Some("8080"));
    assert_eq!(iri.path(), "/b/c/d/");
    assert_eq!(iri.query(), Some("123&aa=1&aa=2"));
    assert_eq!(iri.fragment(), Some("455"));
  }
}

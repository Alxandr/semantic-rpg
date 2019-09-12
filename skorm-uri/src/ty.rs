use std::borrow::Borrow;
use std::cmp::Ordering;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::{Deref, Range};

macro_rules! as_str {
  ($val:expr) => {
    AsRef::<str>::as_ref($val)
  };
}

macro_rules! header {
  ($val:expr) => {
    AsRef::<IriHeader>::as_ref($val)
  };
}

macro_rules! impl_iri_type {
  ($ty:ty $(,$im:tt)?) => {
    impl$(<$im>)? PartialEq for $ty {
      #[inline]
      fn eq(&self, other: &Self) -> bool {
        PartialEq::eq(as_str!(self), as_str!(other))
      }
    }

    impl$(<$im>)? Eq for $ty {}

    impl$(<$im>)? PartialEq<str> for $ty {
      #[inline]
      fn eq(&self, other: &str) -> bool {
        PartialEq::eq(as_str!(self), other)
      }
    }

    impl$(<$im>)? PartialEq<&str> for $ty {
      #[inline]
      fn eq(&self, other: &&str) -> bool {
        PartialEq::eq(as_str!(self), *other)
      }
    }

    impl$(<$im>)? PartialOrd for $ty {
      #[inline]
      fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        PartialOrd::partial_cmp(as_str!(self), as_str!(other))
      }
    }

    impl$(<$im>)? Ord for $ty {
      #[inline]
      fn cmp(&self, other: &Self) -> Ordering {
        Ord::cmp(as_str!(self), as_str!(other))
      }
    }

    impl$(<$im>)? Hash for $ty {
      #[inline]
      fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash(as_str!(self), state)
      }
    }

    impl$(<$im>)? Borrow<str> for $ty {
      #[inline]
      fn borrow(&self) -> &str {
        as_str!(self)
      }
    }

    impl$(<$im>)? Display for $ty {
      #[inline]
      fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        <str as Display>::fmt(as_str!(self), f)
      }
    }

    impl$(<$im>)? Debug for $ty {
      #[inline]
      fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        <IriHeader as Debug>::fmt(header!(self), f)
      }
    }
  };
}

#[derive(Clone)]
pub struct IriRanges {
  pub(crate) scheme: Range<usize>,
  pub(crate) authority: Option<Range<usize>>,
  pub(crate) userinfo: Option<Range<usize>>,
  pub(crate) host: Option<Range<usize>>,
  pub(crate) port: Option<Range<usize>>,
  pub(crate) path: Range<usize>,
  pub(crate) query: Option<Range<usize>>,
  pub(crate) fragment: Option<Range<usize>>,
}

struct IriHeader {
  ranges: IriRanges,
  ptr: *const str,
}

impl Debug for IriHeader {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    let ptr = self as *const IriHeader as *const IriStr;
    let s: &IriStr = unsafe { &*ptr };

    f.debug_struct("Iri")
      .field("iri", &s.as_str())
      .field("scheme", &s.scheme())
      .field("authority", &s.authority())
      .field("userinfo", &s.userinfo())
      .field("host", &s.host())
      .field("port", &s.port())
      .field("path", &s.path())
      .field("query", &s.query())
      .field("fragment", &s.fragment())
      .finish()
  }
}

impl AsRef<str> for IriHeader {
  #[inline]
  fn as_ref(&self) -> &str {
    unsafe { &*self.ptr }
  }
}

pub struct IriString {
  header: IriHeader,
  content: String,
}

impl AsRef<IriHeader> for IriString {
  #[inline]
  fn as_ref(&self) -> &IriHeader {
    &self.header
  }
}

impl AsRef<str> for IriString {
  #[inline]
  fn as_ref(&self) -> &str {
    &self.content
  }
}

impl_iri_type!(IriString);

extern "C" {
  /// A dummy type used to force Slice to by unsized without requiring fat pointers
  type OpaqueSliceContents;
}

#[repr(C)]
pub struct IriStr {
  header: IriHeader,
  opaque: OpaqueSliceContents,
}

impl AsRef<IriHeader> for IriStr {
  #[inline]
  fn as_ref(&self) -> &IriHeader {
    &self.header
  }
}

impl AsRef<str> for IriStr {
  #[inline]
  fn as_ref(&self) -> &str {
    self.header.as_ref()
  }
}

impl IriStr {
  #[inline]
  pub fn as_str(&self) -> &str {
    self.as_ref()
  }

  #[inline]
  fn range(&self, range: &Range<usize>) -> &str {
    &self.as_str()[range.clone()]
  }

  #[inline]
  pub fn scheme(&self) -> &str {
    self.range(&self.header.ranges.scheme)
  }

  #[inline]
  pub fn authority(&self) -> Option<&str> {
    self.header.ranges.authority.as_ref().map(|r| self.range(r))
  }

  #[inline]
  pub fn userinfo(&self) -> Option<&str> {
    self.header.ranges.userinfo.as_ref().map(|r| self.range(r))
  }

  #[inline]
  pub fn host(&self) -> Option<&str> {
    self.header.ranges.host.as_ref().map(|r| self.range(r))
  }

  #[inline]
  pub fn port(&self) -> Option<&str> {
    self.header.ranges.port.as_ref().map(|r| self.range(r))
  }

  #[inline]
  pub fn path(&self) -> &str {
    self.range(&self.header.ranges.path)
  }

  #[inline]
  pub fn query(&self) -> Option<&str> {
    self.header.ranges.query.as_ref().map(|r| self.range(r))
  }

  #[inline]
  pub fn fragment(&self) -> Option<&str> {
    self.header.ranges.fragment.as_ref().map(|r| self.range(r))
  }
}

impl_iri_type!(IriStr);

impl AsRef<IriStr> for IriString {
  #[inline]
  fn as_ref(&self) -> &IriStr {
    let ptr = &self.header as *const IriHeader as *const IriStr;
    unsafe { &*ptr }
  }
}

impl Borrow<IriStr> for IriString {
  #[inline]
  fn borrow(&self) -> &IriStr {
    self.as_ref()
  }
}

impl Deref for IriString {
  type Target = IriStr;

  #[inline]
  fn deref(&self) -> &IriStr {
    self.borrow()
  }
}

impl ToOwned for IriStr {
  type Owned = IriString;

  fn to_owned(&self) -> Self::Owned {
    let content = self.header.as_ref().to_owned();
    IriString::new(content, self.header.ranges.clone())
  }
}

pub struct IriBorrowed<'a> {
  header: IriHeader,
  content: &'a str,
}

impl<'a> IriBorrowed<'a> {
  pub fn to_owned(&self) -> IriString {
    let ranges = self.header.ranges.clone();
    let content = self.content.to_owned();
    IriString::new(content, ranges)
  }
}

impl<'a> Clone for IriBorrowed<'a> {
  #[inline]
  fn clone(&self) -> Self {
    let content = self.content;
    let header = IriHeader {
      ranges: self.header.ranges.clone(),
      ptr: self.header.ptr,
    };

    IriBorrowed { header, content }
  }
}

impl<'a> AsRef<IriHeader> for IriBorrowed<'a> {
  #[inline]
  fn as_ref(&self) -> &IriHeader {
    &self.header
  }
}

impl<'a> AsRef<str> for IriBorrowed<'a> {
  #[inline]
  fn as_ref(&self) -> &str {
    &self.content
  }
}

impl_iri_type!(IriBorrowed<'a>, 'a);

impl<'a> AsRef<IriStr> for IriBorrowed<'a> {
  #[inline]
  fn as_ref(&self) -> &IriStr {
    let ptr = &self.header as *const IriHeader as *const IriStr;
    unsafe { &*ptr }
  }
}

impl<'a> Borrow<IriStr> for IriBorrowed<'a> {
  #[inline]
  fn borrow(&self) -> &IriStr {
    self.as_ref()
  }
}

impl<'a> Deref for IriBorrowed<'a> {
  type Target = IriStr;

  #[inline]
  fn deref(&self) -> &IriStr {
    self.borrow()
  }
}

pub trait IntoIri: Borrow<str> {
  type Iri: Deref<Target = IriStr>;

  fn into_iri(self, ranges: IriRanges) -> Self::Iri;
}

impl IriString {
  fn new(content: String, ranges: IriRanges) -> Self {
    let ptr = as_str!(&content) as *const str;
    let header = IriHeader { ptr, ranges };
    Self { content, header }
  }
}

impl<'a> IriBorrowed<'a> {
  pub(crate) fn new(content: &'a str, ranges: IriRanges) -> Self {
    let ptr = content as *const str;
    let header = IriHeader { ptr, ranges };
    Self { content, header }
  }
}

impl IntoIri for String {
  type Iri = IriString;

  #[inline]
  fn into_iri(self, ranges: IriRanges) -> Self::Iri {
    IriString::new(self, ranges)
  }
}

impl<'a> IntoIri for &'a str {
  type Iri = IriBorrowed<'a>;

  #[inline]
  fn into_iri(self, ranges: IriRanges) -> Self::Iri {
    IriBorrowed::new(self, ranges)
  }
}

// #[cfg(test)]
// mod tests {
//   use super::*;

//   #[test]
//   fn it_works() {
//     let header_ref_size = std::mem::size_of::<&IriHeader>();
//     let iristr_ref_size = std::mem::size_of::<&IriStr>();
//     assert_eq!(header_ref_size, iristr_ref_size);

//     let ranges = IriRanges { scheme: 0..4 };
//     let owned = IriString::new("http://schema.org/Person".into(), ranges);
//     let borrowed: &IriStr = &owned;
//     let inner: &str = borrowed.as_ref();
//     assert_eq!(owned, "http://schema.org/Person");
//     assert_eq!(borrowed, "http://schema.org/Person");
//     assert_eq!(inner, "http://schema.org/Person");
//     assert_eq!(2 + 2, 4);
//   }
// }

use static_assertions::{assert_eq_align, assert_eq_size};
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

impl IriHeader {
  fn as_iri(&self) -> &Iri {
    assert_eq_align!(*const IriHeader, *const Iri);
    assert_eq_size!(*const IriHeader, *const Iri);
    let ptr = self as *const IriHeader as *const Iri;
    unsafe { &*ptr }
  }
}

impl Debug for IriHeader {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    let s = self.as_iri();

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

pub struct IriBuf {
  header: IriHeader,
  content: String,
}

impl Clone for IriBuf {
  #[inline]
  fn clone(&self) -> Self {
    Borrow::<Iri>::borrow(self).to_owned()
  }
}

impl AsRef<IriHeader> for IriBuf {
  #[inline]
  fn as_ref(&self) -> &IriHeader {
    &self.header
  }
}

impl AsRef<str> for IriBuf {
  #[inline]
  fn as_ref(&self) -> &str {
    &self.content
  }
}

impl_iri_type!(IriBuf);

extern "C" {
  /// A dummy type used to force Slice to by unsized without requiring fat pointers
  type OpaqueSliceContents;
}

#[repr(C)]
pub struct Iri {
  header: IriHeader,
  opaque: OpaqueSliceContents,
}

impl AsRef<IriHeader> for Iri {
  #[inline]
  fn as_ref(&self) -> &IriHeader {
    &self.header
  }
}

impl AsRef<str> for Iri {
  #[inline]
  fn as_ref(&self) -> &str {
    self.header.as_ref()
  }
}

impl Iri {
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

impl_iri_type!(Iri);

impl AsRef<Iri> for IriBuf {
  #[inline]
  fn as_ref(&self) -> &Iri {
    self.header.as_iri()
  }
}

impl Borrow<Iri> for IriBuf {
  #[inline]
  fn borrow(&self) -> &Iri {
    self.as_ref()
  }
}

impl Deref for IriBuf {
  type Target = Iri;

  #[inline]
  fn deref(&self) -> &Iri {
    self.borrow()
  }
}

impl ToOwned for Iri {
  type Owned = IriBuf;

  fn to_owned(&self) -> Self::Owned {
    let content = self.header.as_ref().to_owned();
    IriBuf::new(content, self.header.ranges.clone())
  }
}

pub struct IriBorrowed<'a> {
  header: IriHeader,
  content: &'a str,
}

impl<'a> IriBorrowed<'a> {
  pub fn to_owned(&self) -> IriBuf {
    let ranges = self.header.ranges.clone();
    let content = self.content.to_owned();
    IriBuf::new(content, ranges)
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

impl<'a> AsRef<Iri> for IriBorrowed<'a> {
  #[inline]
  fn as_ref(&self) -> &Iri {
    self.header.as_iri()
  }
}

impl<'a> Borrow<Iri> for IriBorrowed<'a> {
  #[inline]
  fn borrow(&self) -> &Iri {
    self.as_ref()
  }
}

impl<'a> Deref for IriBorrowed<'a> {
  type Target = Iri;

  #[inline]
  fn deref(&self) -> &Iri {
    self.borrow()
  }
}

pub trait IntoIri: Borrow<str> {
  type Iri: Deref<Target = Iri>;

  fn into_iri(self, ranges: IriRanges) -> Self::Iri;
}

impl IriBuf {
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
  type Iri = IriBuf;

  #[inline]
  fn into_iri(self, ranges: IriRanges) -> Self::Iri {
    IriBuf::new(self, ranges)
  }
}

impl<'a> IntoIri for &'a str {
  type Iri = IriBorrowed<'a>;

  #[inline]
  fn into_iri(self, ranges: IriRanges) -> Self::Iri {
    IriBorrowed::new(self, ranges)
  }
}

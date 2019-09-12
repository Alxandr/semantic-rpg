use nom::{
  error::{ErrorKind, ParseError},
  Compare, CompareResult, Err, IResult, InputIter, InputLength, InputTake, InputTakeAtPosition,
  Slice,
};
use std::fmt;
use std::ops::RangeFrom;

pub(super) trait InputLocate {
  fn offset(&self) -> usize;
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(super) struct WithPos<T> {
  pos: usize,
  input: T,
}

impl<T: Clone> WithPos<T> {}

impl<T> WithPos<T> {
  pub(super) fn new(input: T) -> Self {
    Self { pos: 0, input }
  }

  pub(super) fn into_inner(self) -> T {
    self.input
  }
}

impl<T: fmt::Display> fmt::Display for WithPos<T> {
  #[inline]
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.input.fmt(f)
  }
}

impl<T> InputLocate for WithPos<T> {
  #[inline]
  fn offset(&self) -> usize {
    self.pos
  }
}

impl<U, T: Compare<U>> Compare<U> for WithPos<T> {
  #[inline]
  fn compare(&self, t: U) -> CompareResult {
    self.input.compare(t)
  }

  #[inline]
  fn compare_no_case(&self, t: U) -> CompareResult {
    self.input.compare_no_case(t)
  }
}

impl<T: InputLength> InputLength for WithPos<T> {
  #[inline]
  fn input_len(&self) -> usize {
    self.input.input_len()
  }
}

impl<T: InputIter> InputIter for WithPos<T> {
  type Item = <T as InputIter>::Item;
  type Iter = <T as InputIter>::Iter;
  type IterElem = <T as InputIter>::IterElem;

  #[inline]
  fn iter_indices(&self) -> Self::Iter {
    self.input.iter_indices()
  }

  #[inline]
  fn iter_elements(&self) -> Self::IterElem {
    self.input.iter_elements()
  }

  #[inline]
  fn position<P>(&self, predicate: P) -> Option<usize>
  where
    P: Fn(Self::Item) -> bool,
  {
    self.input.position(predicate)
  }

  #[inline]
  fn slice_index(&self, count: usize) -> Option<usize> {
    self.input.slice_index(count)
  }
}

impl<T: InputTake> InputTake for WithPos<T> {
  #[inline]
  fn take(&self, count: usize) -> Self {
    Self {
      pos: self.pos,
      input: self.input.take(count),
    }
  }

  #[inline]
  fn take_split(&self, count: usize) -> (Self, Self) {
    let (l, r) = self.input.take_split(count);
    (
      Self {
        pos: self.pos + count,
        input: l,
      },
      Self {
        pos: self.pos,
        input: r,
      },
    )
  }
}

impl<T: InputIter + InputTake + InputLength + Clone> InputTakeAtPosition for WithPos<T> {
  type Item = <T as InputIter>::Item;

  /// looks for the first element of the input type for which the condition returns true,
  /// and returns the input up to this position
  ///
  /// *streaming version*: if no element is found matching the condition, this will return `Incomplete`
  fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E>
  where
    P: Fn(Self::Item) -> bool,
  {
    match self.input.position(predicate) {
      Some(n) => Ok(self.take_split(n)),
      None => Err(Err::Incomplete(nom::Needed::Size(1))),
    }
  }

  /// looks for the first element of the input type for which the condition returns true
  /// and returns the input up to this position
  ///
  /// fails if the produced slice is empty
  ///
  /// *streaming version*: if no element is found matching the condition, this will return `Incomplete`
  fn split_at_position1<P, E: ParseError<Self>>(
    &self,
    predicate: P,
    e: ErrorKind,
  ) -> IResult<Self, Self, E>
  where
    P: Fn(Self::Item) -> bool,
  {
    match self.input.position(predicate) {
      Some(0) => Err(Err::Error(E::from_error_kind(self.clone(), e))),
      Some(n) => Ok(self.take_split(n)),
      None => Err(Err::Incomplete(nom::Needed::Size(1))),
    }
  }

  /// looks for the first element of the input type for which the condition returns true,
  /// and returns the input up to this position
  ///
  /// *complete version*: if no element is found matching the condition, this will return the whole input
  fn split_at_position_complete<P, E: ParseError<Self>>(
    &self,
    predicate: P,
  ) -> IResult<Self, Self, E>
  where
    P: Fn(Self::Item) -> bool,
  {
    match self.input.position(predicate) {
      Some(n) => Ok(self.take_split(n)),
      None => Ok(self.take_split(self.input_len())),
    }
  }

  /// looks for the first element of the input type for which the condition returns true
  /// and returns the input up to this position
  ///
  /// fails if the produced slice is empty
  ///
  /// *complete version*: if no element is found matching the condition, this will return the whole input
  fn split_at_position1_complete<P, E: ParseError<Self>>(
    &self,
    predicate: P,
    e: ErrorKind,
  ) -> IResult<Self, Self, E>
  where
    P: Fn(Self::Item) -> bool,
  {
    match self.input.position(predicate) {
      Some(0) => Err(Err::Error(E::from_error_kind(self.clone(), e))),
      Some(n) => Ok(self.take_split(n)),
      None => {
        let len = self.input_len();
        if len == 0 {
          Err(Err::Error(E::from_error_kind(self.clone(), e)))
        } else {
          Ok(self.take_split(len))
        }
      }
    }
  }
}

impl<T: Slice<RangeFrom<usize>>> Slice<RangeFrom<usize>> for WithPos<T> {
  #[inline]
  fn slice(&self, range: RangeFrom<usize>) -> Self {
    let pos = self.pos + range.start;
    let input = self.input.slice(range);

    Self { pos, input }
  }
}

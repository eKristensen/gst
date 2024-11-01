use std::str::{CharIndices, Chars, FromStr};

use std::ops::{Range, RangeFrom, RangeFull, RangeTo};

use nom::{
    error::{ErrorKind, ParseError},
    AsBytes, Compare, CompareResult, Err, FindSubstring, FindToken, IResult, InputIter,
    InputLength, InputTake, InputTakeAtPosition, Needed, Offset, ParseTo, Slice,
};

// https://github.com/rust-bakery/nom/blob/main/doc/custom_input_types.md
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CInput<'a> {
    pub line_offsets: Vec<usize>,
    pub input: &'a str,
}

impl<'a> CInput<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut line_offsets = vec![0];
        for (i, c) in input.chars().enumerate() {
            if c == '\n' {
                line_offsets.push(i + 1);
            }
        }
        CInput {
            line_offsets,
            input,
        }
    }
}

impl<'a> AsBytes for CInput<'a> {
    #[inline(always)]
    fn as_bytes(&self) -> &[u8] {
        (*self.input).as_bytes()
    }
}

impl<'a, 'b> Compare<CInput<'b>> for CInput<'a> {
    #[inline(always)]
    fn compare(&self, t: CInput<'b>) -> CompareResult {
        AsBytes::as_bytes(self.input).compare(t.input)
    }
    #[inline(always)]
    fn compare_no_case(&self, t: CInput<'b>) -> CompareResult {
        AsBytes::as_bytes(self.input).compare_no_case(t.input)
    }
}

impl<'a, 'b> Compare<&'b str> for CInput<'a> {
    #[inline(always)]
    fn compare(&self, t: &'b str) -> CompareResult {
        AsBytes::as_bytes(self.input).compare(t)
    }
    #[inline(always)]
    fn compare_no_case(&self, t: &'b str) -> CompareResult {
        AsBytes::as_bytes(self.input).compare_no_case(t)
    }
}

impl<'a, 'b> FindSubstring<CInput<'b>> for CInput<'a> {
    //returns byte index
    fn find_substring(&self, substr: CInput<'b>) -> Option<usize> {
        self.input.find(substr.input)
    }
}

impl<'a, 'b> FindSubstring<&'b str> for CInput<'a> {
    //returns byte index
    fn find_substring(&self, substr: &'b str) -> Option<usize> {
        self.input.find(substr)
    }
}

impl<'a> FindToken<char> for CInput<'a> {
    fn find_token(&self, token: char) -> bool {
        self.input.find_token(token)
    }
}

impl<'a> InputIter for CInput<'a> {
    type Item = char;
    type Iter = CharIndices<'a>;
    type IterElem = Chars<'a>;
    #[inline]
    fn iter_indices(&self) -> Self::Iter {
        self.input.char_indices()
    }
    #[inline]
    fn iter_elements(&self) -> Self::IterElem {
        self.input.chars()
    }
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        for (o, c) in self.input.char_indices() {
            if predicate(c) {
                return Some(o);
            }
        }
        None
    }
    #[inline]
    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        let mut cnt = 0;
        for (index, _) in self.input.char_indices() {
            if cnt == count {
                return Ok(index);
            }
            cnt += 1;
        }
        if cnt == count {
            return Ok(self.input.len());
        }
        Err(Needed::Unknown)
    }
}

impl<'a> InputLength for CInput<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        self.input.len()
    }
}

impl<'a> InputTake for CInput<'a> {
    #[inline]
    fn take(&self, count: usize) -> Self {
        CInput {
            input: &self.input[..count],
            line_offsets: self.line_offsets,
        }
    }

    // return byte index
    #[inline]
    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.input.split_at(count);
        (
            CInput {
                input: suffix,
                line_offsets: self.line_offsets,
            },
            CInput {
                input: prefix,
                line_offsets: self.line_offsets,
            },
        )
    }
}

impl<'a> InputTakeAtPosition for CInput<'a> {
    type Item = char;

    fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.input.find(predicate) {
            // find() returns a byte index that is already in the slice at a char boundary
            Some(i) => unsafe {
                Ok((
                    CInput {
                        input: self.input.get_unchecked(i..),
                        line_offsets: self.line_offsets,
                    },
                    CInput {
                        input: self.input.get_unchecked(..i),
                        line_offsets: self.line_offsets,
                    },
                ))
            },
            None => Err(Err::Incomplete(Needed::new(1))),
        }
    }

    fn split_at_position1<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.input.find(predicate) {
            Some(0) => Err(Err::Error(E::from_error_kind(*self, e))),
            // find() returns a byte index that is already in the slice at a char boundary
            Some(i) => unsafe {
                Ok((
                    CInput {
                        input: self.input.get_unchecked(i..),
                        line_offsets: self.line_offsets,
                    },
                    CInput {
                        input: self.input.get_unchecked(..i),
                        line_offsets: self.line_offsets,
                    },
                ))
            },
            None => Err(Err::Incomplete(Needed::new(1))),
        }
    }

    fn split_at_position_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.input.find(predicate) {
            // find() returns a byte index that is already in the slice at a char boundary
            Some(i) => unsafe {
                Ok((
                    CInput {
                        input: self.input.get_unchecked(i..),
                        line_offsets: self.line_offsets,
                    },
                    CInput {
                        input: self.input.get_unchecked(..i),
                        line_offsets: self.line_offsets,
                    },
                ))
            },
            // the end of slice is a char boundary
            None => unsafe {
                Ok((
                    CInput {
                        input: self.input.get_unchecked(self.input.len()..),
                        line_offsets: self.line_offsets,
                    },
                    CInput {
                        input: self.input.get_unchecked(..self.input.len()),
                        line_offsets: self.line_offsets,
                    },
                ))
            },
        }
    }

    fn split_at_position1_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.input.find(predicate) {
            Some(0) => Err(Err::Error(E::from_error_kind(*self, e))),
            // find() returns a byte index that is already in the slice at a char boundary
            Some(i) => unsafe {
                Ok((
                    CInput {
                        input: self.input.get_unchecked(i..),
                        line_offsets: self.line_offsets,
                    },
                    CInput {
                        input: self.input.get_unchecked(..i),
                        line_offsets: self.line_offsets,
                    },
                ))
            },
            None => {
                if self.input.is_empty() {
                    Err(Err::Error(E::from_error_kind(*self, e)))
                } else {
                    // the end of slice is a char boundary
                    unsafe {
                        Ok((
                            CInput {
                                input: self.input.get_unchecked(self.input.len()..),
                                line_offsets: self.line_offsets,
                            },
                            CInput {
                                input: self.input.get_unchecked(..self.input.len()),
                                line_offsets: self.line_offsets,
                            },
                        ))
                    }
                }
            }
        }
    }
}

impl<'a> Offset for CInput<'a> {
    fn offset(&self, second: &Self) -> usize {
        let fst = self.input.as_ptr();
        let snd = second.input.as_ptr();

        snd as usize - fst as usize
    }
}

impl<'a, R: FromStr> ParseTo<R> for CInput<'a> {
    fn parse_to(&self) -> Option<R> {
        self.input.parse().ok()
    }
}

// Macro to generate Slice trait implementations
macro_rules! impl_slice_for_cinput {
    ($range_type:ty) => {
        impl<'a> Slice<$range_type> for CInput<'a> {
            fn slice(&self, range: $range_type) -> Self {
                let new_input = &self.input[range];
                let new_line_offsets = self.line_offsets.clone(); // Adjust as needed
                CInput {
                    line_offsets: new_line_offsets,
                    input: new_input,
                }
            }
        }
    };
}

// Use the macro to implement Slice for different range types
impl_slice_for_cinput!(Range<usize>);
impl_slice_for_cinput!(RangeFrom<usize>);
impl_slice_for_cinput!(RangeTo<usize>);
impl_slice_for_cinput!(RangeFull);

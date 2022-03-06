//! # fast-json-fmt
//!
//! A fast, simple, opinionated JSON formatter.

pub fn format(s: &str) -> Option<String> {
    format_with_config(s, &Config::default())
}

pub fn format_with_config(s: &str, config: &Config) -> Option<String> {
    Node::parse(s).map(|ast| ast.format(config))
}

#[derive(Debug, Clone)]
pub struct Config {
    pub width_limit: usize,
    pub indent_width: usize,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            width_limit: 100,
            indent_width: 2,
        }
    }
}

/////////
// AST //
/////////

/// An element in an AST
#[derive(Debug, PartialEq, Eq)]
struct Node<'source> {
    /// The width of this node if it were to be 'unsplit' - i.e. all on one line
    unsplit_width: usize,
    kind: NodeKind<'source>,
}

#[derive(Debug, PartialEq, Eq)]
enum NodeKind<'source> {
    /// A JSON value that cannot be reformatted (number, string, null, true, false).  For the
    /// purposes of an autoformatter, these are perfectly equivalent
    Atom(&'source str),
    /// A JSON array
    Array(Vec<Node<'source>>),
    /// A JSON object.  Each element in the [`Vec`] is a `(key, value)` pair, and `key` is required
    /// to be a string.
    Object(Vec<(Node<'source>, Node<'source>)>),
}

/////////////
// PARSING //
/////////////

mod parsing {
    use crate::{Node, NodeKind};

    enum ValueParseResult<'source> {
        /// The value parsed to a node
        Node(Node<'source>),
        /// The first non-whitespace char was `']'`
        CloseSquare,
        /// The first non-whitespace char was `'}'`
        CloseBrace,
    }

    /// Consume the next character of `$iter`, assuming it matches a `$pattern`.
    macro_rules! expect_char_pattern {
        ($iter: expr, $pattern: pat) => {
            match $iter.next() {
                Some((_idx, $pattern)) => (),
                _ => return None,
            }
        };
    }

    impl<'source> Node<'source> {
        /// Parse a [`str`]ing into a JSON node
        pub fn parse(s: &'source str) -> Option<Self> {
            let mut iter = Iter::new(s);
            // Parse the JSON value as the root node
            let ast_root = match Self::parse_value(&mut iter) {
                Some(ValueParseResult::Node(n)) => n,
                _ => return None, // If we're parsing a JSON string, then anything other than an
                                  // JSON value isn't valid
            };
            // Assert that there's only whitespace until the end of the file
            loop {
                match iter.next() {
                    Some((_, ' ' | '\r' | '\n' | '\t')) => continue, // Consume any whitespace
                    Some(_) => return None, // Anything other than whitespace is an error
                    None => return Some(ast_root), // EOF with only whitespace is fine
                }
            }
        }

        /// Attempt to parse a single JSON value.  Special cases:
        /// - If the first non-whitespace char is `]`, then `Some(CloseSquare)` is returned
        /// - If the first non-whitespace char is `}`, then `Some(CloseBrace)` is returned
        ///
        /// NOTE: This does not parse trailing whitespace
        fn parse_value(iter: &mut Iter<'source>) -> Option<ValueParseResult<'source>> {
            while let Some((start_idx, c)) = iter.next() {
                /// Consume a sequence of chars, erroring if it's not as expected
                macro_rules! expect_ident {
                    ($len: literal => $( $chars: pat ),*) => {{
                        // Consume each char in turn, erroring if any of them aren't expected
                        $( expect_char_pattern!(iter, $chars); )*
                        // Construct and return an atom node
                        let sub_str = &iter.source[start_idx..start_idx + $len];
                        Node {
                            unsplit_width: $len,
                            kind: NodeKind::Atom(sub_str),
                        }
                    }};
                }

                let value_node = match c {
                    ' ' | '\t' | '\n' | '\r' => continue, // Ignore whitespace
                    '[' => Self::parse_array(iter)?,
                    '{' => Self::parse_object(iter)?,
                    '"' => Self::parse_string(start_idx, iter)?,
                    '-' => {
                        // If a '-' is reached, it must be followed by a digit then a number
                        // without the leading digit
                        expect_char_pattern!(iter, '0'..='9');
                        Self::parse_number_after_first_digit(start_idx, iter)?
                    }
                    // TODO: Handle the fact that leading 0s aren't allowed
                    '0'..='9' => Self::parse_number_after_first_digit(start_idx, iter)?,
                    'n' => expect_ident!(4 => 'u', 'l', 'l'),
                    't' => expect_ident!(4 => 'r', 'u', 'e'),
                    'f' => expect_ident!(5 => 'a', 'l', 's', 'e'),
                    _ => return None, // Any other chars would violate the JSON spec
                };
                // If a JSON value was successfully parsed, return that value
                return Some(ValueParseResult::Node(value_node));
            }
            None // If a JSON object was missing when the file ended, then that's an error
        }

        /// Attempt to parse the chars in `iter` as an array, **assuming that the initial `[` has
        /// been consumed**.
        fn parse_array(iter: &mut Iter<'source>) -> Option<Self> {
            todo!()
        }

        /// Attempt to parse the chars in `iter` as an object, **assuming that the initial `{` has
        /// been consumed**.
        fn parse_object(iter: &mut Iter<'source>) -> Option<Self> {
            todo!()
        }

        /// Attempt to parse the chars in `iter` as an string, **assuming that the initial `"` has
        /// been consumed**.
        fn parse_string(start_idx: usize, iter: &mut Iter<'source>) -> Option<Self> {
            todo!()
        }

        /// Parse a number, **assuming that the first digit has been consumed**
        fn parse_number_after_first_digit(
            start_idx: usize,
            iter: &mut Iter<'source>,
        ) -> Option<Self> {
            todo!()
        }
    }

    /// Char iterator that always lets you peek indices (even if the whole input has been consumed)
    // TODO: Consume the string byte-wise, since we're only interested in ASCII.  This'll probably
    // have a massive performance benefit
    struct Iter<'source> {
        inner: std::iter::Peekable<std::str::CharIndices<'source>>,
        source: &'source str,
    }

    impl<'source> Iter<'source> {
        fn new(source: &'source str) -> Self {
            Self {
                inner: source.char_indices().peekable(),
                source,
            }
        }

        fn substr_from_start(&mut self, start_idx: usize) -> &'source str {
            &self.source[start_idx..self.peek_idx()]
        }

        /// Gets the byte index of the next char to be popped (or the source's length if no chars
        /// are left)
        fn peek_idx(&mut self) -> usize {
            self.inner
                .peek()
                .map_or(self.source.len(), |(idx, _c)| *idx)
        }
    }

    impl<'source> Iterator for Iter<'source> {
        type Item = (usize, char);

        fn next(&mut self) -> Option<Self::Item> {
            self.inner.next()
        }
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[track_caller]
        fn check_fail(s: &str) {
            assert_eq!(Node::parse(s), None,);
        }

        #[test]
        fn fails() {
            check_fail("]");
            check_fail("}");
            check_fail("}   ");
            check_fail("  \t\n}   ");
        }

        #[test]
        fn literal() {
            #[track_caller]
            fn check(s: &str, literal: &str) {
                assert_eq!(
                    Node::parse(s).unwrap(),
                    Node {
                        unsplit_width: literal.len(),
                        kind: NodeKind::Atom(literal),
                    }
                );
            }

            check("true", "true");
            check("false", "false");
            check("null", "null");
            check("    null", "null");
            check("    null\t\n  ", "null");
            check_fail("    null  x"); // Check for things in trailing ws
        }
    }
}

//////////////////
// REFORMATTING //
//////////////////

impl<'source> Node<'source> {
    /// Convert the AST into a pretty-formatted string
    fn format(&self, config: &Config) -> String {
        todo!()
    }
}

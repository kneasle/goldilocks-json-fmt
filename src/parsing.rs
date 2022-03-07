//! Code to parse JSON string into an AST

use crate::{Node, NodeKind};

/// The result generated when a JSON value is parsed.  Special cases are made for ']' and '}' to
/// prevent lookahead for empty arrays/objects.
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
        // If the pattern contains '|', then we're forced to put brackets round the pattern,
        // which unnecessarily trips this lint
        #[allow(unused_parens)]
        match $iter.next() {
            Some((_idx, $pattern)) => (),
            _ => return None,
        }
    };
}

impl<'source> Node<'source> {
    /// Parse a [`str`]ing into a JSON node
    pub(crate) fn parse(s: &'source str) -> Option<Self> {
        let mut iter = Iter::new(s);
        // Parse the JSON value as the root node
        let ast_root = match parse_value(&mut iter) {
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
}

/// Attempt to parse a single JSON value.  Special cases:
/// - If the first non-whitespace char is `]`, then `Some(CloseSquare)` is returned
/// - If the first non-whitespace char is `}`, then `Some(CloseBrace)` is returned
///
/// NOTE: This does not parse trailing whitespace
fn parse_value<'source>(iter: &mut Iter<'source>) -> Option<ValueParseResult<'source>> {
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
            '[' => parse_array(iter)?,
            '{' => parse_object(iter)?,
            '"' => Node::new_atom(parse_string(start_idx, iter)?),
            // If a '-' is reached, it must be followed by a digit then a number without
            // the leading digit
            '-' => match iter.next() {
                Some((_, '0')) => parse_number_after_leading_0(start_idx, iter)?,
                Some((_, '1'..='9')) => parse_number_after_first_non_zero(start_idx, iter)?,
                _ => return None, // '-' must be followed by [0-9]
            },
            '0' => parse_number_after_leading_0(start_idx, iter)?,
            '1'..='9' => parse_number_after_first_non_zero(start_idx, iter)?,
            'n' => expect_ident!(4 => 'u', 'l', 'l'),
            't' => expect_ident!(4 => 'r', 'u', 'e'),
            'f' => expect_ident!(5 => 'a', 'l', 's', 'e'),
            ']' => return Some(ValueParseResult::CloseSquare),
            '}' => return Some(ValueParseResult::CloseBrace),
            _ => return None, // No other chars are a valid start to a JSON value
        };
        // If a JSON value was successfully parsed, return that value
        return Some(ValueParseResult::Node(value_node));
    }
    None // If a JSON object was missing when the file ended, then that's an error
}

/// Attempt to parse the chars in `iter` as an array, **assuming that the initial `[` has
/// been consumed**.
fn parse_array<'source>(iter: &mut Iter<'source>) -> Option<Node<'source>> {
    // Parse the first element
    match parse_value(iter)? {
        ValueParseResult::Node(n) => {
            // Found one value, so parse the others
            let mut unsplit_width = "[".len() + n.unsplit_width + "]".len();
            let mut contents = vec![n];
            // Repeatedly expect either:
            // - ',' followed by another element, or
            // - ']', finishing the array
            while let Some((_, c)) = iter.next() {
                match c {
                    ' ' | '\t' | '\n' | '\r' => continue, // Ignore whitespace
                    ']' => {
                        // Finish the array
                        return Some(Node {
                            unsplit_width,
                            kind: NodeKind::Array(contents),
                        });
                    }
                    ',' => {
                        // Parse another element, before looking for ',' or ']' again
                        if let Some(ValueParseResult::Node(n)) = parse_value(iter) {
                            unsplit_width += ", ".len() + n.unsplit_width;
                            contents.push(n);
                        } else {
                            return None; // If we didn't parse a value, then error
                        }
                    }
                    _ => return None, // No other chars are allowed
                }
            }
            return None; // It's an error for the file to end part-way through an array
        }
        // Array is `[]`, and therefore empty
        ValueParseResult::CloseSquare => Some(Node {
            unsplit_width: "[]".len(),
            kind: NodeKind::Array(vec![]),
        }),
        ValueParseResult::CloseBrace => None, // Can't end an array with `}`
    }
}

/// Attempt to parse an object, **assuming that the initial `{` has been consumed**.
fn parse_object<'source>(iter: &mut Iter<'source>) -> Option<Node<'source>> {
    let mut fields = Vec::<(&str, Node)>::new();
    let mut unsplit_width = "{ ".len() + " }".len();
    loop {
        // Parse ws until we get to a '"' for the key (returning if we see '}' and this is
        // the first field)
        let key = loop {
            let (start_idx, c) = iter.next()?;
            match c {
                ' ' | '\t' | '\n' | '\r' => continue, // Ignore whitespace
                '"' => break parse_string(start_idx, iter)?,
                '}' if fields.is_empty() => {
                    // '}' before the first key is an empty object
                    // TODO: Report idempotence bug
                    return Some(Node {
                        unsplit_width: 2, // "{}"
                        kind: NodeKind::Object(vec![]),
                    });
                }
                _ => return None, // Any other char is an error
            }
        };
        // Read whitespace until we find a ':'
        loop {
            match iter.next()?.1 {
                ' ' | '\t' | '\n' | '\r' => continue,
                ':' => break,     // ':' means we parse the key
                _ => return None, // Anything other than ':' or whitespace is an error
            }
        }
        // Parse the contained value
        let value = match parse_value(iter) {
            Some(ValueParseResult::Node(n)) => n,
            _ => return None,
        };
        // Add the field we just parsed
        unsplit_width += key.len() + ": ".len() + value.unsplit_width;
        fields.push((key, value));
        // Consume either ',' (parse next key/value pair) or '}' (end object)
        loop {
            match iter.next()?.1 {
                ' ' | '\t' | '\n' | '\r' => continue,
                ',' => break, // if ',', parse the next key/value pair
                '}' => {
                    return Some(Node {
                        unsplit_width,
                        kind: NodeKind::Object(fields),
                    });
                }
                _ => return None, // Any other char is an error
            }
        }
        unsplit_width += ", ".len(); // Add space required by the comma
    }
}

/// Attempt to parse the chars in `iter` as an string, **assuming that the initial `"` has
/// been consumed**.  This returns a string slice **from the JSON source code**, i.e. the fully
/// escaped string complete with the enclosing `"`s.  We do not attempt to decode the string, we
/// simply verify that it conforms to the JSON standard.
fn parse_string<'source>(start_idx: usize, iter: &mut Iter<'source>) -> Option<&'source str> {
    while let Some((idx, c)) = iter.next() {
        match c {
            // If we find an unescaped quote, then terminate the string.
            // `+ 1` is OK because '"' has UTF-8 length of 1 byte
            '"' => return Some(&iter.source[start_idx..idx + 1]),
            '\\' => match iter.next()?.1 {
                '"' | '\\' | '/' | 'b' | 'f' | 'n' | 'r' | 't' => {} // Valid escape chars
                'u' => {
                    // `\u` should be followed by 4 hex chars
                    for _ in 0..4 {
                        expect_char_pattern!(iter, ('0'..='9' | 'a'..='f' | 'A'..='F'));
                    }
                }
                _ => return None, // Invalid escape sequence
            },
            '\0'..='\x19' => return None, // Control chars aren't allowed in strings
            _ => {}                       // Any other char is just part of the string
        }
    }
    None // If a file ended during a string, then that's an error
}

////////////////////
// NUMBER PARSING //
////////////////////

/// Parse a number, assuming that the leading 0 has been consumed (i.e. the number so far
/// is `0` or `-0`)
fn parse_number_after_leading_0<'source>(
    start_idx: usize,
    iter: &mut Iter<'source>,
) -> Option<Node<'source>> {
    Some(match iter.peek_char() {
        Some('1'..='9') => return None, // Leading 0
        Some('.') => {
            iter.next();
            parse_number_after_decimal_point(start_idx, iter)?
        }
        Some('e' | 'E') => {
            iter.next();
            parse_number_after_exponent(start_idx, iter)?
        }
        // Number is '0' or '-0'.  Therefore, the peeked char is part of the next token
        // (e.g. it could be a ',')
        _ => iter.new_atom_starting_from(start_idx),
    })
}

/// Parse a number, assuming that a single non-zero digit has been consumed (i.e. the
/// number so far matches `[1-9]` or `-[1-9]`
fn parse_number_after_first_non_zero<'source>(
    start_idx: usize,
    iter: &mut Iter<'source>,
) -> Option<Node<'source>> {
    // TODO: Refactor all these functions into one loop?
    loop {
        match iter.peek_char() {
            Some('0'..='9') => {
                iter.next(); // Keep consuming numbers
            }
            Some('.') => {
                iter.next();
                return parse_number_after_decimal_point(start_idx, iter);
            }
            Some('e' | 'E') => {
                iter.next();
                return parse_number_after_exponent(start_idx, iter);
            }
            // Anything that isn't part of a number belongs to the next token (e.g. ',' to
            // move onto the next array element)
            _ => return Some(iter.new_atom_starting_from(start_idx)),
        }
    }
}

/// Parse a number, assuming that everything up to **and including** the decimal point has
/// been consumed.
fn parse_number_after_decimal_point<'source>(
    start_idx: usize,
    iter: &mut Iter<'source>,
) -> Option<Node<'source>> {
    loop {
        match iter.peek_char() {
            Some('0'..='9') => {
                iter.next(); // Keep consuming numbers
            }
            Some('.') => return None, // Can't have multiple decimal points
            Some('e' | 'E') => {
                iter.next(); // Consume the 'e'/'E'
                return parse_number_after_exponent(start_idx, iter);
            }
            // Anything that isn't part of a number belongs to the next token (e.g. ',' to
            // move onto the next array element)
            _ => return Some(iter.new_atom_starting_from(start_idx)),
        }
    }
}

/// Parse a number, assuming that everything up to **and including** the 'e' or 'E' has been
/// consumed
fn parse_number_after_exponent<'source>(
    start_idx: usize,
    iter: &mut Iter<'source>,
) -> Option<Node<'source>> {
    // An exponent can optionally start with a '+' or '-'
    if let Some('+' | '-') = iter.peek_char() {
        iter.next(); // Consume the '+/-' if it exists, otherwise start parsing digits
    }

    let mut has_at_least_one_digit = false;
    loop {
        match iter.peek_char() {
            Some('0'..='9') => iter.next(), // Numbers are always valid exponents
            Some('.') => return None,       // Can't have decimal exponents
            Some('e' | 'E') => return None, // Can't have multiple exponents
            Some('+' | '-') => return None, // Can't have '+' or '-' in the middle of an exponent
            // Anything that isn't part of a number belongs to the next token (e.g. ',' to
            // move onto the next array element)
            _ => match has_at_least_one_digit {
                true => return Some(iter.new_atom_starting_from(start_idx)),
                false => return None,
            },
        };
        has_at_least_one_digit = true;
    }
}

//////////////
// ITERATOR //
//////////////

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

    fn peek_char(&mut self) -> Option<char> {
        self.inner.peek().map(|(_idx, c)| *c)
    }

    fn new_atom_starting_from(&mut self, start_idx: usize) -> Node<'source> {
        Node::new_atom(&self.source[start_idx..self.peek_idx()])
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

    #[track_caller]
    fn check_atom(s: &str, literal: &str) {
        assert_eq!(
            Node::parse(s).expect("Parsing atom unexpectedly failed"),
            Node {
                unsplit_width: literal.len(),
                kind: NodeKind::Atom(literal),
            }
        );
    }

    #[track_caller]
    fn check_atom_no_ws(s: &str) {
        check_atom(s, s);
    }

    #[track_caller]
    fn check_ok(s: &str, exp_node: Node) {
        assert_eq!(Node::parse(s), Some(exp_node));
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
        check_atom_no_ws("true");
        check_atom_no_ws("false");
        check_atom_no_ws("null");
        check_atom("    null", "null");
        check_atom("    null\t\n  ", "null");
        check_fail("    null  x"); // Check for things in trailing ws
    }

    #[test]
    fn string() {
        check_atom_no_ws(r#""""#);
        // Escape sequences
        check_atom_no_ws(r#""thing \" thing""#);
        check_atom_no_ws(r#""thing \\ thing""#);
        check_atom_no_ws(r#""thing \\""#);
        check_atom_no_ws(r#""thing \"  \\""#);
        check_atom_no_ws(r#""thing \/ thing""#);
        check_atom_no_ws(r#""thing \uAFFF thing""#);
        check_atom_no_ws(r#""thing \u01aF thing""#);
        // Invalid escape
        check_fail(r#""thing \x thing""#);
        check_fail(r#""thing \uAFXF thing""#);
        // Leading/trailing whitespace
        check_atom("\r   \"\"", r#""""#);
        check_atom("\"\"  \r\t  \n  ", r#""""#);
        check_atom("\r   \"\"  \r\t  \n  ", r#""""#);
        check_atom("\r   \"string\"  \r\t  \n  ", r#""string""#);
        // Control chars in a string aren't allowed
        check_fail("    \"\0\"  x");
        check_fail("    \"\n\"  x");
        check_fail("    \"\t\"  x");
        check_fail(r#"    "string"  x"#); // Check for things in trailing ws
    }

    #[test]
    fn number() {
        check_atom_no_ws("0");
        check_atom_no_ws("-0");
        check_atom("   0   \t\n ", "0");
        check_fail("02");
        check_fail("-02");
        check_atom_no_ws("10233415216992347901");
        check_atom_no_ws("0.2");
        check_fail("0.2.3");
        check_atom_no_ws("-0.00002");
        check_atom_no_ws("0.0200000");
        check_atom_no_ws("0.02e1");
        check_atom_no_ws("0.02E-1201");
        check_atom_no_ws("0.02e-1201");
        check_atom_no_ws("0.02e+01201");
        check_fail("0.02e");
        check_fail("0.02e-");
        check_atom_no_ws("0e-01"); // Leading 0s in exponents is apparently allowed?
        check_atom_no_ws("0e01"); // Leading 0s in exponents is apparently allowed?
    }

    #[test]
    fn array() {
        check_ok(
            "[]",
            Node {
                unsplit_width: 2,
                kind: NodeKind::Array(vec![]),
            },
        );
        check_ok(
            "    [  ]\r\t  \n",
            Node {
                unsplit_width: 2,
                kind: NodeKind::Array(vec![]),
            },
        );
        check_fail("    [  }\r\t  \n");
        check_ok(
            "    [ true ]\r\t  \n",
            Node {
                unsplit_width: 6,
                kind: NodeKind::Array(vec![Node::new_atom("true")]),
            },
        );
        check_fail("    [ true, ]\r\t  \n");
        check_ok(
            "    [ true, false ]\r\t  \n",
            Node {
                unsplit_width: 13,
                kind: NodeKind::Array(vec![Node::new_atom("true"), Node::new_atom("false")]),
            },
        );
        check_ok(
            "    [ true, [\n\nfalse, []] ]\r\t  \n",
            Node {
                unsplit_width: "[true, [false, []]]".len(),
                kind: NodeKind::Array(vec![
                    Node::new_atom("true"),
                    Node {
                        unsplit_width: "[false, []]".len(),
                        kind: NodeKind::Array(vec![
                            Node::new_atom("false"),
                            Node {
                                unsplit_width: 2,
                                kind: NodeKind::Array(vec![]),
                            },
                        ]),
                    },
                ]),
            },
        );
    }

    #[test]
    fn object() {
        check_ok(
            "{}",
            Node {
                unsplit_width: "{}".len(),
                kind: NodeKind::Object(vec![]),
            },
        );
        check_ok(
            "  {\t \t \n\n    }    \t\t\n   ",
            Node {
                unsplit_width: "{}".len(),
                kind: NodeKind::Object(vec![]),
            },
        );
        check_ok(
            r#"{"key": "value"}"#,
            Node {
                unsplit_width: r#"{ "key": "value" }"#.len(),
                kind: NodeKind::Object(vec![("\"key\"", Node::new_atom("\"value\""))]),
            },
        );
        check_ok(
            r#"{"key": "value", "key2": "value2"}"#,
            Node {
                unsplit_width: r#"{ "key": "value", "key2": "value2" }"#.len(),
                kind: NodeKind::Object(vec![
                    ("\"key\"", Node::new_atom("\"value\"")),
                    ("\"key2\"", Node::new_atom("\"value2\"")),
                ]),
            },
        );
        check_ok(
            "  {\t \t \n\n \"key\"  \t:\n\n \"value\"  \t\n  }    \t\t\n   ",
            Node {
                unsplit_width: r#"{ "key": "value" }"#.len(),
                kind: NodeKind::Object(vec![("\"key\"", Node::new_atom("\"value\""))]),
            },
        );
        check_ok(
            "  {\t \t \n\n \"key\"  \t:\n\n [   \"value\" \t ]  \t\n  }    \t\t\n   ",
            Node {
                unsplit_width: r#"{ "key": ["value"] }"#.len(),
                kind: NodeKind::Object(vec![(
                    "\"key\"",
                    Node {
                        unsplit_width: r#"["value"]"#.len(),
                        kind: NodeKind::Array(vec![Node::new_atom("\"value\"")]),
                    },
                )]),
            },
        );

        let phat_node = Node {
            unsplit_width:
                r#"{ "key": "value", "key2": [{ "is_open": true }, { "is_open": false }, null] }"#
                    .len(),
            kind: NodeKind::Object(vec![
                ("\"key\"", Node::new_atom("\"value\"")),
                (
                    "\"key2\"",
                    Node {
                        unsplit_width: r#"[{ "is_open": true }, { "is_open": false }, null]"#.len(),
                        kind: NodeKind::Array(vec![
                            Node {
                                unsplit_width: r#"{ "is_open": true }"#.len(),
                                kind: NodeKind::Object(vec![(
                                    "\"is_open\"",
                                    Node::new_atom("true"),
                                )]),
                            },
                            Node {
                                unsplit_width: r#"{ "is_open": false }"#.len(),
                                kind: NodeKind::Object(vec![(
                                    "\"is_open\"",
                                    Node::new_atom("false"),
                                )]),
                            },
                            Node::new_atom("null"),
                        ]),
                    },
                ),
            ]),
        };
        check_ok(
            r#"{"key": "value", "key2": [{ "is_open": true }, { "is_open": false }, null ] }"#,
            phat_node.clone(),
        );
        check_ok(
            "  {\t \t \n\n \"key\"  \t:\n\n \"value\"  \t\n
                \t\t\r,
                \"key2\": [  { \"is_open\": true }, { \"is_open\"  \t: false }, null ]
                }    \t\t\n   ",
            phat_node,
        );
    }
}

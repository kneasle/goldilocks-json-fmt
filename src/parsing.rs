//! Code to parse JSON string into an AST

use crate::{Error, Expected, Item, Node, NodeKind, Result};

impl<'source> Node<'source> {
    /// Parse a [`str`]ing into a JSON node
    pub(crate) fn parse(s: &'source str) -> Result<Self> {
        let mut iter = Iter::new(s);
        // Parse the JSON value as the root node
        let ast_root = parse_value(Item::TopLevelValue, &mut iter)?;
        // Assert that there's only whitespace until the end of the file
        loop {
            match iter.next() {
                Some((_, ' ' | '\r' | '\n' | '\t')) => continue, // Consume any whitespace
                // Anything other than whitespace is an error
                Some((idx, c)) => return Err(Error::InvalidTrailingWhitespace(idx, c)),
                None => return Ok(ast_root), // EOF with only whitespace is fine
            }
        }
    }
}

/// Attempt to parse a JSON value.
///
/// NOTE: This does not parse trailing whitespace
fn parse_value<'source>(item: Item, iter: &mut Iter<'source>) -> Result<Node<'source>> {
    let (idx, unexpected_char) =
        match parse_value_or_closing_bracket(item, &[Expected::Value], iter)? {
            ValueParseResult::Node(n) => return Ok(n),
            ValueParseResult::CloseBrace(idx) => (idx, '}'),
            ValueParseResult::CloseSquare(idx) => (idx, ']'),
        };
    Err(Error::ExpectedXsFoundY(
        item,
        idx,
        &[Expected::Value],
        unexpected_char,
    ))
}

/// Attempt to parse a single JSON value, or a closing bracket (']' or '}').  Special cases:
/// - If the first non-whitespace char is `]`, then `Some(CloseSquare)` is returned
/// - If the first non-whitespace char is `}`, then `Some(CloseBrace)` is returned
///
/// NOTE: This does not parse trailing whitespace
fn parse_value_or_closing_bracket<'source>(
    item: Item,
    expected: &'static [Expected],
    iter: &mut Iter<'source>,
) -> Result<ValueParseResult<'source>> {
    while let Some((start_idx, c)) = iter.next() {
        /// Consume a sequence of chars, erroring if it's not as expected
        macro_rules! expect_ident {
            ($ident_name: literal => $( $chars: literal ),*) => {{
                // Consume each char in turn, erroring if any of them aren't expected
                $(
                match iter.next() {
                    Some((_, $chars)) => (),
                    v => return Err(Error::expected_xs_found(
                        Item::Literal($ident_name),
                        &[Expected::Char($chars)],
                        v
                    )),
                }
                )*
                // Construct and return an atom node
                let len = $ident_name.len();
                let sub_str = &iter.source[start_idx..start_idx + len];
                Node {
                    unsplit_width: len,
                    kind: NodeKind::Atom(sub_str),
                }
            }};
        }

        let value_node = match c {
            ' ' | '\t' | '\n' | '\r' => continue, // Ignore whitespace
            '[' => parse_array(start_idx, iter)?,
            '{' => parse_object(start_idx, iter)?,
            '"' => Node::new_atom(parse_string(start_idx, iter)?),
            // If a '-' is reached, it must be followed by a digit then a number without
            // the leading digit
            '-' => match iter.next() {
                Some((_, '0')) => parse_number_after_leading_0(start_idx, iter)?,
                Some((_, '1'..='9')) => parse_number_after_first_non_zero(start_idx, iter)?,
                v => {
                    return Err(Error::expected_xs_found(
                        Item::Number,
                        &[Expected::Digit],
                        v,
                    ));
                }
            },
            '0' => parse_number_after_leading_0(start_idx, iter)?,
            '1'..='9' => parse_number_after_first_non_zero(start_idx, iter)?,
            'n' => expect_ident!("null" => 'u', 'l', 'l'),
            't' => expect_ident!("true" => 'r', 'u', 'e'),
            'f' => expect_ident!("false" => 'a', 'l', 's', 'e'),
            ']' => return Ok(ValueParseResult::CloseSquare(start_idx)),
            '}' => return Ok(ValueParseResult::CloseBrace(start_idx)),
            _ => return Err(Error::ExpectedXsFoundY(item, start_idx, expected, c)),
        };
        // If a JSON value was successfully parsed, return that value
        return Ok(ValueParseResult::Node(value_node));
    }
    // If a JSON object was missing when the file ended, then that's an error
    Err(Error::ExpectedXsFoundEof(item, &[Expected::Value]))
}

/// The result generated when a JSON value is parsed.  Special cases are made for ']' and '}' to
/// prevent lookahead for empty arrays/objects.
enum ValueParseResult<'source> {
    /// The value parsed to a node
    Node(Node<'source>),
    /// The first non-whitespace char was `']'`
    CloseSquare(usize),
    /// The first non-whitespace char was `'}'`
    CloseBrace(usize),
}

/// Attempt to parse the chars in `iter` as an array, **assuming that the initial `[` has
/// been consumed**.
fn parse_array<'source>(start_idx: usize, iter: &mut Iter<'source>) -> Result<Node<'source>> {
    // Parse the first element
    let first_value = match parse_value_or_closing_bracket(
        Item::Array(start_idx),
        &[Expected::Value, Expected::Char(']')],
        iter,
    )? {
        // Array is `[]`, and therefore empty
        ValueParseResult::CloseSquare(_) => {
            return Ok(Node {
                unsplit_width: "[]".len(),
                kind: NodeKind::Array(vec![]),
            })
        }
        // Can't end an array with `}`
        ValueParseResult::CloseBrace(idx) => {
            return Err(Error::ExpectedXsFoundY(
                Item::Array(start_idx),
                idx,
                &[Expected::Value, Expected::Char(']')],
                '}',
            ))
        }
        // We parsed a value, so parse the rest of the array
        ValueParseResult::Node(n) => n,
    };

    let mut unsplit_width = "[".len() + first_value.unsplit_width + "]".len();
    let mut contents = vec![first_value];
    // Repeatedly expect either:
    // - ',' followed by another element, or
    // - ']', finishing the array
    loop {
        match iter.next() {
            Some((_, ' ' | '\t' | '\n' | '\r')) => continue, // Ignore whitespace
            // If ']', finish the array
            Some((_, ']')) => {
                return Ok(Node {
                    unsplit_width,
                    kind: NodeKind::Array(contents),
                });
            }
            // If ',', parse another element and repeat
            Some((_, ',')) => {
                let n = parse_value(Item::Array(start_idx), iter)?;
                unsplit_width += ", ".len() + n.unsplit_width;
                contents.push(n);
            }
            // Anything except whitespace, ',' or ']' is an error
            v => {
                return Err(Error::expected_xs_found(
                    Item::Array(start_idx),
                    &[Expected::Char(','), Expected::Char(']')],
                    v,
                ));
            }
        }
    }
}

/// Attempt to parse an object, **assuming that the initial `{` has been consumed**.
fn parse_object<'source>(start_idx: usize, iter: &mut Iter<'source>) -> Result<Node<'source>> {
    let mut fields = Vec::<(&str, Node)>::new();
    let mut unsplit_width = "{ ".len() + " }".len();
    loop {
        // Parse ws until we get to a '"' for the key (returning if we see '}' and this is
        // the first field)
        let key = loop {
            match iter.next() {
                Some((_, ' ' | '\t' | '\n' | '\r')) => continue, // Ignore whitespace
                Some((start_idx, '"')) => break parse_string(start_idx, iter)?, // Parse key
                Some((_, '}')) if fields.is_empty() => {
                    // '}' before the first key is an empty object
                    // TODO: Report idempotence bug in rustfmt
                    return Ok(Node {
                        unsplit_width: 2, // "{}"
                        kind: NodeKind::Object(vec![]),
                    });
                }
                // Any other char is an error
                v => {
                    let expected: &[Expected] = match fields.len() {
                        0 => &[Expected::Key, Expected::Char('}')],
                        _ => &[Expected::Key],
                    };
                    return Err(Error::expected_xs_found(
                        Item::Object(start_idx),
                        expected,
                        v,
                    ));
                }
            }
        };
        // Read whitespace until we find a ':'
        loop {
            match iter.next() {
                Some((_, ' ' | '\t' | '\n' | '\r')) => continue,
                Some((_, ':')) => break, // Found ':', parse the value
                // Anything other than ':' or whitespace is an error
                v => {
                    return Err(Error::expected_xs_found(
                        Item::Object(start_idx),
                        &[Expected::Char(':')],
                        v,
                    ));
                }
            }
        }
        // Parse the contained value
        let value = parse_value(Item::Object(start_idx), iter)?;
        // Add the field we just parsed
        unsplit_width += key.len() + ": ".len() + value.unsplit_width;
        fields.push((key, value));
        // Consume either ',' (parse next key/value pair) or '}' (end object)
        loop {
            match iter.next() {
                Some((_, ' ' | '\t' | '\n' | '\r')) => continue,
                Some((_, ',')) => break, // if ',', parse the next key/value pair
                Some((_, '}')) => {
                    // if '}', the object is finished
                    return Ok(Node {
                        unsplit_width,
                        kind: NodeKind::Object(fields),
                    });
                }
                v => {
                    // Anything except whitespace, ',' or '}' is an error
                    return Err(Error::expected_xs_found(
                        Item::Object(start_idx),
                        &[Expected::Char(','), Expected::Char('}')],
                        v,
                    ));
                }
            }
        }
        unsplit_width += ", ".len(); // Add space required by the comma
    }
}

/// Attempt to parse the chars in `iter` as an string, **assuming that the initial `"` has
/// been consumed**.  This returns a string slice **from the JSON source code**, i.e. the fully
/// escaped string complete with the enclosing `"`s.  We do not attempt to decode the string, we
/// simply verify that it conforms to the JSON standard.
fn parse_string<'source>(start_idx: usize, iter: &mut Iter<'source>) -> Result<&'source str> {
    while let Some((idx, c)) = iter.next() {
        match c {
            // If we find an unescaped quote, then terminate the string.
            // `+ 1` is OK because '"' has UTF-8 length of 1 byte
            '"' => return Ok(&iter.source[start_idx..idx + 1]),
            '\\' => match iter.next() {
                Some((_, '"' | '\\' | '/' | 'b' | 'f' | 'n' | 'r' | 't')) => {} // Valid escape chars
                Some((_, 'u')) => {
                    // `\u` should be followed by 4 hex chars
                    for _ in 0..4 {
                        match iter.next() {
                            Some((_, '0'..='9' | 'a'..='f' | 'A'..='F')) => {} // Valid hex char
                            Some((bad_idx, bad_char)) => {
                                return Err(Error::InvalidHexEscape(idx, bad_idx, bad_char));
                            }
                            None => return Err(Error::EofDuringString(start_idx)),
                        }
                    }
                }
                // Invalid escape sequence
                Some((bad_escape_idx, bad_escape_char)) => {
                    return Err(Error::InvalidEscape(bad_escape_idx, bad_escape_char));
                }
                None => return Err(Error::EofDuringString(start_idx)),
            },
            // Control chars aren't allowed in strings
            '\0'..='\x19' => return Err(Error::ControlCharInString(idx, c)),
            _ => {} // Any other char is just part of the string
        }
    }
    // If a file ended during a string, then that's an error
    Err(Error::EofDuringString(start_idx))
}

////////////////////
// NUMBER PARSING //
////////////////////

/// Parse a number, assuming that the leading 0 has been consumed (i.e. the number so far
/// is `0` or `-0`)
fn parse_number_after_leading_0<'source>(
    start_idx: usize,
    iter: &mut Iter<'source>,
) -> Result<Node<'source>> {
    Ok(match iter.peek_char() {
        // `iter.peek_idx() - 1` is the index of the last ASCII value consumed (in this case, the
        // leading '0')
        Some('1'..='9') => return Err(Error::LeadingZero(iter.peek_idx() - 1)),
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
) -> Result<Node<'source>> {
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
            _ => return Ok(iter.new_atom_starting_from(start_idx)),
        }
    }
}

/// Parse a number, assuming that everything up to **and including** the decimal point has
/// been consumed.
fn parse_number_after_decimal_point<'source>(
    start_idx: usize,
    iter: &mut Iter<'source>,
) -> Result<Node<'source>> {
    loop {
        match iter.peek_char() {
            Some('0'..='9') => {
                iter.next(); // Keep consuming numbers
            }
            // Can't have multiple decimal points
            Some('.') => return Err(Error::SecondDecimalPoint(iter.peek_idx())),
            Some('e' | 'E') => {
                iter.next(); // Consume the 'e'/'E'
                return parse_number_after_exponent(start_idx, iter);
            }
            // Anything that isn't part of a number belongs to the next token (e.g. ',' to
            // move onto the next array element)
            _ => return Ok(iter.new_atom_starting_from(start_idx)),
        }
    }
}

/// Parse a number, assuming that everything up to **and including** the 'e' or 'E' has been
/// consumed
fn parse_number_after_exponent<'source>(
    start_idx: usize,
    iter: &mut Iter<'source>,
) -> Result<Node<'source>> {
    // 'E' or 'e' is the last char popped, and it has UTF-8 length of 1 byte
    let exponent_idx = iter.peek_idx() - 1;
    // An exponent can optionally start with a '+' or '-'
    if let Some('+' | '-') = iter.peek_char() {
        iter.next(); // Consume the '+/-' if it exists, otherwise start parsing digits
    }

    let mut has_at_least_one_digit = false;
    loop {
        match iter.peek_char() {
            Some('0'..='9') => iter.next(), // Numbers are always valid exponents
            Some(c @ ('.' | 'e' | 'E' | '+' | '-')) => {
                return Err(Error::InvalidCharInExponent(iter.peek_idx(), c));
            }
            // Anything that isn't part of a number belongs to the next token (e.g. ',' to
            // move onto the next array element)
            _ => match has_at_least_one_digit {
                true => return Ok(iter.new_atom_starting_from(start_idx)),
                // `- 1` steps backwards over the `E` or `e` (both of which occupy 1 byte in UTF-8)
                false => return Err(Error::EmptyExponent(exponent_idx)),
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
    fn check_fail(s: &str, err: Error) {
        assert_eq!(Node::parse(s), Err(err));
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
        assert_eq!(Node::parse(s), Ok(exp_node));
    }

    #[test]
    fn unmatched_closing_bracket() {
        check_fail(
            "]",
            Error::ExpectedXsFoundY(Item::TopLevelValue, 0, &[Expected::Value], ']'),
        );
        check_fail(
            "}",
            Error::ExpectedXsFoundY(Item::TopLevelValue, 0, &[Expected::Value], '}'),
        );
        check_fail(
            "}   ",
            Error::ExpectedXsFoundY(Item::TopLevelValue, 0, &[Expected::Value], '}'),
        );
        check_fail(
            "  \t\n}   ",
            Error::ExpectedXsFoundY(Item::TopLevelValue, 4, &[Expected::Value], '}'),
        );
    }

    #[test]
    fn literal() {
        check_atom_no_ws("true");
        check_atom_no_ws("false");
        check_atom_no_ws("null");
        check_atom("    null", "null");
        check_atom("    null\t\n  ", "null");
        // Check for things in trailing whitespace
        check_fail("    null  x", Error::InvalidTrailingWhitespace(10, 'x'));
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
        check_fail(r#""thing \x thing""#, Error::InvalidEscape(8, 'x'));
        check_fail(
            r#""thing \uAFXF thing""#,
            Error::InvalidHexEscape(7, 11, 'X'),
        );
        // Leading/trailing whitespace
        check_atom("\r   \"\"", r#""""#);
        check_atom("\"\"  \r\t  \n  ", r#""""#);
        check_atom("\r   \"\"  \r\t  \n  ", r#""""#);
        check_atom("\r   \"string\"  \r\t  \n  ", r#""string""#);
        // Control chars in a string aren't allowed
        check_fail("    \"\0\"  x", Error::ControlCharInString(5, '\0'));
        check_fail("    \"\n\"  x", Error::ControlCharInString(5, '\n'));
        check_fail("    \"\t\"  x", Error::ControlCharInString(5, '\t'));
        // Check for things in trailing whitespace
        check_fail(
            r#"    "string"  x"#,
            Error::InvalidTrailingWhitespace(14, 'x'),
        );
    }

    #[test]
    fn number() {
        check_atom_no_ws("0");
        check_atom_no_ws("-0");
        check_atom("   0   \t\n ", "0");
        check_fail("02", Error::LeadingZero(0));
        check_fail("-02", Error::LeadingZero(1));
        check_atom_no_ws("10233415216992347901");
        check_atom_no_ws("0.2");
        check_fail("0.2.3", Error::SecondDecimalPoint(3));
        check_atom_no_ws("-0.00002");
        check_atom_no_ws("0.0200000");
        check_atom_no_ws("0.02e1");
        check_atom_no_ws("0.02E-1201");
        check_atom_no_ws("0.02e-1201");
        check_atom_no_ws("0.02e+01201");
        check_fail("0.02e", Error::EmptyExponent(4));
        check_fail("0.02e-", Error::EmptyExponent(4));
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
        check_fail(
            "    [  }\r\t  \n",
            Error::ExpectedXsFoundY(
                Item::Array(4),
                7,
                &[Expected::Value, Expected::Char(']')],
                '}',
            ),
        );
        check_ok(
            "    [ true ]\r\t  \n",
            Node {
                unsplit_width: 6,
                kind: NodeKind::Array(vec![Node::new_atom("true")]),
            },
        );
        check_fail(
            "    [ true, ]\r\t  \n",
            Error::ExpectedXsFoundY(Item::Array(4), 12, &[Expected::Value], ']'),
        );
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

    #[test]
    fn json_check_fail() {
        // These tests are taken from JSON_checker's test suite, found here:
        // http://www.json.org/JSON_checker/ (the tests themselves are here:
        // http://www.json.org/JSON_checker/test.zip).
        //
        // Note:
        // - fail1.json was removed because top-level types other than object or array were allowed
        //   after RFC7159 (https://www.ietf.org/rfc/rfc7159.txt)
        // - fail18.json was removed because JSON doesn't have a depth limit

        check_fail(
            r#"["Unclosed array""#,
            Error::ExpectedXsFoundEof(Item::Array(0), &[Expected::Char(','), Expected::Char(']')]),
        ); // fail2.json
        check_fail(
            r#"{unquoted_key: "keys must be quoted"}"#,
            Error::ExpectedXsFoundY(
                Item::Object(0),
                1,
                &[Expected::Key, Expected::Char('}')],
                'u',
            ),
        ); // fail3.json
        check_fail(
            r#"["extra comma",]"#,
            Error::ExpectedXsFoundY(Item::Array(0), 15, &[Expected::Value], ']'),
        ); // fail4.json
        check_fail(
            r#"["double extra comma",,]"#,
            Error::ExpectedXsFoundY(Item::Array(0), 22, &[Expected::Value], ','),
        ); // fail5.json
        check_fail(
            r#"[   , "<-- missing value"]"#,
            Error::ExpectedXsFoundY(
                Item::Array(0),
                4,
                &[Expected::Value, Expected::Char(']')],
                ',',
            ),
        ); // fail6.json
        check_fail(
            r#"["Comma after the close"],"#,
            Error::InvalidTrailingWhitespace(25, ','),
        ); // fail7.json
        check_fail(
            r#"["Extra close"]]"#,
            Error::InvalidTrailingWhitespace(15, ']'),
        ); // fail8.json
        check_fail(
            r#"{"Extra comma": true,}"#,
            Error::ExpectedXsFoundY(Item::Object(0), 21, &[Expected::Key], '}'),
        ); // fail9.json

        check_fail(
            r#"{"Extra value after close": true} "misplaced quoted value""#,
            Error::InvalidTrailingWhitespace(34, '"'),
        ); // fail10.json
        check_fail(
            r#"{"Illegal expression": 1 + 2}"#,
            Error::ExpectedXsFoundY(
                Item::Object(0),
                25,
                &[Expected::Char(','), Expected::Char('}')],
                '+',
            ),
        ); // fail11.json
        check_fail(
            r#"{"Illegal invocation": alert()}"#,
            Error::ExpectedXsFoundY(Item::Object(0), 23, &[Expected::Value], 'a'),
        ); // fail12.json
        check_fail(
            r#"{"Numbers cannot have leading zeroes": 013}"#,
            Error::LeadingZero(39),
        ); // fail13.json
        check_fail(
            // TODO: Handle this better.  We should probably handle this differently because it's
            // just after a number
            r#"{"Numbers cannot be hex": 0x14}"#,
            Error::ExpectedXsFoundY(
                Item::Object(0),
                27,
                &[Expected::Char(','), Expected::Char('}')],
                'x',
            ),
        ); // fail14.json
        check_fail(
            r#"["Illegal backslash escape: \x15"]"#,
            Error::InvalidEscape(29, 'x'),
        ); // fail15.json
        check_fail(
            r#"[\naked]"#,
            Error::ExpectedXsFoundY(
                Item::Array(0),
                1,
                &[Expected::Value, Expected::Char(']')],
                '\\',
            ),
        ); // fail16.json
        check_fail(
            r#"["Illegal backslash escape: \017"]"#,
            Error::InvalidEscape(29, '0'),
        ); // fail17.json
        check_fail(
            r#"{"Missing colon" null}"#,
            Error::ExpectedXsFoundY(Item::Object(0), 17, &[Expected::Char(':')], 'n'),
        ); // fail19.json

        check_fail(
            r#"{"Double colon":: null}"#,
            Error::ExpectedXsFoundY(Item::Object(0), 16, &[Expected::Value], ':'),
        ); // fail20.json
        check_fail(
            r#"{"Comma instead of colon", null}"#,
            Error::ExpectedXsFoundY(Item::Object(0), 25, &[Expected::Char(':')], ','),
        ); // fail21.json
        check_fail(
            r#"["Colon instead of comma": false]"#,
            Error::ExpectedXsFoundY(
                Item::Array(0),
                25,
                &[Expected::Char(','), Expected::Char(']')],
                ':',
            ),
        ); // fail22.json
        check_fail(
            r#"["Bad value", truth]"#,
            Error::ExpectedXsFoundY(Item::Literal("true"), 17, &[Expected::Char('e')], 't'),
        ); // fail23.json
        check_fail(
            r#"['single quote']"#,
            Error::ExpectedXsFoundY(
                Item::Array(0),
                1,
                &[Expected::Value, Expected::Char(']')],
                '\'',
            ),
        ); // fail24.json
        check_fail(
            r#"["	tab	character	in	string	"]"#,
            Error::ControlCharInString(2, '\t'),
        ); // fail25.json
        check_fail(
            r#"["tab\   character\   in\  string\  "]"#,
            Error::InvalidEscape(6, ' '),
        ); // fail26.json
        check_fail(
            r#"["line
break"]"#,
            Error::ControlCharInString(6, '\n'),
        ); // fail27.json
        check_fail(
            r#"["line\
break"]"#,
            Error::InvalidEscape(7, '\n'),
        ); // fail28.json
        check_fail(r#"[0e]"#, Error::EmptyExponent(2)); // fail29.json

        check_fail(r#"[0e+]"#, Error::EmptyExponent(2)); // fail30.json
        check_fail(r#"[0e+-1]"#, Error::InvalidCharInExponent(4, '-')); // fail31.json
        check_fail(
            r#"{"Comma instead if closing brace": true,"#,
            Error::ExpectedXsFoundEof(Item::Object(0), &[Expected::Key]),
        ); // fail32.json
        check_fail(
            r#"["mismatch"}"#,
            Error::ExpectedXsFoundY(
                Item::Array(0),
                11,
                &[Expected::Char(','), Expected::Char(']')],
                '}',
            ),
        ); // fail33.json
    }

    #[test]
    fn json_check_ok() {
        check_ok(
            r#"{
    "JSON Test Pattern pass3": {
        "The outermost value": "must be an object or array.",
        "In this test": "It is an object."
    }
}
"#,
            Node {
                unsplit_width: 123,
                kind: NodeKind::Object(vec![(
                    "\"JSON Test Pattern pass3\"",
                    Node {
                        unsplit_width: 92,
                        kind: NodeKind::Object(vec![
                            (
                                "\"The outermost value\"",
                                Node::new_atom("\"must be an object or array.\""),
                            ),
                            ("\"In this test\"", Node::new_atom("\"It is an object.\"")),
                        ]),
                    },
                )]),
            },
        ); // pass3.json

        assert!(Node::parse(
            r#"[
    "JSON Test Pattern pass1",
    {"object with 1 member":["array with 1 element"]},
    {},
    [],
    -42,
    true,
    false,
    null,
    {
        "integer": 1234567890,
        "real": -9876.543210,
        "e": 0.123456789e-12,
        "E": 1.234567890E+34,
        "":  23456789012E66,
        "zero": 0,
        "one": 1,
        "space": " ",
        "quote": "\"",
        "backslash": "\\",
        "controls": "\b\f\n\r\t",
        "slash": "/ & \/",
        "alpha": "abcdefghijklmnopqrstuvwyz",
        "ALPHA": "ABCDEFGHIJKLMNOPQRSTUVWYZ",
        "digit": "0123456789",
        "0123456789": "digit",
        "special": "`1~!@#$%^&*()_+-={':[,]}|;.</>?",
        "hex": "\u0123\u4567\u89AB\uCDEF\uabcd\uef4A",
        "true": true,
        "false": false,
        "null": null,
        "array":[  ],
        "object":{  },
        "address": "50 St. James Street",
        "url": "http://www.JSON.org/",
        "comment": "// /* <!-- --",
        "\\# -- --> */\n    ": " ",
    " s p a c e d " :[1,2 , 3

,

4 , 5        ,          6           ,7        ],"compact":[1,2,3,4,5,6,7],
        "jsontext": "{\"object with 1 member\":[\"array with 1 element\"]}",
        "quotes": "&#34; \u0022 %22 0x22 034 &#x22;",
        "\/\\\"\uCAFE\uBABE\uAB98\uFCDE\ubcda\uef4A\b\f\n\r\t`1~!@#$%^&*()_+-=[]{}|;:',./<>?"
: "A key can be any string"
    },
    0.5 ,98.6
,
99.44
,

1066,
1e1,
0.1e1,
1e-1,
1e00,2e+00,2e-00
,"rosebud"]"#,
        )
        .is_ok()); // pass1.json
    }
}

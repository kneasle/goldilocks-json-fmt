/*!
[![crates.io](https://img.shields.io/crates/v/goldilocks-json-fmt.svg)](https://crates.io/crates/goldilocks-json-fmt)

A simple, portable, fast, pretty JSON formatter.  No dependencies or unsafe code.

The resulting JSON strikes a balance between 'too wide' (i.e. minified, all on one line) and 'too
tall' (e.g. `serde_json`'s `pretty_print`).  You give the formatter a line limit (defaults to 100
chars), and it keeps things as wide as possible whilst preserving that limit.  Sometimes the limit
is impossible to achieve (e.g. you have a string that's longer than the line limit), in which case
the formatter will break the limit by as little as possible.

The throughput of the Goldilocks formatter is about 300MB/s, which should be enough for most
situations.  It's about as fast as you can get without cracking out the big guns and using SIMD,
which would break both simplicity and portability.

# Example:

```
// Ewww so horrible
let json = r#"{"test/cases/87s-at-back.toml":{"comps":[{"length":32,
    "string":"sHsH","avg_score":-0.45625},{"length":64,"string":"sHWsMH",
    "avg_score":-0.44062495},{"length":96,"string":"WMsWMHsH","avg_score":
    -0.33124998},{"length":96,"string":"WsMHWsMH","avg_score":-0.33124998},
    {"length":96,"string":"sHWMsWMH","avg_score":-0.33124995},{"length":64,
    "string":"WsMHsH","avg_score":-0.284375}]}}"#;

let perfect_json = goldilocks_json_fmt::format(&json).expect("Invalid JSON");

assert_eq!(
    &perfect_json,
    // So perfect!
    r#"{
  "test/cases/87s-at-back.toml": {
    "comps": [
      { "length": 32, "string": "sHsH", "avg_score": -0.45625 },
      { "length": 64, "string": "sHWsMH", "avg_score": -0.44062495 },
      { "length": 96, "string": "WMsWMHsH", "avg_score": -0.33124998 },
      { "length": 96, "string": "WsMHWsMH", "avg_score": -0.33124998 },
      { "length": 96, "string": "sHWMsWMH", "avg_score": -0.33124995 },
      { "length": 64, "string": "WsMHsH", "avg_score": -0.284375 }
    ]
  }
}"#,
);
```
*/

mod formatting;
mod parsing;

/// Formats the given JSON with default [`Config`]uration.
///
/// Returns an [`Error`] if the input is invalid, and `format(s)` is equivalent to
/// [`format_with_config`]`(s, &Config::default())`.
///
/// # Example:
///
/// ```
/// // Ewww so horrible
/// let json = r#"{"test/cases/87s-at-back.toml":{"comps":[{"length":32,
///     "string":"sHsH","avg_score":-0.45625},{"length":64,"string":"sHWsMH",
///     "avg_score":-0.44062495},{"length":96,"string":"WMsWMHsH","avg_score":
///     -0.33124998},{"length":96,"string":"WsMHWsMH","avg_score":-0.33124998},
///     {"length":96,"string":"sHWMsWMH","avg_score":-0.33124995},{"length":64,
///     "string":"WsMHsH","avg_score":-0.284375}]}}"#;
///
/// let perfect_json = goldilocks_json_fmt::format(&json).expect("Invalid JSON");
///
/// assert_eq!(
///     &perfect_json,
///     // So perfect!
///     r#"{
///   "test/cases/87s-at-back.toml": {
///     "comps": [
///       { "length": 32, "string": "sHsH", "avg_score": -0.45625 },
///       { "length": 64, "string": "sHWsMH", "avg_score": -0.44062495 },
///       { "length": 96, "string": "WMsWMHsH", "avg_score": -0.33124998 },
///       { "length": 96, "string": "WsMHWsMH", "avg_score": -0.33124998 },
///       { "length": 96, "string": "sHWMsWMH", "avg_score": -0.33124995 },
///       { "length": 64, "string": "WsMHsH", "avg_score": -0.284375 }
///     ]
///   }
/// }"#,
/// );
/// ```
pub fn format(s: &str) -> Result<String> {
    format_with_config(s, &Config::default())
}

/// Formats the given JSON with the given [`Config`]uration, returning an [`Error`] if the JSON
/// wasn't valid.
///
/// See [`Config`] for examples.
pub fn format_with_config(s: &str, config: &Config) -> Result<String> {
    Node::parse(s).map(|ast| ast.format(config))
}

/// `Config`uration options for JSON formatting with [`format_with_config`].
#[derive(Debug, Clone)]
pub struct Config {
    /// Maximum line length, in characters.  Defaults to 100.
    ///
    /// # Example
    ///
    /// ```
    /// use goldilocks_json_fmt::{Config, format_with_config};
    ///
    /// // Add more indentation (4 spaces, not the default of 2)
    /// assert_eq!(
    ///     format_with_config(
    ///         r#"{"test/cases/87s-at-back.toml":{"comps":[{"length":32,
    ///             "string":"sHsH","avg_score":-0.45625}]}}"#,
    ///         &Config {
    ///             width_limit: 40,
    ///             ..Config::default()
    ///         }
    ///     ).unwrap(),
    ///     r#"{
    ///   "test/cases/87s-at-back.toml": {
    ///     "comps": [
    ///       {
    ///         "length": 32,
    ///         "string": "sHsH",
    ///         "avg_score": -0.45625
    ///       }
    ///     ]
    ///   }
    /// }"#
    /// );
    /// ```
    pub width_limit: usize,
    /// Number of spaces added for each indentation level.  Defaults to 2.
    ///
    /// # Example
    ///
    /// ```
    /// use goldilocks_json_fmt::{Config, format_with_config};
    ///
    /// // Add more indentation (4 spaces, not the default of 2)
    /// assert_eq!(
    ///     format_with_config(
    ///         r#"{"test/cases/87s-at-back.toml":{"comps":[{"length":32,
    ///             "string":"sHsH","avg_score":-0.45625}]}}"#,
    ///         &Config {
    ///             indent_width: 4,
    ///             width_limit: 40, // Force more splitting for demo purposes
    ///             ..Config::default()
    ///         }
    ///     ).unwrap(),
    ///     r#"{
    ///     "test/cases/87s-at-back.toml": {
    ///         "comps": [
    ///             {
    ///                 "length": 32,
    ///                 "string": "sHsH",
    ///                 "avg_score": -0.45625
    ///             }
    ///         ]
    ///     }
    /// }"#
    /// );
    /// ```
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

/// The possible ways that JSON parsing could fail
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    /* Misc parsing */
    ExpectedXsFoundY(Item, usize, &'static [Expected], char),
    ExpectedXsFoundEof(Item, &'static [Expected]),
    InvalidTrailingWhitespace(usize, char),
    /* String parsing */
    EofDuringString(usize), // Index refers to the start of the unterminated string
    InvalidEscape(usize, char),
    InvalidHexEscape(usize, usize, char), // First index refers to the `\`
    ControlCharInString(usize, char),
    /* Number parsing */
    LeadingZero(usize),
    SecondDecimalPoint(usize),
    InvalidCharInExponent(usize, char),
    EmptyExponent(usize), // `usize` is the index of the 'E'/'e'
}

pub type Result<T> = std::result::Result<T, Error>;

/// An item that could be being parsed when an error was generated
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Item {
    /// The JSON value which ends up being the root of the tree
    TopLevelValue,
    /// A literal (i.e. `true`, `false` or `null`)
    Literal(&'static str),
    /// A number
    Number,
    /// An array, where the `[` is at the given byte index
    Array(usize),
    /// An object, where the `{` is at the given byte index
    Object(usize),
}

/// What was expected in a given position
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Expected {
    /// A JSON key (i.e. a string)
    Key,
    /// Any JSON value
    Value,
    /// The given [`char`]
    Char(char),
    /// Any digit (0 through 9)
    Digit,
}

impl Error {
    /// Given the value from the [`Iter`]ator and an expected value, generate either an
    /// [`Error::ExpectedXFoundY`] or an [`Error::ExpectedXFoundEof`].
    fn expected_xs_found(
        item: Item,
        expected: &'static [Expected],
        v: Option<(usize, char)>,
    ) -> Self {
        match v {
            Some((idx, c)) => Error::ExpectedXsFoundY(item, idx, expected, c),
            None => Error::ExpectedXsFoundEof(item, expected),
        }
    }
}

/////////
// AST //
/////////

/// A single element in the JSON syntax tree.  Each `Node` corresponds to a single JSON value
/// (object, array, number, string, etc.).
#[derive(Debug, Clone, PartialEq, Eq)]
struct Node<'source> {
    /// The width of this node if it were to be 'unsplit' - i.e. all on one line
    unsplit_width: usize,
    kind: NodeKind<'source>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum NodeKind<'source> {
    /// A JSON value that cannot be split onto multiple lines and is thus considered 'atomic'.
    ///
    /// This includes numbers, strings, `null`, `true` and `false`.  As far as a formatter is
    /// concerned, these is no need to tell them apart.
    Atom(&'source str),
    /// A JSON array
    Array(Vec<Node<'source>>),
    /// A JSON object.  Each element in the [`Vec`] is a `(key, value)` pair, and `key` is a string
    /// slice from the source JSON file.
    Object(Vec<(&'source str, Node<'source>)>),
}

impl<'source> Node<'source> {
    fn new_atom(s: &'source str) -> Self {
        Self {
            unsplit_width: s.len(),
            kind: NodeKind::Atom(s),
        }
    }
}

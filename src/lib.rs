//! # fast-json-fmt
//!
//! A fast, simple, opinionated JSON formatter.

mod formatting;
mod parsing;

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

//! Code to convert an AST into a pretty-formatted string

use crate::{Config, Node};

impl Node<'_> {
    /// Convert the AST into a pretty-formatted string
    pub(crate) fn format(&self, config: &Config) -> String {
        dbg!(self);
        todo!()
    }
}

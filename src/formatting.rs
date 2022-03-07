//! Code to convert an AST into a pretty-formatted string

use crate::{Config, Node, NodeKind};

impl Node<'_> {
    /// Convert the AST into a pretty-formatted string
    pub(crate) fn format(&self, config: &Config) -> String {
        let mut s = String::new();
        let mut indentation_str = String::new();
        self.write_to_string(0, &mut indentation_str, &mut s, config);
        s
    }

    fn write_to_string(
        &self,
        indentation: usize,
        indentation_str: &mut String,
        out: &mut String,
        config: &Config,
    ) {
        if self.unsplit_width + indentation < config.width_limit {
            self.fmt_wide(out, config); // If this can fit in one line, then make it 'wide'
        } else {
            self.fmt_tall(indentation_str, out, config);
        }
    }

    fn fmt_wide(&self, out: &mut String, config: &Config) {
        match &self.kind {
            NodeKind::Atom(s) => out.push_str(s), // Atoms are always formatted as-is
            NodeKind::Array(values) => {
                out.push_str("[");
                let mut is_first_time = true;
                for v in values {
                    if !is_first_time {
                        out.push_str(", ");
                    }
                    v.fmt_wide(out, config); // Format all children as also wide
                    is_first_time = false;
                }
                out.push_str("]");
            }
            NodeKind::Object(fields) => {
                out.push_str("{ ");
                let mut is_first_time = true;
                for (key, value) in fields {
                    // Comma for the last value (if it exists)
                    if !is_first_time {
                        out.push_str(", ");
                    }
                    is_first_time = false;
                    // `key: value`
                    out.push_str(key);
                    out.push_str(": ");
                    value.fmt_wide(out, config); // Format all children as also wide
                }
                out.push_str(" }");
            }
        }
    }

    fn fmt_tall(&self, indentation_str: &mut String, out: &mut String, config: &Config) {
        match &self.kind {
            NodeKind::Atom(s) => out.push_str(s), // Atoms are always formatted as-is
            NodeKind::Array(values) => {
                // Leading '['
                out.push_str("[");
                // First value
                let mut val_iter = values.iter();
                if let Some(first_val) = val_iter.next() {
                    // Add an extra indent level
                    for _ in 0..config.indent_width {
                        indentation_str.push_str(" ");
                    }
                    // Add the first value
                    out.push_str("\n");
                    out.push_str(&indentation_str);
                    first_val.write_to_string(indentation_str.len(), indentation_str, out, config);
                    // Other values
                    for val in val_iter {
                        // Comma for the previous value
                        out.push_str(",\n");
                        out.push_str(&indentation_str);
                        // Next value
                        val.write_to_string(indentation_str.len(), indentation_str, out, config)
                    }
                    // Remove indent level before ']'
                    for _ in 0..config.indent_width {
                        assert!(indentation_str.pop().is_some());
                    }
                }
                // Final `]` on a its own line
                out.push_str("\n");
                out.push_str(&indentation_str);
                out.push_str("]");
            }
            NodeKind::Object(fields) => {
                // Add an extra indent level
                for _ in 0..config.indent_width {
                    indentation_str.push_str(" ");
                }
                // Leading '{'
                out.push_str("{");
                // Other values
                let mut is_first_field = true;
                for (key, value) in fields {
                    // Comma for previous value
                    if !is_first_field {
                        out.push_str(",");
                    }
                    is_first_field = false;
                    // New line for the next key
                    out.push_str("\n");
                    out.push_str(&indentation_str);
                    // `key: value`
                    out.push_str(key);
                    out.push_str(": ");
                    value.write_to_string(
                        indentation_str.len() + key.len() + 2, // '{indentation}{key}: '
                        indentation_str,
                        out,
                        config,
                    );
                }
                // Remove indent level before '}'
                for _ in 0..config.indent_width {
                    assert!(indentation_str.pop().is_some());
                }
                // Final `]` on a its own line
                out.push_str("\n");
                out.push_str(&indentation_str);
                out.push_str("}");
            }
        }
    }
}

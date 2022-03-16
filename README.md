# goldilocks-json-fmt

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

## Example:

```rust
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

License: MIT

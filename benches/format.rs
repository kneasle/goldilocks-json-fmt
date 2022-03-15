use criterion::{black_box, criterion_group, criterion_main, Criterion};

/// Define a set of benchmark functions, and combine them all into a single `criterion_group!`.
macro_rules! define_group {
    ($grp_name: ident, $( $fn_name: ident => $file_name: literal ),*) => {
        criterion_group!($grp_name, $( $fn_name ),*);

        $(
            fn $fn_name(c: &mut Criterion) {
                c.bench_function($file_name, |b| {
                    b.iter(|| goldilocks_json_fmt::format(black_box(include_str!($file_name))))
                });
            }
        )*
    };
}

define_group!(
    benches,
    cccbr_methods => "cccbr-methods.json",
    earthquakes => "earthquakes.json",
    historical_events => "historical-events.json",
    meteorites => "meteorites.json",
    monument_test_results => "monument-test-results.json"
);

criterion_main!(benches);

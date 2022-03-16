fn main() {
    for path in std::env::args().skip(1) {
        let raw_json =
            std::fs::read_to_string(&path).unwrap_or_else(|_| panic!("Couldn't read {:?}", path));
        let pretty_json = goldilocks_json_fmt::format(&raw_json)
            .unwrap_or_else(|_| panic!("Failed to parse JSON in {:?}", path));
        println!("{}", pretty_json);
    }
}

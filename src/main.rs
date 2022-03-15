fn main() {
    for path in std::env::args().skip(1) {
        let raw_json = std::fs::read_to_string(&path).expect(&format!("Couldn't read {:?}", path));
        let pretty_json =
            fast_json_fmt::format(&raw_json).expect(&format!("Failed to parse JSON in {:?}", path));
        println!("{}", pretty_json);
    }
}

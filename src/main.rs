fn main() {
    let path = std::env::args()
        .skip(1)
        .next()
        .expect("Expected file path as argument");
    let raw_json = std::fs::read_to_string(&path).expect("Couldn't read file");
    let pretty_json = fast_json_fmt::format(&raw_json).expect("Failed to parse JSON");
    println!("{}", pretty_json);
}

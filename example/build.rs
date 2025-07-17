fn main() {
    println!("cargo:rerun-if-changed=src");
    let examples = std::fs::read_dir("./src/").unwrap();
    for example in examples {
        let example = example.unwrap();
        let path = example.path();
        if path.is_file() && path.file_stem().unwrap().to_str().unwrap() != "main" {
            rust_sitter_tool::build_parser(&path);
        }
    }
}

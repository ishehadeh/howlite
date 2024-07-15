fn main() {
    // build grammar
    println!("cargo::rerun-if-changed=src/parser/grammar.lalrpop");
    lalrpop::process_root().unwrap();
}

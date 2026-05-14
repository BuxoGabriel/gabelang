//! Smoke test that every script in `examples/` parses and runs without error.
//!
//! Run with `cargo test --test examples`. Tests run from the crate root, so
//! scripts that read other files (e.g. `gabelang2.gabe` reads
//! `./examples/app.gl2`) work without adjustment.

use std::fs;
use std::path::PathBuf;

use gabelang::evaluator::Runtime;
use gabelang::parser::Parser;

fn run_example(path: &PathBuf) -> Result<(), String> {
    let contents = fs::read_to_string(path)
        .map_err(|e| format!("read failed: {e}"))?;
    let program = Parser::new(&contents)
        .parse_program()
        .map_err(|e| format!("parse failed: {e}"))?;
    Runtime::new()
        .run_program(&program)
        .map(|_| ())
        .map_err(|e| format!("runtime failed: {e}"))
}

#[test]
fn all_examples_run() {
    let mut failures = Vec::new();
    let mut ran = 0;

    let dir = fs::read_dir("examples").expect("examples directory missing");
    let mut paths: Vec<PathBuf> = dir
        .filter_map(|entry| entry.ok().map(|e| e.path()))
        .filter(|p| p.extension().and_then(|s| s.to_str()) == Some("gabe"))
        .collect();
    paths.sort();

    for path in paths {
        ran += 1;
        let name = path.file_name().unwrap().to_string_lossy().to_string();
        println!("running example: {name}");
        if let Err(err) = run_example(&path) {
            failures.push(format!("{name}: {err}"));
        }
    }

    assert!(ran > 0, "no examples were discovered");
    assert!(
        failures.is_empty(),
        "{} of {} examples failed:\n  - {}",
        failures.len(),
        ran,
        failures.join("\n  - ")
    );
}

# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

Gabelang is a tree-walking interpreter for a small, dynamically-typed scripting language, implemented in Rust. The book "Writing an Interpreter in Go" by Thorsten Ball is the primary reference. The crate ships both a binary (`gabelang`) and a library (`rlib` + `cdylib`) with an optional `wasm` feature that exposes a `Gabelang` struct via `wasm-bindgen`.

## Common commands

- `cargo build --release` — release build of the CLI
- `cargo test` — runs the full test suite
- `cargo test --test examples` — runs only the smoke test that parses and executes every `examples/*.gabe` file
- `cargo test all_examples_run -- --nocapture` — runs the example smoke test and prints which scripts ran
- `cargo run` — start the REPL
- `cargo run -- examples/fib.gabe` — interpret a script (positional form)
- `cargo run -- --file examples/fib.gabe` — interpret a script (flag form)
- `wasm-pack build --features wasm` — build the wasm target

The example smoke test executes from the crate root, so scripts that read sibling files (e.g. `examples/gabelang2.gabe` reads `./examples/app.gl2`) only work when run via `cargo test` from the repo root.

## Pipeline

A program flows through four modules in `src/`:

1. **`lexer.rs`** — `Lexer` produces `TokenWithLocation` items; locations are surfaced in `ParserError` for diagnostics.
2. **`parser.rs`** — `Parser::new(src).parse_program()` returns `Vec<ast::Statement>`. The parser owns a peekable lexer and produces a Pratt-style expression tree.
3. **`ast.rs`** — owns `Statement`, `Expression`, `Assignable`, `Literal`, `InfixOp`, `PrefixOp`, and the `Function` AST node.
4. **`evaluator/`** — `Runtime::new().run_program(&ast)` executes the program.

`lib.rs` wires the binary entry point (`run`) and a wasm-facing `Gabelang` struct. `repl.rs` is a standalone REPL loop that shares the same `Runtime`.

## Evaluator model

The evaluator is the most subtle piece — read it before changing semantics.

- **`Object` is `Rc<RefCell<ObjectInner>>`.** Variables in the env are `Object` handles, so assignment shares references; `Expression::Assignable(_)` in `eval_expression` does a one-level clone of the inner value. This is *shallow* — nested `ARRAY`/`OBJECT` elements still alias through their element `Rc`s. See `todo.md` item 2.
- **Cycle-safe Display.** `Object`'s `Display` uses a thread-local `HashSet<usize>` of `Rc` pointers to print `"Cycle"` when it re-enters the same object — needed because object literals can hold references to themselves.
- **Scopes are an `Environment` chain** (`evaluator/stack.rs`). Each `Environment` owns a `HashMap<String, Object>` and an `Option<Rc<RefCell<Environment>>>` enclosing pointer. `Runtime` holds the active env as `env: Rc<RefCell<Environment>>` plus a `globals: Rc<RefCell<Environment>>` retained so `reset_stack` can rebuild. `Runtime::enter_scope` / `exit_scope` wrap the current env in / unwrap to its enclosing; they're used by `if`, `while`, `do-while`, `for`, and built-in dispatch.
- **Function closures capture by reference.** `FunctionInner.context: Rc<RefCell<Environment>>` is set at declaration time to the *live* env (an `Rc`-clone, not a snapshot). On call, `eval_function_call` uses `mem::replace` to swap `self.env` for a fresh `Environment::new_enclosed(captured)`, evaluates the body, then restores. New bindings or mutations in enclosing scopes are visible through the chain at lookup time, so mutual recursion works at any nesting depth.
- **`current_context()`** returns an `Rc`-clone of the active env. Built-ins read their named params with `rt.current_context().borrow().get_var("_x")`. The wasm bindings on `Gabelang` call `Runtime::enter_scope` / `exit_scope` directly.
- **Built-ins** live in `evaluator/built_ins.rs`. Each implements the `BuiltIn` trait (`get_params`, `eval`) and is registered in `load_built_ins()`. The current calling convention (args passed via the environment) is item 8 in `todo.md`.
- **`returning` propagation.** `GabrValue.returning` is how `return` unwinds. Loops and `eval_program*` check it and short-circuit; do not strip it when refactoring.

## Language quirks worth knowing

- Booleans coerce to `i64` (0/1) in numeric infix ops. String `+` concatenates; `==` and `!=` work on strings; other infix ops on strings error.
- Indexing: arrays accept non-negative integers; `arr[len(arr)]` pushes (assignment only). Objects accept only string indices via `obj["key"]` and `obj.key`. Missing object props read as `NULL`.
- Comments are `//` to end-of-line, stripped at the lexer level.
- File scripts no longer auto-print the final expression — use `print(...)` (see commit `556abcf`).

## Known issues

`todo.md` at the repo root tracks 24 design / implementation issues identified in a 2026-05-15 review. Items 14, 16, 17 (snapshot closures, `loaded_stack`, global fallback) were resolved by the environment-chain refactor described above. Consult `todo.md` before changing evaluator internals — most "this looks weird" reactions are already catalogued, and the file marks open vs. resolved.

## Testing

`tests/examples.rs` is the only integration test; it discovers every `*.gabe` file under `examples/` and asserts that parsing + execution succeeds. When adding a language feature, drop a script in `examples/` to get coverage automatically. There are no unit tests in `ast` or `evaluator` yet (listed under Todo in README).

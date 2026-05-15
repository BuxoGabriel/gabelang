# Implementation TODO

Open design / implementation issues identified during the 2026-05-15 review.
`[x]` items have been addressed; `[ ]` are open.

Each item links to the relevant file:line and explains *why* the issue
matters, so future agents can prioritize and avoid re-litigating.

---

## Variables / `Object` representation

- [ ] **1. `Object::inner()` always uses `borrow_mut()`** — `src/evaluator/mod.rs:546-548`
  `RefCell` enforces "many readers OR one writer." Always taking a mutable
  borrow means nested reads on the same object panic at runtime where two
  `borrow()`s would be fine. Fix: expose separate read / write helpers.

- [ ] **2. "Deep clone on rvalue" is actually a one-level clone** — `mod.rs:362`
  `get_assignable(...)?.inner().clone().as_object()` clones the outer
  `ObjectInner`, but `ARRAY(Vec<Object>)` and `OBJECT(HashMap<…>)` carry `Rc`
  handles that still alias. `let b = a; b[0][0] = 99;` mutates `a[0][0]` too.
  README promises deep-clone semantics; implementation doesn't deliver. Pick
  one model (true deep clone, or document and embrace reference semantics).

- [ ] **3. `let` vs `=` have observably different semantics across closures**
  — `stack.rs:95-103` vs `:75-79`.
  `set_var` mutates through the `Rc`; `create_var` inserts a fresh `Rc`.
  Combined with closure capture: outer `a = 10;` IS visible inside an
  already-declared closure; outer `let a = 10;` is NOT. Pick one form.

- [ ] **4. `set_var` relies on a non-local invariant** — `stack.rs:98`
  `*obj.inner() = val.inner().clone()` holds two `RefMut`s live; if `obj` and
  `val` ever pointed at the same `RefCell` it would panic. Today they don't,
  but only because every `eval_expression` path returns a freshly-wrapped
  `Rc`. Fix: replace the map entry instead of mutating through the existing
  `Rc`.

- [ ] **5. `PartialEq` / `Eq` / `Hash` on `Object` are pointer-based but unused**
  — `mod.rs:551-565`.
  Identity-based impls that disagree with the language's value-equal `==`
  (which is handled inside `eval_infix`). Misleading to readers. Either
  delete or align with language semantics.

- [ ] **6. `Display` cycle guard uses thread-local raw pointers without RAII**
  — `mod.rs:571-587`.
  On panic mid-format, the pointer stays in the set and subsequent prints of
  the same object incorrectly say `"Cycle"`. Fix: use a `Drop` guard.

- [ ] **7. `ObjectInner` is cloned on every numeric `eval_infix`** — `mod.rs:405-406`
  Pattern-matching can borrow the inner instead of cloning. Cheap perf win
  and avoids allocations for `ARRAY`/`OBJECT` operands that fail anyway.

## Built-ins

- [ ] **8. Args are threaded through the variable stack** — `mod.rs:484-494`, `built_ins.rs:52-65`
  Each call pushes a scope, inserts each arg by name as a stack variable,
  and the built-in pulls them back out by string lookup. Slow, stringly-typed
  (typo = silent "not in scope"), and couples every built-in to `Stack` /
  `Environment`. Replace with `fn(&[Object]) -> Result<Object, _>`.

- [ ] **9. Arity is silently mis-handled** — `mod.rs:485-489`, `:460-464`
  `get_params().iter().zip(params.iter())` truncates to the shorter list.
  Too few args → "did not receive arg" raised from inside each built-in; too
  many → silently ignored. Arity check belongs in the dispatcher.

- [ ] **10. `as_built_in(self)` is duplicated trait boilerplate** — `built_ins.rs:40` and every impl
  Every impl writes the identical one-liner. Use a default impl or a free
  function `into_builtin<T: BuiltIn + 'static>(t) -> Rc<dyn BuiltIn>`.

- [ ] **11. `BuiltInError(String)` discards structure** — `built_ins.rs:10-19`
  Callers can't distinguish arity / type / I/O failures. Use a small enum.

- [ ] **12. Per-built-in argument-existence check is copy-paste** — `built_ins.rs:52-57` repeated everywhere
  Arity is fixed; this check should happen once in the dispatcher.

- [ ] **13. Built-ins aren't first-class values** — `mod.rs:448-501`
  `let f = len; f(arr);` doesn't work because built-ins live in a side
  table, not the value space. Optional design choice.

## Runtime / scopes / closures

- [x] **14. Closures snapshotted a flattened copy of the stack** — fixed 2026-05-15
  Was: `FunctionInner.context: Stack` set via `Stack::flat_copy()` at
  declaration time, so bindings added to the parent after declaration were
  invisible and mutual recursion only worked at the top level. Now:
  `FunctionInner.context: Rc<RefCell<Environment>>`, captured by `Rc`-clone of
  the live env at declaration time. The enclosing chain is traversed at
  lookup time, so new bindings, mutations, and nested mutual recursion all
  resolve correctly.

- [ ] **15. Scope push/pop is manually paired and leaks on error** — `mod.rs:121-129`, `:325-336`, etc.
  Every block does push-eval-pop with `?` propagation. On error the pop never
  runs and `self.env` stays at the inner scope, which the REPL / wasm
  `Gabelang` reuses across calls. Fix: an RAII `ScopeGuard`.

- [x] **16. `loaded_stack` was global mutable state pretending to be a parameter**
  — fixed 2026-05-15.
  Removed entirely. `Runtime` now has a single `env: Rc<RefCell<Environment>>`
  pointing at the currently-active environment. Function calls save the prior
  env via `std::mem::replace`, install a fresh enclosed env over the
  captured one, and restore unconditionally after the body returns.

- [x] **17. The `loaded_stack` → `global_stack` fallback** — fixed 2026-05-15.
  Was a side-channel in `get_assignable` that papered over snapshot closures
  so top-level mutual recursion worked. Removed: the enclosing chain reaches
  globals through normal lexical walking, so the fallback isn't needed and
  mutual recursion now works at any nesting depth.

- [ ] **18. Dead code in `stack.rs`** — `Stack::get`, `Stack::load_params`
  Partially addressed by the rewrite (those methods are gone). If similar
  unused helpers reappear, delete them.

- [ ] **19. `Statement::FuncDecl` clones the AST** — `mod.rs:340`
  `func.clone()` clones the entire body `Vec<Statement>`; another clone
  happens per call. Wrap the AST in `Rc<ast::Function>` to share cheaply.

- [ ] **20. `&Vec<Statement>` parameters instead of `&[Statement]`** — `mod.rs:105`, `:119`, `:141`
  Clippy `ptr_arg`. Idiomatic Rust takes slices.

- [ ] **21. Comparison ops return `NUMBER`; `Bang` returns `BOOL`** — `mod.rs:431-435`, `:442-443`, `:391-393`
  `1 == 1` prints `1`; `!false` prints `true`. Pick one (`BOOL` is natural).

- [ ] **22. Division by zero / overflow panic the host** — `mod.rs:430`
  `i64` division by zero aborts; addition overflows wrap or panic depending
  on build. Should surface as `RuntimeError`. Use `checked_*` operators.

- [ ] **23. Runtime errors lack source locations** — `mod.rs:18-77`
  Parser carries `Location` into `ParserError`; runtime throws it away.
  Major UX gap. Thread `Location` through `eval_statement`.

- [ ] **24. No host-stack guard for recursion** — `mod.rs:448-502`
  Unbounded user recursion segfaults the interpreter. Add a configurable
  depth counter.

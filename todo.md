# Built-in Function Requests for the Gabelang Development Team

The Gabelang2 parser (`gabelang2.gabe`) depends on the following built-in
functions. Each was needed because there is no efficient way to express
the operation purely in the current language surface.

---

**char_code(s, i) -> number**

- Returns the integer code point (ASCII / Unicode) of the character at
  index `i` in string `s`.
- Index is 0-based.
- Throws an error if `s` is not a string, if `i` is not a number, or if
  `i` is out of bounds (`i < 0` or `i >= len(s)`).
- Rationale: the parser needs to classify characters (digit, letter,
  whitespace) byte by byte during lexing. Without this, every classifier
  would have to do a chain of full-string equality checks, which is both
  awkward and quadratic.

---

**substring(s, start, end) -> string**

- Returns a new string consisting of the characters of `s` from index
  `start` (inclusive) through `end` (exclusive).
- Throws an error if `s` is not a string.
- Throws an error if `start` or `end` is not a number.
- Throws an error if `start < 0`, `end > len(s)`, or `start > end`.
- Rationale: identifier tokens need to capture the textual name of the
  identifier from the source buffer. Building strings character by
  character would require a separate `char_from_code` primitive plus N
  concatenations per identifier.

---

**concat(a, b) -> string**

- Returns a new string formed by appending the string representation of
  `b` to the string representation of `a`.
- If either argument is a number, it is rendered as its decimal digits
  with a leading `-` for negatives (e.g. `-42` -> `"-42"`).
- If either argument is a string, it is used as-is.
- Behavior for booleans / arrays / objects is left to the implementation
  (the parser only ever passes strings and numbers).
- Rationale: the AST printer needs to assemble human-readable lines
  ("LetAssign x", "Num 42", indentation, etc.) and emit them with a
  single `print` call so the output stays on one line per node. Without
  `concat`, every printable line would have to be split across multiple
  `print` calls and would appear on separate lines.

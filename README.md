# Gabelang

## Overview

This is a language I am writing in rust for fun and to learn more about lexers, parsers, and to dymistify coding languages in general.
The *Writing an Interpreter in Go* book by Thorsten Ball was used as a reference for constructing my lexer and parser.

## BNF / Language Grammer

```bnf
<program> = <statement> | <program> <statement>
<statement> = <let_statement> | <if_statement>
<let_statement> = let <identifier> = <expression>;
<if_statement> = if <expression> <code_block>
<code_block> = {<program>}
<identifier> = <ident_char> | <ident_char><identifier>
<indent_char> = <ALPHACHAR> | _
<expression> = <group_expression> |<number_literal> | <operation_expression>
<group_expression> = (<expression>)
<number_literal> = <number> | <number><number_literal>
<number> = 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 0
<operation_expression> = <expression> <op> <expression>
<op> = + | - | * | /
```

## Build

Build with
```sh
cargo build
```

## Run

Run with
```sh
cargo run
```

## Test

Run tests with
```sh
cargo test
```

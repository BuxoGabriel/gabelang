# Gabelang

## Overview

This is a language I am writing in rust for fun and to learn more about lexers, parsers, and to dymistify coding languages in general.
The *Writing an Interpreter in Go* book by Thorsten Ball was used as a reference for constructing my lexer and parser.

## BNF / Language Grammer

```bnf
<program> = <statement> | <program> <statement>
<statement> = <let_statement> | <if_statement> | <while_loop> | <function>
<let_statement> = let <identifier> = <expression>;
<if_statement> = if <expression> <code_block>
<while_loop> = while <expression> <code_block>
<function> = fn <identifier> <parameters> <codeblock>
<code_block> = {<program>}
<parameters> = (<_parameters>)
<_parameters> = <identifier> | <_parameters>, <_parameters>
<identifier> = <ALPHACHAR> | <ident_char><identifier>
<indent_char> = <ALPHACHAR> | _
<expression> = <group_expression> | <operation_expression> | <object_literal> | <array_literal>  | <number_literal> 
<group_expression> = (<expression>)
<operation_expression> = <expression> <op> <expression>
<op> = + | - | * | /
<object_literal> = {<object_field_literals>}
<object_field_literals> = <identifier>: <expression> | <object_field_literals>, <object_field_literals>
<array_literal> = [<expression_list>]
<expression_list> = <expression> | <expression_list>, <expression_list>
<number_literal> = <number> | <number><number_literal>
<number> = 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 0
```

## Built in Functions

**len(arr) -> number**

- returns the length of an array
- throws an error if provided with something other than an array

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

## Todo

- object property parsing and evaluation
- Built in Functions
- Add tests to ast and eval modules
- Make repl nicer to use
- Improve error messages
- Add fun language syntax

### Todo Reach

- Add bytecode compiler
- Create VM that can run bytecode
- Compile to/through c

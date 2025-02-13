# Gabelang

## Overview

This is a language I am writing in rust for fun and to learn more about lexers, parsers, interpreters, and to dymistify programming languages in general.
The [Writing an Interpreter in Go](interpreterbook.com) book by Thorsten Ball was used as a reference and insperation for this project as well as [mkdb](github.com/antoniosarosi/mkdb) by Antonio Sarosi.

## BNF / Language Grammer

```bnf
<program> = <statement> | <program> <statement>
<statement> = <let_statement> | <if_statement> | <while_loop> | <func_decl> | <expression>
<let_statement> = let <identifier> = <expression>;
<assign_statement> = <assignable> = <expression>;
<if_statement> = if <expression> <code_block>
<while_loop> = while <expression> <code_block>
<func_decl> = fn <identifier> <param_idents> <codeblock>
<code_block> = {<program>}
<param_idents> = (<_param_idents>)
<_param_idents> = <identifier> | <_param_idents>, <_param_idents>
<identifier> = <ident_char> | <ident_char><identifier>
<indent_char> = <ALPHACHAR> | _
<expression> = <group_expression> | <operation_expression> | <assignable> | <func_call> | <object_literal> | <array_literal>  | <number_literal>
<group_expression> = (<expression>)
<operation_expression> = <expression> <op> <expression>
<op> = + | - | * | /
<assignable> = <identifier> | <array_index> | <object_prop>
<array_index> = <assignable>[<expression>]
<object_prop> = <assignable>.<identifier>
<func_call> = <assignable>(<expression_list>)
<object_literal> = {<object_field_literals>}
<object_field_literals> = <identifier>: <expression> | <object_field_literals>, <object_field_literals>
<array_literal> = [<expression_list>]
<expression_list> = <expression> | <expression_list>, <expression_list>
<number_literal> = <number> | <number><number_literal>
<number> = 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 0
```

## Commenting

Comment your code using the `//comment` syntax
Any text to the right of a double slash does not make it to the parser or interpretter and will not be evaluated as code

```
// This function doubles a number
fn double_num(num) {
    // Multiplies num by 2 to get the answer
    return num * 2;
}
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

Run as repl with
```sh
cargo run
```
or run a script with
```sh
cargo run -- --file [script name]
```


## Test

Run tests with
```sh
cargo test
```

## Todo

- String Type
- Object get and set/string indexing
- For Loops
- Do While Loops
- Better Documentation
- Built in Functions(print, file, fetch)
- Add tests to ast and eval modules
- Add fun language syntax

### Todo Reach

- Add tooling/language server
- Add bytecode compiler
- Create VM that can run bytecode
- Compile to/through C, NASM, or MIPS

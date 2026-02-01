# Chapter 2: Variables and Types

Adam is a statically typed language with powerful type inference. This chapter
covers variable declaration, mutability, and the built-in types.

## Immutable Variables with let

By default, variables are immutable:

```adam
fn main() {
    let x = 10
    let name = "Alice"
    // x = 20  // Error: cannot assign to immutable variable
}
```

## Mutable Variables with mut

Use `mut` when you need to change a variable:

```adam
fn main() {
    mut counter := 0
    counter = counter + 1
    print("counter is {counter}")  // counter is 1
}
```

## Shorthand Declaration with :=

The `:=` operator declares a variable with an inferred type:

```adam
let x := 42         // inferred as Int
mut y := 3.14       // inferred as Float
let msg := "hello"  // inferred as String
```

## Type Annotations

You can explicitly annotate types:

```adam
let x Int = 42
let pi Float = 3.14159
let active Bool = true
```

## Basic Types

| Type     | Description             | Example       |
|----------|-------------------------|---------------|
| `Int`    | 64-bit signed integer   | `42`          |
| `Float`  | 64-bit floating point   | `3.14`        |
| `Bool`   | Boolean                 | `true, false` |
| `String` | UTF-8 string            | `"hello"`     |
| `Byte`   | Unsigned 8-bit integer  | `0xFF`        |

Numeric literals support underscores: `1_000_000`, `0xFF_FF`, `0b1010_0101`.

## Strings and String Interpolation

Strings support interpolation with `{}`:

```adam
fn main() {
    let a := 10
    let b := 20
    print("The sum of {a} and {b} is {a + b}")
}
```

Multi-line strings use triple quotes:

```adam
let poem := """
    Roses are red,
    Violets are blue.
"""
```

## Type Conversions

Adam requires explicit conversions:

```adam
let x := 42
let y := Float(x)    // Int to Float
let s := String(x)   // Int to String
```

## Constants

Top-level constants use `const`:

```adam
const MAX_SIZE = 1024

fn main() {
    print("Max size is {MAX_SIZE}")
}
```

## What's Next

In [Chapter 3: Functions](03_functions.md), you will learn how to define and
call functions, work with parameters, and return values.

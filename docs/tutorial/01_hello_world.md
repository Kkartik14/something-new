# Chapter 1: Hello World

Welcome to Adam! In this first chapter, you will write your first program,
learn how to build and run it, and understand the basic project structure.

## Your First Program

Create a file called `hello.adam`:

```adam
fn main() {
    print("Hello, world!")
}
```

Every Adam program starts with a `main` function. The `print` function writes
text to the console followed by a newline.

## Running Your Program

Use `adam run` to compile and execute in one step:

```bash
adam run hello.adam
```

To compile into a standalone binary, use `adam build`:

```bash
adam build hello.adam
./hello
```

## Project Structure

Create a new project with `adam init`:

```bash
adam init myapp
```

This generates:

```
myapp/
  adam.toml        # Project manifest (name, version, dependencies)
  src/
    main.adam      # Entry point
```

## The main Function

The `main` function is the entry point. It takes no parameters and returns
nothing by default:

```adam
fn main() {
    print("Program starts here")
}
```

## Printing Output

Use `print` for output with a newline, and string interpolation with `{}`:

```adam
fn main() {
    let name := "Adam"
    let year := 2025
    print("Welcome to {name}, born in {year}!")
}
```

## Comments

Single-line comments use `//`, block comments use `/* */`:

```adam
// This is a single-line comment
/* This is a block comment */

fn main() {
    print("Comments are ignored by the compiler") // inline comment
}
```

## What's Next

In [Chapter 2: Variables and Types](02_variables_and_types.md), you will learn
how to declare variables, work with basic types, and use type inference.

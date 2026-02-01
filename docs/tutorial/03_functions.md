# Chapter 3: Functions

Functions are the primary building blocks in Adam. This chapter covers how to
define functions, pass parameters, return values, and use closures.

## Defining a Function

Functions are declared with `fn`:

```adam
fn greet() {
    print("Hello from a function!")
}

fn main() {
    greet()
}
```

## Parameters and Return Types

Parameters list their types. Return types follow `->`:

```adam
fn add(a Int, b Int) -> Int {
    return a + b
}

fn greet(name String) {
    print("Hello, {name}!")
}
```

Parameters are borrowed by default -- the caller keeps ownership.

## Expression-Based Returns

The last expression in a function body is implicitly returned:

```adam
fn square(x Int) -> Int {
    x * x
}

fn describe(n Int) -> String {
    if n > 0 { "positive" } else { "non-positive" }
}
```

## Multiple Return Values

Return multiple values with tuples:

```adam
fn divide(a Int, b Int) -> (Int, Int) {
    (a / b, a % b)
}

fn main() {
    let (quotient, remainder) := divide(17, 5)
    print("{quotient} remainder {remainder}")
}
```

## Named Arguments

Use named arguments at the call site for clarity:

```adam
fn create_user(name String, age Int, active Bool) -> User {
    User { name, age, active }
}

fn main() {
    let user := create_user(name: "Alice", age: 30, active: true)
}
```

## Functions as Values

Functions are first-class. Assign them to variables or pass them as arguments:

```adam
fn apply(x Int, f fn(Int) -> Int) -> Int {
    f(x)
}

fn double(n Int) -> Int { n * 2 }

fn main() {
    let result := apply(5, double)
    print("Result: {result}")  // 10

    // Anonymous function (closure)
    let triple := fn(n Int) -> Int { n * 3 }
    print("Triple 4: {triple(4)}")  // 12
}
```

## What's Next

In [Chapter 4: Control Flow](04_control_flow.md), you will learn about
conditionals, loops, and the `match` expression.

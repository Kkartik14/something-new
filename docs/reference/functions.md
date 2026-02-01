# Functions

Functions are declared with `fn`. Parameters are `name Type` (no colon),
borrow by default, and the last expression in a block is its return value.

## Basic Syntax

```adam
fn greet(name String) -> String {
    "Hello, {name}!"
}

fn log(message String) {
    print("[LOG] {message}")
}
```

Omit `->` when the function returns nothing.

## Borrow by Default

Parameters borrow their arguments -- the caller keeps ownership. Use `mut`
for a mutable borrow and `own` for ownership transfer. See [Ownership](ownership.md).

```adam
fn print_len(s String)       { print("{s.len()}") }     // immutable borrow
fn append(mut s String)      { s.push("!") }            // mutable borrow
fn consume(own s String)     { print("Gone: {s}") }     // takes ownership
```

## Early Return

```adam
fn abs(n i32) -> i32 {
    if n < 0 { return -n }
    n
}
```

## Multiple Returns via Tuples

```adam
fn divide(a i32, b i32) -> (i32, i32) {
    (a / b, a % b)
}

fn main() {
    (q, r) := divide(17, 5)
    print("{q} r {r}")
}
```

## Generic Functions

Type parameters use `[T]` with optional trait bounds.

```adam
fn max[T: Comparable](a T, b T) -> T {
    if a > b { a } else { b }
}

fn print_sorted[T: Comparable + Display](items [T]) {
    mut sorted := items.clone()
    sorted.sort()
    for item in sorted { print(item.to_string()) }
}
```

See [Traits](traits.md) for details on trait bounds.

## Closures and Lambdas

Closures capture variables from their enclosing scope.

```adam
fn apply(x i32, f fn(i32) -> i32) -> i32 { f(x) }

fn main() {
    doubled := apply(5, |x| x * 2)       // 10
    offset := 10
    shifted := apply(5, |x| x + offset)  // 15 (captures offset)
}
```

Multi-line closures use braces: `|a, b| { ... }`.

## Method Syntax

Methods are defined in `impl` blocks with `self` as the first parameter.
See [Structs](structs.md) for full details.

```adam
impl Point {
    fn distance(self, other Point) -> f64 {
        dx := self.x - other.x
        dy := self.y - other.y
        (dx * dx + dy * dy).sqrt()
    }
}
```

Methods that mutate use `mut self`. Static methods (no `self`) are called
on the type: `Point.new(1.0, 2.0)`.

## Result-Returning Functions

Use `T ! E` as the return type and `?` to propagate errors.

```adam
fn read_port(s String) -> i32 ! ParseError {
    val := parse_int(s)?
    if val < 0 || val > 65535 {
        return err(ParseError("port out of range"))
    }
    val
}
```

See [Enums](enums.md) for matching on `ok`/`err`.

## Common Mistakes

**Mutating a borrowed parameter.**

```adam
fn broken(s String) { s.push("!") }      // ERROR: immutable borrow
fn fixed(mut s String) { s.push("!") }   // OK
```

**Accidental return type change.** The last expression is the return value.

```adam
fn oops(n i32) -> i32 {
    n * 2
    print("done")  // ERROR: returns () not i32
}
```

## See Also

- [Variables](variables.md) -- bindings and type annotations
- [Structs](structs.md) -- impl blocks and method syntax
- [Ownership](ownership.md) -- borrow, mut, and own semantics
- [Traits](traits.md) -- trait bounds on generic parameters

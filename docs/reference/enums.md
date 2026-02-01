# Enums and Pattern Matching

Enums are algebraic data types. Each variant can carry data, and `match`
provides exhaustive pattern matching.

## Definition

```adam
enum Direction { North; South; East; West }

enum Shape {
    Circle(f64)
    Rect(f64, f64)
    Triangle(f64, f64, f64)
}
```

## Creating Values

Prefix variants with the enum name.

```adam
dir := Direction.North
shape := Shape.Circle(5.0)
```

## Match Expressions

Every variant must be handled (exhaustive). Bind matched values to names;
use `_` to ignore a value. The last expression in an arm is its value.

```adam
fn area(shape Shape) -> f64 {
    match shape {
        Circle(r) => 3.14159 * r * r
        Rect(w, h) => w * h
        Triangle(a, b, c) => {
            s := (a + b + c) / 2.0
            (s * (s - a) * (s - b) * (s - c)).sqrt()
        }
    }
}
```

Use `_` as a catch-all wildcard: `_ => print("something else")`.

## Guard Clauses

Add `if` after a pattern to refine the match. Guards are checked top to
bottom; a failing guard falls through.

```adam
match shape {
    Circle(r) if r > 10.0 => "large circle"
    Circle(_) => "small circle"
    Rect(w, h) if w == h => "square"
    Rect(_, _) => "rectangle"
    Triangle(_, _, _) => "triangle"
}
```

## Or-Patterns

Match multiple patterns in the same arm with `|`.

```adam
match dir {
    East | West => true
    North | South => false
}
```

## Generic Enums

```adam
enum Option[T] { Some(T); None }
enum Result[T, E] { Ok(T); Err(E) }
```

## Option (`?T`) and Result (`T ! E`)

`?T` is shorthand for `Option[T]`; `T ! E` for `Result[T, E]`. Return
`nil` for no value. Use `?` to propagate errors (see [Functions](functions.md)).

```adam
fn find_user(users [String], name String) -> ?String {
    for user in users {
        if user == name { return user }
    }
    nil
}

if user := find_user(users, "bob") { print("Found: {user}") }

match load_config("config.toml") {
    ok(config) => print("Host: {config.host}")
    err(e) => print("Failed: {e}")
}
```

## Nested Patterns

Patterns can destructure nested data.

```adam
enum Expr { Num(f64); Add(Expr, Expr) }

fn eval(e Expr) -> f64 {
    match e {
        Num(n) => n
        Add(Num(a), Num(b)) => a + b
        Add(l, r) => eval(l) + eval(r)
    }
}
```

## Common Mistakes

**Non-exhaustive match.** The compiler rejects missing variants.

```adam
match dir {
    North => "up"
    South => "down"
    // COMPILE ERROR: East and West not handled
}
```

**Forgetting the enum prefix.**

```adam
s := Circle(5.0)        // ERROR: Circle not in scope
s := Shape.Circle(5.0)  // Correct
```

**Mixing up optional and result matching.**

```adam
val := find_user(users, "bob")  // returns ?String
match val {
    ok(u) => print(u)  // WRONG: use pattern/nil for optionals
}
```

## See Also

- [Variables](variables.md) -- type annotations for enum values
- [Structs](structs.md) -- when to use structs vs enums
- [Traits](traits.md) -- implementing traits for enums
- [Functions](functions.md) -- `T ! E` returns and `?` propagation

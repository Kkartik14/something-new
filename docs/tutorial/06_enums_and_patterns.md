# Chapter 6: Enums and Pattern Matching

Enums in Adam are algebraic data types: each variant can carry different data.
Combined with `match`, they provide a safe way to model alternatives.

## Defining Enums

```adam
enum Direction {
    North
    South
    East
    West
}
```

## Enums with Data

Variants can carry associated data:

```adam
enum Shape {
    Circle(Float)
    Rectangle(Float, Float)
}

fn area(shape Shape) -> Float {
    match shape {
        Shape.Circle(r) => 3.14159 * r * r
        Shape.Rectangle(w, h) => w * h
    }
}
```

## Named Fields in Variants

```adam
enum Event {
    Click { x Int, y Int }
    KeyPress { key String, shift Bool }
    Resize { width Int, height Int }
}
```

## Match Expressions

The compiler ensures every variant is handled. Use `_` as a wildcard:

```adam
fn handle(event Event) {
    match event {
        Event.Click { x, y } => print("Clicked ({x}, {y})")
        Event.KeyPress { key, shift } => print("Key: {key}")
        Event.Resize { width, height } => print("{width}x{height}")
    }
}

fn is_click(event Event) -> Bool {
    match event {
        Event.Click { .. } => true
        _ => false
    }
}
```

## Match Guards

Add conditions to arms with `if`:

```adam
fn describe_click(event Event) -> String {
    match event {
        Event.Click { x, y } if x == 0 && y == 0 => "origin click"
        Event.Click { x, y } => "click at ({x}, {y})"
        _ => "not a click"
    }
}
```

## Nested Patterns

Patterns can destructure nested data:

```adam
enum Expr {
    Num(Int)
    Add(Expr, Expr)
    Mul(Expr, Expr)
}

fn eval(expr Expr) -> Int {
    match expr {
        Expr.Num(n) => n
        Expr.Add(left, right) => eval(left) + eval(right)
        Expr.Mul(left, right) => eval(left) * eval(right)
    }
}
```

## Methods on Enums

Enums can have methods via `impl`:

```adam
impl Direction {
    fn opposite(self) -> Direction {
        match self {
            Direction.North => Direction.South
            Direction.South => Direction.North
            Direction.East => Direction.West
            Direction.West => Direction.East
        }
    }
}
```

## What's Next

In [Chapter 7: Error Handling](07_error_handling.md), you will learn how Adam
handles errors using result types, optionals, and the `?` operator.

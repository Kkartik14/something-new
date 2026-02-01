# Structs

Structs define custom data types. Fields are `name Type` (no colon, no
comma) and methods live in separate `impl` blocks.

## Definition and Instantiation

```adam
struct Point {
    x f64
    y f64
}

origin := Point { x: 0.0, y: 0.0 }
```

## Shorthand Initialization

When a variable name matches the field name, omit the value.

```adam
x := 3.0
y := 4.0
p := Point { x, y }  // same as Point { x: x, y: y }
```

## Field Access

```adam
print("x = {p.x}, y = {p.y}")
```

## Methods with `impl`

The first parameter is `self`, which borrows the struct by default.

```adam
impl Point {
    fn new(x f64, y f64) -> Point {
        Point { x, y }
    }

    fn distance(self, other Point) -> f64 {
        dx := self.x - other.x
        dy := self.y - other.y
        (dx * dx + dy * dy).sqrt()
    }
}
```

Static methods (no `self`) are called on the type name. Instance methods
use dot syntax.

```adam
p1 := Point.new(0.0, 0.0)
dist := p1.distance(Point.new(3.0, 4.0))
```

## Mutable Methods

Methods that modify the receiver take `mut self`. The variable must also be
declared `mut`.

```adam
impl Config {
    fn set_port(mut self, port i32) { self.port = port }
}

mut cfg := Config { host: "localhost", port: 80, debug: false }
cfg.set_port(8080)
```

## Public Fields and Methods

Use `pub` to expose fields and methods outside the module.

```adam
pub struct Vec2 {
    pub x f64
    pub y f64
}
```

## Generic Structs

Type parameters use `[T]` syntax.

```adam
struct Pair[A, B] { first A; second B }

impl Pair[A, B] {
    fn new(first A, second B) -> Pair[A, B] { Pair { first, second } }
    fn swap(self) -> Pair[B, A] { Pair { first: self.second, second: self.first } }
}

struct Stack[T] { items [T] }

impl Stack[T] {
    fn new() -> Stack[T] { Stack { items: [] } }
    fn push(mut self, item own T) { self.items.push(item) }
    fn pop(mut self) -> ?T { self.items.pop() }
}

mut s := Stack[i32].new()
s.push(1)
```

## Implementing Traits

Use `impl Trait for Struct`. See [Traits](traits.md).

```adam
impl Display for Point {
    fn to_string(self) -> String { "({self.x}, {self.y})" }
}
```

## Common Mistakes

**Forgetting `mut` on the variable when calling a mutable method.**

```adam
cfg := Config { host: "localhost", port: 80, debug: false }
cfg.set_port(8080)  // COMPILE ERROR: cfg is not mutable
```

**Mixing up static and instance calls.**

```adam
p := Point.distance(other)  // WRONG: distance needs self
d := Point.new(1.0, 2.0)    // Correct: new is static
```

**Using a struct after it was moved.** See [Ownership](ownership.md).

```adam
p := Point.new(1.0, 2.0)
consume(own p)
print("{p.x}")  // COMPILE ERROR: p was moved
```

## See Also

- [Functions](functions.md) -- method syntax and `self`
- [Traits](traits.md) -- implementing traits for structs
- [Enums](enums.md) -- algebraic types as an alternative
- [Ownership](ownership.md) -- struct ownership rules

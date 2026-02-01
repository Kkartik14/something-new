# Chapter 5: Structs and Methods

Structs are Adam's primary way to define custom data types. Combined with `impl`
blocks, they let you group data and behavior together.

## Defining a Struct

```adam
struct Point {
    x Float
    y Float
}

struct User {
    name String
    email String
    age Int
}
```

## Creating Instances

Provide values for all fields. Use shorthand when variable names match fields:

```adam
fn main() {
    let origin := Point { x: 0.0, y: 0.0 }
    let name := "Alice"
    let user := User { name, email: "alice@example.com", age: 30 }
    print("{user.name}, age {user.age}")
}
```

## Mutable Fields

If the variable is mutable, its fields can be modified:

```adam
mut p := Point { x: 1.0, y: 2.0 }
p.x = 10.0
```

## Methods with impl

Attach methods using `impl`. The `self` parameter refers to the instance:

```adam
struct Rectangle {
    width Float
    height Float
}

impl Rectangle {
    fn area(self) -> Float {
        self.width * self.height
    }

    fn is_square(self) -> Bool {
        self.width == self.height
    }
}

fn main() {
    let rect := Rectangle { width: 10.0, height: 5.0 }
    print("Area: {rect.area()}")
}
```

## Mutating Methods

Methods that modify `self` declare it as `mut self`:

```adam
impl Rectangle {
    fn scale(mut self, factor Float) {
        self.width = self.width * factor
        self.height = self.height * factor
    }
}
```

## Static Methods

Methods without `self` are called on the type:

```adam
impl Rectangle {
    fn square(size Float) -> Rectangle {
        Rectangle { width: size, height: size }
    }
}

fn main() {
    let sq := Rectangle.square(5.0)
    print("Area: {sq.area()}")  // 25.0
}
```

## Default Field Values

```adam
struct Config {
    host String = "localhost"
    port Int = 8080
    debug Bool = false
}

fn main() {
    let cfg := Config { debug: true }
    print("{cfg.host}:{cfg.port}")  // localhost:8080
}
```

## What's Next

In [Chapter 6: Enums and Pattern Matching](06_enums_and_patterns.md), you will
learn how to define enums with associated data and destructure them with `match`.

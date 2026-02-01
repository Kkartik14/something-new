# Traits

Traits define shared behavior. A trait declares method signatures that
types must implement, and can provide default implementations. Traits
also serve as bounds on generic type parameters.

## Defining a Trait

```adam
trait Display {
    fn to_string(self) -> String
}
```

## Implementing a Trait

```adam
struct Point { x f64; y f64 }

impl Display for Point {
    fn to_string(self) -> String {
        "({self.x}, {self.y})"
    }
}
```

## Required Methods

All methods without a body are required. The compiler rejects incomplete
implementations.

```adam
trait Serializable {
    fn serialize(self) -> String
    fn byte_size(self) -> u64
}

// COMPILE ERROR: missing byte_size
impl Serializable for Point {
    fn serialize(self) -> String { "{self.x},{self.y}" }
}
```

## Default Methods

Provide a body in the trait to supply a default. Types may override it.

```adam
trait Describable {
    fn name(self) -> String
    fn describe(self) -> String { "A {self.name()}" }
}

struct Dog { breed String }

impl Describable for Dog {
    fn name(self) -> String { self.breed }
    // describe() uses the default
}
```

## Trait Bounds on Generics

Constrain type parameters with `T: Trait`. Multiple bounds use `+`.

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

## Core Traits

Built-in traits that primitive types implement automatically:

| Trait | Methods | Description |
|-------|---------|-------------|
| `Eq` | `eq`, `ne` | Equality |
| `Comparable` | `cmp`, `lt`, `gt`, `le`, `ge` | Ordering |
| `Display` | `to_string` | Human-readable format |
| `Debug` | `debug_string` | Debug format |
| `Hash` | `hash` | Hash value for maps/sets |
| `Clone` | `clone` | Deep copy |
| `Copy` | (marker) | Implicit bitwise copy |
| `Default` | `default` | Default constructor |
| `Drop` | `drop` | Cleanup on scope exit |
| `Iterable` | `iter` | Produces an iterator |
| `Iterator` | `next` | Yields next element |
| `Add` `Sub` `Mul` `Div` `Mod` | operator | Arithmetic |
| `Neg` | `neg` | Unary negation |
| `Send` `Sync` | (marker) | Concurrency safety |

Integers and floats implement arithmetic. `bool` has `Eq` and `Copy` but
not arithmetic. `String` has `Add`, `Iterable`, `Drop`, but not `Copy`.

## Trait Objects

Use a trait as a type for dynamic dispatch.

```adam
fn log_all(items [Display]) {
    for item in items { print(item.to_string()) }
}
```

## Common Mistakes

**Missing a required method.**

```adam
impl Serializable for Point {
    fn serialize(self) -> String { "{self.x},{self.y}" }
    // COMPILE ERROR: deserialize not implemented
}
```

**Using a bound the type doesn't satisfy.**

```adam
sort_items([true, false])  // ERROR: bool is not Comparable
```

**Confusing bounds with trait objects.** Bounds (`[T: Display]`) are
monomorphized at compile time. Trait types (`Display` as a param) use
dynamic dispatch at runtime.

```adam
fn show[T: Display](item T) { ... }   // compile-time dispatch
fn show_dyn(item Display) { ... }      // dynamic dispatch
```

## See Also

- [Structs](structs.md) -- types that implement traits
- [Enums](enums.md) -- implementing traits for enums
- [Functions](functions.md) -- generic functions with bounds
- [Ownership](ownership.md) -- `Copy`, `Drop`, `Send`, `Sync`

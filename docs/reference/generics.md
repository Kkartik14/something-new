# Generics

Adam uses square-bracket syntax (`[T]`) for generic type parameters.

## Generic Functions

```adam
fn identity[T](val T) T {
    val
}

let x = identity[i32](42)
let y = identity("hello")   // T inferred as String
```

Multiple type parameters are comma-separated:

```adam
fn pair[A, B](a A, b B) (A, B) { (a, b) }
```

## Generic Structs

```adam
struct Box[T] {
    value T
}

impl[T] Box[T] {
    fn unwrap(self) T { self.value }
}
```

## Generic Enums

The built-in Result and Optional types are generic enums:

```adam
enum Option[T] { Some(T), None }
enum Result[T, E] { Ok(T), Err(E) }
```

See [Error Handling](error_handling.md) for `?T` and `T ! E` shorthand.

## Trait Bounds

Constrain type parameters with bounds after a colon:

```adam
fn max[T: Comparable](a T, b T) T {
    if a > b { a } else { b }
}
```

Multiple bounds use `+`:

```adam
fn print_sorted[T: Comparable + Display](items [T]) {
    for item in items.clone().sort() { print("{item}") }
}
```

### Common Trait Bounds

| Trait        | Enables                                |
|--------------|----------------------------------------|
| `Eq`         | `==` and `!=`                          |
| `Comparable` | `<`, `>`, `<=`, `>=`, sorting          |
| `Display`    | String interpolation, `to_string()`    |
| `Hash`       | Map key or Set element                 |
| `Clone`      | `.clone()`                             |
| `Copy`       | Implicit bitwise copy                  |
| `Send`       | Safe to send across green threads      |
| `Default`    | `.default()` constructor               |

## Where Clauses

For complex bounds, use a `where` clause:

```adam
fn merge[K, V](a Map[K, V], b Map[K, V]) Map[K, V]
where K: Eq + Hash, V: Clone
{
    let result = a.clone()
    for key, val in b { result.insert(key, val.clone()) }
    result
}
```

## Type Inference

Adam infers type parameters whenever possible:

```adam
let nums = [1, 2, 3]          // inferred as [i32]
let empty = Vec[String].new() // explicit, no value to infer from

fn first[T](items [T]) ?T {
    if items.len() > 0 { items[0] } else { nil }
}
let f = first([10, 20, 30])   // T inferred as i32
```

## Common Generic Patterns

**Container methods:**

```adam
impl[T] Vec[T] {
    fn map[U](self, f fn(T) U) Vec[U] {
        let result = Vec[U].new()
        for item in self { result.push(f(item)) }
        result
    }
}
```

**Generic channels** (see [Concurrency](concurrency.md)):

```adam
let ch = chan[String](5)
spawn { ch.send("hello") }
let msg = ch.recv()
```

## Common Mistakes

- **Forgetting a trait bound.** Using `==` without requiring `Eq` is a
  compile error.
- **Specifying type parameters unnecessarily.** Let inference work.
  Write `identity(42)` instead of `identity[i32](42)`.
- **Mixing up `[T]` meanings.** After a name it is a generic parameter;
  standalone it is a dynamic array. Context disambiguates.
- **Over-constraining bounds.** Only require traits your function uses.
  Extra bounds reject valid caller types for no reason.
- **Using `Copy` when `Clone` suffices.** `Copy` is stricter. If you
  only need duplication, bound on `Clone`.

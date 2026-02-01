# Variables and Types

Adam uses `let` for immutable bindings, `mut` for mutable bindings, and `:=`
as a shorthand for type-inferred `let`. Statements are newline-terminated.

## Immutable Bindings and `:=`

```adam
let name String = "Adam"       // explicit type
let year i32 = 2025

name := "Adam"                 // := infers the type from the right-hand side
count := 42                    // i32
ratio := 3.14                  // f64
active := true                 // bool
letter := 'A'                  // char
```

## Mutable Bindings

Prefix with `mut` to allow reassignment.

```adam
mut counter := 0
counter = counter + 1
```

## Type Annotations

Annotations follow the name, separated by a space (no colon).

```adam
let x i32 = 10
mut flag bool = false
```

## Primitive (Copy) Types

Copy types are duplicated on assignment -- the original stays valid.

| Type | Description |
|------|-------------|
| `i8` `i16` `i32` `i64` | Signed integers |
| `u8` `u16` `u32` `u64` | Unsigned integers |
| `f32` `f64` | Floating point |
| `bool` | Boolean |
| `char` | Unicode character |

```adam
x := 42
y := x          // x is copied, both valid
print("{x} {y}")
```

## Heap Types

`String`, `Vec`, `Map`, and [structs](structs.md) follow [ownership](ownership.md)
rules -- borrowed by default, moved with `own`.

```adam
names := ["alice", "bob"]
scores := Map[String, i32]()
```

## Tuples

```adam
pair := (42, "hello")
print("{pair.0}")   // 42
```

## Shadowing

A new `:=` in the same scope shadows the previous binding.

```adam
x := 10
x := "now a string"  // shadows with a different type
```

## Scope Rules

Variables live until the end of their enclosing `{}` block.

```adam
fn example() {
    outer := "outer"
    {
        inner := "inner"
        print("{outer} {inner}")
    }
    // inner is no longer accessible here
}
```

## Constants

Top-level `let` declarations outside functions act as constants.

```adam
let MAX_RETRIES i32 = 5

fn main() {
    print("Max retries: {MAX_RETRIES}")
}
```

## Common Mistakes

**Using `:=` when you mean `=`.** `:=` creates a new binding; `=` reassigns.

```adam
mut x := 10
x := 20   // shadows x with a NEW immutable binding
x = 20    // reassigns the mutable x -- correct
```

**Forgetting `mut`.**

```adam
count := 0
count = 1   // COMPILE ERROR: count is immutable
```

**Using a value after move.** See [Ownership](ownership.md).

```adam
name := "Adam"
consume(own name)
print(name)       // COMPILE ERROR: value used after move
```

## See Also

- [Functions](functions.md) -- parameter types and return types
- [Ownership](ownership.md) -- move semantics and borrow rules
- [Structs](structs.md) -- user-defined types

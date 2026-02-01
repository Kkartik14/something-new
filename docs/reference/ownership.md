# Ownership and Borrowing

Adam manages memory with an ownership model -- no garbage collector, no
lifetime annotations. Every value has one owner, and borrows are the
default when passing values to functions.

## The Ownership Rule

Each value has a single owning variable. When the owner goes out of scope,
the value is dropped.

```adam
fn example() {
    name := "Adam"    // name owns the String
}                     // name is dropped here
```

## Borrow by Default

Parameters borrow (shared, immutable) by default. The caller keeps ownership.

```adam
fn print_length(s String) {
    print("Length: {s.len()}")
}

name := "Adam"
print_length(name)
print(name)        // still valid
```

## The `own` Keyword

Use `own` to take ownership. The caller also writes `own` to make the
transfer explicit.

```adam
fn consume(own s String) {
    print("Got: {s}")
}   // s dropped here

name := "Adam"
consume(own name)
// print(name)  // COMPILE ERROR: used after move
```

## Mutable Borrows

Use `mut` to borrow mutably. Only one mutable borrow can exist at a time.

```adam
fn append_exclaim(mut s String) { s.push("!") }

mut greeting := "Hello"
append_exclaim(mut greeting)
print(greeting)  // "Hello!"
```

## Parameter Mode Summary

| Syntax | Meaning | Caller writes | Caller keeps value? |
|--------|---------|---------------|---------------------|
| `name Type` | Immutable borrow | nothing | Yes |
| `mut name Type` | Mutable borrow | `mut name` | Yes (modified) |
| `own name Type` | Ownership transfer | `own name` | No (moved) |

## Copy Types

Primitives (`i32`, `f64`, `bool`, `char`) implement `Copy` and are always
duplicated -- never moved.

```adam
x := 42
y := x       // copied
print("{x}") // still valid
```

## Move Semantics for Heap Types

Heap types (`String`, `Vec`, `Map`, structs) are moved with `own` and
borrowed otherwise.

```adam
fn takes(own names [String]) {
    for name in names { print(name) }
}

names := ["alice", "bob"]
takes(own names)
// names no longer accessible
```

## Scope-Based Dropping

Values are dropped at end of scope. `Drop::drop` runs automatically.

```adam
{
    inner := "inner"
}   // inner dropped here
```

## No Lifetime Annotations

The compiler infers borrow lifetimes from scope structure -- no annotations needed.

## Ownership in Structs

Struct fields own their values. Passing a struct borrows all fields.

```adam
struct User { name String; age i32 }

fn greet(user User) { print("Hi, {user.name}!") }

u := User { name: "Alice", age: 30 }
greet(u)
print(u.name)  // still valid -- u was borrowed
```

## Common Mistakes

**Using a value after move.**

```adam
name := "Adam"
consume(own name)
print(name)       // COMPILE ERROR: used after move
```

**Forgetting `mut` on the variable for a mutable borrow.**

```adam
greeting := "Hello"
append_exclaim(mut greeting)  // ERROR: greeting is not mut
```

**Two simultaneous mutable borrows.**

```adam
mut list := [1, 2, 3]
a := mut list
b := mut list  // ERROR: already borrowed mutably
```
## See Also

- [Variables](variables.md) -- `let`, `mut`, copy types
- [Functions](functions.md) -- parameter modes
- [Structs](structs.md) -- struct field ownership
- [Traits](traits.md) -- `Copy`, `Clone`, `Drop`, `Send`, `Sync`

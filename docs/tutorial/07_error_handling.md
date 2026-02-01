# Chapter 7: Error Handling

Adam has no exceptions. Errors are values represented by the result type `T ! E`
and optionals `?T`. This makes error handling explicit and composable.

## The Result Type

A function that can fail returns `T ! E`:

```adam
fn read_file(path String) -> String ! FileError {
    if !exists(path) {
        return error(FileError.NotFound)
    }
    // ... read and return contents
}
```

## Handling Results with match

```adam
fn main() {
    match read_file("config.toml") {
        ok(contents) => print(contents)
        error(FileError.NotFound) => print("File not found")
        error(FileError.PermissionDenied) => print("Access denied")
    }
}
```

## The ? Operator

The `?` operator propagates errors. If the result is an error, the function
returns it immediately. Otherwise, the success value is unwrapped:

```adam
fn load_config() -> Config ! FileError {
    let text := read_file("config.toml")?
    let config := parse_config(text)?
    config
}
```

## Optionals

`?T` represents a value that may or may not be present. There is no null in
Adam; use `nil` for the absent case:

```adam
fn find_user(id Int) -> ?User {
    if id == 1 { User { name: "Alice", age: 30 } } else { nil }
}
```

## if let

Conditionally unwrap an optional with `if let`:

```adam
fn main() {
    if let user := find_user(1) {
        print("Found: {user.name}")
    } else {
        print("Not found")
    }
}
```

## Default Values with ??

Provide a fallback when an optional is nil:

```adam
let name := find_user(99)?.name ?? "Unknown"
```

## Match on Optionals

```adam
fn greet(id Int) {
    match find_user(id) {
        some(user) => print("Hello, {user.name}!")
        nil => print("Hello, stranger!")
    }
}
```

## Optional Chaining

Use `?.` to safely access fields on optionals:

```adam
struct Company {
    ceo ?User
}

fn ceo_name(company Company) -> ?String {
    company.ceo?.name
}
```

## Converting Result to Optional

Discard the error with `.ok()`:

```adam
let contents := read_file("data.txt").ok() ?? "default"
```

## What's Next

In [Chapter 8: Ownership and Borrowing](08_ownership.md), you will learn how
Adam manages memory safely without a garbage collector.

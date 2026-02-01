# Chapter 8: Ownership and Borrowing

Adam manages memory without a garbage collector by tracking ownership at compile
time. This chapter explains the rules and how borrowing keeps them ergonomic.

## The Three Rules

1. Every value has exactly one owner at a time.
2. When the owner goes out of scope, the value is dropped.
3. You can borrow a value without taking ownership.

## Borrow by Default

Function parameters borrow by default. The caller keeps ownership:

```adam
fn greet(name String) {
    print("Hello, {name}!")
}

fn main() {
    let name := "Alice"
    greet(name)
    print("Still have: {name}")  // name is still valid
}
```

## The own Keyword

Use `own` to take ownership. The caller gives up the value:

```adam
fn consume(own message String) {
    print("Consumed: {message}")
}

fn main() {
    let msg := "important data"
    consume(msg)
    // print(msg)  // Error: `msg` has been moved
}
```

## Mutable Borrows with mut

To modify a borrowed value, use `mut`. The caller must also mark it:

```adam
fn add_item(mut list Vec[String]) {
    list.push("new item")
}

fn main() {
    mut items := Vec[String].new()
    add_item(mut items)
    print("{items}")  // ["new item"]
}
```

## Borrowing Rules

Enforced at compile time to prevent data races:

- Any number of immutable borrows at once.
- Exactly one mutable borrow at a time.
- No immutable and mutable borrows simultaneously.

## Copy Types

Small types (`Int`, `Float`, `Bool`, `Byte`) are copied, never moved:

```adam
let n := 21
let result := double(n)
print("{n} doubled is {result}")  // n still valid
```

## Move Semantics

For non-copy types, assignment moves the value. Use `.clone()` for a copy:

```adam
fn main() {
    let a := Vec[Int].from([1, 2, 3])
    let b := a   // a is moved to b
    // print(a)  // Error: `a` has been moved

    let c := b.clone()  // explicit copy, both b and c valid
}
```

## Ownership in Structs

```adam
fn archive(own doc Document) {
    print("Archived: {doc.title}")
}

fn main() {
    let doc := Document { title: "Report", body: "..." }
    archive(doc)
    // doc is no longer accessible
}
```

## What's Next

In [Chapter 9: Collections](09_collections.md), you will learn about Adam's
built-in collection types and how ownership interacts with them.

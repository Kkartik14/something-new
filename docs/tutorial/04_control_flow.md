# Chapter 4: Control Flow

Adam provides familiar control flow with a few twists: `if/else` blocks are
expressions, `match` is exhaustive, and `for` works with ranges and iterators.

## if / else

Parentheses around the condition are not required. Since `if` is an expression,
it can return a value:

```adam
fn main() {
    let temp := 72
    if temp > 80 {
        print("Hot")
    } else if temp > 60 {
        print("Pleasant")
    } else {
        print("Cold")
    }

    let status := if temp > 80 { "hot" } else { "cool" }
}
```

## for Loops

Iterate over ranges and collections:

```adam
fn main() {
    for i in 0..5 { print("{i}") }         // 0 to 4
    for i in 1..=5 { print("{i}") }        // 1 to 5 inclusive

    let names := ["Alice", "Bob", "Carol"]
    for name in names { print("Hello, {name}!") }

    for i, name in names { print("{i}: {name}") }  // with index
}
```

## while and loop

`while` runs while a condition is true. `loop` runs forever until `break`:

```adam
fn main() {
    mut n := 1
    while n <= 64 {
        print("{n}")
        n = n * 2
    }

    mut count := 0
    loop {
        count = count + 1
        if count > 5 { break }
    }
}
```

## break and continue

`break` exits the loop. `continue` skips to the next iteration:

```adam
fn main() {
    for i in 0..10 {
        if i % 2 == 0 { continue }
        if i > 7 { break }
        print("{i}")  // prints 1, 3, 5, 7
    }
}
```

## match Basics

Match compares a value against patterns. Every case must be covered:

```adam
fn describe(n Int) -> String {
    match n {
        0 => "zero"
        1 => "one"
        _ => "many"
    }
}
```

Match on multiple values, ranges, and with guards:

```adam
fn is_vowel(c String) -> Bool {
    match c {
        "a", "e", "i", "o", "u" => true
        _ => false
    }
}

fn grade(score Int) -> String {
    match score {
        90..=100 => "A"
        80..=89  => "B"
        _        => "below B"
    }
}

fn classify(n Int) -> String {
    match n {
        x if x < 0 => "negative"
        0           => "zero"
        _           => "positive"
    }
}
```

## What's Next

In [Chapter 5: Structs and Methods](05_structs_and_methods.md), you will learn
how to define custom data types and attach methods to them.

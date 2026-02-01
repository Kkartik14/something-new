# Chapter 9: Collections

Adam provides three core generic collections: `Vec` for ordered sequences, `Map`
for key-value pairs, and `Set` for unique elements.

## Vec

`Vec[T]` is a growable, ordered list:

```adam
fn main() {
    mut nums := Vec[Int].new()
    nums.push(10)
    nums.push(20)
    print("Length: {nums.len()}, first: {nums[0]}")

    let names := ["Alice", "Bob", "Carol"]  // literal syntax
}
```

Common operations:

```adam
mut items := [1, 2, 3, 4, 5]
items.push(6)            // append
let last := items.pop()  // remove last
items.insert(0, 99)      // insert at index
items.remove(2)          // remove at index
print("{items.contains(3)}, {items.is_empty()}")
```

## Iterating

```adam
let scores := [85, 92, 78, 95]
for score in scores { print("{score}") }
for i, score in scores { print("#{i}: {score}") }  // with index
```

## Functional Operations

```adam
let nums := [1, 2, 3, 4, 5]
let doubled := nums.map(fn(n Int) -> Int { n * 2 })
let evens := nums.filter(fn(n Int) -> Bool { n % 2 == 0 })
let sum := nums.reduce(0, fn(acc Int, n Int) -> Int { acc + n })
```

## Map

`Map[K, V]` stores key-value pairs:

```adam
fn main() {
    mut ages := Map[String, Int].new()
    ages["Alice"] = 30
    ages["Bob"] = 25

    print("Alice: {ages["Alice"]}")

    // Safe access returns optional
    let age := ages.get("Carol") ?? 0

    for key, value in ages { print("{key}: {value}") }
}
```

Literal syntax:

```adam
let scores := Map.from([("math", 95), ("science", 88)])
```

## Set

`Set[T]` stores unique elements:

```adam
fn main() {
    mut tags := Set[String].new()
    tags.insert("adam")
    tags.insert("adam")  // duplicate, not added
    print("Count: {tags.len()}")  // 1
}
```

Set operations:

```adam
let a := Set.from([1, 2, 3, 4])
let b := Set.from([3, 4, 5, 6])
let union := a.union(b)           // {1, 2, 3, 4, 5, 6}
let inter := a.intersection(b)    // {3, 4}
let diff := a.difference(b)       // {1, 2}
```

## Slices

A slice is a borrowed view into a contiguous sequence:

```adam
fn sum(numbers [Int]) -> Int {
    mut total := 0
    for n in numbers { total = total + n }
    total
}

fn main() {
    let data := [10, 20, 30, 40, 50]
    let middle := data[1..4]  // [20, 30, 40]
    print("Sum: {sum(middle)}")
}
```

## What's Next

In [Chapter 10: Concurrency](10_concurrency.md), you will learn how Adam makes
concurrent programming safe with green threads and channels.

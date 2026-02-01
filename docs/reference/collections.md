# Collections

Adam provides three built-in generic collections: `Vec` (dynamic array),
`Map` (hash map), and `Set` (hash set).

## Vec

`Vec[T]` is a growable, ordered sequence.

```adam
let empty = Vec[i32].new()
let nums = [1, 2, 3, 4, 5]
let pre = Vec[String].with_capacity(100)
```

### Indexing

```adam
let first = nums[0]
nums[2] = 30
```

Use `get` for safe access (returns `?T`, see [Error Handling](error_handling.md)):

```adam
if let val = nums.get(10) { print("{val}") }
```

### Common Methods

| Method             | Description                            |
|--------------------|----------------------------------------|
| `push(val)`        | Append to end                          |
| `pop()`            | Remove and return last (`?T`)          |
| `get(idx)`         | Safe access (`?T`)                     |
| `set(idx, val)`    | Overwrite element at index             |
| `insert(idx, val)` | Insert at position, shifting right     |
| `remove(idx)`      | Remove at position, shifting left      |
| `len()`            | Number of elements                     |
| `is_empty()`       | True if length is zero                 |
| `contains(val)`    | True if the value is present           |
| `first()` / `last()` | First or last element (`?T`)        |
| `reverse()`        | Reverse in place                       |
| `sort()`           | Sort in place (requires `Comparable`)  |
| `clear()`          | Remove all elements                    |
| `extend(other)`    | Append all elements from another Vec   |
| `clone()`          | Deep copy                              |

### Iteration

```adam
for val in nums { print("{val}") }
for i, val in nums { print("{i}: {val}") }
```

## Map

`Map[K, V]` is an unordered hash map. Keys must implement `Eq + Hash`
(see [Generics](generics.md)).

```adam
let scores = {"alice": 95, "bob": 87}
```

### Common Methods

| Method              | Description                          |
|---------------------|--------------------------------------|
| `insert(key, val)`  | Insert or update; returns old (`?V`) |
| `get(key)`          | Look up, returns `?V`                |
| `remove(key)`       | Remove, returns removed value (`?V`) |
| `contains_key(key)` | True if key exists                   |
| `len()` / `is_empty()` | Count and emptiness check        |
| `clear()` / `clone()` | Clear all or deep copy             |

### Iteration

```adam
for key, val in scores { print("{key} scored {val}") }
```

## Set

`Set[T]` is an unordered collection of unique values. Elements must
implement `Eq + Hash`.

```adam
let tags = Set.from(["urgent", "bug", "p1"])
```

### Common Methods

| Method         | Description                             |
|----------------|-----------------------------------------|
| `insert(val)`  | Add element; returns true if new        |
| `remove(val)`  | Remove element; returns true if present |
| `contains(val)`| True if present                         |
| `len()` / `is_empty()` | Count and emptiness check       |
| `clear()` / `clone()` | Clear all or deep copy            |

### Iteration

```adam
for tag in tags { print("{tag}") }
```

## Literals

```adam
let names = ["Alice", "Bob"]               // Vec[String]
let config = {"host": "localhost", "port": "8080"} // Map[String, String]
```

## Capacity and Performance

Pre-allocate with `with_capacity` to avoid repeated allocations:

```adam
let big = Vec[i32].with_capacity(10000)
```

Vec uses 2x growth. Map and Set resize at 75% load factor.

## Common Mistakes

- **Non-hashable Map keys.** `f64` does not implement `Hash`. Use
  integer or string keys.
- **Modifying a collection while iterating.** Collect changes in a
  separate list and apply after the loop.
- **Indexing without bounds checking.** `[]` panics on out-of-bounds.
  Use `.get()` when the index may be invalid.
- **Assuming Map/Set order.** Iteration order is not guaranteed. Sort
  keys if deterministic order is needed.
- **Comparing Vecs for set equality.** Two Vecs are equal only with the
  same elements in the same order. Convert to Sets for order-independent
  comparison.

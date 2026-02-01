# Control Flow

All control flow constructs in Adam are expressions -- they produce
values and can appear on the right-hand side of a `let` binding.

## If / Else

```adam
let status = if count > 0 { "active" } else { "idle" }
```

The `else` branch is optional when the result is unused:

```adam
if needs_update { refresh() }
```

Chained conditions:

```adam
let tier = if score >= 90 { "gold" }
    else if score >= 70 { "silver" }
    else { "bronze" }
```

## For Loops

### Range Iteration

```adam
for i in 0..10 { print("{i}") }    // 0 through 9
for i in 1..=5 { print("{i}") }    // 1 through 5
```

### Collection Iteration

```adam
let names = ["Alice", "Bob", "Charlie"]
for name in names { print("hello, {name}") }
```

Index-value iteration:

```adam
for i, val in items { print("{i}: {val}") }
```

Map iteration yields key-value pairs:

```adam
for key, val in config { print("{key} = {val}") }
```

See [Collections](collections.md) for iterable types.

## While Loops

```adam
while attempts < 3 {
    if try_connect() { break }
    attempts += 1
}
```

## Loop (Infinite)

`loop` repeats until explicitly broken:

```adam
loop {
    let event = poll()
    if event == .quit { break }
}
```

## Break and Continue

`break` exits the innermost loop. `continue` skips to the next iteration.

```adam
for i in 0..100 {
    if i % 2 == 0 { continue }
    if i > 50 { break }
    print("{i}")
}
```

### Labeled Loops

Labels let you break or continue an outer loop from inside a nested one:

```adam
@outer for row in grid {
    for cell in row {
        if cell == target {
            print("found")
            break @outer
        }
    }
}
```

## Match Expressions

`match` performs exhaustive pattern matching:

```adam
match direction {
    .north => move(0, 1)
    .south => move(0, -1)
    .east  => move(1, 0)
    .west  => move(-1, 0)
}
```

### Pattern Types

| Pattern      | Example                 |
|--------------|-------------------------|
| Literal      | `42`, `"hello"`, `true` |
| Enum variant | `.north`, `.some(val)`  |
| Binding      | `val`                   |
| Tuple        | `(x, y)`                |
| Wildcard     | `_`                     |
| Guard        | `n if n > 0`            |

### Match as Expression

```adam
let label = match status {
    .active  => "running"
    .paused  => "paused"
    _        => "other"
}
```

See [Error Handling](error_handling.md) for matching on `ok`/`err`.

## Common Mistakes

- **Omitting `else` in an expression `if`.** Both branches are required
  when the result type is not `Unit`.
- **Infinite `loop` without `break`.** Compiles fine but hangs.
- **Non-exhaustive `match`.** The compiler rejects a match that does not
  cover all variants. Add missing arms or `_`.
- **Shadowing the loop variable.** A `let` with the same name inside
  the body hides the iteration variable.
- **`break value` outside `loop`.** Only `loop` supports returning a
  value via `break`; `for` and `while` do not.

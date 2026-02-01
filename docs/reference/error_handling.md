# Error Handling

Adam uses `T ! E` (Result) for recoverable errors and `?T` (Optional)
for values that may be absent.

## Result Type (`T ! E`)

A Result holds either a success value of type `T` or an error of type `E`.

```adam
fn parse_int(s String) i32 ! ParseError {
    // returns ok value or error
}
```

### Match on Ok / Err

```adam
match parse_int("42") {
    ok(val)  => print("got {val}")
    err(e)   => print("failed: {e}")
}
```

See [Control Flow](control_flow.md) for more on `match`.

### The `?` Propagation Operator

Inside a function that returns a Result, `?` propagates errors to the
caller. On ok, the inner value is unwrapped.

```adam
fn load_config(path String) Config ! IoError {
    let text = read_file(path)?
    let config = parse_toml(text)?
    config
}
```

## Optional Type (`?T`)

An Optional represents a value that might be `nil`.

```adam
fn find(haystack [i32], needle i32) ?i32 {
    for i, val in haystack {
        if val == needle { return i }
    }
    nil
}
```

### If-Let for Optionals

Conditionally unwrap an optional with `if let`:

```adam
if let idx = find(items, target) {
    print("found at {idx}")
} else {
    print("not found")
}
```

## Chaining Results

Chain fallible operations with `?` to keep the happy path linear:

```adam
fn process(path String) Report ! AppError {
    let data = read_file(path)?
    let parsed = parse(data)?
    let validated = validate(parsed)?
    generate_report(validated)
}
```

## Custom Error Types

Define errors as enums and implement `Display`:

```adam
enum AppError {
    NotFound(String)
    PermissionDenied
    ParseFailed(String)
}

impl Display for AppError {
    fn to_string(self) String {
        match self {
            NotFound(path)   => "not found: {path}"
            PermissionDenied => "permission denied"
            ParseFailed(msg) => "parse error: {msg}"
        }
    }
}
```

## Error Conversion

Implement `From` so `?` automatically wraps inner errors:

```adam
impl From[IoError] for AppError {
    fn from(e IoError) AppError {
        AppError.ParseFailed(e.to_string())
    }
}
```

## Combining Results and Optionals

```adam
let opt ?i32 = some_optional()
let res i32 ! String = opt.ok_or("value was nil")

let res2 i32 ! AppError = try_something()
let opt2 ?i32 = res2.ok()   // discards the error
```

## Common Mistakes

- **Ignoring a Result.** The compiler warns when a `T ! E` value is
  discarded. Always handle or propagate it.
- **Using `?` outside a Result-returning function.** The enclosing
  function must have a compatible return type.
- **Comparing an Optional to a value directly.** Use `if let` instead
  of `if opt == 42` -- the latter is a type mismatch.
- **Forgetting `nil` is not `false`.** `?Bool` can be `nil`, `true`, or
  `false`. Use `if let` to distinguish all three states.
- **Nesting optionals unintentionally.** Calling a `?T` function from
  another `?T` function without unwrapping produces `??T`.

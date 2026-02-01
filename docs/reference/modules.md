# Modules and Imports

Adam organizes code with `mod`, controls visibility with `pub`, and
brings names into scope with `use`.

## The `mod` Keyword

```adam
mod math {
    pub fn add(a i32, b i32) i32 { a + b }

    fn internal_helper() {
        // not visible outside this module
    }
}
```

Items inside a module are private by default.

## `use` Statements

```adam
use math.add

fn main() {
    let sum = add(1, 2)
}
```

### Selective Imports

```adam
use collections.{Vec, Map, Set}
```

### Wildcard Imports

```adam
use math.*
```

Use sparingly -- wildcards make it harder to trace name origins.

### Aliased Imports

```adam
use network.http.Client as HttpClient
use network.grpc.Client as GrpcClient
```

## `pub` Visibility

`pub` makes items visible outside their parent module.

```adam
pub struct Point {
    pub x f64
    pub y f64
}
```

Without `pub` on a field, it is only accessible within the defining module.

## File-as-Module

Each `.adam` file implicitly defines a module named after the file:

```
project/
  main.adam       // root module
  math.adam       // module `math`
  utils.adam      // module `utils`
```

```adam
// main.adam
use math.add
use utils.format_output
```

## Directory Modules

A directory with `mod.adam` forms a module whose sub-modules are the
other files in the directory:

```
project/
  network/
    mod.adam       // module `network`
    http.adam      // sub-module `network.http`
    tcp.adam       // sub-module `network.tcp`
```

```adam
// network/mod.adam
pub use http.Client
pub use tcp.connect
```

## Re-Exports

Use `pub use` to re-export items so consumers skip internal structure:

```adam
// lib.adam
pub use internal.parser.parse
pub use internal.codegen.emit
```

Callers import from `lib` directly:

```adam
use lib.{parse, emit}
```

## Module Paths

Dot-separated paths reference nested modules:

```adam
use network.http.Client
use std.collections.Vec
```

## Common Mistakes

- **Forgetting `pub`.** The compiler reports "item is private" when a
  type or function used across modules lacks `pub`.
- **Circular imports.** Not allowed. Extract shared types into a third
  module.
- **Wildcard imports shadowing locals.** A wildcard can silently bring
  in a conflicting name. Prefer selective imports.
- **Missing `mod.adam`.** Without it the compiler does not recognize a
  directory as a module.
- **Re-exporting private items.** `pub use` only works on items that
  are already `pub` in their source module.

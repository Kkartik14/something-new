# Views (UI Framework)

Adam includes a declarative UI framework built around the `view` keyword.
Views describe the interface as a function of state, and the runtime
handles updates when state changes.

## The `view` Keyword and Body Block

```adam
view Greeting {
    body {
        Text("Hello, Adam!")
    }
}
```

The `body` block is re-evaluated whenever reactive state changes.

## `@state` -- Reactive State

Fields marked `@state` are local to the view. Mutations trigger a
re-render.

```adam
view Counter {
    @state count i32 = 0

    body {
        VStack {
            Text("Count: {count}")
            Button("Increment") { count += 1 }
        }
    }
}
```

## `@prop` -- Component Props

`@prop` declares values passed from a parent view. Props are read-only.

```adam
view UserCard {
    @prop name String
    @prop avatar_url String

    body {
        HStack {
            Image(avatar_url)
            Text(name).font(.title)
        }
    }
}
```

Instantiate by providing prop values:

```adam
UserCard(name: user.name, avatar_url: user.avatar)
```

## Modifiers

Style views by chaining modifier methods:

```adam
Text("Hello")
    .font(.headline)
    .color(.blue)
    .padding(16)
    .background(.gray)
    .corner_radius(8)
```

Common modifiers: `.font`, `.color`, `.padding`, `.frame`,
`.background`, `.border`, `.opacity`, `.corner_radius`.

## Event Handling

Handle user interactions with closures passed to interactive views:

```adam
TextField("Username", text: &username)
Button("Log in") { submit(username, password) }
```

## Conditional Rendering

Use `if`/`else` inside the body (see [Control Flow](control_flow.md)):

```adam
body {
    if is_loading {
        Spinner()
    } else if items.is_empty() {
        Text("No items")
    } else {
        ItemList(items: items)
    }
}
```

## List Rendering

Use `for` to render a collection:

```adam
body {
    VStack {
        for item in items {
            ItemRow(title: item.title, done: item.done)
        }
    }
}
```

## Navigation

Navigate between screens with `NavigationStack` and `NavigationLink`:

```adam
NavigationStack {
    List {
        for item in items {
            NavigationLink(destination: DetailView(item: item)) {
                Text(item.title)
            }
        }
    }.title("My Items")
}
```

## Common Mistakes

- **Mutating `@prop` values.** Props are read-only. Pass a callback or
  use shared state for child-to-parent communication.
- **Heavy computation in `body`.** The body runs every re-render. Move
  expensive work into a `spawn` block and write results to `@state`.
- **Forgetting to initialize `@state`.** Every `@state` field needs a
  default value.
- **Deeply nesting views.** Extract sub-views into separate `view`
  declarations to keep the body readable.
- **Using `for` without stable identity.** Without a stable identity
  field the framework falls back to index-based diffing, causing
  glitches on reorder.

# Chapter 11: Building a Mobile App

Adam includes a declarative UI framework that compiles to native iOS and Android.
This chapter introduces `view`, state management, and building a complete app.

## Your First View

A `view` defines a UI component with a `body` block:

```adam
view HelloScreen {
    body { Text("Hello, Adam!") }
}
```

## State with @state

`@state` declares reactive state. Changes trigger re-rendering:

```adam
view Counter {
    @state count Int = 0

    body {
        VStack {
            Text("Count: {count}")
            Button("Increment") { count = count + 1 }
        }
    }
}
```

## Props with @prop

Accept data from a parent view with `@prop`:

```adam
view Greeting {
    @prop name String

    body { Text("Hello, {name}!") }
}

view App {
    body { Greeting(name: "Alice") }
}
```

## UI Components and Modifiers

Compose layouts and style with chained modifiers:

```adam
view ProfileCard {
    @prop user User

    body {
        HStack {
            Image(user.avatar_url).size(48, 48).cornerRadius(24)
            VStack(alignment: .leading) {
                Text(user.name).font(.title)
                Text(user.email).font(.caption).color(.gray)
            }
        }
        .padding(16).background(.white).cornerRadius(12)
    }
}
```

## Event Handling

Bind state to controls and handle events:

```adam
view LoginForm {
    @state username String = ""
    @state password String = ""

    body {
        VStack(spacing: 16) {
            TextField("Username", text: username)
            SecureField("Password", text: password)
            Button("Log In") { navigate(HomeScreen()) }
                .disabled(username.is_empty())
        }
    }
}
```

## Lists and Navigation

```adam
view TodoList {
    @state items Vec[Todo] = []

    body {
        NavigationStack {
            List {
                ForEach(items) { item in
                    NavigationLink(item.title, destination: DetailScreen(item))
                }
            }
            .navigationTitle("Todos")
        }
    }
}
```

## Building for Mobile

```bash
adam build --target ios
adam build --target android
adam run --simulator iphone-16
```

## What's Next

In [Chapter 12: Platform APIs](12_platform_apis.md), you will learn how to
access file systems, networks, sensors, and platform-specific functionality.

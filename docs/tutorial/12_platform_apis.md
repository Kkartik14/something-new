# Chapter 12: Platform APIs

Adam provides a unified standard library for file I/O, networking, and device
access across platforms, with compile-time platform checks where needed.

## File I/O

```adam
import io

fn main() {
    io.write_file("output.txt", "Hello, file!")?
    let contents := io.read_file("output.txt")?
    print(contents)
}
```

Process files line by line:

```adam
fn count_lines(path String) -> Int ! io.Error {
    let file := io.open(path)?
    mut count := 0
    for line in file.lines() { count = count + 1 }
    count
}
```

## Networking

```adam
import net

fn main() {
    let resp := net.get("https://api.example.com/data")?
    print("Status: {resp.status}")

    let req := net.Request.new("POST", "https://api.example.com/users")
        .header("Content-Type", "application/json")
        .body("{\"name\": \"Alice\"}")
    net.send(req)?
}
```

## JSON

```adam
import json

struct User { name String, age Int }

fn main() {
    let text := json.encode(User { name: "Alice", age: 30 })?
    let parsed := json.decode[User](text)?
    print("Name: {parsed.name}")
}
```

## Device Sensors

```adam
import device

fn main() {
    let loc := device.location.current()?
    print("Lat: {loc.latitude}, Lon: {loc.longitude}")
}
```

## Platform-Specific Code

Use `#platform` blocks for targeted code:

```adam
fn open_browser(url String) {
    #platform(ios) { ios.UIApplication.shared.open(url) }
    #platform(android) { android.Intent.view(url).start() }
    #platform(macos, linux, windows) { os.exec("open", [url]) }
}
```

## Bridging to Native Code

```adam
#bridge(swift)
fn swift_encrypt(data [Byte], key String) -> [Byte]

#bridge(kotlin)
fn kotlin_biometric_auth() -> Bool ! Error
```

## Environment and Arguments

```adam
import os

fn main() {
    for arg in os.args() { print("arg: {arg}") }
    let home := os.env("HOME") ?? "/tmp"
}
```

## What's Next

Congratulations! You have completed the Adam tutorial. You now have a solid
foundation from basic syntax through ownership, concurrency, UI, and platform
APIs. Here are some next steps:

- Read the [Standard Library Reference](../reference/stdlib.md) for full API docs.
- Explore [Example Projects](../examples/) for real-world applications.
- Join the Adam community to share feedback and ask questions.

Happy coding with Adam!

# Chapter 10: Concurrency

Adam provides lightweight concurrency through green threads, channels, and spawn
groups. The ownership system ensures data-race freedom at compile time.

## Spawning Green Threads

Use `spawn` to run code concurrently. Green threads are cheap -- spawn thousands:

```adam
fn main() {
    spawn { print("Hello from a green thread!") }
    for i in 0..1000 {
        spawn { print("Thread {i}") }
    }
}
```

## Channels

Channels communicate between threads. Create with `chan[T](capacity)`:

```adam
fn main() {
    let ch := chan[String](1)
    spawn { ch.send("hello") }
    print(ch.recv())
}
```

Use capacity 0 for unbuffered (synchronous), or a positive number for buffered.

## Channel Iteration

Iterate until a channel is closed:

```adam
let ch := chan[Int](5)
spawn {
    for i in 0..5 { ch.send(i * i) }
    ch.close()
}
for value in ch { print("Got: {value}") }
```

## Select

Wait on multiple channel operations:

```adam
let ch1 := chan[String](1)
let ch2 := chan[String](1)
spawn { ch1.send("one") }
spawn { ch2.send("two") }

select {
    msg := ch1.recv() => print("From ch1: {msg}")
    msg := ch2.recv() => print("From ch2: {msg}")
}
```

## Timeouts with after

Use `after` inside `select` to set a deadline. Duration literals: `ms`, `s`, `m`, `h`.

```adam
select {
    msg := ch.recv() => print("Got: {msg}")
    after 2s => print("Timed out")
}
```

## Spawn Groups

Launch tasks and collect results:

```adam
let results := spawn_group {
    for url in urls {
        spawn { fetch(url) }
    }
}
for result in results {
    match result {
        ok(body) => print("Got {body.len()} bytes")
        error(e) => print("Failed: {e}")
    }
}
```

## Shared State with Mutex

When shared mutable state is needed, use `Mutex[T]`:

```adam
let counter := Mutex[Int].new(0)
spawn_group {
    for _ in 0..100 {
        spawn {
            counter.lock(fn(mut val Int) { val = val + 1 })
        }
    }
}
print("Count: {counter.get()}")  // 100
```

## What's Next

In [Chapter 11: Building a Mobile App](11_building_apps.md), you will learn how
to use Adam's UI framework to build cross-platform mobile applications.

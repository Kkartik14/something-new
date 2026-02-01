# Concurrency

Adam uses green threads scheduled by a work-stealing runtime. Thousands
of concurrent tasks run on a small number of OS threads.

## Green Threads vs OS Threads

The runtime spawns one OS worker per CPU core. Green threads are
multiplexed onto those workers and cooperatively yield at channel
operations and `select` statements. Idle workers steal from others.

## Spawn Blocks

Use `spawn` to launch a green thread. The block captures variables by value.

```adam
let name = "world"
spawn {
    print("hello, {name}")
}
```

`spawn` is non-blocking -- the parent continues immediately.

## Channels

Create a typed channel with `chan[T](capacity)`.

```adam
let ch = chan[i32](0)      // unbuffered
let buf = chan[String](10) // buffered, capacity 10
```

**Unbuffered** (capacity 0): send blocks until a receiver is ready.
**Buffered** (capacity > 0): send blocks only when the buffer is full.

### Send and Receive

```adam
ch.send(42)
let val = ch.recv()   // returns ?T; nil when closed and drained
```

See [Error Handling](error_handling.md) for working with optionals.

## Select Statement

`select` waits on multiple channel operations. When several are ready,
one is chosen at random for fairness.

```adam
select {
    msg := inbox.recv() => handle(msg)
    outbox.send(reply)  => print("sent")
    after 5.seconds     => print("timeout")
}
```

The `after` branch fires if no other branch becomes ready in time.

## Spawn Groups

Structured concurrency with fork-join semantics. Results are collected
in spawn order.

```adam
let group = spawn_group()
for url in urls {
    group.spawn { fetch(url) }
}
let results = group.wait()
```

Panics in individual tasks are caught -- `wait` returns `Result` values.

## Channel Closing

Close a channel to signal no more values. Blocked receivers wake and
get `nil`. Sending on a closed channel panics.

```adam
ch.close()
```

## Concurrent Patterns

**Fan-Out** -- distribute work across workers reading one channel:

```adam
let jobs = chan[Task](100)
for i in 0..num_workers {
    spawn { worker(jobs) }
}
```

**Fan-In** -- merge producers into one channel:

```adam
let results = chan[Result](0)
for ch in sources {
    spawn { for val in ch { results.send(val) } }
}
```

**Pipeline** -- chain stages with channels:

```adam
fn stage(input chan[i32], output chan[i32]) {
    for val in input { output.send(val * 2) }
    output.close()
}
```

## Common Mistakes

- **Forgetting to close a channel.** Receivers block forever if the
  producer never closes.
- **Sending on a closed channel.** This panics. Only the producer
  should close.
- **Deadlocking with unbuffered channels.** If sender and receiver are
  on the same green thread, an unbuffered `send` blocks forever.
- **Ignoring `spawn_group` results.** Panicked task errors are only
  visible in the `Result` returned by `wait`.
- **Using `select` with a single branch.** Equivalent to a plain
  `send`/`recv` with unnecessary overhead.

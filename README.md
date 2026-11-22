# scala-js-snabbdom

![Maven Central](https://img.shields.io/maven-central/v/io.github.buntec/scala-js-snabbdom_sjs1_2.13)

This project started as a Scala.js clone of [Snabbdom](https://github.com/snabbdom/snabbdom)
(roughly equivalent to version 3.4.0). Version 0.1.x stays close to the original,
while 0.2.x departs from it in several ways, notably by making many things immutable.
We are happy to fix bugs in the 0.1.x line within reason.

Artifacts are published to Maven Central for Scala 2.13 and Scala 3.

```scala
libraryDependencies += "io.github.buntec" %%% "scala-js-snabbdom" % "x.y.z"
```

See the test suites for usage examples.

## Projects using scala-js-snabbdom

- [tyrian](https://github.com/PurpleKingdomGames/tyrian)
- [ff4s](https://github.com/buntec/ff4s)
- Your library here...

## Contributing

Don't hesitate to open issues or pull requests!

### Benchmarks

Run `sbt benchmarks/fullLinkJS`, serve the repository with
`python3 -m http.server 8000 --bind 127.0.0.1`, and open
`http://127.0.0.1:8000/benchmarks/`. The browser console reports median patch times
for keyed reordering, a text update in a deep tree, and updates to elements with
event listeners. Each timed sample reuses prebuilt VNodes and excludes the
initial mount.

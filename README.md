# scala-js-snabbdom

![Maven Central](https://img.shields.io/maven-central/v/io.github.buntec/scala-js-snabbdom_sjs1_2.13)

This project started as a Scala.js clone of [Snabbdom](https://github.com/snabbdom/snabbdom)
(roughly equivalent to version 3.4.0). Relevant bug fixes from later Snabbdom
releases, up to 3.6.4, have since been ported. Version 0.1.x stays close to the original,
while 0.2.x departs from it in several ways, notably by making many things immutable.
We are happy to fix bugs in the 0.1.x line within reason.

Artifacts are published to Maven Central for Scala 2.13 and Scala 3.

```scala
libraryDependencies += "io.github.buntec" %%% "scala-js-snabbdom" % "0.2.1"
```

See the test suites and the [examples](examples/src/main/scala/snabbdom/examples)
for usage.

## SVG and namespaces

- `h("svg", ...)` puts the SVG namespace on the `svg` element and its descendants,
  except for the children of `foreignObject`, which are HTML again. A descendant
  that names its own `ns` keeps it.
- A new element whose vnode has no `ns` takes its namespace from the DOM element it
  is created in, using the same rule. For example, a `circle` added to an SVG
  element you mounted yourself is an SVG `circle`.
- An existing element is replaced only if the new vnode names a namespace the
  element isn't in. A vnode without `ns` keeps the element in whatever namespace it
  is. To turn an SVG element back into an HTML one, set
  `ns = Some("http://www.w3.org/1999/xhtml")`.

## Projects using scala-js-snabbdom

- [tyrian](https://github.com/PurpleKingdomGames/tyrian)
- [ff4s](https://github.com/buntec/ff4s)
- Your library here...

## Contributing

Don't hesitate to open issues or pull requests!

### Running the tests

By default the tests run in headless Chrome, which requires
[ChromeDriver](https://developer.chrome.com/docs/chromedriver). To run them in
[jsdom](https://github.com/jsdom/jsdom) instead, install it with `npm install`
and run:

```
sbt "set Global / useJSEnv := JSEnv.JSDOM" test
```

Use `JSEnv.Firefox` to run them in headless Firefox (requires geckodriver).

### Examples

Run `sbt examples/fastLinkJS`, serve the repository with
`python3 -m http.server 8000 --bind 127.0.0.1`, and open
`http://127.0.0.1:8000/examples/`.

### Benchmarks

Run `sbt benchmarks/fullLinkJS`, serve the repository as above, and open
`http://127.0.0.1:8000/benchmarks/`. The browser console reports median patch times
for keyed reordering, a text update in a deep tree, and updates to elements with
event listeners. Each timed sample reuses prebuilt VNodes and excludes the
initial mount. The page also runs two of the benchmarks against the latest
Snabbdom from a CDN for comparison.

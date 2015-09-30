# ocaml-d3

This library provides OCaml bindings for the core selection API of the
[D3.js][d3] library. It enables you to create type-safe, composable widgets
using HTML, SVG, and CSS.

[d3]: https://d3js.org

## Installation

Install the library and its depenencies via [OPAM][opam]:

[opam]: http://opam.ocaml.org/

```bash
opam install d3
```

## Development

To install development versions of the library, pin the package to the root of
your local repository:

```bash
opam pin add d3 .
```

You can install the latest changes by commiting them to the local git
repository and running:

```bash
opam upgrade d3
```

For building and running the examples during development, you will need to
reconfigure the build process:

```bash
./configure --enable-examples
make && $BROWSER examples/rectangle.html
```

And you're good to go&mdash;though [styling the TodoMVC app][css] will make it easier on the eyes.

[css]: https://github.com/tastejs/todomvc-app-css/blob/master/index.css

## Conceptual Prerequisites

In order to effectively use ocaml-d3, you should be familiar with the core
concepts of D3.js, including data joins, nested selections, and reusable
charts. The following series of blog posts by [@mbostock][] introduce these
concepts quite nicely, and independently of ocaml-d3 should be ready by anbody
that is interested developing their D3.js skills:

* [Thinking in Joins][join]
* [Nested Selections][nest]
* [Towards Reusable Charts][chart]

[join]: http://bost.ocks.org/mike/join/
[nest]: http://bost.ocks.org/mike/nest/
[chart]: http://bost.ocks.org/mike/chart/

[@mbostock]: https://twitter.com/mbostock

## Usage

ocaml-d3 is designed to be a very literal interpretation of the D3.js API. In
fact, any D3.js code that uses only the [Core Selection API][core] should be
fairly straightforward to port over to ocaml-d3. For example, here's a fragment
of code from a [D3.js Voronoi diagram example][voronoi-original] translated to
use ocaml-d3:

[core]: https://github.com/mbostock/d3/wiki/Selections
[voronoi-original]: http://bl.ocks.org/mbostock/4060366

```ocaml
type point = { x : int, y : int }

voronoi : (point list * point list) D3.t =
  nest (selectAll "path" <.> bind cells)
    [ enter <.> append "path"
    ; update
      |. fun attr "d"     (fun _ ps _ -> path ps)
      |. fun attr "class" (fun _ _  i -> "q" ++ (string_of_int (i mod 9)) ++ "-9") ]
```

Operations such as `selectAll`, `enter`, and `attr` have the same behavior as
their D3.js counterparts. The `bind` operation is equivalent to the `data()`
operator from D3.js, though it requires its argument to be a function.
Similarly, `attr` also requires a function as its second argument, which takes
the data bound to the element and the element's index in the selection. Another
difference is that ocaml-d3 replaces method chaining with the `|.` operator.
For example,

```ocaml
selectAll "path"
|. bind cells
```

is equlivalent to

```javascript
d3.selectAll("path")
  .data(cells)
```

Sequencing is another operation that's slightly different in ocaml-d3. In
Javascript, there's a common pattern where you apply a data bound to a
selection, assign it to a variable, and then apply `enter()`, update, and
`exit()` operations to the variable. In place of this pattern, you use the
`nest` operator. Its use is illustrated in the example above. Below is the
equivalent JavaScript code.
```javascript
var path = d3.selectAll('path')
    .data(function(d) { ... });

path.enter()
  .append('path');

path
    .attr('d', function(d) { ... })
    .attr('class', function(d) { ... });
```

## Rendering

Creating a selection such as `voronoi` above does not actually draw anything to
the screen. Rather, it defines a computation that the runtime knows how to draw
to the screen. To do this, you use the `run` function. Its first argument is a
selector that will be used as the root element to render within. The second
argument is the datum of type `'a` that will be bound to the selection. The
final argument is D3 selection that will be rendered.

```ocaml
(* val run : string -> 'a -> ('a, _) D3.t -> unit *)
let () = D3.run "body" voronoi [{x = 200, y = 200}; {x = 320, y = 100}]
```

## Linking

As these are simply bindings for the D3.js library, it's necessary to load
D3.js into your web page at some point or another. You may choose to link it in
when converting your project to JavaScript via js\_of\_ocaml. Alternatively,
you can include it in the page using a `<script>` tag.

# License

BSD3, see LICENSE file for its text.

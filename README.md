Ffmt is an experimental reimplementation of the OCaml format strings and Format module using standard features of
OCaml.

Format strings are thus implemented using a ppx and GADTs.

The end result correctly looks like this:
```ocaml
let stdout =
    stdout
    |> fp {%fmt|Hello %s! |} ["Earth"]
    |> fp {%fmt|%d + %g = %s|} [1;2.;"3"]
```


Another interesting features of Ffmt is that it decouples the format string interpreter, the formatting engine and
the backends. As an illustration, Ffmt comes with a string interpolation-inspired interpreter for format string.

```ocaml
let stdout =
    let earth = string "Earth" in
    let one = int 1 in
    stdout
    |> fp {%fmt|Hello %{earth}! |} ["Earth"]
    |> fp {%fmt|%{one} + %{float 2.} = %{string "3"}|} [1;2.;"3"]
```

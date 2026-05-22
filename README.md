# facade

`facade` is a library for serialization in OCaml.
You describe the shape of your types and get encoding and decoding for free.

```ocaml
type point = { x : int; y : int }

let point_shape =
  Facade.Shape.(
    record (fun x y -> { x; y })
    |> required "x" int (fun p -> p.x)
    |> required "y" int (fun p -> p.y)
    |> seal)

let point_of_json v =
  Facade.decode (module Facade_yojson) point_shape v
```

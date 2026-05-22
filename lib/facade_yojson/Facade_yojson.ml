type ext = unit
type t = Yojson.Safe.t

let of_repr : ext Facade.Repr.t -> Yojson.Safe.t =
  let rec go =
    let open Facade.Repr in
    function
    | Null -> `Null
    | Bool b -> `Bool b
    | Int i -> `Intlit (Int64.to_string i)
    | Float f -> `Float f
    | String s -> `String s
    | List xs -> `List (List.map go xs)
    | Object fields -> `Assoc (List.map (fun (k, v) -> (k, go v)) fields)
    | Ext () -> assert false
  in
  go

let to_repr : Yojson.Safe.t -> ext Facade.Repr.t =
  let rec go =
    let open Facade.Repr in
    function
    | `Null -> Null
    | `Bool b -> Bool b
    | `Int i -> Int (Int64.of_int i)
    | `Intlit s -> Int (Int64.of_string s)
    | `Float f -> Float f
    | `String s -> String s
    | `List xs -> List (List.map go xs)
    | `Assoc fields -> Object (List.map (fun (k, v) -> (k, go v)) fields)
    | `Tuple xs -> List (List.map go xs)
    | `Variant (tag, None) -> String tag
    | `Variant (tag, Some v) -> List [ String tag; go v ]
  in
  go

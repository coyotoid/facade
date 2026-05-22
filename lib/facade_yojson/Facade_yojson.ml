type ext = unit
type t = Yojson.Safe.t

let to_yojson : ext Facade.Repr.t -> Yojson.Safe.t =
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

let of_yojson : Yojson.Safe.t -> ext Facade.Repr.t =
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

let encode : ('a, ext) Facade.Shape.t -> 'a -> Yojson.Safe.t =
 fun codec x -> to_yojson (codec.enc x)

let decode : ('a, ext) Facade.Shape.t -> Yojson.Safe.t -> 'a Facade.Validate.t =
 fun codec json -> Facade.Shape.decode codec (of_yojson json)

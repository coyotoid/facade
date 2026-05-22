type ext =
  [ `Bytes of string
  | `Ext of int * string
  | `Map of (Msgpck.t * Msgpck.t) list ]

type t = Msgpck.t

let to_msgpck : ext Facade.Repr.t -> Msgpck.t =
  let rec go =
    let open Facade.Repr in
    function
    | Null -> Msgpck.Nil
    | Bool b -> Msgpck.Bool b
    | Int i -> Msgpck.Int64 i
    | Float f -> Msgpck.Float f
    | String s -> Msgpck.String s
    | List xs -> Msgpck.List (List.map go xs)
    | Object fields ->
        Msgpck.Map (List.map (fun (k, v) -> (Msgpck.String k, go v)) fields)
    | Ext (`Bytes b) -> Msgpck.Bytes b
    | Ext (`Ext (t, d)) -> Msgpck.Ext (t, d)
    | Ext (`Map pairs) -> Msgpck.Map pairs
  in
  go

let of_msgpck : Msgpck.t -> ext Facade.Repr.t =
  let is_string_key = function Msgpck.String _, _ -> true | _ -> false in
  let rec go =
    let open Facade.Repr in
    function
    | Msgpck.Nil -> Null
    | Msgpck.Bool b -> Bool b
    | Msgpck.Int i -> Int (Int64.of_int i)
    | Msgpck.Uint32 i -> Int (Int64.logand (Int64.of_int32 i) 0xFFFFFFFFL)
    | Msgpck.Int32 i -> Int (Int64.of_int32 i)
    | Msgpck.Uint64 i -> Int i
    | Msgpck.Int64 i -> Int i
    | Msgpck.Float32 bits -> Float (Int32.float_of_bits bits)
    | Msgpck.Float f -> Float f
    | Msgpck.String s -> String s
    | Msgpck.Bytes b -> Ext (`Bytes b)
    | Msgpck.Ext (t, d) -> Ext (`Ext (t, d))
    | Msgpck.List xs -> List (List.map go xs)
    | Msgpck.Map pairs when List.for_all is_string_key pairs ->
        Object
          (List.map
             (fun (k, v) ->
               match k with Msgpck.String s -> (s, go v) | _ -> assert false)
             pairs)
    | Msgpck.Map pairs -> Ext (`Map pairs)
  in
  go

let encode : ('a, ext) Facade.Shape.t -> 'a -> Msgpck.t =
 fun codec x -> to_msgpck (codec.enc x)

let decode : ('a, ext) Facade.Shape.t -> Msgpck.t -> 'a Facade.Validate.t =
 fun codec m -> Facade.Shape.decode codec (of_msgpck m)

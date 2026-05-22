type ext =
  [ `Bytes of string
  | `Msgpack_ext of int * string
  | `Msgpack_map of (Msgpck.t * Msgpck.t) list ]

type t = Msgpck.t

val encode : ('a, ext) Facade.Shape.t -> 'a -> Msgpck.t
val decode : ('a, ext) Facade.Shape.t -> Msgpck.t -> 'a Facade.Validate.t

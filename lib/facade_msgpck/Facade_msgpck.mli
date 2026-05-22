type ext =
  [ `Bytes of string
  | `Msgpack_ext of int * string
  | `Msgpack_map of (Msgpck.t * Msgpck.t) list ]

type t = Msgpck.t

val of_repr : ext Facade.Repr.t -> t
val to_repr : t -> ext Facade.Repr.t

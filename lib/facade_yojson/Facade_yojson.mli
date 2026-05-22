type ext
type t = Yojson.Safe.t

val of_repr : ext Facade.Repr.t -> t
val to_repr : t -> ext Facade.Repr.t

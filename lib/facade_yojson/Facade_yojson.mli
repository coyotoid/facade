type ext
type t = Yojson.Safe.t

val encode : ('a, ext) Facade.Shape.t -> 'a -> Yojson.Safe.t
val decode : ('a, ext) Facade.Shape.t -> Yojson.Safe.t -> 'a Facade.Validate.t

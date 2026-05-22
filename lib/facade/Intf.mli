module type FACADE = sig
  type ext
  type t

  val encode : ('a, ext) Shape.t -> 'a -> t
  val decode : ('a, ext) Shape.t -> t -> 'a Validate.t
end

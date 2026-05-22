module type BACKEND = sig
  type ext
  type t

  val of_repr : ext Repr.t -> t
  val to_repr : t -> ext Repr.t
end

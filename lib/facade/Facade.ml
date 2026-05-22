type ('a, 'ext) shape = ('a, 'ext) Shape.t
type 'ext repr = 'ext Repr.t
type 'a validate = 'a Validate.t
type error = Error.t

module type BACKEND = Backend_intf.BACKEND

let encode (type t ext) (module M : BACKEND with type t = t and type ext = ext)
    (shape : ('a, ext) shape) (x : 'a) : t =
  M.of_repr (Shape.encode shape x)

let decode (type t ext) (module M : BACKEND with type t = t and type ext = ext)
    (shape : ('a, ext) shape) (x : t) : 'a validate =
  Shape.decode shape (M.to_repr x)

module Shape = Shape
module Repr = Repr
module Validate = Validate
module Error = Error

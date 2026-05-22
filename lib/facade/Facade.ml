module Shape = Shape
module Repr = Repr
module Validate = Validate
module Error = Error

type ('a, 'ext) shape = ('a, 'ext) Shape.t
type 'ext repr = 'ext Repr.t
type 'a validate = 'a Validate.t
type error = Error.t

module type INTF = Intf.FACADE

let encode (type t ext) (module M : INTF with type t = t and type ext = ext)
    (shape : ('a, ext) shape) (x : 'a) : t =
  M.encode shape x

let decode (type t ext) (module M : INTF with type t = t and type ext = ext)
    (shape : ('a, ext) shape) (x : t) : 'a validate =
  M.decode shape x

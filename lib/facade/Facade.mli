(** Facade is a library for serialization in OCaml that lets you describe the
    shape of your types once and get encoding and decoding for free.

    See each module for in-depth documentation. *)

type ('a, 'ext) shape = ('a, 'ext) Shape.t
(** A shape for values of type ['a]. See {!Shape.t}. *)

type 'ext repr = 'ext Repr.t
(** Internal representation of a value. See {!Repr.t}. *)

type 'a validate = 'a Validate.t
(** Result of decoding. See {!Validate.t}. *)

type error = Error.t
(** Decode error with path and message. See {!Error.t}. *)

module type BACKEND = Backend_intf.BACKEND
(** Module type for a backend. *)

val encode :
  (module BACKEND with type ext = 'ext and type t = 't) ->
  ('a, 'ext) shape ->
  'a ->
  't
(** [encode (module Backend) shape x] encodes [x] to the backend format using
    [shape]. The backend must match the shape's extension type ['ext].*)

val decode :
  (module BACKEND with type ext = 'ext and type t = 't) ->
  ('a, 'ext) shape ->
  't ->
  'a validate
(** [decode (module Backend) shape data] decodes [data] from the backend format
    using [shape]. Returns {!Validate.t} containing either the decoded value or
    a list of errors. *)

module Shape = Shape
module Repr = Repr
module Validate = Validate
module Error = Error

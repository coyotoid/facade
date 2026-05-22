(** Bidirectional shape descriptions.

    A {{!t}shape} bundles an encoder and a decoder, so one value describes both
    directions of serialization. *)

module type ENUM = sig
  (** A finite type with a canonical string mapping, used by {!enum}. *)

  type t

  val all : t list
  val to_string : t -> string
end

(** {1 The [field] applicative} *)

type 'a field
(** A field-level decoder operating at some path in the {!Repr.t} tree. *)

val return : 'a -> 'a field
(** [return x] always succeeds with [x]. *)

val fail : string -> 'a field
(** [fail msg] always fails with [msg] at the current path. *)

val map : ('a -> 'b) -> 'a field -> 'b field
val bind : 'a field -> ('a -> 'b field) -> 'b field
val both : 'a field -> 'b field -> ('a * 'b) field
val ( let* ) : 'a field -> ('a -> 'b field) -> 'b field
val ( let+ ) : 'a field -> ('a -> 'b) -> 'b field
val ( and+ ) : 'a field -> 'b field -> ('a * 'b) field

module Syntax : sig
  val ( let* ) : 'a field -> ('a -> 'b field) -> 'b field
  val ( let+ ) : 'a field -> ('a -> 'b) -> 'b field
  val ( and+ ) : 'a field -> 'b field -> ('a * 'b) field
end

(** {1 Running a shape} *)

type ('a, 'ext) t
(** A shape for values of type ['a], serializable over the backend extension
    type ['ext] (see {!Facade.BACKEND}). *)

val encode : ('a, 'ext) t -> 'a -> 'ext Repr.t
(** [encode shape x] produces the {!Repr.t} for [x]. *)

val decode : ('a, 'ext) t -> 'ext Repr.t -> 'a Validate.t
(** [decode shape repr] returns [Valid x] or an [Invalid] list of errors with
    paths. *)

(** {1 Primitive shapes} *)

val null : (unit, _) t
(** Matches only the [Null] constructor *)

val bool : (bool, _) t

val int : (int, _) t
(** Native [int] over the {!Repr.Int} ([int64]) carrier. Values outside the host
    platform's [int] range are silently truncated on decode. *)

val int64 : (int64, _) t
val float : (float, _) t
val string : (string, _) t

(** {1 Combinators} *)

val option : ('a, 'ext) t -> ('a option, 'ext) t
(** Encodes [None] as [Null]. As a consequence, [option null] cannot distinguish
    [Some ()] from [None]. *)

val list : ('a, 'ext) t -> ('a list, 'ext) t

val object' : ('a, 'ext) t -> ((string * 'a) list, 'ext) t
(** A homogeneous string-keyed map. For heterogeneous records, use the
    {{!record}record builder}. *)

val pair : ('a, 'ext) t -> ('b, 'ext) t -> ('a * 'b, 'ext) t
(** Encoded as a two-element {!Repr.List}. *)

val enum : (module ENUM with type t = 'a) -> ('a, _) t
(** A string-discriminated finite type. Raises [Invalid_argument] at
    construction time if [E.to_string] is not injective over [E.all] (i.e. two
    values share a string). *)

val defer : ('a, 'ext) t Lazy.t -> ('a, 'ext) t
(** Wrap a [Lazy.t] shape so it can be embedded in other combinators before
    being forced. *)

val rec' : (('a, 'ext) t -> ('a, 'ext) t) -> ('a, 'ext) t
(** Tie a recursive knot for self-referential shapes.

    Equivalent to a [let rec ... = lazy ...] + {!defer}; use {!defer} directly
    for mutually recursive shapes. *)

val validate : ('a -> (unit, string) result) -> ('a, 'b) t -> ('a, 'b) t
(** Run an extra predicate after a successful decode. An [Error msg] becomes a
    decode error at the current path; encoding is unaffected. *)

val bimap : ('a -> 'b) -> ('b -> 'a) -> ('b, 'c) t -> ('a, 'c) t
(** [bimap f g shape] adapts a shape over ['b] into one over ['a]: [f] is
    applied on decode, [g] on encode. Useful for newtype wrappers and small
    representation changes. *)

val conv : ('a -> 'b) -> ('b -> 'a) -> ('b, 'c) t -> ('a, 'c) t
(** Alias for {!bimap}. *)

(** {1 Custom shapes and low-level object primitives} *)

val field : string -> ('a, 'ext) t -> 'ext Repr.t -> 'a field
(** [field name shape repr] decodes the value at [name] inside the object
    [repr]. Fails if the field is missing or if [repr] is not a {!Repr.Object}.
    Errors are reported at the field path. *)

val field_opt : string -> ('a, 'ext) t -> 'ext Repr.t -> 'a option field
(** Like {!val-field}, but yields [None] when the field is absent. Fails if
    [repr] is not a {!Repr.Object}. *)

val custom :
  encode:('a -> 'ext Repr.t) -> decode:('ext Repr.t -> 'a field) -> ('a, 'ext) t
(** Build a shape from an explicit encoder/decoder pair. *)

(** {1 Record builder} *)

type ('cons, 'record, 'ext) builder
(** ['cons] is the remaining constructor arrow, ['record] the final record type,
    ['ext] the backend extension. *)

val record : 'cons -> ('cons, 'record, 'ext) builder
(** Start a builder from a constructor function. *)

val required :
  string ->
  ('head, 'ext) t ->
  ('record -> 'head) ->
  ('head -> 'tail, 'record, 'ext) builder ->
  ('tail, 'record, 'ext) builder
(** Add a required field. Decoding fails with ["missing field"] if it's absent.
    Raises [Invalid_argument] if [name] has already been used by this builder.
*)

val optional :
  string ->
  ('head, 'ext) t ->
  ('record -> 'head option) ->
  ('head option -> 'tail, 'record, 'ext) builder ->
  ('tail, 'record, 'ext) builder
(** Add an optional field. Absent on decode means [None]; encoding [None] omits
    the field entirely. Raises [Invalid_argument] on duplicate field name. *)

val default :
  string ->
  ('head, 'ext) t ->
  'head ->
  ('record -> 'head) ->
  ('head -> 'tail, 'record, 'ext) builder ->
  ('tail, 'record, 'ext) builder
(** Add a field with a default. Absent on decode yields the default; encoding
    always writes the field. Raises [Invalid_argument] on duplicate field name.
*)

val seal : ('a, 'a, 'ext) builder -> ('a, 'ext) t
(** Close the builder into a shape. *)

module type ENUM = sig
  type t

  val all : t list
  val to_string : t -> string
end

type 'a decode = Error.path -> 'a Validate.t
type ('a, 'ext) t = { enc : 'a -> 'ext Repr.t; dec : 'ext Repr.t -> 'a decode }

val return : 'a -> 'a decode
val fail : string -> 'a decode
val map : ('a -> 'b) -> 'a decode -> 'b decode
val bind : 'a decode -> ('a -> 'b decode) -> 'b decode
val both : 'a decode -> 'b decode -> ('a * 'b) decode
val ( let* ) : 'a decode -> ('a -> 'b decode) -> 'b decode
val ( let+ ) : 'a decode -> ('a -> 'b) -> 'b decode
val ( and+ ) : 'a decode -> 'b decode -> ('a * 'b) decode

module Syntax : sig
  val ( let* ) : 'a decode -> ('a -> 'b decode) -> 'b decode
  val ( let+ ) : 'a decode -> ('a -> 'b) -> 'b decode
  val ( and+ ) : 'a decode -> 'b decode -> ('a * 'b) decode
end

val encode : ('a, 'ext) t -> 'a -> 'ext Repr.t
val decode : ('a, 'ext) t -> 'ext Repr.t -> 'a Validate.t

(* shapes *)
val null : (unit, _) t
val bool : (bool, _) t
val int : (int, _) t
val int64 : (int64, _) t
val float : (float, _) t
val string : (string, _) t
val option : ('a, 'ext) t -> ('a option, 'ext) t
val list : ('a, 'ext) t -> ('a list, 'ext) t
val object' : ('a, 'ext) t -> ((string * 'a) list, 'ext) t
val pair : ('a, 'ext) t -> ('b, 'ext) t -> ('a * 'b, 'ext) t
val enum : (module ENUM with type t = 'a) -> ('a, _) t
val rec' : (('a, 'b) t -> ('a, 'b) t) -> ('a, 'b) t

(* ... *)
val validate : ('a -> (unit, string) result) -> ('a, 'b) t -> ('a, 'b) t
val bimap : ('a -> 'b) -> ('b -> 'a) -> ('b, 'c) t -> ('a, 'c) t
val conv : ('a -> 'b) -> ('b -> 'a) -> ('b, 'c) t -> ('a, 'c) t

(* low-level object primitives *)
val field : string -> ('a, 'ext) t -> 'ext Repr.t -> 'a decode
val field_opt : string -> ('a, 'ext) t -> 'ext Repr.t -> 'a option decode

(* shape builder *)
type ('cons, 'record, 'ext) builder

val record : 'cons -> ('cons, 'record, 'ext) builder

val required :
  string ->
  ('head, 'ext) t ->
  ('record -> 'head) ->
  ('head -> 'cons, 'record, 'ext) builder ->
  ('cons, 'record, 'ext) builder

val optional :
  string ->
  ('head, 'ext) t ->
  ('record -> 'head option) ->
  ('head option -> 'cons, 'record, 'ext) builder ->
  ('cons, 'record, 'ext) builder

val default :
  string ->
  ('head, 'ext) t ->
  'head ->
  ('record -> 'head) ->
  ('head -> 'cons, 'record, 'ext) builder ->
  ('cons, 'record, 'ext) builder

val seal : ('a, 'a, 'ext) builder -> ('a, 'ext) t

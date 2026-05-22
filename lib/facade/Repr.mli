(** Intermediate representation for serialized data. *)

type 'ext t =
  | Null
  | Bool of bool
  | Int of int64
  | Float of float
  | String of string
  | List of 'ext t list
  | Object of (string * 'ext t) list
  | Ext of 'ext

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** [pp pp_ext fmt repr] pretty-prints [repr] using [pp_ext] to print extension
    values.*)

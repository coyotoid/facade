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

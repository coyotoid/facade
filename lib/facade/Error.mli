type segment = Field of string | Index of int

val equal_segment : segment -> segment -> bool
val pp_segment : Format.formatter -> segment -> unit

type path = segment list

val equal_path : path -> path -> bool
val pp_path : Format.formatter -> path -> unit

type t = { path : path; message : string }

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit

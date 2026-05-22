(** Result type for decoding with error accumulation. *)

type 'a t =
  | Valid of 'a  (** [Valid a] represents a decoded value [a]. *)
  | Invalid of Error.t list
      (** [Invalid es] contains all errors encountered during decoding. *)

val pure : 'a -> 'a t
(** [pure x] is [Valid x]. Alias for {!valid}. *)

val valid : 'a -> 'a t
(** [valid x] always succeeds with [x]. *)

val error : Error.t -> _ t
(** [error e] always fails with the single error [e]. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f v] applies [f] to the value if [v] is [Valid], preserves errors if
    [Invalid]. *)

val bind : 'a t -> ('a -> 'b t) -> 'b t
(** Monadic bind. *)

val both : 'a t -> 'b t -> ('a * 'b) t
(** [both a b] combines two results. Returns [Valid (a, b)] if both succeed,
    otherwise collects errors from both. *)

module Syntax : sig
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end

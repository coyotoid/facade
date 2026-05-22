type 'a t = Valid of 'a | Invalid of Error.t list

let pure x = Valid x
let valid x = Valid x
let error err = Invalid [ err ]
let map f = function Valid x -> Valid (f x) | Invalid e -> Invalid e
let bind m f = match m with Valid x -> f x | Invalid e -> Invalid e

let both a b =
  match (a, b) with
  | Valid x, Valid y -> Valid (x, y)
  | Invalid e, Valid _ -> Invalid e
  | Valid _, Invalid e -> Invalid e
  | Invalid e1, Invalid e2 -> Invalid (e1 @ e2)

module Syntax = struct
  let ( let* ) = bind
  let ( let+ ) m f = map f m
  let ( and+ ) = both
end

type segment = Field of string | Index of int

let equal_segment a b =
  match (a, b) with
  | Field a, Field b -> String.equal a b
  | Index a, Index b -> Int.equal a b
  | _ -> false

let pp_segment fmt = function
  | Field s -> Format.fprintf fmt ".%s" s
  | Index i -> Format.fprintf fmt "[%d]" i

type path = segment list

let equal_path = List.equal equal_segment
let pp_path = Format.pp_print_list ~pp_sep:(fun _ () -> ()) pp_segment

type t = { path : path; message : string }

let equal a b = equal_path a.path b.path && String.equal a.message b.message
let pp fmt { path; message } = Format.fprintf fmt "%a: %s" pp_path path message

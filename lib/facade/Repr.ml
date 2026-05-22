type 'ext t =
  | Null
  | Bool of bool
  | Int of int64
  | Float of float
  | String of string
  | List of 'ext t list
  | Object of (string * 'ext t) list
  | Ext of 'ext

let pp_comma_sep fmt () = Format.fprintf fmt ",@;"

let rec pp pp_ext fmt = function
  | Null -> Format.pp_print_string fmt "null"
  | Bool b -> Format.pp_print_bool fmt b
  | Int i -> Format.fprintf fmt "%Ld" i
  | Float f -> Format.fprintf fmt "%g" f
  | String s -> Format.fprintf fmt "%S" s
  | List [] -> Format.pp_print_string fmt "[]"
  | List xs ->
      Format.fprintf fmt "[@;<0 2>@[<v>%a@]@;<0 0>]"
        (Format.pp_print_list ~pp_sep:pp_comma_sep (pp pp_ext))
        xs
  | Object [] -> Format.pp_print_string fmt "{}"
  | Object fields ->
      Format.fprintf fmt "{@;<0 2>@[<v>%a@]@;<0 0>}"
        (Format.pp_print_list ~pp_sep:pp_comma_sep (fun fmt (k, v) ->
             Format.fprintf fmt "%S: %a" k (pp pp_ext) v))
        fields
  | Ext e -> pp_ext fmt e

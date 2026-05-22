type 'a decode = Error.path -> 'a Validate.t
type ('a, 'ext) t = { enc : 'a -> 'ext Repr.t; dec : 'ext Repr.t -> 'a decode }

let return x = fun _ -> Validate.pure x
let fail message = fun path -> Validate.error { Error.path; message }
let map f m = fun path -> Validate.map f (m path)

let bind m f =
 fun path ->
  match m path with
  | Validate.Valid x -> f x path
  | Validate.Invalid e -> Validate.Invalid e

let both a b =
 fun path ->
  match (a path, b path) with
  | Validate.Valid x, Validate.Valid y -> Validate.Valid (x, y)
  | Validate.Invalid es, Validate.Valid _ -> Validate.Invalid es
  | Validate.Valid _, Validate.Invalid es -> Validate.Invalid es
  | Validate.Invalid es1, Validate.Invalid es2 -> Validate.Invalid (es1 @ es2)

let ( let* ) = bind
let ( let+ ) m f = map f m
let ( and+ ) = both

module Syntax = struct
  let ( let* ) = ( let* )
  let ( let+ ) = ( let+ )
  let ( and+ ) = ( and+ )
end

let at segment m = fun path -> m (path @ [ segment ])
let encode codec v = codec.enc v
let decode codec repr = codec.dec repr []

let null =
  {
    enc = (fun () -> Null);
    dec = (function Null -> return () | _ -> fail "expected null");
  }

let bool =
  {
    enc = (fun b -> Bool b);
    dec = (function Bool b -> return b | _ -> fail "expected boolean");
  }

let int =
  {
    enc = (fun i -> Int (Int64.of_int i));
    dec =
      (function
      | Int i -> return (Int64.to_int i)
      | _ -> fail "expected integer");
  }

let int64 =
  {
    enc = (fun i -> Int i);
    dec = (function Int i -> return i | _ -> fail "expected 64-bit integer");
  }

let float =
  {
    enc = (fun f -> Float f);
    dec = (function Float f -> return f | _ -> fail "expected float");
  }

let string =
  {
    enc = (fun s -> String s);
    dec = (function String s -> return s | _ -> fail "expected string");
  }

let option codec =
  {
    enc = (function None -> Null | Some x -> codec.enc x);
    dec =
      (function
      | Null -> return None
      | repr ->
          let* x = codec.dec repr in
          return @@ Some x);
  }

let collect_results ms path =
  List.fold_left
    (fun acc m ->
      match (acc, m path) with
      | Validate.Valid xs, Validate.Valid x -> Validate.Valid (x :: xs)
      | Validate.Invalid es, Validate.Valid _ -> Validate.Invalid es
      | Validate.Valid _, Validate.Invalid es -> Validate.Invalid es
      | Validate.Invalid es1, Validate.Invalid es2 ->
          Validate.Invalid (es1 @ es2))
    (Validate.Valid []) ms
  |> Validate.map List.rev

let list codec =
  {
    enc = (fun xs -> List (List.map codec.enc xs));
    dec =
      (function
      | List xs ->
          List.mapi (fun i x -> at (Error.Index i) (codec.dec x)) xs
          |> collect_results
      | _ -> fail "expected list");
  }

let object' codec =
  {
    enc =
      (fun pairs -> Object (List.map (fun (k, v) -> (k, codec.enc v)) pairs));
    dec =
      (function
      | Repr.Object fields ->
          List.map
            (fun (k, v) ->
              let+ x = at (Error.Field k) (codec.dec v) in
              (k, x))
            fields
          |> collect_results
      | _ -> fail "expected object");
  }

let pair ca cb =
  {
    enc = (fun (a, b) -> List [ ca.enc a; cb.enc b ]);
    dec =
      (function
      | Repr.List [ va; vb ] ->
          let+ a = at (Error.Index 0) (ca.dec va)
          and+ b = at (Error.Index 1) (cb.dec vb) in
          (a, b)
      | Repr.List _ -> fail "expected list of length 2"
      | _ -> fail "expected list");
  }

let validate f codec =
  {
    enc = codec.enc;
    dec =
      (fun repr ->
        let* x = codec.dec repr in
        fun path ->
          match f x with
          | Ok () -> Validate.valid x
          | Error msg -> Validate.error { Error.path; message = msg });
  }

let bimap ef df codec =
  {
    enc = (fun x -> codec.enc (ef x));
    dec =
      (fun repr ->
        let+ x = codec.dec repr in
        df x);
  }

let field_from_fields name codec fields path =
  match List.assoc_opt name fields with
  | None ->
      Validate.error
        {
          Error.path = path @ [ Error.Field name ];
          message = "missing field " ^ name;
        }
  | Some v -> at (Error.Field name) (codec.dec v) path

let field name codec repr path =
  match repr with
  | Repr.Object fields -> field_from_fields name codec fields path
  | _ -> Validate.error { Error.path; message = "expected object" }

let field_opt_from_fields name codec fields =
  match List.assoc_opt name fields with
  | None -> return None
  | Some v -> at (Error.Field name) ((option codec).dec v)

let field_opt name codec repr =
  match repr with
  | Repr.Object fields -> field_opt_from_fields name codec fields
  | _ -> fail "expected object"

let enum pairs =
  let by_string = List.map (fun (a, s) -> (s, a)) pairs in
  let valid = String.concat ", " (List.map snd pairs) in
  {
    enc =
      (fun v ->
        match List.find_opt (fun (a, _) -> a = v) pairs with
        | Some (_, s) -> Repr.String s
        | None -> assert false);
    dec =
      (function
      | Repr.String s -> (
          match List.assoc_opt s by_string with
          | Some a -> return a
          | None -> fail ("expected one of: " ^ valid))
      | _ -> fail ("expected one of: " ^ valid));
  }

let rec' f =
  let enc = ref (fun _ -> assert false) in
  let dec = ref (fun _ -> assert false) in
  let proxy = { enc = (fun x -> !enc x); dec = (fun r -> !dec r) } in
  let codec = f proxy in
  enc := codec.enc;
  dec := codec.dec;
  codec

type ('cons, 'record, 'ext) builder = {
  dec_fields : (string * 'ext Repr.t) list -> 'cons decode;
  enc_fields : 'record -> (string * 'ext Repr.t) list;
  names : string list;
}

let record cons =
  {
    dec_fields = (fun _ -> return cons);
    enc_fields = (fun _ -> []);
    names = [];
  }

let add_field_name name b =
  if List.mem name b.names then invalid_arg ("duplicate record field " ^ name);
  name :: b.names

let required name codec get b =
  {
    dec_fields =
      (fun fields ->
        let+ f = b.dec_fields fields
        and+ v = field_from_fields name codec fields in
        f v);
    enc_fields =
      (fun record -> (name, codec.enc (get record)) :: b.enc_fields record);
    names = add_field_name name b;
  }

let optional name codec get b =
  {
    dec_fields =
      (fun fields ->
        let+ f = b.dec_fields fields
        and+ v = field_opt_from_fields name codec fields in
        f v);
    enc_fields =
      (fun record ->
        (name, (option codec).enc (get record)) :: b.enc_fields record);
    names = add_field_name name b;
  }

let default name codec default get b =
  {
    dec_fields =
      (fun fields ->
        let+ f = b.dec_fields fields
        and+ v = field_opt_from_fields name codec fields in
        f (Option.value ~default v));
    enc_fields =
      (fun record -> (name, codec.enc (get record)) :: b.enc_fields record);
    names = add_field_name name b;
  }

let seal b =
  {
    enc = (fun record -> Repr.Object (List.rev (b.enc_fields record)));
    dec =
      (function
      | Repr.Object fields -> b.dec_fields fields
      | _ -> fail "expected object");
  }

module type ENUM = sig
  type t

  val all : t list
  val to_string : t -> string
end

type 'a field = Error.path -> 'a Validate.t
type ('a, 'ext) t = { enc : 'a -> 'ext Repr.t; dec : 'ext Repr.t -> 'a field }

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
let encode shape v = shape.enc v
let decode shape repr = shape.dec repr []

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

let option shape =
  {
    enc = (function None -> Null | Some x -> shape.enc x);
    dec =
      (function
      | Null -> return None
      | repr ->
          let* x = shape.dec repr in
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

let list shape =
  {
    enc = (fun xs -> List (List.map shape.enc xs));
    dec =
      (function
      | List xs ->
          List.mapi (fun i x -> at (Error.Index i) (shape.dec x)) xs
          |> collect_results
      | _ -> fail "expected list");
  }

let object' shape =
  {
    enc =
      (fun pairs -> Object (List.map (fun (k, v) -> (k, shape.enc v)) pairs));
    dec =
      (function
      | Repr.Object fields ->
          List.map
            (fun (k, v) ->
              let+ x = at (Error.Field k) (shape.dec v) in
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

let validate f shape =
  {
    enc = shape.enc;
    dec =
      (fun repr ->
        let* x = shape.dec repr in
        fun path ->
          match f x with
          | Ok () -> Validate.valid x
          | Error msg -> Validate.error { Error.path; message = msg });
  }

let bimap ef df shape =
  {
    enc = (fun x -> shape.enc (ef x));
    dec =
      (fun repr ->
        let+ x = shape.dec repr in
        df x);
  }

let conv = bimap

let field_from_fields name shape fields path =
  match List.assoc_opt name fields with
  | None ->
      Validate.error
        {
          Error.path = path @ [ Error.Field name ];
          message = "missing field " ^ name;
        }
  | Some v -> at (Error.Field name) (shape.dec v) path

let field name shape repr path =
  match repr with
  | Repr.Object fields -> field_from_fields name shape fields path
  | _ -> Validate.error { Error.path; message = "expected object" }

let field_opt_from_fields name shape fields =
  match List.assoc_opt name fields with
  | None -> return None
  | Some v -> at (Error.Field name) ((option shape).dec v)

let field_opt name shape repr =
  match repr with
  | Repr.Object fields -> field_opt_from_fields name shape fields
  | _ -> fail "expected object"

let enum (type a) (module E : ENUM with type t = a) : (a, _) t =
  let pairs = List.map (fun v -> (v, E.to_string v)) E.all in
  let strings = List.map snd pairs in
  let rec check_unique = function
    | a :: b :: _ when a = b ->
        invalid_arg ("Shape.enum': duplicate string " ^ a)
    | _ :: rest -> check_unique rest
    | [] -> ()
  in
  check_unique (List.sort compare strings);
  let by_string = List.map (fun (v, s) -> (s, v)) pairs in
  let valid = "expected one of: " ^ String.concat ", " strings in
  {
    enc = (fun v -> Repr.String (E.to_string v));
    dec =
      (function
      | Repr.String s -> (
          match List.assoc_opt s by_string with
          | Some v -> return v
          | None -> fail valid)
      | _ -> fail valid);
  }

let defer lz =
  {
    enc = (fun x -> (Lazy.force lz).enc x);
    dec = (fun r -> (Lazy.force lz).dec r);
  }

let rec' f =
  let rec lz = lazy (f (defer lz)) in
  Lazy.force lz

let custom ~encode ~decode = { enc = encode; dec = decode }

type ('cons, 'record, 'ext) builder = {
  dec_fields : (string * 'ext Repr.t) list -> 'cons field;
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

let required name shape get b =
  {
    dec_fields =
      (fun fields ->
        let+ f = b.dec_fields fields
        and+ v = field_from_fields name shape fields in
        f v);
    enc_fields =
      (fun record -> (name, shape.enc (get record)) :: b.enc_fields record);
    names = add_field_name name b;
  }

let optional name shape get b =
  {
    dec_fields =
      (fun fields ->
        let+ f = b.dec_fields fields
        and+ v = field_opt_from_fields name shape fields in
        f v);
    enc_fields =
      (fun record ->
        match get record with
        | None -> b.enc_fields record
        | Some v -> (name, shape.enc v) :: b.enc_fields record);
    names = add_field_name name b;
  }

let default name shape default get b =
  {
    dec_fields =
      (fun fields ->
        let+ f = b.dec_fields fields
        and+ v = field_opt_from_fields name shape fields in
        f (Option.value ~default v));
    enc_fields =
      (fun record -> (name, shape.enc (get record)) :: b.enc_fields record);
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

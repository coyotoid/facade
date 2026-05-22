open OUnit2
open Facade

let roundtrip (type t ext) (module M : INTF with type t = t and type ext = ext)
    shape v =
  match Facade.decode (module M) shape (Facade.encode (module M) shape v) with
  | Validate.Valid v' -> v'
  | Validate.Invalid es ->
      assert_failure
        (Format.asprintf "decode failed: %a" (Format.pp_print_list Error.pp) es)

let expect_one_error_message (type t ext)
    (module M : INTF with type t = t and type ext = ext) shape input expected =
  match Facade.decode (module M) shape input with
  | Validate.Invalid [ { Error.message; _ } ] ->
      assert_equal ~printer:Fun.id expected message
  | Validate.Invalid _ -> assert_failure "expected exactly one error"
  | Validate.Valid _ -> assert_failure "expected decode to fail"

(* unfortunately if you want to write truly agnostic shapes you need to write
   them like this *)
let pair_record () =
  Shape.(
    record (fun x y -> (x, y))
    |> required "x" int (fun (x, _) -> x)
    |> required "y" string (fun (_, y) -> y)
    |> seal)

let test_int_yojson _ =
  assert_equal ~printer:string_of_int 42
    (roundtrip (module Facade_yojson) Shape.int 42)

let test_int_msgpck _ =
  assert_equal ~printer:string_of_int 42
    (roundtrip (module Facade_msgpck) Shape.int 42)

let test_string_yojson _ =
  assert_equal ~printer:Fun.id "hi"
    (roundtrip (module Facade_yojson) Shape.string "hi")

let test_string_msgpck _ =
  assert_equal ~printer:Fun.id "hi"
    (roundtrip (module Facade_msgpck) Shape.string "hi")

let test_record_yojson _ =
  let x, y = roundtrip (module Facade_yojson) (pair_record ()) (1, "a") in
  assert_equal ~printer:string_of_int 1 x;
  assert_equal ~printer:Fun.id "a" y

let test_record_msgpck _ =
  let x, y = roundtrip (module Facade_msgpck) (pair_record ()) (1, "a") in
  assert_equal ~printer:string_of_int 1 x;
  assert_equal ~printer:Fun.id "a" y

let test_list_yojson _ =
  let shape = Shape.(list int) in
  assert_equal [ 1; 2; 3 ] (roundtrip (module Facade_yojson) shape [ 1; 2; 3 ])

let test_list_msgpck _ =
  let shape = Shape.(list int) in
  assert_equal [ 1; 2; 3 ] (roundtrip (module Facade_msgpck) shape [ 1; 2; 3 ])

let test_encoded_type_yojson _ =
  let j : Yojson.Safe.t = Facade.encode (module Facade_yojson) Shape.int 7 in
  match j with
  | `Intlit _ | `Int _ -> ()
  | _ -> assert_failure "expected an integer yojson value"

let test_encoded_type_msgpck _ =
  let m : Msgpck.t = Facade.encode (module Facade_msgpck) Shape.int 7 in
  match m with
  | Msgpck.Int _ | Msgpck.Int32 _ | Msgpck.Int64 _ | Msgpck.Uint32 _
  | Msgpck.Uint64 _ ->
      ()
  | _ -> assert_failure "expected an integer msgpck value"

let test_missing_field_yojson _ =
  let shape = Shape.(record Fun.id |> required "n" int Fun.id |> seal) in
  expect_one_error_message
    (module Facade_yojson)
    shape (`Assoc []) "missing field n"

let test_missing_field_msgpck _ =
  let shape = Shape.(record Fun.id |> required "n" int Fun.id |> seal) in
  expect_one_error_message
    (module Facade_msgpck)
    shape (Msgpck.Map []) "missing field n"

let test_optional_record_rejects_non_object_yojson _ =
  let shape = Shape.(record Fun.id |> optional "n" int Fun.id |> seal) in
  expect_one_error_message
    (module Facade_yojson)
    shape (`String "oops") "expected object"

let test_optional_record_rejects_non_object_msgpck _ =
  let shape = Shape.(record Fun.id |> optional "n" int Fun.id |> seal) in
  expect_one_error_message
    (module Facade_msgpck)
    shape (Msgpck.String "oops") "expected object"

let test_default_record_rejects_non_object_yojson _ =
  let shape = Shape.(record Fun.id |> default "n" int 0 Fun.id |> seal) in
  expect_one_error_message
    (module Facade_yojson)
    shape (`String "oops") "expected object"

let test_default_record_rejects_non_object_msgpck _ =
  let shape = Shape.(record Fun.id |> default "n" int 0 Fun.id |> seal) in
  expect_one_error_message
    (module Facade_msgpck)
    shape (Msgpck.String "oops") "expected object"

let test_duplicate_required_field_rejected _ =
  assert_raises (Invalid_argument "duplicate record field n") (fun () ->
      ignore
        Shape.(
          record (fun n m -> (n, m))
          |> required "n" int fst |> required "n" int snd |> seal))

let test_duplicate_mixed_field_rejected _ =
  assert_raises (Invalid_argument "duplicate record field n") (fun () ->
      ignore
        Shape.(
          record (fun n m -> (n, m))
          |> optional "n" int fst |> default "n" int 0 snd |> seal))

let test_wrong_type_yojson _ =
  expect_one_error_message
    (module Facade_yojson)
    Shape.int (`String "oops") "expected integer"

let test_wrong_type_msgpck _ =
  expect_one_error_message
    (module Facade_msgpck)
    Shape.int (Msgpck.String "oops") "expected integer"

let () =
  run_test_tt_main
    ("facade-intf"
    >::: [
           "roundtrip via (module INTF)"
           >::: [
                  "int / yojson" >:: test_int_yojson;
                  "int / msgpck" >:: test_int_msgpck;
                  "string / yojson" >:: test_string_yojson;
                  "string / msgpck" >:: test_string_msgpck;
                  "list / yojson" >:: test_list_yojson;
                  "list / msgpck" >:: test_list_msgpck;
                  "record / yojson" >:: test_record_yojson;
                  "record / msgpck" >:: test_record_msgpck;
                ];
           "encoded type matches backend"
           >::: [
                  "yojson" >:: test_encoded_type_yojson;
                  "msgpck" >:: test_encoded_type_msgpck;
                ];
           "errors propagate through Facade.decode"
           >::: [
                  "missing field / yojson" >:: test_missing_field_yojson;
                  "missing field / msgpck" >:: test_missing_field_msgpck;
                  "optional record rejects non-object / yojson"
                  >:: test_optional_record_rejects_non_object_yojson;
                  "optional record rejects non-object / msgpck"
                  >:: test_optional_record_rejects_non_object_msgpck;
                  "default record rejects non-object / yojson"
                  >:: test_default_record_rejects_non_object_yojson;
                  "default record rejects non-object / msgpck"
                  >:: test_default_record_rejects_non_object_msgpck;
                  "duplicate required field rejected"
                  >:: test_duplicate_required_field_rejected;
                  "duplicate mixed field rejected"
                  >:: test_duplicate_mixed_field_rejected;
                  "wrong type / yojson" >:: test_wrong_type_yojson;
                  "wrong type / msgpck" >:: test_wrong_type_msgpck;
                ];
         ])

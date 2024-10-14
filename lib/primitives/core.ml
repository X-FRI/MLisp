(****************************************************************************)
(* This Source Code Form is subject to the terms of the                     *)
(* Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed *)
(* with this file, You can obtain one at http://mozilla.org/MPL/2.0/.       *)
(****************************************************************************)

open Mlisp_object
open Mlisp_error

let rec list = function [] -> Object.Nil | car :: cdr -> Object.Pair (car, list cdr)

let pair = function
  | [ a; b ] ->
      Object.Pair (a, b)
  | _ ->
      raise (Errors.Parse_error_exn (Errors.Type_error "(pair a b)"))
;;

let car = function
  | [ Object.Pair (car, _) ] ->
      car
  | _ ->
      raise (Errors.Parse_error_exn (Errors.Type_error "(car non-nil-pair)"))
;;

let cdr = function
  | [ Object.Pair (_, cdr) ] ->
      cdr
  | _ ->
      raise (Errors.Parse_error_exn (Errors.Type_error "(cdr non-nil-pair)"))
;;

let atomp = function
  | [ Object.Pair (_, _) ] ->
      Object.Boolean false
  | [ _ ] ->
      Object.Boolean true
  | _ ->
      raise (Errors.Parse_error_exn (Errors.Type_error "(atom? something)"))
;;

let eq = function
  | [ a; b ] ->
      Object.Boolean (a = b)
  | _ ->
      raise (Errors.Parse_error_exn (Errors.Type_error "(eq a b)"))
;;

let symp = function
  | [ Object.Symbol _ ] ->
      Object.Boolean true
  | [ _ ] ->
      Object.Boolean false
  | _ ->
      raise (Errors.Parse_error_exn (Errors.Type_error "(sym? single-arg)"))
;;

let getchar = function
  | [] -> (
      try Object.Fixnum (int_of_char @@ input_char stdin) with
      | End_of_file ->
          Object.Fixnum (-1))
  | _ ->
      raise (Errors.Parse_error_exn (Errors.Type_error "(getchar)"))
;;

let print = function
  | [ v ] ->
      let () = print_string @@ Object.string_object v in
          Object.Symbol "ok"
  | _ ->
      raise (Errors.Parse_error_exn (Errors.Type_error "(print object)"))
;;

let int_to_char = function
  | [ Object.Fixnum i ] ->
      Object.Symbol (Object.string_of_char @@ char_of_int i)
  | _ ->
      raise (Errors.Parse_error_exn (Errors.Type_error "(int_to_char int)"))
;;

let cat = function
  | [ Object.Symbol a; Object.Symbol b ] ->
      Object.Symbol (a ^ b)
  | _ ->
      raise (Errors.Parse_error_exn (Errors.Type_error "(cat sym sym)"))
;;

let record_get = function
  | [ Object.Record (_, fields); Object.Symbol get ] ->
      List.assoc get fields
  | _ ->
      raise (Errors.Parse_error_exn (Errors.Type_error "(record field-name)"))
;;

let record_create = function
  | [ Object.Symbol record_name; fields ] ->
      let rec record_fields fields record =
        match fields with
        | Object.Pair (field_name, field_value) -> (
            match field_name, field_value with
            | (Object.Pair _ as field_1), (Object.Pair _ as field_2) ->
                record_fields field_1 record @ record_fields field_2 record
            | Object.Pair (Object.Symbol field_name, field_value), Nil
            | Object.Symbol field_name, field_value ->
                (field_name, field_value) :: record
            | _ ->
                raise
                  (Errors.Syntax_error_exn
                     (Errors.Record_field_name_must_be_a_symbol record_name)))
        | _ ->
            failwith "record fields must be a list of pairs"
    in
          Object.Record (record_name, record_fields fields [])
  | _ ->
      raise (Errors.Parse_error_exn (Errors.Type_error "(record field-name)"))
;;

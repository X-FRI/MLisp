(****************************************************************************)
(* This Source Code Form is subject to the terms of the                     *)
(* Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed *)
(* with this file, You can obtain one at http://mozilla.org/MPL/2.0/.       *)
(****************************************************************************)

open Mlisp_object
open Mlisp_error
open Core

let rec assert_unique : string list -> unit = function
  | [] ->
      ()
  | x :: xs ->
      if List.mem xs x ~equal:String.equal then
        raise (Errors.Parse_error_exn (Unique_error x))
      else
        assert_unique xs
;;

let assert_unique_args : Object.lobject -> string list =
  fun args ->
  let names =
    List.map
      ~f:(function
        | Object.Symbol s ->
            s
        | _ ->
            raise (Errors.Parse_error_exn (Type_error "(declare-expr symbol-name (formals) body)")))
      (Object.pair_to_list args)
in
  let () = assert_unique names in
      names
;;

let let_kinds : (string * Object.let_kind) list = [ "%=", Object.LET; "%==", Object.LETSTAR; "=%=", Object.LETREC ]
let valid_let : string -> bool = fun s -> List.Assoc.mem ~equal:(fun k s -> String.equal k s) let_kinds s
let to_kind : string -> Object.let_kind = fun s -> List.Assoc.find_exn ~equal:(fun k s -> String.equal k s) let_kinds s

let rec build_ast : Object.lobject -> Object.expr =
  fun sexpr ->
  match sexpr with
  | Object.Primitive _ | Object.Closure _ ->
      raise Errors.This_can't_happen_exn
  | Object.Fixnum _ | Object.Boolean _ | Object.Quote _ | Object.String _ | Object.Record _ | Object.Nil ->
      literal_expr sexpr
  | Object.Symbol s ->
      symbol_expr s
  | Object.Pair _ when Object.is_list sexpr -> (
      match Object.pair_to_list sexpr with
      | [ Object.Symbol "?"; cond; if_true; if_false ] ->
          if_expr cond if_true if_false
      | Object.Symbol "??" :: conditions ->
          cond_to_if conditions
      | [ Object.Symbol "&&"; cond_x; cond_y ] ->
          and_expr cond_x cond_y
      | [ Object.Symbol "||"; cond_x; cond_y ] ->
          or_expr cond_x cond_y
      | [ Object.Symbol "`"; expr ] ->
          quote_expr expr
      | [ Object.Symbol ":="; Object.Symbol name; expr ] ->
          setq_expr name expr
      | [ Object.Symbol "=>"; args; body ] when Object.is_list args ->
          lambda_expr args body
      | [ Object.Symbol ">>"; fn_expr; args ] ->
          apply_expr fn_expr args
      | [ Object.Symbol "|="; Object.Symbol fn_name; args; body ] ->
          defun_expr fn_name args body
      | [ Object.Symbol s; bindings; expr ] when Object.is_list bindings && valid_let s ->
          let_expr s bindings expr
      | fn_expr :: args ->
          call_expr fn_expr args
      | [] ->
          raise (Errors.Parse_error_exn Poorly_formed_expression))
  | Pair _ ->
      Object.Literal sexpr

and literal_expr : Object.lobject -> Object.expr = fun sexpr -> Object.Literal sexpr
and symbol_expr : string -> Object.expr = fun s -> Object.Var s

and and_expr : Object.lobject -> Object.lobject -> Object.expr =
  fun cond_x cond_y -> Object.And (build_ast cond_x, build_ast cond_y)

and or_expr cond_x cond_y = Object.Or (build_ast cond_x, build_ast cond_y)
and quote_expr expr = Object.Literal (Quote expr)
and setq_expr name expr = Object.Defexpr (Object.Setq (name, build_ast expr))
and if_expr cond if_true if_false = If (build_ast cond, build_ast if_true, build_ast if_false)
and lambda_expr args body = Lambda ("lambda", assert_unique_args args, build_ast body)

and defun_expr fn_name args body =
  let lam = Object.Lambda (fn_name, assert_unique_args args, build_ast body) in
      Object.Defexpr (Object.Setq (fn_name, Let (Object.LETREC, [ fn_name, lam ], Object.Var fn_name)))

and apply_expr fn_expr args = Apply (build_ast fn_expr, build_ast args)

and let_expr s bindings expr =
  let make_binding = function
    | Object.Pair (Object.Symbol n, Pair (expr, Object.Nil)) ->
        n, build_ast expr
    | _ ->
        raise (Errors.Parse_error_exn (Errors.Type_error "(let bindings expr)"))
in
  let bindings = List.map ~f:make_binding (Object.pair_to_list bindings) in
  let () = assert_unique (List.map ~f:fst bindings) in
      Object.Let (to_kind s, bindings, build_ast expr)

and call_expr fn_expr args = Call (build_ast fn_expr, List.map ~f:build_ast args)

and cond_to_if = function
  | [] ->
      Object.Literal (Object.Symbol "error")
  | Object.Pair (cond, Object.Pair (res, Object.Nil)) :: condpairs ->
      If (build_ast cond, build_ast res, cond_to_if condpairs)
  | _ ->
      raise (Errors.Parse_error_exn (Errors.Type_error "(cond conditions)"))
;;

let rec string_expr =
  let spacesep_exp es = Mlisp_utils.String.spacesep (List.map ~f:string_expr es) in
  let string_of_binding (n, e) = [%string "(%{n} %{string_expr e})"] in
      function
      | Object.Literal e ->
          Object.string_object e
      | Object.Var n ->
          n
      | Object.If (c, t, f) ->
          [%string "(if %{string_expr c} %{string_expr t} %{string_expr f})"]
      | Object.And (c0, c1) ->
          [%string "(and %{string_expr c0} %{string_expr c1})"]
      | Object.Or (c0, c1) ->
          [%string "(or %{string_expr c0} %{string_expr c1})"]
      | Object.Apply (f, e) ->
          [%string "(apply %{string_expr f} %{string_expr e})"]
      | Object.Call (f, es) ->
          if List.length es = 0 then
            [%string "(%{string_expr f}%{spacesep_exp es})"]
          else
            [%string "(%{string_expr f} %{spacesep_exp es})"]
      | Object.Lambda (_, args, body) ->
          [%string "(lambda (%{Mlisp_utils.String.spacesep args}) %{string_expr body})"]
      | Object.Defexpr (Object.Setq (n, e)) ->
          [%string "(:= %{n} %{string_expr e})"]
      | Object.Defexpr (Object.Defun (n, ns, e)) ->
          "(defun " ^ n ^ "(" ^ Mlisp_utils.String.spacesep ns ^ ") " ^ string_expr e ^ ")"
      | Object.Defexpr (Object.Expr e) ->
          string_expr e
      | Object.Let (kind, bs, e) ->
          let str = match kind with LET -> "let" | LETSTAR -> "let*" | LETREC -> "letrec" in
          let bindings = Mlisp_utils.String.spacesep (List.map ~f:string_of_binding bs) in
              "(" ^ str ^ " (" ^ bindings ^ ") " ^ string_expr e ^ ")"
;;

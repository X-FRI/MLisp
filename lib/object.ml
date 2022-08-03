(****************************************************************************)
(* MLisp                                                                    *)
(* Copyright (C) 2022 Muqiu Han                                             *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU Affero General Public License as published *)
(* by the Free Software Foundation, either version 3 of the License, or     *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU Affero General Public License for more details.                      *)
(*                                                                          *)
(* You should have received a copy of the GNU Affero General Public License *)
(* along with this program.  If not, see <https://www.gnu.org/licenses/>.   *)
(****************************************************************************)

open Types.Object

let rec is_list = function
  | Nil -> true
  | Pair (_, b) -> is_list b
  | _ -> false
;;

let rec print_sexpr sexpr =
  match sexpr with
  | Fixnum v -> print_int v
  | Boolean b -> print_string (if b then "#t" else "#f")
  | Symbol s -> print_string s
  | Nil -> print_string "nil"
  | Pair (_, _) ->
    print_string "(";
    if is_list sexpr then print_list sexpr else print_pair sexpr;
    print_string ")"
  | _ -> failwith "print_sexpr"

and print_list lst =
  match lst with
  | Pair (a, Nil) -> print_sexpr a
  | Pair (a, b) ->
    print_sexpr a;
    print_string " ";
    print_list b
  | _ -> raise This_can't_happen_exn

and print_pair pair =
  match pair with
  | Pair (a, b) ->
    print_sexpr a;
    print_string " . ";
    print_sexpr b
  | _ -> raise This_can't_happen_exn
;;

let rec pair_to_list pair =
  match pair with
  | Nil -> []
  | Pair (a, b) -> a :: pair_to_list b
  | _ -> raise This_can't_happen_exn
;;

let string_of_char a_char = String.make 1 a_char
let spacesep ns = String.concat " " ns

let rec string_expr =
  let spacesep_exp es = spacesep (List.map string_expr es) in
  let string_of_binding (n, e) = "(" ^ n ^ " " ^ string_expr e ^ ")" in
  function
  | Literal e -> string_object e
  | Var n -> n
  | If (c, t, f) ->
    "(if " ^ string_expr c ^ " " ^ string_expr t ^ " " ^ string_expr f ^ ")"
  | And (c0, c1) -> "(and " ^ string_expr c0 ^ " " ^ string_expr c1 ^ ")"
  | Or (c0, c1) -> "(or " ^ string_expr c0 ^ " " ^ string_expr c1 ^ ")"
  | Apply (f, e) -> "(apply " ^ string_expr f ^ " " ^ string_expr e ^ ")"
  | Call (f, es) ->
    if List.length es == 0
    then "(" ^ string_expr f ^ spacesep_exp es ^ ")"
    else "(" ^ string_expr f ^ " " ^ spacesep_exp es ^ ")"
  | Lambda (args, body) -> "(lambda (" ^ spacesep args ^ ") " ^ string_expr body ^ ")"
  | Defexpr (Setq (n, e)) -> "(setq " ^ n ^ " " ^ string_expr e ^ ")"
  | Defexpr (Defun (n, ns, e)) ->
    "(defun " ^ n ^ "(" ^ spacesep ns ^ ") " ^ string_expr e ^ ")"
  | Defexpr (Expr e) -> string_expr e
  | Defexpr (Defrecord (name, field_list)) ->
    "(record " ^ name ^ spacesep field_list ^ ")"
  | Let (kind, bs, e) ->
    let str =
      match kind with
      | LET -> "let"
      | LETSTAR -> "let*"
      | LETREC -> "letrec"
    in
    let bindings = spacesep (List.map string_of_binding bs) in
    "(" ^ str ^ " (" ^ bindings ^ ") " ^ string_expr e ^ ")"

and string_object e =
  let rec string_list l =
    match l with
    | Pair (a, Nil) -> string_object a
    | Pair (a, b) -> string_object a ^ " " ^ string_list b
    | _ -> raise This_can't_happen_exn
  in
  let string_pair p =
    match p with
    | Pair (a, b) -> string_object a ^ " . " ^ string_object b
    | _ -> raise This_can't_happen_exn
  in
  match e with
  | Fixnum v -> string_of_int v
  | Boolean b -> if b then "#t" else "#f"
  | String s -> "\"" ^ s ^ "\""
  | Symbol s -> s
  | Nil -> "nil"
  | Pair _ -> "(" ^ (if is_list e then string_list e else string_pair e) ^ ")"
  | Primitive (name, _) -> "#<primitive:" ^ name ^ ">"
  | Quote expr -> "'" ^ string_object expr
  | Closure (name_list, _, _) -> "#<closure:(" ^ String.concat " " name_list ^ ")>"
  | Record (name, field_list) ->
    "#<record:"
    ^ name
    ^ "("
    ^ String.concat " " (List.map string_object field_list)
    ^ ")>"
;;

let object_type = function
  | Fixnum _ -> "int"
  | Boolean _ -> "boolean"
  | String _ -> "string"
  | Symbol _ -> "symbol"
  | Nil -> "nil"
  | Pair _ -> "pair"
  | Primitive _ -> "primitive"
  | Quote _ -> "quote"
  | Closure _ -> "closure"
  | Record _ -> "record"
;;

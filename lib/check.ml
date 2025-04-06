(* TODO: Define Env *)
module S = Shapes
module A = Ast_types.Types
module Env = Map.Make (String)
exception TypeError of string
(* Phase one: building an environment *)
(* in this section, I would like to go through each of the
  macro definitions in order, building up the Gamma and Phi 
  contexts, as well as the rho substitution environment *)
let initial_ctx = Context.empty
  |> Context.set Macro "lambda" (S.Arrow (List [List[Ident]; Type Expr], Expr))
  |> Context.set Macro "define" (S.Arrow (List [Ident; Type Expr], Def))
  |> Context.set Macro "quote"  (S.Arrow (List[Any], Expr))
  |> Context.set Macro "app"    (S.Arrow (List[Type Expr; Type Expr], Expr))

let check_program ctx env ast = raise (TypeError "not implemented yet")

let rec symbol_to_type s = 
  match s with 
  | "def" -> S.Def
  | "expr" -> S.Expr
  | t -> raise (TypeError ("type " ^ t ^ " does not exist"))

let rec guarded = raise (TypeError "Not Implemented")
let rec unguarded = raise (TypeError "Not Implemented")

let macro_type ctx env clauses t = 
  let t = symbol_to_type t in
  let rec go clauses acc =
  match clauses with 
  | [] -> if S.no_overlap acc then S.Arrow (Mclauses acc, t) else raise (TypeError "macro contains overlapping clauses" )
  | A.List [pattern; guards; _] :: clauses ->
    let g = guarded guards pattern in
    let u = unguarded pattern in
      go clauses ((g, u) :: acc)
  | _ -> raise (TypeError "malformed macro")
  in go clauses []

 let check_macros ast =
  let rec go ctx env ast = 
    match ast with
    | A.List [Symbol "define-syntax"; Symbol macro;
            List[Symbol "syntax-laws"; Symbol t; 
              List macro_clauses]] :: ast ->
      let env = Env.add macro (macro_clauses, t) env in
      let ctx = Context.set Macro macro (macro_type env ctx macro_clauses t) ctx in
      go ctx env ast
    | _ -> check_program ctx env ast
in go initial_ctx Env.empty ast 


(* Phase two: checking the rest of the program as well as
  macro templates *)
(* in this section, we should be able to recieve some Gamma
  and phi, and then see if the macros are possible to expand *)
(* I believe that the output of this section should be the "typed"
  AST written in the Surface language *)



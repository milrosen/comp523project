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
  |> Context.set Macro "lambda" (S.Arrow (List [List [Ident]; Type Expr], Expr))
  |> Context.set Macro "define" (S.Arrow (List [Ident; Type Expr], Def))
  |> Context.set Macro "quote"  (S.Arrow (List [Any], Expr))
  |> Context.set Macro "app"    (S.Arrow (List [Type Expr; Type Expr], Expr))

let check_program ctx subst_env ast = raise (TypeError "not implemented yet")

 
(* in the paper, it seems that the context can contain
   way more than just pattern variables, it can also contain 
   named abbreviations and the dot (a b . (c d)) = (a b c d) 
   however, strict to the studs, it seems that neither of
   these are allowed in the judgements they actually define *)

let guarded ctx pattern macro = 
  let rec go pattern =
    (match pattern with 
    | A.List [] -> S.List []
    | A.List l -> S.List (List.map go l)
    | A.Symbol pvar -> match Context.get pvar ctx with
      | Some s -> s
      | None -> raise (TypeError ("unbound pvar " ^ pvar ^ " in macro " ^ macro)))
  in
  match pattern with 
  | A.List [A.Symbol m ; pattern] when m = macro -> go pattern
  | _ -> raise (TypeError "macro clauses must begin with the name of the macro")

let unguarded pattern macro = 
  let rec go pattern =
    (match pattern with 
    | A.List [] -> S.List []
    | A.List l -> S.List (List.map go l)
    | A.Symbol _ -> S.Any)
  in 
  match pattern with 
  | A.List [A.Symbol m; pattern] when m = macro -> go pattern
  | _ -> raise (TypeError "macro clauses must begin with the name of the macro")

let rec templates = failwith "Not Implemented"

let macro_type ctx clauses t macro = 
  let rec go clauses acc =
  match clauses with 
  | [] -> if S.no_overlap acc then S.Arrow (Mclauses acc, t) else raise (TypeError "macro contains overlapping clauses" )
  | A.List [pattern; guard] :: clauses ->
    let ctx = Context.from_guard guard ctx in
    let g = guarded ctx pattern macro in
    let u = unguarded pattern macro in
      go clauses ((g, u) :: acc)
  | _ -> raise (TypeError "malformed macro")
  in go clauses []

let check_macros ast =
  let rec go ctx subst_env ast = 
    match ast with
    | A.List [Symbol "define-syntax"; Symbol macro;
            List[Symbol "syntax-laws"; Symbol t; 
              List clauses]] :: ast ->
      let t = S.symbol_to_type t in
      let subst_env = Env.add macro (clauses, t) subst_env in
      let ctx = Context.set Macro macro (macro_type ctx clauses t macro) ctx in

      go ctx subst_env ast
    | _ -> check_program ctx subst_env ast
  in go initial_ctx Env.empty ast 


(* Phase two: checking the rest of the program as well as
  macro templates *)
(* in this section, we should be able to recieve some Gamma
  and phi, and then see if the macros are possible to expand *)
(* I believe that the output of this section should be the "typed"
  AST written in the Surface language *)




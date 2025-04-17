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
  |> Context.add Macro "lambda" (S.Arrow (List [List [Ident]; Type Expr], Expr))
  |> Context.add Macro "define" (S.Arrow (List [Ident; Type Expr], Def))
  |> Context.add Macro "quote"  (S.Arrow (List [Any], Expr))
  |> Context.add Macro "app"    (S.Arrow (List [Type Expr; Type Expr], Expr))

let true_or_error b msg =
  if b then () else raise (TypeError msg)

let rec type_check ctx t ast = 
  match ast with
  | A.Symbol s when int_of_string_opt s != None -> true_or_error (t = S.Expr) ("Symbol " ^ s ^ " expected to be datum")
  | A.Symbol s when not (Context.has s ctx) -> true_or_error (t = S.Expr) ("Symbol " ^ s ^ " expected to be ident")
  | A.Symbol s ->
      (match Context.find_opt ~vartype:Context.PVar s ctx with
      | Some shape -> true_or_error S.(shape <= Type Expr) ("Symbol " ^ s ^ " has shape " ^ S.show_s shape ^ " which is not a subtype of " ^ S.show t)
      | None -> raise (TypeError ("Symbol " ^ s ^ " either macro or undefined, expected typeable variable")))
  | A.List (Symbol m :: args) ->
    (match Context.find_opt ~vartype:Context.Macro m ctx with
    | Some (Arrow (s, t')) ->  
      let arg_shape = shape_of ctx (A.List args) in 
      true_or_error (S.(arg_shape <= s)) ("application " ^ m ^ " " ^ S.show_s arg_shape ^ " does not match any clause " ^ S.show_s s);
      true_or_error (t = t') ("output type " ^ S.show t' ^ " of macro " ^ m ^ " does not match expected output " ^ S.show t);
    | Some _ -> failwith ("macro or keyword " ^ m ^ " is not arrow type, catastrophic")
    | None -> 
      true_or_error (t = Expr) "found application, expected definition";
      let _ = List.map (type_check ctx t) (Symbol m :: args) in ())
  | _ -> failwith ("not implemented")

and shape_of ctx ast = 
  match ast with 
  | A.Symbol s when int_of_string_opt s != None -> S.Type S.Expr
  | A.Symbol s when not (Context.has s ctx) -> S.Ident
  | A.Symbol s -> 
    (match Context.find_opt s ctx with Some s -> s | None -> S.Any)
  | A.List l -> S.List (List.map (shape_of ctx) l)
 
(* in the paper, it seems that the context can contain
   way more than just pattern variables, it can also contain 
   named abbreviations and the dot (a b . (c d)) = (a b c d) 
   however, strict to the studs, it seems that neither of
   these are allowed in the judgements they actually define *)

let guarded ctx macro pattern = 
  let rec go pattern =
    (match pattern with 
    | A.List [] -> S.List []
    | A.List l -> S.List (List.map go l)
    | A.Symbol pvar -> match Context.find_opt ~vartype:PVar pvar ctx with
      | Some s -> s
      | None -> raise (TypeError ("unguarded pvar " ^ pvar ^ " in macro " ^ macro)))
  in
  match pattern with 
  | A.List (A.Symbol m :: pattern) when m = macro -> go (A.List pattern) 
  | _ -> raise (TypeError "macro clauses must begin with the name of the macro")

let unguarded macro pattern = 
  let rec go pattern =
    (match pattern with 
    | A.List [] -> S.List []
    | A.List l -> S.List (List.map go l)
    | A.Symbol _ -> S.Any)
  in 
  match pattern with 
  | A.List (A.Symbol m :: pattern) when m = macro -> go (A.List pattern) 
  | _ -> raise (TypeError "macro clauses must begin with the name of the macro")

let rec check_templates ctx t clauses = 
  match clauses with 
  | A.List [_pattern; _guard; A.List template] :: clauses ->
    type_check ctx t (A.List template) ; check_templates ctx t clauses
  | [] -> ()
  | _ -> raise (TypeError "malformed macro definition")

let macro_shape ctx macro t clauses = 
  let rec go clauses acc =
  match clauses with 
  | [] -> if S.no_overlap acc then S.Arrow (Mclauses acc, t) else raise (TypeError "macro contains overlapping clauses" )
  | A.List [pattern; guard; _template] :: clauses ->
    let ctx = Context.from_guard guard ctx in
    let g = guarded ctx macro pattern in
    let u = unguarded macro pattern in
      go clauses ((g, u) :: acc)
  | _ -> raise (TypeError "malformed macro")
  in go clauses []

let check_program ast =
  let rec go ctx subst_env ast = 
    match ast with
    | A.List [Symbol "define-syntax"; Symbol macro;
            List[Symbol "syntax-laws"; Symbol t; 
              List clauses]] :: ast ->
      let t = S.symbol_to_type t in
      let subst_env = Env.add macro (clauses, t) subst_env in
      let ctx = Context.add Macro macro (macro_shape ctx macro t clauses) ctx in
      check_templates ctx t clauses ;
      go ctx subst_env ast
    | l :: ls -> 
       (try type_check ctx Expr l with | TypeError _ -> type_check ctx Def l);
       go ctx subst_env ls
    | [] -> subst_env
  in go initial_ctx Env.empty ast 


(* Phase two: checking the rest of the program as well as
  macro templates *)
(* in this section, we should be able to recieve some Gamma
  and phi, and then see if the macros are possible to expand *)
(* I believe that the output of this section should be the "typed"
  AST written in the Surface language *)




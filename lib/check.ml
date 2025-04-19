module S = Shapes
module A = Ast_types.Types
module Env = Map.Make (String)
exception TypeError of string

let initial_ctx = Context.empty
  |> Context.add Macro "lambda" (S.Arrow (List [List [Ident]; Type Expr], Expr))
  |> Context.add Macro "define" (S.Arrow (List [Ident; Type Expr], Def))
  |> Context.add Macro "quote"  (S.Arrow (List [Any], Expr))
  |> Context.add Macro "app"    (S.Arrow (List [Type Expr; Type Expr], Expr))

let true_or_error b msg =
  if b then () else raise (TypeError msg)

(* the way I read the paper, type checking should sometimes fail, while
 shaping should always suceed. To that end, the type checker is constantly
 asserting. The only interesting case is the application of macros to arguments.
 In that case, we get the shape of the argument and check if it is a subshape of the
 input shape. If we have a macro, then this is true if we match one of the clauses.
 and if we have a keyword, then this is true if we match whatever the input shape is *)
let rec type_check ctx t ast = 
  match ast with
  | A.Symbol s when int_of_string_opt s != None -> true_or_error (t = S.Expr) ("Symbol " ^ s ^ " expected to be datum")
  | A.Symbol s when not (Context.has s ctx) -> true_or_error (t = S.Expr) ("Symbol " ^ s ^ " expected to be ident")
  | A.Symbol s ->
      (match Context.find_opt ~vartype:Context.PVar s ctx with
      | Some shape -> true_or_error S.(shape <= Type t) ("Symbol " ^ s ^ " has shape " ^ S.show_s shape ^ " which is not a subtype of " ^ S.show t)
      | None -> raise (TypeError ("Symbol " ^ s ^ " either macro or undefined, expected typeable variable")))
  | A.List (Symbol m :: args) ->
    (match Context.find_opt ~vartype:Context.Macro m ctx with
    | Some (Arrow (s, t')) ->  
      let arg_shape = shape_of ctx (A.List args) in 
      true_or_error (S.(arg_shape <= s))  ("application " ^ m ^ " " ^ S.show_s arg_shape ^ " does not match any clause " ^ S.show_s s);
      true_or_error (t = t') ("output type " ^ S.show t' ^ " of macro " ^ m ^ " does not match expected output " ^ S.show t);
    | Some _ -> failwith ("macro or keyword " ^ m ^ " is not arrow type, catastrophic")
    | None -> 
      true_or_error (t = Expr) "found application, expected definition";
      ignore (List.map (type_check ctx t) (Symbol m :: args)))
   | A.List (l :: ls) ->
      type_check ctx t l ;
      true_or_error (t = Expr) "attempted to apply definition as function" ;
      ignore (List.map (type_check ctx t) ls)
   | A.List [] -> ()
(* here, we generate the least-general shape possible for a given sexpr. Everything is an
  identifier unless it is syntactically a number. In the paper, they say that they assign
  unbound keywords the "any" shape, and we do that here too. However, I am unsure
  how we could possibly have an unbound keyword in practice. Note that we aren't ever checking
  for unbound variables, since we need to know that the "x" in lambda (x) (...) is an ident
  before it has a chance to be bound
  Also, note that we always just generate the arrow shape of any macros and keywords in our context
  This might seem like a problem, but remember that the application of arrow shapes is actually
  Done in the subshaping judgement, so it's fine. Also, note that an arrow is never a subshape of anything
  except an any, so we can't have a macro that expects a macro as input. *)
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
   these are allowed in the judgements they actually define, since 
   I don't know how'd they'd ever get in Phi *)
let rec guarded_shape ctx macro pattern =
    match pattern with 
    | A.List [] -> S.List []
    | A.List l -> S.List (List.map (guarded_shape ctx macro) l)
    | A.Symbol pvar -> match Context.find_opt ~vartype:PVar pvar ctx with
      | Some s -> s
      | None -> raise (TypeError ("unguarded pvar " ^ pvar ^ " in macro " ^ macro))

let guarded ctx macro pattern = 
  match pattern with 
  | A.List (A.Symbol m :: pattern) when m = macro -> guarded_shape ctx macro (A.List pattern) 
  | _ -> raise (TypeError "macro clauses must begin with the name of the macro")

let rec unguarded_shape pattern = 
  match pattern with 
  | A.List [] -> S.List []
  | A.List l -> S.List (List.map unguarded_shape l)
  | A.Symbol _ -> S.Any

let unguarded macro pattern = 
   match pattern with 
  | A.List (A.Symbol m :: pattern) when m = macro -> unguarded_shape (A.List pattern) 
  | _ -> raise (TypeError "macro clauses must begin with the name of the macro")

let rec check_templates ctx t clauses = 
  match clauses with 
  | A.List [_pattern; guard; template] :: clauses ->
    let ctx = Context.from_guard guard ctx in 
    type_check ctx t template ; check_templates ctx t clauses
  | [] -> ()
  | _ -> raise (TypeError "malformed macro definition")

(* here, and in the paper, we add guard clauses to our context twice. Once when
  we are generating the shape of the patterns, and a second time when we are
  checking the templates. This is important since we don't want any pattern varaibles
  in our context when we are type checking the program itself *)
let macro_shape ctx macro t clauses = 
  let rec go clauses acc =
  match clauses with 
  | [] -> if S.no_overlap acc then S.Arrow (Mclauses acc, t) else raise (TypeError "macro contains overlapping clauses" )
  | A.List [pattern; guard; _template] :: clauses ->
    let ctx = Context.from_guard guard ctx in
    let g = guarded ctx macro pattern in
    let u = unguarded macro pattern in
      go clauses ((g, u) :: acc)
  | _ -> raise (TypeError ("malformed macro: " ^ Reader.print_sexpr_list clauses))
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
    | A.List (Symbol "define-syntax" :: ast) :: _ ->
     raise (TypeError ("malformed macro definition: " ^ Reader.print_sexpr_list ast))
     
     (* admittedly, this is silly, but it seemed like the most straightforward option, since
        we can have macros that generate defs and exprs, it doesn't seem like there's a way to
        know in advance what type we want for our toplevel forms *)
    | l :: ls -> 
       let some_type l = (try type_check ctx Expr l with | TypeError _ -> type_check ctx Def l) in
       ignore (List.map some_type (l :: ls)) ;
       subst_env
    | [] -> subst_env
  in go initial_ctx Env.empty ast 
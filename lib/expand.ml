module A = Ast_types.Types
module Env = Map.Make (String)

let match_pattern pattern args =
  let pattern_shape = Check.unguarded_shape pattern in 
  let args_shape = Check.unguarded_shape args in
  Shapes.(args_shape <= pattern_shape)

let rec mtch macro args = 
  match macro with 
  | A.List [pattern; _guard; template] :: macro ->
    if match_pattern pattern args then template, pattern else mtch macro args
  | _ -> failwith ("Macro Expansion Stuck at " ^ Reader.print_sexpr_list macro)

let pick_snd _ _ v2 = Some v2

let rec make_subst pattern args =
  match pattern, args with
  | A.Symbol s, x -> Env.empty |> Env.add s x
  | A.List l1, A.List l2 -> List.fold_left2 (fun subst x y -> 
      subst |> Env.union pick_snd (make_subst x y)) 
      Env.empty l1 l2
  | _, _ -> failwith ("Macro Expansion Stuck, this should never happen :(")
  
let rec transcribe subst template = 
  match template with 
  | A.List l -> A.List (List.map (transcribe subst) l)
  | A.Symbol x ->
    match Env.find_opt x subst with
    | Some x -> x
    | None -> Symbol x

let expand macro args = 
  let template, pattern = mtch macro args in 
  let subst = make_subst pattern args in
  transcribe subst template

let rec expand_program env ast =
  List.filter_map (expand_expr env) ast

and expand_expr env expr =
  match expr with
  | A.List (Symbol "define-syntax" :: _) -> None
  | A.List (Symbol m :: args) ->
    let args = expand_program env args in
    (match Env.find_opt m env with
    | Some (macro, _t) ->
      let expanded = expand macro (A.List (Symbol m :: args)) in
      expand_expr env expanded 
    | None -> Some (A.List (A.Symbol m :: args)))
   | A.List l -> Some (A.List (expand_program env l))
   | A.Symbol s -> Some (A.Symbol s)


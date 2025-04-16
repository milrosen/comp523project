module Data = Map.Make (String)
module S = Shapes
module A = Ast_types.Types
exception TypeError of string;;


type varType = PVar | Macro

type env = {
    gamma : S.s Data.t;
    phi : S.s Data.t;
}


let empty = { gamma = Data.empty; phi = Data.empty}
let check_against key ctx =
    match Data.find_opt key ctx with 
    | None -> ()
    | Some _ -> raise (TypeError ("attempted to create variable " ^ key ^ " which is already a pvar/macro in this context"))

let update_ctx key value ctx = 
    match Data.find_opt key ctx with 
    | None -> Data.add key value ctx
    | Some s -> assert S.(value <= s) ; Data.add key value ctx
let set vartype key value {gamma ; phi} =
    if vartype = Macro then 
        (check_against key phi ; 
        {gamma=update_ctx key value gamma;phi=phi})
    else 
        (check_against key gamma ;
        {gamma=gamma;phi=update_ctx key value phi})

let get key {gamma ; phi} =
    match Data.find_opt key gamma with
    | Some s -> Some s
    | None -> match Data.find_opt key phi with
        | Some s -> Some s
        | None -> None

let rec sexpr_to_shape s =
  match s with
  | A.List l -> S.List (List.map sexpr_to_shape l)
  | A.Symbol s -> try S.Type (S.symbol_to_type s) with 
    | TypeError _ -> raise (TypeError ("invalid type " ^ s ^ " in guard clauses"))


let rec phi_from_guard guard phi =
    match guard with
    | A.List (A.List [A.Symbol pvar; s] :: rst) -> 
        phi_from_guard (A.List rst) (Data.add pvar (sexpr_to_shape s) phi)
    | _ -> raise (TypeError "malformed guard clause")

let from_guard guard {gamma ; _} =
    {gamma = gamma ;
    phi = phi_from_guard guard Data.empty}
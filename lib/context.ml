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

let add vartype key value {gamma ; phi} =
    if vartype = Macro then 
        (check_against key phi ; 
        {gamma=Data.add key value gamma;phi=phi})
    else 
        (check_against key gamma ;
        {gamma=gamma;phi=Data.add key value phi})

let rec has ?vartype key {gamma ; phi} = 
    match vartype with 
    | Some PVar -> not (Data.find_opt key phi = None)
    | Some Macro -> not (Data.find_opt key gamma = None)
    | None -> (has ~vartype:PVar key {gamma ; phi}) || (has ~vartype:Macro key {gamma ; phi})

let find_opt ?vartype key {gamma ; phi} =
    match vartype with 
    | Some PVar -> Data.find_opt key phi 
    | Some Macro -> Data.find_opt key gamma
    | None ->
         match Data.find_opt key gamma with
        | Some s -> Some s 
        | None -> match Data.find_opt key phi with
            | Some s -> Some s
            | None -> None

                                                   (* (a a... b, () b) -> t, u (a a... b, (), b)      *)
(* so then I pull unions to the front, so a clause looks like (s1, union s2 s3) -> t, s1 *)
(* and then when I apply mclauses to a union, I check that a at least one clause matches each case *)
let rec sexpr_to_shape s =
  match s with
  | A.List (Symbol "union" :: rst) ->  let l = List.map sexpr_to_shape rst in S.Union (l, ref l)
  | A.List l -> S.List (List.map sexpr_to_shape l)
  | A.Symbol s when s = "ident" -> Ident
  | A.Symbol s -> try S.Type (S.symbol_to_type s) with 
    | TypeError _ -> raise (TypeError ("invalid type " ^ s ^ " in guard clauses"))

let rec phi_from_guard guard phi =
    match guard with
    | A.List [] -> phi
    | A.List ((A.List [Symbol s; shape]) :: rest) -> 
        phi |> Data.add s (sexpr_to_shape shape) |> phi_from_guard (A.List rest)
    | _ -> raise (TypeError "malformed guard clause")

let from_guard guard {gamma ; _} =
    {gamma = gamma ;
    phi = phi_from_guard guard Data.empty}
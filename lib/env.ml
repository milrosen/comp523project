module Data = Map.Make (String)
type s = Shapes.s
exception TypeError of string;;

type varType = PVar | Macro

type env = {
    gamma : s Data.t ref;
    phi : s Data.t ref;
}

let empty = { gamma = ref Data.empty; phi = ref Data.empty}

let check_against key ctx =
    match Data.find_opt key ctx with 
    | None -> ()
    | Some _ -> raise (TypeError ("attempted to create variable " ^ key ^ " which is already a pvar/macro in this context"))

let update_ctx key value  ctx = 
    match Data.find_opt key !ctx with 
    | None -> ctx := Data.add key value !ctx
    | Some s -> assert Shapes.(s <= value) ; ctx := Data.add key value !ctx

let set {gamma ; phi} key value vartype =
    if vartype = Macro then (check_against key !phi ; update_ctx key value gamma)
    else (check_against key !gamma ; update_ctx key value phi)
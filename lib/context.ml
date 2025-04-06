module Data = Map.Make (String)
module S = Shapes
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

let set vartype  key value {gamma ; phi} =
    if vartype = Macro then 
        (check_against key phi ; 
        {gamma=update_ctx key value gamma;phi=phi})
    else 
        (check_against key gamma ;
        {gamma=gamma;phi=update_ctx key value phi})
module T = Ast_types.Types
let token_regex = Str.regexp {|\([()]\|[^ ()]+\)|}

type 'a reader = {
    form: 'a;
    tokens: string list
}

let tokenize s = Str.full_split token_regex s 
    |> List.filter_map(function
    | Str.Delim x -> Some x
    | Str.Text _ -> None)

let rec read_list eol reader = 
    match reader.tokens with 
    | [] -> raise End_of_file
    | token :: tokens ->
        if token = eol then {form = reader.form ; tokens = tokens} else
        let {form ; tokens} = read_form (token :: tokens) in
        read_list eol {form = reader.form @ [form] ; tokens = tokens}
and read_form tokens =
    match tokens with 
    | [] -> {form = T.List []; tokens = []}
    | "(" :: tokens -> 
        let {form ; tokens} = read_list ")" {form = [] ; tokens = tokens} in
        { form = T.List form ; tokens = tokens}
    | tk :: tokens -> {form = Symbol tk ; tokens = tokens}
    ;;
let rec print_sexpr s = 
    match s with
    | T.Symbol str -> str
    | T.List s -> "(" ^ (List.map print_sexpr s |> String.concat " ") ^ ")"
let read_str x = 
    let {form ; _} = read_form (tokenize x) in
    form
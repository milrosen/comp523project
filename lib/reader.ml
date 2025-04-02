let token_regex = Str.regexp {|\([()]\|[^ ()]+\)|}

type sexpr = 
    | List of sexpr list
    | Symbol of string

let rec sexpr_eq a b =
    match (a, b) with
    | (List (l1::l1s), List (l2::l2s)) -> sexpr_eq l1 l2 && sexpr_eq (List l1s) (List l2s)
    | (Symbol s1, Symbol s2) -> s1 = s2
    | _ -> false

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
    | [] -> {form = List []; tokens = []}
    | "(" :: tokens -> 
        let {form ; tokens} = read_list ")" {form = [] ; tokens = tokens} in
        { form = List form ; tokens = tokens}
    | tk :: tokens -> {form = Symbol tk ; tokens = tokens}
    ;;
let rec print_sexpr s = 
    match s with
    | Symbol str -> str
    | List s -> "[" ^ (List.map print_sexpr s |> String.concat " ") ^ "]"
let read_str x = 
    let {form ; _} = read_form (tokenize x) in
    form
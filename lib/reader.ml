module A = Ast_types.Types
let token_regex = Str.regexp "\\([()]\\|[^\n ()]+\\)"

type 'a reader = {
    form: 'a;
    tokens: string list
}

let rec lispy form = 
    match form with 
    (* | (A.Symbol m) :: [] -> A.List [A.Symbol m]
    | (A.List l) :: [] -> A.List [lispy l] *)
    | (A.Symbol m) :: rst -> A.List [Symbol m; lispy rst]
    | (A.List l) :: rst -> A.List [lispy l; lispy rst]
    | [] -> A.List []

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
    | [] -> {form = A.List []; tokens = []}
    | "l" :: "(" :: tokens ->
        let {form ; tokens} = read_list ")" {form = []; tokens = tokens} in
        { form = lispy form ; tokens = tokens}
    | "(" :: tokens -> 
        let {form ; tokens} = read_list ")" {form = [] ; tokens = tokens} in
        { form = A.List form ; tokens = tokens}
    | tk :: tokens -> {form = Symbol tk ; tokens = tokens}
    ;;
let rec print_sexpr s = 
    match s with
    | A.Symbol str -> str
    | A.List s -> "(" ^ (List.map print_sexpr s |> String.concat " ") ^ ")"

let print_sexpr_list l =
    List.map print_sexpr l |> String.concat "\n"
let read_str x = 
    let {form ; _} = read_list "EOF" {form = []; tokens = (tokenize x) @ ["EOF"] } in
    form
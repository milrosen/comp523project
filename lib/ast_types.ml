module Types = struct
   type t = 
   | Symbol of string
   | List of t list [@@deriving show, eq]
end
   (* let rec eq a b =
    match (a, b) with 
    | (Symbol s1, Symbol s2) -> s1 = s2
    | (List [], List []) -> true
    | (List (s1 :: l1), List (s2 :: l2)) -> s1 = s2 && eq (List l1) (List l2)
    | _ -> false
   end *)
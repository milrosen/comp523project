exception TypeError of string
type t = 
  | Expr
  | Def

type s = 
  | Type of t
  | Ident
  | Any
  | List of s list
  | Mclauses of (s * s) list
  | Arrow of s * t

let every_pair l1 l2 =
  let rec every_pair_except l1 l2 n =
    match l1 with
    | [] -> []
    | _ :: [] -> []
    | x :: xs -> (List.map (fun y -> (x, y)) l2 |> List.drop 1) @ every_pair_except xs l2 (n-1)
  in every_pair_except l1 l2 (List.length l1 - 1)
  let rec overlap s1 s2 = 
  match s1, s2 with
  | Any, _ -> true
  | _, Any -> true
  | List l1, List l2 -> List.length l1 == List.length l2 && 
    List.for_all2 (fun u1 u2 -> overlap u1 u2) l1 l2
  | _, _ -> raise (TypeError "Invalid shapes (neither any nor list) checked for overlap")

let no_overlap clauses = 
  let uclauses = List.map snd clauses in 
  List.for_all (fun (u1, u2) -> not (overlap u1 u2)) (every_pair uclauses uclauses)

let rec (<=) s1 s2 =
  match s1, s2 with
  | Type t1, Type t2 -> t1 = t2
  | Ident, (Type Expr) -> true
  | _, Any -> true
  | List (Arrow (s, t1) :: ss), Type t2 -> t1 = t2 && (List ss <= s)
  | List l1, List l2 -> (List.length l1 == List.length l2) && List.for_all2 (<=) l1 l2
  | List l, Type Expr -> (List.length l >= 2) && List.for_all (fun s -> s <= Type Expr) l
  | s, Mclauses clauses -> List.exists (fun (_, sk) -> s <= sk) clauses && no_overlap clauses
  | s1, s2 -> s1 = s2

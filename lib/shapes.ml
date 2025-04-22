exception TypeError of string
type t =
  | Expr
  | Def [@@deriving eq, show]

type s  = 
  | Type of t
  | Ident
  | Any
  | List of s list
  | Mclauses of (s * s) list
  | Union of s list * s list ref
  | Repeat of s 
  | Arrow of s * t  [@@deriving eq, show] 

let rec every_pair l =
  match l with 
  | [x; y] -> [(x, y)]
  | [] -> []
  | x :: xs -> List.map (fun y -> (x, y)) xs @ every_pair xs

let rec overlap s1 s2 = 
  match s1, s2 with
  | Any, _ -> true
  | _, Any -> true
  | List l1, List l2 -> List.length l1 == List.length l2 && 
    List.for_all2 (fun u1 u2 -> overlap u1 u2) l1 l2
  | _, _ -> failwith "Catastrophic, Invalid shapes (neither any nor list) checked for overlap"

let no_overlap clauses = 
  let uclauses = List.map snd clauses in 
  List.for_all (fun (u1, u2) -> not (overlap u1 u2) && not (overlap u2 u1)) (every_pair uclauses)

(* essentially, I imagine checking a union type as kinda like a search for a counter-example
  each union has a ref list that they keep track of, every time they are checked against a type, they
  remove each of the types from their list that did suceed the check. Then, if any union variable is 
  left after checking all of the clauses, we could use the remaining refs to generate a counter example
  This gets much more complex if we want to have unions which are also recursive, like the list
  shape type, (which is a union of nil and a list ending with itself), and I couldn't figure that out. I had some rules, 
  but nothing really worked.*)
  
let rec refresh s =
  match s with
  | Union (l, refl) -> refl := l ; Union (l, refl)
  | Arrow (s, t) -> Arrow (refresh s, t)
  | List l -> List (List.map refresh l)
  | Mclauses c -> Mclauses (List.map (fun (sk, uk) -> (refresh sk, uk)) c)
  | Repeat s -> Repeat (refresh s)
  | s -> s

let rec check_union s =
  match s with
  | Union (_l, refl) -> !refl = []
  | Arrow (s, _t) -> check_union s
  | List l -> List.for_all check_union l
  | Mclauses c -> List.for_all (fun (sk, _) -> check_union sk) c
  | _ -> true

let rec (<=) s1 s2 =
  match s1, s2 with
  | Union (_l, refl), s -> 
      let len = List.length !refl in
      refl := (List.filter (fun s' -> s' <= s) !refl) ;
      List.length !refl = 0 || List.length !refl != len
  | s, Union (l, _refl) -> List.exists (fun s' -> s <= s') l
  | Type t1, Type t2 -> t1 = t2
  | Ident, (Type Expr) -> true
  | _, Any -> true
  | List (Arrow (s, t1) :: args), Type t2 -> t1 = t2 && 
    let s = refresh s in (List args <= s) && check_union s
  | List l1, List l2 -> (List.length l1 == List.length l2) && List.for_all2 (<=) l1 l2
  | List l, Type Expr -> (List.length l >= 2) && List.for_all (fun s -> s <= Type Expr) l
  | s, Mclauses clauses -> 
    List.exists (fun (sk, _) -> s <= sk) clauses && no_overlap clauses
  | s1, s2 -> s1 = s2

let symbol_to_type s =
  match s with 
  | "def" -> Def
  | "expr" -> Expr
  | t -> raise (TypeError ("type " ^ t ^ " does not exist"))


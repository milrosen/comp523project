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

let overlap s1 s2 = true

let no_overlap clauses = false

let (<=) s1 s2 =
  match s1, s2 with
  | Type t1, Type t2 -> t1 = t2
  | Ident, (Type Expr) -> true
  | _, Any -> true
  | List (Arrow (s, t1) :: ss), Type t2 -> t1 = t2 && (List ss <= s)
  | List l1, List l2 -> (List.length l1 == List.length l2) && List.for_all2 (<=) l1 l2
  | List l, Type Expr -> List.for_all (fun s -> s <= Type Expr) l
  | s, Mclauses clauses -> List.for_all (fun (_, sk) -> s <= sk) clauses && no_overlap clauses
  | s1, s2 -> s1 = s2

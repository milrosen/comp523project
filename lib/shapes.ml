type t = 
  | Int
  | Expr
  | Def

type s = 
  | Type of t
  | Ident
  | Any
  | Mclauses of (s * s) list
  | Arrow of (s -> t)
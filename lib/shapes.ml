type t = 
  | Int
  | Expr
  | Def

type s = 
  | Type of t
  | Ident
  | Any
  | Mclauses of (s * s) 
  | Arrow of (s -> t)
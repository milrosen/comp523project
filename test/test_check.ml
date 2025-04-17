module S = MacrosProject.Shapes

module Check = MacrosProject.Check
module A = MacrosProject.Ast_types.Types
module Context = MacrosProject.Context
exception TypeError = MacrosProject.Check.TypeError
open Base 

let test_macro_shape ?error:msg test_name ctx macro_name ast expected_type =
  let test_name = "Macro Type :" ^ test_name in
  match msg with
  | Some msg -> 
    (test_name
    ,`Quick
    , fun () -> Alcotest.check_raises test_name (TypeError msg) (fun () ->
        (let _ = Check.macro_shape ctx ast S.Expr macro_name in ())))
  | None ->
    (test_name
  ,`Quick ,
    fun () -> Alcotest.(check (testable S.pp_s S.equal_s)) test_name expected_type (Check.macro_shape ctx ast S.Expr macro_name) )


let (^::) symbol symbols = 
    match symbols with 
    | A.List s -> A.List ([A.Symbol symbol] @ s)
    | A.Symbol s -> A.List [A.Symbol symbol; Symbol s]
let (^.) s1 s2 = A.List [A.Symbol s1 ; A.Symbol s2]
let (^@)  symbol symbols  = A.List [Symbol symbol; symbols]
let (~^) l = A.List l


let tests =
  [test_macro_shape "one clause" Check.initial_ctx "incr"
    [~^["incr" ^. "x"; ~^["x" ^. "ident"]; ~^[]]]
    (S.Arrow (S.Mclauses [(S.List [S.Ident], S.List[S.Any])], S.Expr));

   test_macro_shape ~error:"unguarded pvar y in macro incr" "unbound" Check.initial_ctx "incr"
    [~^["incr" ^. "y"; ~^["x" ^. "ident"]; ~^[]]]
    (S.Arrow (S.Mclauses [(S.List [S.Ident], S.List [S.Any])], S.Expr));

   test_macro_shape "two clauses" Check.initial_ctx "incr"
   [~^["incr" ^. "x"; ~^["x" ^. "ident"]; ~^[]];
    ~^["incr" ^:: "x" ^. "y"; ~^["x" ^. "ident"; "y" ^. "ident"]; ~^[]]]
    (S.Arrow (S.Mclauses [(S.List [S.Ident; S.Ident], S.List [S.Any; S.Any]);
                          (S.List [S.Ident], S.List [S.Any])], S.Expr));

   test_macro_shape ~error:"macro contains overlapping clauses" "overlapping clauses" 
    Check.initial_ctx "incr"
    [~^["incr" ^:: "x" ^. "y"; ~^["x" ^. "ident"; "y" ^. "expr" ]; ~^[]];
     ~^["incr" ^:: "x" ^. "y"; ~^["x" ^. "ident"; "y" ^. "ident"]; ~^[]]] S.Any]
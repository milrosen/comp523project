module S = MacrosProject.Shapes

module Check = MacrosProject.Check
module A = MacrosProject.Ast_types.Types
module Context = MacrosProject.Context
module Env = Map.Make (String)
exception TypeError = MacrosProject.Check.TypeError
open Base 

let test_macro_shape ?error:msg test_name ctx macro_name ast expected_type =
  let test_name = "Macro Type: " ^ test_name in
  match msg with
  | Some msg -> 
    (test_name
    ,`Quick
    , fun () -> Alcotest.check_raises test_name (TypeError msg) (fun () ->
        (let _ = Check.macro_shape ctx macro_name S.Expr ast in ())))
  | None ->
    (test_name
  ,`Quick ,
    fun () -> Alcotest.(check (testable S.pp_s S.equal_s)) 
    test_name expected_type (Check.macro_shape ctx macro_name S.Expr ast) )

let test_expansion_env test_name macro_name ast expected_exn = 
  let test_name = "Expansion Env: " ^ test_name in
  let rho = Check.check_program ast in
  match rho |> Env.find_opt macro_name with None -> Alcotest.fail test_name
  | Some (macro, _t) -> 
    (test_name
    ,`Quick
    , fun () -> Alcotest.(check (list (testable A.pp A.equal))) 
    test_name expected_exn macro)

let test_type_check ?error:msg test_name ctx ast expected_type =
  let test_name = "Type Check: " ^ test_name in
  match msg with 
  | Some msg ->
    (test_name
    ,`Quick
    , fun () -> Alcotest.match_raises test_name 
    (function TypeError m -> String.is_prefix ~prefix:msg m | _ -> false) (fun () ->
      Check.type_check ctx ast expected_type))
  | None ->
    (test_name
    ,`Quick
    , fun () -> Alcotest.(check unit) test_name () (Check.type_check ctx ast expected_type))

let test_shape_of test_name ctx ast expected_shape =
  let test_name = "Shape of: " ^ test_name in
  (test_name
  ,`Quick
  ,fun () -> Alcotest.(check (testable S.pp_s S.equal_s))
  test_name expected_shape (Check.shape_of ctx ast))

let (^::) symbol symbols = 
    match symbols with 
    | A.List s -> A.List ([A.Symbol symbol] @ s)
    | A.Symbol s -> A.List [A.Symbol symbol; Symbol s]
let (^.) s1 s2 = A.List [A.Symbol s1 ; A.Symbol s2]
let (^@)  symbol symbols = A.List [A.Symbol symbol; symbols]
let (~^) l = A.List l
let sy s = A.Symbol s 


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
     ~^["incr" ^:: "x" ^. "y"; ~^["x" ^. "ident"; "y" ^. "ident"]; ~^[]]] S.Any;

   test_shape_of "looks up keyword" Check.initial_ctx 
   (sy "app")
   (Arrow (S.List [S.Type Expr; S.Type Expr], Expr));
    
   test_type_check "applications are expressions" Check.initial_ctx S.Expr
   ~^["incr" ^. "x"] ;

   test_type_check ~error:"found application, expected definition" "applications are not defs" Check.initial_ctx S.Def
   ~^["incr" ^. "x"];

   test_type_check "correct apllication of keyword" Check.initial_ctx S.Expr 
   ~^[sy "lambda"; ~^[sy "x"]; sy "y"];

   test_type_check "more general type in argument" Check.initial_ctx S.Expr
   ~error:"application lambda"
   ~^[sy "lambda"; "quote" ^. "5" ; sy "y"];

   test_type_check "too many args lambda" Check.initial_ctx S.Expr
   ~error:"application lambda"
   ~^[sy "lambda"; ~^[sy "x"]; sy "y"; sy "z"];
   
   test_type_check "correct only after keyword" Check.initial_ctx S.Expr
   ~^[sy "lambda"; ~^[sy "x"]; "app" ^:: "x" ^. "y"];

   test_type_check "unary expressions not subtype of expression" Check.initial_ctx S.Expr
   ~error:"application lambda"
   ~^[sy "lambda"; ~^[sy "x"]; ~^[sy "x"]];

   test_type_check "correct outer shape, but body fails" Check.initial_ctx S.Expr
   ~error:"application lambda"
   ~^[sy "lambda"; ~^[sy "x"]; "app" ^. "x"];

   test_expansion_env "generates expansion env" "incr" 
   ["define-syntax" ^:: "incr" ^@ 
    "syntax-laws" ^:: "expr" ^@ 
      ~^[~^["incr" ^. "x"; ~^["x" ^. "ident"]; "x" ^:: "+" ^. "1"]]]
   [~^["incr" ^. "x"; ~^["x" ^. "ident"]; "x" ^:: "+" ^. "1"]
     ]]
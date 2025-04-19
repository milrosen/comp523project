module S = MacrosProject.Shapes
open Base 

let test_ordering test_name a b expected =
  let test_name = "Shapes: " ^ test_name in
  (test_name
  ,`Quick
  ,fun () ->
    Alcotest.check Alcotest.bool test_name expected S.(a <= b) )
let test_no_overlap test_name clauses expected =
  let test_name = "Shapes, Overlaps: " ^ test_name in
  (test_name
  ,`Quick
  ,fun () -> 
    Alcotest.check Alcotest.bool test_name expected (S.no_overlap clauses))


let tests =
  [test_ordering "Works for lists" 
    (List [Type Expr; Type Expr]) (List [Type Expr; Any]) true;
  test_ordering "Fails for bad lists"
    (List [Any; Any ]) (List [Type Expr; Type Expr]) false;
  test_ordering "transitivity" 
    (List [Type Expr; Ident]) (Type Expr) true;
  test_ordering "arrow types"
    (List [Arrow (List [Type Expr; Any], Expr); Type Expr; Ident]) (Type Expr) true;
  test_ordering "arrow type bad arguments"
    (List [Arrow (List [Type Expr; Any], Expr); Any]) (Type Expr) false;
  test_ordering "arrow type bad output" 
    (List [Arrow (List [Type Expr; Any], Expr); Type Expr; Ident]) (Ident) false;
  test_ordering "mclauses correct"
    (List [Any; Any]) (Mclauses [(List [Any;Any], List[Any;Any])]) true;
  test_ordering "mclauses matches at least one"
    (List [Type Expr; List [Ident]]) (Mclauses [
      (List [Type Expr; List [Ident]], List[Any;List[Any]]); 
      (List [Type Expr; List [Any;Any]], List [Any; List[Any;Any]])]) true;
  
 
  test_no_overlap "single clause never overlap" 
    [(S.Any, Any)] true;
  test_no_overlap "clauses with different lengths dont overlap"
    [(S.Any, List[Any]); (S.Any, List[Any;Any])] true;
  test_no_overlap "identical clauses overlap"
    [(S.Any, List [Any;Any]); (S.Any, List [Any;Any])] false;
  test_no_overlap "nested clauses overlap with non-nested"
    [(S.Any, List[Any;Any]); (S.Any, List [Any; List[Any;Any]])] false;
  test_no_overlap "overlap non-ordered" 
    [(S.Any, List[Any;(List[Any;Any])]); (S.Any, List[Any;Any])] false;
  test_no_overlap "lists of same length, but different shape"
    [(S.Any, List[List[Any];Any;List[Any;Any;Any]]); 
     (S.Any, List[Any;List[Any];List[Any;Any]])] true
  ]
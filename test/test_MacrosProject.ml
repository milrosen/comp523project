open Base
module Reader = MacrosProject.Reader
module A = MacrosProject.Ast_types.Types

let test_reader test_name input expected =
    (test_name
    , `Quick 
    , fun () ->  
        let actual = input |> Reader.read_str in
        let expected = expected in
        Alcotest.(check (testable A.pp A.equal )) test_name (expected) (A.List actual))
 
let test_basic  =
    [test_reader "parses sucessfully" "(how (nested) (is (too) (nested))) ()"
        (List[List[Symbol "how"; List [Symbol "nested"] ; List [Symbol "is"; List [Symbol "too"]; List [Symbol "nested"]]]; List[]]);
    ]

let () =
    Alcotest.run "tests" [("AST", test_basic);
                          ("Shapes", Test_shapes.tests);
                          ("Check",  Test_check.tests) ] ~compact:true 
                          


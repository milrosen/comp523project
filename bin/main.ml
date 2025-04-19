open Core
exception TypeError = MacrosProject.Check.TypeError


let () =
  (Sys.get_argv ()).(1) 
  |> In_channel.read_all
  |> MacrosProject.Reader.read_str
  |> (fun ast ->
       MacrosProject.Reader.print_sexpr (List ast) |> print_endline ;
       try 
          (let rho = MacrosProject.Check.check_program ast in
          (MacrosProject.Expand.expand_program rho ast)
          |> MacrosProject.Reader.print_sexpr_list
          |> print_endline)
       with | TypeError m -> print_endline m )

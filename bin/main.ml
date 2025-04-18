open Core


let () =
  (Sys.get_argv ()).(1) 
  |> In_channel.read_all
  |> MacrosProject.Reader.read_str
  |> (fun ast ->
       MacrosProject.Reader.print_sexpr (List ast) |> print_endline ;
       let rho = MacrosProject.Check.check_program ast in
       MacrosProject.Ast_types.Types.List
       (MacrosProject.Expand.expand_program rho ast))
  |> MacrosProject.Reader.print_sexpr 
  |> print_endline
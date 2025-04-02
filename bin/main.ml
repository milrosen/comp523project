open Core


let () =
  (Sys.get_argv ()).(1) 
  |> In_channel.read_all
  |> MacrosProject.Reader.read_str 
  |> MacrosProject.Reader.print_sexpr
  |> print_endline
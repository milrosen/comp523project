let read str = Reader.read_str str;;

let check ast = ast;;
let print exp = List.map Reader.print_sexpr exp |> String.concat "\n";;
let rep str = read str |> check |> print

(* mostly useless without eval or multiline stuff, but was helpfull for some debugging *)
let main = 
  try 
    while true do 
      print_string "repl> ";
      let line = read_line() in 
        try 
          print_endline (rep line)
        with End_of_file -> ()
    done 
  with End_of_file -> ()
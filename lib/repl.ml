let read str = Reader.read_str str;;

let check ast = ast;;
let print exp = Reader.print_sexpr exp;;
let rep str = read str |> check |> print

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
let read str = str;;
let eval ast _any = ast;;
let print exp = exp;;
let rep str = print (eval (read str) "")

let main = 
  try 
    while true do 
      print_string "user> ";
      let line = read_line() in 
        try 
          print_endline (rep line)
        with End_of_file -> ()
    done 
  with End_of_file -> ()
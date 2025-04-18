(* was much longer before learning about ppx *)
module Types = struct
   type t = 
   | Symbol of string
   | List of t list [@@deriving show, eq]
end
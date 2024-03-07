(* File: mp4.ml *)

open Common

(* Problem 1 *)
let rec import_list lst =
  | [] -> constexp nilconst
  | (x,y) :: xs -> binopappexp (consop, (binopappexp (commaop, constexp (intconst x), constexp (intconst y))), import_list xs)

(* Problem 2 *)
let pair_sums = ConstExp (StringConst "Not implemented yet.")

(* Problem 3 *)
let rec count_const_in_exp exp =  raise (Failure "Not implemented yet.")

(* Problem 4 *)
let rec freeVarsInExp exp = raise (Failure "Not implemented yet")

(* Problem 5 *)
let rec cps_exp e k =  raise (Failure "Not implemented yet.")


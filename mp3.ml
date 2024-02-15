(* CS342 - Spring 2024
 * MP3
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You will want to change how a number of these start.
 *)


(*Problem 1*)
(** 10 minutes *)
let rec product l = 
  match l with
  | [] -> 1.
  | x :: xs -> x *. product xs
 
(*Problem 2*)
(** 15 minutes *)
let rec double_all l =
  match l with
  | [] -> []
  | x :: xs -> x *. 2. :: double_all xs 

(*Problem 3*)
(** 20 minutes *)
let rec pair_with_all x l = 
  match l with 
  | [] -> []
  | y :: ys -> (x,y) :: pair_with_all x ys;;

(*Problee 4*)
(** 20 minutes *)
let rec interleave l1 l2 =
  match l1, l2 with
  | [], l2 -> l2
  | l1, [] -> l1
  | x :: xs, y :: ys -> x :: y :: interleave xs ys 
 
(*Problem 5*)
(** 20 minutes *)
let rec even_count_fr l =
  match l with
  | [] -> 0
  | x :: xs -> match x mod 2 with
    | 0 -> 1 + even_count_fr xs
    | _ -> even_count_fr xs

(*Problem 6*)
(** 15 minutes  *)
let rec pair_sums l = 
  match l with
  | [] -> []
  | x :: xs -> match x with
    | (y,z) -> y + z :: pair_sums xs

(*Problem 7*)
(** 15 minutes  *)
let rec remove_even list =
  match list with
  | [] -> []
  | x :: xs -> match x mod 2 with
    | 0 -> remove_even xs
    | _ -> x :: remove_even xs

(*Problem 8*)
let rec sift p l = 
        match l with
        | [] -> ([],[])
        | x :: xs -> let l,r = sift p xs in
        if p x then (x :: l,r) else (l,x::r)

(* 9 tr acc *)
let rec even_count_tr_acc l acc =
match l with
| [] -> acc
| x :: xs -> match x mod 2 with
  | 0 -> even_count_tr_acc xs (acc+1)
  | _ -> even_count_tr_acc xs acc

(*Problem 9*)
(** 15 minutes *)
let rec even_count_tr l = even_count_tr_acc l 0 


(** 10 tr acc *)
let rec count_element_acc l m acc =
  match l with
  | [] -> acc
  | x :: xs -> 
    if x = m then count_element_acc xs m (acc+1)
    else count_element_acc xs m acc

(*Problem 10*)
(** 15 minutes  *)
let rec count_element l m = count_element_acc l m 0 


(* 11 tr acc *)
let rec all_nonneg_acc list acc =
match list with
| [] -> if acc = 0 then true else false
| x :: xs -> if x < 0 then all_nonneg_acc xs (acc+1) else all_nonneg_acc xs acc

(*Problem 11*)
(** 10 minutes  *)
let rec all_nonneg list = all_nonneg_acc list 0 

(** 12 tr true acc *)
let rec split_sum_true_acc l f acc =
  match l with
  | [] -> acc
  | x :: xs ->
    if f x = true then split_sum_true_acc l f (acc+x)
    else split_sum_true_acc l f acc

(** 12 tr false acc *)
let rec split_sum_false_acc l f acc =
  match l with
  | [] -> acc
  | x :: xs ->
    if f x = true then split_sum_true_acc l f acc
    else split_sum_true_acc l f (acc+x)


(*Problem 12*)
(** 20 minutes  *)
let rec split_sum l f = 
  (
    split_sum_true_acc l f 0,
    split_sum_false_acc l f 0
  ) 

(*Problem 13*)
(** 20 minutes  *)
let even_count_fr_base = 0
let even_count_fr_rec x rec_val =
  if x mod 2 = 0 then rec_val + 1
  else rec_val 
  
(*Problem 14*)
(** 25 minutes  *)
let pair_sums_map_arg p =
  match p with
  | (x,y) -> x+y;;

(*Problem 15*)
(** 25 minutes  *)
let remove_even_base = []
let remove_even_rec n r = 
  if n mod 2 = 1 then n :: r else r

(*Problem 16*)
(** 25 minutes  *)
let even_count_tr_start = 0
let even_count_tr_step acc_val x =
  if x mod 2 = 0 then acc_val + 1 else acc_val

(*Problem 17*)
(** 25 minutes  *)
let split_sum_start = (0,0)
let split_sum_step f =
  (fun p (x : int) -> 
    if f x then match p with
    | (y,z) -> (y+x,z)
    else match p with 
    | (y,z) -> (y,x+z)
  )

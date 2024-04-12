open Core;;

(* open Cmp;;
open Heap;;
let read () = Out_channel.(flush stdout); In_channel.(input_line_exn stdin)
let rec read_inputs () : int list list = match String.strip ~drop:Char.is_whitespace (read ()) with
  | "" -> []
  | s -> List.map ~f:Int.of_string (String.split ~on:' ' s) :: read_inputs ()

let main () =
  let lists = read_inputs () in
  let cmp a b : Cmp.order = match a, b with
  | [], [] -> EQ
  | [], _ -> LT
  | _, [] -> GT
  | x::_, y::_ -> Cmp.from_int_cmp Int.compare x y
  in
  let heap = Heap.create cmp in
  ignore (List.map ~f:(Heap.push heap) lists);
  let rec merge rev_acc =
    match Heap.pop_opt heap with
  | None -> List.rev rev_acc
  | Some [] -> merge rev_acc
  | Some (x::xs) -> (Heap.push heap xs; merge (x::rev_acc))
  in
  print_endline (List.to_string ~f:Int.to_string (merge []));
  printf "%d\n" (Arr.Arr.total_cost ())
;; *)
;;
(* module Typ = Typ.Typ *)
open Typ
open Exp
let (x, y, z) = (Var.var_x, Var.var_y, Var.var_z)
let (tx, ty, _tz) = (t_var x, t_var y, t_var z)
let (ex, _ey, ez) = (e_var x, e_var y, e_var z)
let example_type_x_to_y : typ = arr (prod tx tx) ty
let example_type_id = arr (t_var (Var.Var.make "b")) (let a = t_var (Var.Var.make "a") in arr a a)
let println str = print_string (str ^ "\n")
let print_typ typ = println (show_typ typ)
(* let print_exp exp = println (show_exp exp) *)
;;
println "Test 1: Unify x * x -> y with b -> a -> a";;
println (Typ.Unifier.show_result (Typ.Unifier.unify example_type_x_to_y example_type_id));;
println "Test 2: Infer the type of fun x -> x";;
print_typ (Infer.infer (lam x ex));;
println "Test 3: Infer the type of fun x -> fst x";;
print_typ (Infer.infer (lam x (fst ex)));;
(* println "Test Omitted: Should not pass. Unify x with y -> x";; *)
(* println (Typ.Unifier.show_result (Typ.Unifier.unify tx (arr ty tx)));; *)
println "Test 4: Infer the type of fun x -> Inl x";;
print_typ (Infer.infer (lam x (case ex y ex z ex)));;
println "Test 5: Infer the type of fun x -> case (fst x) of Inl y -> (snd x) | Inr z -> (snd z)";;
(* Should be " (a + b * c) * c -> c " *)
print_typ (Infer.infer (lam x (case (fst ex) y (snd ex) z (snd ez))));;
(* println "Test 6: Infer the type of fun x -> case (fst x) of Inl y -> (y, snd x) | Inr z -> (snd z)";;
print_typ (Infer.infer (lam x (case (fst ex) y (tup ey (snd ex)) z (snd ez))));; *)


(* main ();; *)

(* 
2 11
2 2
10
0 0 123


1 2 3 4 5 6 7 8 9 10               
11 20 414 21321
2 2 2 2 2 2 2 2
0 0 1 2 3 5 8 13


 *)
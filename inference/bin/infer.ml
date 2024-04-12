open Core;;
open Var;;
open Exp;;
open Typ;;

type ctx = typ VarMap.t
module U = Typ.Unifier
(* type Exp.view =
| EVar of Var.t
| Lam of Var.t * exp
| App of exp * exp
| Tup of exp * exp
| Fst of exp
| Snd of exp *)
type subst = Var.t * typ
[@@deriving show]
type subst_list = subst list
[@@deriving show]
let rec synth (ctx : ctx) (acc : subst list) (exp : exp) : typ * subst list = let res, acc = (print_string ("Synth for " ^ show_exp exp ^ "\n");
match e_out exp with
| EVar x -> (Map.find_exn ctx x, acc)
| Lam (x, e) -> 
    (* let () = print_string ("Infer: fun " ^ Var.show x ^ " -> " ^ show_exp e ^ "\n") in *)
    let ty_x = t_var (Var.make ("ty_" ^ Var.name x)) in
    let ctx = Map.add_exn ctx ~key:x ~data:ty_x in
    (* let () = print_string ("Infer substs: " ^ show_subst_list acc ^ "\n") in *)
    let ty_e, acc = synth ctx acc e in
      (arr ty_x ty_e, acc)
| App (e1, e2) ->
    let ty_e1, acc = synth ctx acc e1 in
    let ty_e2, acc = synth ctx acc e2 in
      U.unify ~acc ty_e1 (arr ty_e2 (t_var (Var.fresh ())))
| Tup (e1, e2) ->
    let ty_e1, acc = synth ctx acc e1 in
    let ty_e2, acc = synth ctx acc e2 in
    (prod ty_e1 ty_e2, acc)
| Fst e ->
    let ty_e, acc = synth ctx acc e in (
      match t_out ty_e with
      | Prod (ty1, _) -> (ty1, acc)
      | _ ->
        let ty1 = t_var (Var.fresh ()) in
        let ty2 = t_var (Var.fresh ()) in
        let _, acc = U.unify ~acc ty_e (prod ty1 ty2) in
          (ty1, acc)
    )
| Snd e ->
    let ty_e, acc = synth ctx acc e in
    let ty1 = t_var (Var.fresh ()) in
    let ty2 = t_var (Var.fresh ()) in
    let _, acc = U.unify ~acc ty_e (prod ty1 ty2) in
      (ty2, acc)
| Inl e ->
    let ty_e, acc = synth ctx acc e in
    let ty_inr = t_var (Var.fresh ()) in
      (sum ty_e ty_inr, acc)
| Inr e ->
    let ty_e, acc = synth ctx acc e in
    let ty_inl = t_var (Var.fresh ()) in
      (sum ty_inl ty_e, acc)
| Cse (e, (x, e1), (y, e2)) ->
    let ty_e, acc = synth ctx acc e in
    let ty_x = t_var (Var.fresh ()) in
    let ty_y = t_var (Var.fresh ()) in
    let _, acc = U.unify ~acc ty_e (sum ty_x ty_y) in
    let ty_e1, acc = synth (Map.add_exn ctx ~key:x ~data:ty_x) acc e1 in
    let ty_e2, acc = synth (Map.add_exn ctx ~key:y ~data:ty_y) acc e2 in
      U.unify ~acc ty_e1 ty_e2
| Let (e1, (x, e2)) ->
    let ty_e1, acc = synth ctx acc e1 in
    let ty_e2, acc = synth (Map.add_exn ctx ~key:x ~data:ty_e1) acc e2 in
      (ty_e2, acc)
) in
  let () = print_string ("Synth result    " ^ show_exp exp ^ " : " ^ show_typ res ^ "\n") in
    (res, acc)
let infer (exp : exp) : typ =
  let t, acc = synth VarMap.empty [] exp
  in List.fold_right acc ~init:t ~f:(fun (x, ty) -> TTerm.subst ty x)
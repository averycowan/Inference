open Core;;
open Var;;

module Unify (T : Term.TERM) = struct
  module T = T
  module O = T.O

  type term = T.t

  let alpha_vary (v_old) (v_new) (t : term) : term = T.subst (T.var v_new) v_old t
  let alpha_vary_list (vs_old) (vs_new) (t : term) : term = List.fold_right ~f:(fun (v_old, v_new) -> alpha_vary v_old v_new) ~init:t (Util.zip vs_old vs_new)

  let subst_all (substs : (Var.t * term) list) (t : term) : term = List.fold_right ~f:(fun (v, t) -> T.subst t v) ~init:t substs

  let rec try_unify (t1 : term) (t2 : term) (((yes : unit -> 'z), (no : term -> term -> 'z), (bind : var -> term -> 'z)) as k) : 'z = match (T.out t1, T.out t2) with
  | (T.Var v1, T.Var v2) -> if Var.equal v1 v2 then yes () else bind v1 t2
  | (T.Var v, _) -> bind v t2
  | (_, T.Var v) -> bind v t1
  | (T.Oper (o1, ts1), T.Oper (o2, ts2)) when O.equal o1 o2 ->
    let rec try_unify_list ts1 ts2 k = match (ts1, ts2) with
    | ([], []) -> yes ()
    | ((vs1, t1)::ts1, (vs2,t2)::ts2) -> try_unify t1 (alpha_vary_list vs2 vs1 t2) ((fun () -> try_unify_list ts1 ts2 k), no, bind)
    | _ -> failwith "Malformed term: different number of arguments in the same operator"
    in try_unify_list ts1 ts2 k
  (* | (T.T0 o1, T.T0 o2) -> if O.equal_oper0 o1 o2 then yes () else no t1 t2
  | (T.T1 (o1, a1), T.T1 (o2, a2)) -> if O.equal_oper1 o1 o2 then try_unify a1 a2 k else no t1 t2
  | (T.T2 (o1, a1, b1), T.T2 (o2, a2, b2)) -> if O.equal_oper2 o1 o2 then try_unify a1 a2 ((fun () -> try_unify b1 b2 k), no, bind) else no t1 t2 *)
  | _ -> no t1 t2

  type unify_step =
  | Yes of (Var.t * T.t) list * T.t
  | No of (Var.t * T.t) list * T.t * T.t
  [@@deriving sexp,equal,compare,show]

  let rec unify' (t1 : term) (t2 : term) (acc : (var * term) list) : unify_step = (
  print_string ("Unifying " ^ T.show t1 ^ " with " ^ T.show t2);
  try_unify t1 t2 (
    (fun () -> print_string " Yes\n"; Yes (acc, t1)),
    (fun p1 p2 -> print_string " No\n"; No (acc, p1, p2)),
    (fun x t ->
      print_string (" Bind " ^ Var.show x ^ " to " ^ T.show t ^ "\n");
      if Set.mem (T.freevars t) x
        then No (acc, T.var x, t)
        else
          unify'
          (T.subst t x t1)
          (T.subst t x t2)
          (
            (x,t)
            ::
            List.map acc ~f:(
              fun (y, q) -> (y, T.subst t x q)
            )
          )
    )
  ))

  type result = T.t * (Var.t * T.t) list
  [@@deriving sexp,equal,compare,show]

  let unify ?(acc=[]) t1 t2 = match unify' t1 t2 acc with
  | Yes (acc, t) -> (t, acc)
  | No (acc, t1, t2) -> failwith ("Unification failed: " ^ T.show t1 ^ " and " ^ T.show t2 ^ " are not unifiable after bindings: " ^ String.concat ~sep:", " (List.map ~f:(fun (v, t) -> Var.show v ^ " to " ^ T.show t) acc))
end
open Core;;
open Var;;

module type OPER = sig
  (* type oper0
  [@@deriving equal,sexp,compare,show]
  
  type oper1
  [@@deriving equal,sexp,compare,show]
  
  type oper2
  [@@deriving equal,sexp,compare,show] *)
  type t
  [@@deriving equal,sexp,compare,show]
  val valence : t -> int list
  val arity : t -> int
end

module type TERM = sig
  module O : OPER

  (* type t =
  | Var of var
  | T0 of O.oper0
  | T1 of O.oper1 * t
  | T2 of O.oper2 * t * t *)
  type t
  [@@deriving sexp,equal,compare,show]
  type view =
  | Var of Var.t
  | Oper of O.t * (Var.t list * t) list
  [@@deriving sexp,equal,compare,show]

  val subst : t -> var -> t -> t
  val freevars : t -> VarSet.t
  val into : view -> t
  val out : t -> view
  val var : Var.t -> t
  val oper : O.t -> (Var.t list * t) list -> t
end

module MakeTerm (O : OPER) : TERM with module O = O = struct
  module O = O
  type t =
  | FV of Var.t
  | BV of int
  | OP of O.t * (int * string list * t) list
  [@@deriving sexp,equal,compare]

  let rec show_vars = function
  | [] -> ""
  | v::vs -> show_vars vs ^ v ^ "."

  let rec show_abs (_, vs, t) = (show_vars vs ^ show t)
  and show : t -> string = function
  | FV v -> Var.show v
  | BV i -> "$" ^ string_of_int i
  | OP (o, []) -> O.show o
  | OP (o, [a1]) -> O.show o ^ show_abs a1
  | OP (o, [a1; a2]) -> "(" ^ show_abs a1 ^ " " ^ O.show o ^ " " ^ show_abs a2 ^ ")"
  | OP (o, abss) -> O.show o ^ "(" ^ String.concat ~sep:", " (List.map ~f:show_abs abss) ^ ")"
  
  let pp fmt v = Format.pp_print_text fmt (show v)

  type view =
  | Var of Var.t
  | Oper of O.t * (Var.t list * t) list
  [@@deriving sexp,equal,compare,show]

  (* let rec show = function
  | Var v -> Var.show v
  | T0 o -> O.show_oper0 o
  | T1 (o, t1) -> O.show_oper1 o ^ "(" ^ show t1 ^ ")"
  | T2 (o, t1, t2) -> "(" ^ show t1 ^ " " ^ O.show_oper2 o ^ " " ^ show t2 ^ ")"
  let pp fmt v = Format.pp_print_text fmt (show v) *)

  let rec subst t x = function
  | FV v -> if Var.equal x v then t else FV v
  | BV i -> BV i
  | OP (oper, ts) -> OP (oper, List.map ~f:(fun (j, vs, t1) -> (j, vs, subst t x t1)) ts)

  let rec freevars' acc = function
  | FV v -> Set.add acc v
  | BV _ -> acc
  | OP (_, ts) -> List.fold_left ~f:(fun acc (_, _, t) -> freevars' acc t) ~init:acc ts

  let freevars = freevars' VarSet.empty

  let rec bind (x : Var.t) (i : int) : t -> t = function
  | FV v -> if Var.equal x v then BV i else FV v
  | BV j -> BV (if j < i then j else j+1)
  | OP (oper, ts) -> OP (oper, List.map ~f:(fun (j, vs, t) -> (j, vs, bind x (i+j) t)) ts)

  let rec bind_all (i : int) (t : t) (acc : string list) : Var.t list -> int * string list * t = function
  | [] -> (i, acc, t)
  | x::xs -> bind_all (i+1) (bind x 0 t) (Var.name x :: acc) xs


  let rec unbind (x : Var.t) (i : int) : t -> t = function
  | FV v -> FV v
  | BV j when j < i -> BV j
  | BV j when j > i -> BV (j-1)
  | BV _ -> FV x
  | OP (oper, ts) -> OP (oper, List.map ~f:(fun (j, vs, t) -> (j, vs, unbind x (i+j) t)) ts)
  (* let unbind x i t = (print_string ("Unbind: "^ Var.show x ^ " for " ^ Int.to_string i ^ " in " ^ show t ^ "\n"); unbind x i t) *)

  let rec unbind_all (acc : Var.t list) (t : t) = function
  | [] -> (acc, t)
  | name::names -> let v = Var.make name in unbind_all (v::acc) (unbind v 0 t) names
  (* let unbind_all acc t i =
    let acc, t' = unbind_all acc t i in
    let () = print_string ("unbind_all: "^ show t ^ " = " ^ String.concat ~sep:"," (List.map ~f:Var.show acc) ^ "to get " ^ show t' ^ "\n") in
    (acc, t') *)

  let extract_args (i, (vs, t)) : int * string list * t =
    if List.length vs <> i then failwith "Malformed"
    else bind_all 0 t [] vs

  let into = function
  | Var v -> FV v
  | Oper (oper, ts) -> OP (oper, List.map ~f:extract_args (Util.zip (O.valence oper) ts))

  let out = function
  | FV v -> Var v
  | OP (oper, ts) -> Oper (oper, List.map ~f:(fun (_i, vs, t) -> unbind_all [] t vs) ts)
  | BV _ -> failwith "Malformed"
  (* let out t = (print_string ("Out: "^ show t ^ "\n"); out t) *)

  let var v = into (Var v)
  let oper o ts = into (Oper (o, ts))
end
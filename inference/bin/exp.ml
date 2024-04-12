open Core;;

open Var;;

module Op = struct
  type t = Lam | App | Tup | Fst | Snd | Inl | Inr | Cse | Let
  [@@deriving equal,sexp,compare]
  let show = function
  (* Use unicode when possible *)
  | Lam -> "λ"
  | App -> "·"
  | Tup -> ","
  | Fst -> "π₁"
  | Snd -> "π₂"
  | Inl -> "inl"
  | Inr -> "inr"
  | Cse -> "case"
  | Let -> "let"

  let pp fmt v = String.pp fmt (show v)

  let arity = function
  | Lam -> 1
  | App -> 2
  | Tup -> 2
  | Fst -> 1
  | Snd -> 1
  | Inl -> 1
  | Inr -> 1
  | Cse -> 3
  | Let -> 2

  let valence = function
  | Lam -> [1]
  | App -> [0; 0]
  | Tup -> [0; 0]
  | Fst -> [0]
  | Snd -> [0]
  | Inl -> [0]
  | Inr -> [0]
  | Cse -> [0; 1; 1]
  | Let -> [0; 1]
end

module ETerm = Term.MakeTerm(Op)
module Unifier = Unify.Unify(ETerm)

type exp = ETerm.t
[@@deriving equal,sexp,compare,show]

let e_var x = ETerm.var x
let lam x a = ETerm.oper Op.Lam [([x], a)]
let app a b = ETerm.oper Op.App [([], a); ([], b)]
let tup a b = ETerm.oper Op.Tup [([], a); ([], b)]
let fst a = ETerm.oper Op.Fst [([], a)]
let snd a = ETerm.oper Op.Snd [([], a)]
let inl a = ETerm.oper Op.Inl [([], a)]
let inr a = ETerm.oper Op.Inr [([], a)]
let case a x b y c = ETerm.oper Op.Cse [([], a); ([x], b); ([y], c)]
let let_ a x b = ETerm.oper Op.Let [([], a); ([x], b)]

type view =
| EVar of Var.t
| Lam of Var.t * exp
| App of exp * exp
| Tup of exp * exp
| Fst of exp
| Snd of exp
| Inl of exp
| Inr of exp
| Cse of exp * (Var.t * exp) * (Var.t * exp)
| Let of exp * (Var.t * exp)
[@@deriving equal,sexp,compare]

let e_out e = match ETerm.out e with
| ETerm.Var x -> EVar x
| ETerm.Oper (Op.Lam, [([x], a)]) -> Lam (x, a)
| ETerm.Oper (Op.App, [([], a); ([], b)]) -> App (a, b)
| ETerm.Oper (Op.Tup, [([], a); ([], b)]) -> Tup (a, b)
| ETerm.Oper (Op.Fst, [([], a)]) -> Fst a
| ETerm.Oper (Op.Snd, [([], a)]) -> Snd a
| ETerm.Oper (Op.Inl, [([], a)]) -> Inl a
| ETerm.Oper (Op.Inr, [([], a)]) -> Inr a
| ETerm.Oper (Op.Cse, [([], a); ([x], b); ([y], c)]) -> Cse (a, (x, b), (y, c))
| ETerm.Oper (Op.Let, [([], a); ([x], b)]) -> Let (a, (x, b))
| _ -> failwith ("Invalid expression: " ^ (show_exp e))

let e_into = function
| EVar x -> e_var x
| Lam (x, a) -> lam x a
| App (a, b) -> app a b
| Tup (a, b) -> tup a b
| Fst a -> fst a
| Snd a -> snd a
| Inl a -> inl a
| Inr a -> inr a
| Cse (a, (x, b), (y, c)) -> case a x b y c
| Let (a, (x, b)) -> let_ a x b

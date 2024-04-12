open Core;;

module Op = struct
  type t = Arrow | Prod | Sum
  [@@deriving equal,sexp,compare]
  let show = function
  | Arrow -> "->"
  | Prod -> "*"
  | Sum -> "+"
  let pp fmt v = String.pp fmt (show v)

  let arity = function
  | Arrow -> 2
  | Prod -> 2
  | Sum -> 2

  let valence _ = [0; 0]

end
module TTerm = Term.MakeTerm(Op)
module Unifier = Unify.Unify(TTerm)

type typ = TTerm.t
[@@deriving equal,sexp,compare,show]

let t_var x = TTerm.var x
let arr a b = TTerm.oper Op.Arrow [([], a); ([], b)]
let prod a b = TTerm.oper Op.Prod [([], a); ([], b)]
let sum a b = TTerm.oper Op.Sum [([], a); ([], b)]

type view =
| TVar of Var.Var.t
| Arrow of typ * typ
| Prod of typ * typ
| Sum of typ * typ

let t_out t = match TTerm.out t with
| TTerm.Var x -> TVar x
| TTerm.Oper(Op.Arrow, [([], a); ([], b)]) -> Arrow(a, b)
| TTerm.Oper(Op.Prod, [([], a); ([], b)]) -> Prod(a, b)
| TTerm.Oper(Op.Sum, [([], a); ([], b)]) -> Sum(a, b)
| _ -> failwith ("Invalid type: " ^ (show_typ t))

let t_into = function
| TVar x -> t_var x
| Arrow(a, b) -> arr a b
| Prod(a, b) -> prod a b
| Sum(a, b) -> sum a b

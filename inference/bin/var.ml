open Core;;

module Var : sig
  type t
  [@@deriving sexp,equal,compare,show]
  val name : t -> string
  val make : string -> t
  val fresh : unit -> t
end = struct
  let varno = ref (-1)
  type t = string * int
  [@@deriving sexp,equal,compare]

  let show (n, i) = n ^ "_" ^ Int.to_string i
  let pp fmt v = Format.pp_print_text fmt (show v)

  let name (n, _) = n
  let make n = (varno := !varno + 1 ; (n, !varno))
  let fresh () = make ("%" )
end
type var = Var.t

module VarSet = Set.Make(Var)
module VarMap = Map.Make(Var)

let var_x = Var.make "x"
let var_y = Var.make "z"
let var_z = Var.make "y"
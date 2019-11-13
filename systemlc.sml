use "unify.sml";

signature SYSTEMLC = sig
  structure Typ : sig
    datatype typ =
      TVar of Var.t
    | Arrow of typ * typ
    val subst : typ -> Var.t -> typ -> typ
    val toString : typ -> string
  end
  structure Exp : sig
    datatype exp =
      Var of Var.t
    | Ap of exp * exp
    | Lam of Var.t * exp
    val subst : exp -> Var.t -> exp -> exp
    val toString : exp -> string
  end
end
structure SystemLC :> SYSTEMLC = struct
  structure Typ = struct
    datatype typ =
      TVar of Var.t
    | Arrow of typ * typ
    fun subst t x = fn
      TVar v => if v = x then t else TVar v
    | Arrow (t1,t2) => Arrow(subst t x t1, subst t x t2)

    val rec toString = fn
      TVar v => Var.toString v
    | Arrow (TVar v,t2) => Var.toString v ^ " -> " ^ toString t2
    | Arrow (t1, t2) => "("^toString t1^")"^" -> " ^toString t2
  end
  structure Exp = struct
    datatype exp =
      Var of Var.t
    | Ap of exp * exp
    | Lam of Var.t * exp
    fun subst e x = fn
      Var v => if v = x then e else Var v
    | Ap (e1,e2) => Ap (subst e x e1, subst e x e2)
    | Lam (v,m) => if v = x then Lam (v,m) else Lam (v,subst e x m)

    val rec toString = fn
      Var v => Var.toString v
    | Ap (e1,e2) => "(" ^ toString e1 ^ " " ^toString e2 ^ ")"
    | Lam (v,m) => "(\\"^Var.toString v ^ "." ^ toString m ^ ")"
  end
end

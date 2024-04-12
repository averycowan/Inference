local
  signature VOID = sig
    eqtype void
    val abort : void -> 'a
  end
  structure Void :> VOID = struct
    datatype void = Void of void
    fun abort (Void t) = abort t
  end
in
  open Void
end

signature LABEL = sig
  eqtype label
  type t = label

  val new : string -> label
  val toString : label -> string
end

signature OPERATORS = sig
  eqtype oper0
  eqtype oper1
  eqtype oper2

  datatype oper =
    O0 of oper0
  | O1 of oper1
  | O2 of oper2

  val toString : oper -> string
end

signature TERM = sig
  structure O : OPERATORS
  structure V : LABEL
  type var = V.t
  datatype term =
    Var of var
  | T0 of O.oper0
  | T1 of O.oper1 * term
  | T2 of O.oper2 * term * term

  val subst : term -> var -> term -> term

  val toString : term -> string
end

signature UNIFY = sig
  structure T : TERM
  type term = T.term
  type var = T.var
  datatype result = Yes | No | Bind of var * term

  val find : (term * term) list -> result
  val unify : (term * term) list -> (var * term) list option
end

functor MkTerm(structure Ops : OPERATORS structure Vars : LABEL) :> TERM where O = Ops and V = Vars = struct
  structure O = Ops
  structure V = Vars
  open O
  type var = V.t
  datatype term =
    Var of var
  | T0 of oper0
  | T1 of oper1 * term
  | T2 of oper2 * term * term
  fun subst e x = fn
    Var v => if x = v then e else Var v
  | T0 t => T0 t
  | T1 (t,e1) => T1 (t,subst e x e1)
  | T2 (t,e1,e2) => T2 (t,subst e x e1,subst e x e2)
  val rec toString : term -> string = fn
    Var v => V.toString v
  | T0 t => O.toString (O.O0 t)
  | T1 (t,e1) => O.toString (O.O1 t) ^ "(" ^ toString e1 ^ ")"
  | T2 (t,e1,e2) => "(" ^ toString e1 ^ " " ^ O.toString (O.O2 t) ^ " " ^ toString e2 ^ ")"
end

functor MkUnify(structure Term : TERM) : UNIFY where T = Term = struct
  structure T = Term
  open T
  datatype result = Yes | No | Bind of var * term
  fun find nil = Yes
    | find (p::L) = case p of
      (Var v1, Var v2) => if v1=v2 then find L else Bind (v1, Var v2)
    | (Var v, e) => Bind (v, e)
    | (e, Var v) => Bind (v, e)
    | (T0 t1, T0 t2) => if t1 = t2 then find L else No
    | (T1 (t1,a1), T1 (t2,b1)) => if t1 = t2 then find ((a1,b1)::L) else No
    | (T2 (t1,a1,a2), T2 (t2,b1,b2)) => if t1 = t2 then find ((a1,b1)::(a2,b2)::L) else No
    | _ => No

  fun unify L = case find L of
    No => NONE
  | Yes => SOME []
  | Bind (v,e) =>
    let
      val substboth = fn (e1,e2) => (subst e v e1, subst e v e2)
      val L' = map substboth L
      val r = unify L'
    in
      case r of NONE => NONE | SOME B => SOME((v,foldl (fn ((v',e'),e) => subst e' v e) e B)::B)
    end
end

structure Var :> LABEL = struct
  type label = int * string
  type t = label
  val c = ref 0
  fun new s = ((c:=(!c+1));(!c-1,s))
  fun toString (i,s) = s ^ (Int.toString i)
end

structure UnifyTest = struct
  structure TypOp = struct
    datatype oper0 = void
    type oper1 = void
    datatype oper2 = Arrow

    datatype oper =
      O0 of oper0
    | O1 of oper1
    | O2 of oper2

    val toString = fn
      O0 Unit => "Unit"
    | O1 v => abort v
    | O2 Arrow => "Arr"
  end
  structure Term = MkTerm(structure Ops = TypOp structure Vars = Var)
  structure Unify = MkUnify(structure Term = Term)
  structure Term = Unify.T
  structure Var = Term.V
  fun toString NONE = print ("No" ^ "\n")
    | toString (SOME L) = print (String.concatWithMap "\n" (fn (x,e) => Var.toString x ^ " -> " ^ Term.toString e) L ^ "\n")
  val unify = Unify.unify
  val x = Var.new "x"
  val y = Var.new "y"
  val z = Var.new "z"
  local val a = Var.new "'a" in
    val id = Term.T2 (TypOp.Arrow, Term.Var a, Term.Var a)
  end
  local val (b,c) = (Var.new "'b",Var.new "'c") in
    val k1 = Term.T2 (TypOp.Arrow, Term.Var b, Term.T2 (TypOp.Arrow, Term.Var c, Term.Var c))
  end
  local val (b,c) = (Var.new "'b",Var.new "'c") in
    val k2 = Term.T2 (TypOp.Arrow, Term.Var b, Term.T2 (TypOp.Arrow, Term.Var c, Term.Var c))
  end
end








(*_*)

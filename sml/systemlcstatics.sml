use "systemlc.sml";
structure TypOp = struct
  type oper0 = void
  type oper1 = void
  datatype oper2 = Arrow

  datatype oper =
    O0 of oper0
  | O1 of oper1
  | O2 of oper2

  val toString = fn
    O0 v => abort v
  | O1 v => abort v
  | O2 Arrow => "Arr"
end
structure Term = MkTerm(structure Ops = TypOp structure Vars = Var)
structure Unify = MkUnify(structure Term = Term)
structure Term = Unify.T
structure Var = Term.V

signature STATICS = sig
  structure L : SYSTEMLC
  val synthtype : L.Exp.exp -> L.Typ.typ
end

structure Statics : STATICS = struct
  structure L = SystemLC
  structure T = L.Typ
  structure E = L.Exp
  val rec toOps = fn
    T.TVar v => Term.Var v
  | T.Arrow (t1,t2) => Term.T2(TypOp.Arrow, toOps t1, toOps t2)

  val rec fromOps = fn
    Term.Var v => T.TVar v
  | Term.T0(void) => abort void
  | Term.T1(void,_) => abort void
  | Term.T2(TypOp.Arrow, t1, t2) => T.Arrow(fromOps t1, fromOps t2)
  fun find v [] = raise Fail "Not found"
    | find v ((x,t)::L) = if x = v then t else find v L

  fun trysub v [] = T.TVar v
    | trysub v ((x,e)::L) = if x = v then fromOps e else trysub v L
  fun search e = case e of
    E.Var v => (T.TVar v, nil)
  | E.Lam (x,m) =>
    let
      val (t,A) = search m
    in
      (T.Arrow(trysub x A, t), A)
    end
  | E.Ap (e1,e2) =>
    let
      val (t1,A1) = search e1
      val (t2,A2) = search e2
      val b = Var.new "b"
      val t = T.Arrow(t2, T.TVar b)
      (* val () = print ("Unifying "^E.toString (E.Ap (e1,e2))^"\n" ^ T.toString t1 ^ "\n" ^ T.toString t ^ "\n") *)
      val A = A1 @ A2
      (* val () = print ("With " ^ String.concatWithMap "," (fn (x,e) => Var.toString x ^ " --> " ^ Term.toString e) A ^ "\n") *)
      val unif = Unify.unify ((toOps t,toOps t1)::(map (fn (a,b) => (toOps (T.TVar a), b)) A))
      (* val () = case unif of NONE => () | SOME L => print (String.concatWithMap "\n" (fn (x,e) => Var.toString x ^ " --> " ^ Term.toString e) L ^ "\n") *)
      val r = case unif of NONE => raise Fail "Could not unify" | SOME u => ((trysub b u), A1 @ A2 @ u)
      (* val () = print (T.toString (#1 r) ^ "\n") *)
    in
      r
    end
  fun synthtype e = #1 (search e)
end
structure StaticsTest = struct
  open Statics
  fun id () =
    let
      val x = Var.new "x"
    in
      L.Exp.Lam(x,L.Exp.Var x)
    end
  fun k () =
    let
      val x = Var.new "x"
      val y = Var.new "y"
    in
      L.Exp.Lam(x,L.Exp.Lam(y,L.Exp.Var x))
    end
  fun kk () = L.Exp.Ap(k(),k())
  fun ap (a,b) () = L.Exp.Ap(a(),b())
  fun c () =
    let
      val f = Var.new "f"
      val g = Var.new "g"
      val x = Var.new "x"
    in
      L.Exp.Lam(f,L.Exp.Lam(g,L.Exp.Lam(x,L.Exp.Ap(L.Exp.Var f,L.Exp.Ap(L.Exp.Var g,L.Exp.Var x)))))
    end
  fun run e = print (L.Typ.toString (synthtype (e())) ^ "\n")
end
open StaticsTest

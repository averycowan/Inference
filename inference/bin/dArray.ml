open Core;;

open Arr;;

module type D_ARRAY = sig
  type 'a t
  val create : unit -> 'a t
  val length : 'a t -> int
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a
  val replace : 'a t -> int -> 'a -> 'a
  val swap : 'a t -> int -> int -> unit
  (* val to_list : 'a t -> 'a list *)
end

module DArray : D_ARRAY = struct
  type 'a t = int ref * 'a option Arr.t ref
  let create () : 'a t = (ref 0, ref (Arr.create ~len:8 None))
  let length ((n, _) : 'a t) : int = !n
  let get ((_, a) : 'a t) (i : int) : 'a = Option.value_exn (Arr.get !a i)
  let set ((_, a) : 'a t) (i : int) (x : 'a) : unit = Arr.set !a i (Some x)
  let push ((n, a) : 'a t) (x : 'a) : unit =
    if Int.equal !n (Arr.length !a)
      then a := Arr.append !a (Arr.create ~len:!n None)
      else ()
    ;
    Arr.set !a !n (Some x);
    n := !n + 1
  let pop ((n, a) : 'a t) : 'a = 
    let value = Option.value_exn (Arr.get !a (!n - 1)) in
    n := !n - 1;
    value
  let replace ((_, a) : 'a t) (i : int) (x : 'a) : 'a =
    let value = Option.value_exn (Arr.get !a i) in
    Arr.set !a i (Some x);
    value
  let swap ((_, a) : 'a t) = Arr.swap !a
  (* let to_list ((n, a) : 'a t) : 'a list = List.map ~f:(fun x -> Option.value_exn x) (Arr.to_list (Arr.sub !a ~pos:0 ~len:(!n))) *)
end

open Core;;

module Arr : sig
  val total_cost : unit -> int

  type 'a t
  val create : len:int -> 'a -> 'a t
  val length : 'a t -> int
  val append : 'a t -> 'a t -> 'a t
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit
  val swap : 'a t -> int -> int -> unit

end = struct
  let cost_counter = ref 0
  let total_cost () = !cost_counter
  let incr i = cost_counter := !cost_counter + i

  type 'a t = 'a Array.t
  let create ~len x = incr len; Array.create ~len x
  let length a = incr 1; Array.length a
  let append a b = incr (Array.length a + Array.length b); Array.append a b
  let get a i = incr 1; Array.get a i
  let set a i x = incr 1; Array.set a i x
  let swap a i j = incr 2; Array.swap a i j
end
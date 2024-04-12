open Core;;
open Cmp;;
open DArray;;

module type HEAP = sig
  type 'a t
  val create : 'a Cmp.t -> 'a t
  val size : 'a t -> int
  val push : 'a t -> 'a -> unit
  val peek : 'a t -> 'a
  val peek_opt : 'a t -> 'a option
  val pop : 'a t -> 'a
  val pop_opt : 'a t -> 'a option
  (* val print_heap : ('a -> string) -> 'a t -> unit *)
end

module Heap : HEAP = struct
  type 'a t = 'a Cmp.t * 'a DArray.t
  let parent i = (i+1) / 2 - 1
  let childL i = 2 * i + 1
  let childR i = 2 * i + 2
  let create (cmp : 'a Cmp.t) : 'a t = (cmp, DArray.create ())
  let size (_, a) : int = DArray.length a
  let push ((cmp, a) : 'a t) (x : 'a) : unit = 
    DArray.push a x;
    let rec sift i : unit = if Int.equal 0 i then () else
      let p = parent i in
      match cmp (DArray.get a i) (DArray.get a p) with
      | LT -> DArray.swap a i p; sift p
      | _ -> ()
    in sift (DArray.length a - 1)
  let peek ((_, a) : 'a t) : 'a = DArray.get a 0
  let peek_opt (h : 'a t) : 'a option = if size h > 0 then Some (peek h) else None
  let pop ((cmp, a) : 'a t) : 'a =
    let head = DArray.get a 0 in
    DArray.set a 0 (DArray.pop a);
    let l = DArray.length a in
    let rec sift i : unit =
      let child = match childL i, childR i with
      | b, _ when b >= l -> 0
      | c, b when b >= l -> c
      | cL, cR -> ( match cmp (DArray.get a cL) (DArray.get a cR) with
        | LT -> cL
        | _ -> cR
      ) in
      if child = 0 then () else (
        match cmp (DArray.get a i) (DArray.get a child) with
      | GT -> DArray.swap a i child; sift child
      | _ -> ()
      )
    in sift 0;
    head
  let pop_opt (h : 'a t) : 'a option = if size h > 0 then Some (pop h) else None
  (* let print_heap (to_string : 'a -> string) ((_, a) : 'a t) : unit =
    let print_entry (i : int) (x : 'a) : unit = printf "%2d %2d %2d %2d: %s\n" i (parent i) (childL i) (childR i) (to_string x) in
    List.iteri ~f:print_entry (DArray.to_list a) *)
end
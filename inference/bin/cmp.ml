open Core;;

module Cmp : sig
  type order = LT | GT | EQ
  type 'a cmp = 'a -> 'a -> order
  type 'a t = 'a cmp
  val from_int_cmp : ('a -> 'a -> int) -> 'a -> 'a -> order
  val to_int_cmp : ('a -> 'a -> order) -> 'a -> 'a -> int
end = struct
  type order = LT | GT | EQ
  type 'a cmp = 'a -> 'a -> order
  type 'a t = 'a cmp
  let from_int_cmp (int_cmp : 'a -> 'a -> int) (x : 'a) (y : 'a) : order =
    match int_cmp x y with
    | a when a < 0 -> LT
    | a when a > 0 -> GT
    | _ -> EQ
  let to_int_cmp (cmp : 'a cmp) (x : 'a) (y : 'a) : int =
    match cmp x y with
    | LT -> -1
    | GT -> 1
    | EQ -> 0
end
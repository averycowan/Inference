open Core;;

module type TREE = sig
  type 'a t
  type ('a, 'tree) nodeview = {l : 'tree; x : 'a; r : 'tree}
  type 'a node = ('a, 'a t) nodeview
  

  val node : 'a node -> 'a t
  val empty : 'b -> 'a t
  val view : 'a t -> 'a node option

  val map : ('a -> 'b) -> 'a t -> 'b t

  val reduce : (('a, 'b) nodeview -> 'b) -> 'b -> 'a t -> 'b

  val depth : 'a t -> int
  val size : 'a t -> int

end
module Tree : TREE = struct
  type size_info = {n : int; d: int}
  type ('a, 'tree) nodeview = {l : 'tree; x : 'a; r : 'tree}
  
  type 'a node = ('a, 'a t) nodeview
  and 'a t =
  | Empty
  | Node of 'a node * size_info

  let size = function Empty -> 0 | Node (_, {n;d=_}) -> n
  let depth = function Empty -> 0 | Node (_, {n=_;d}) -> d
  
  let rec map (f : 'a -> 'b) : 'a t -> 'b t = function
  | Empty -> Empty
  | Node ({l;x;r}, nd) -> Node ({l=map f l; x=f x; r=map f r}, nd)

  let rec reduce (f : ('a, 'b) nodeview -> 'b) (z : 'b) : 'a t -> 'b = function
  | Empty -> z
  | Node ({l;x;r}, _nd) -> f {l=reduce f z l; x; r=reduce f z r}

  let node {l;x;r} = Node ({l;x;r}, {n=1+size l+size r;d=1+Int.max(depth l)(depth r)})
  let empty _ = Empty
  let view = function
  | Empty -> None
  | Node (n, _) -> Some n
end
module Void = struct
  type t = |
  [@@deriving equal,sexp,compare,show]

  let abort : t -> 'a = function _ -> .
end

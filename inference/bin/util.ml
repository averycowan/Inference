open Core;;

let rec zip ?(msg = "Zip mismatch") (l1:'a list) (l2:'b list) : ('a * 'b) list = match (l1, l2) with
| ([], []) -> []
| (x::xs, y::ys) -> (x,y) :: zip ~msg xs ys
| _ -> failwith msg


(* Find the tail of a list when the list is provided *)

let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: xs -> last xs ;;

let rec last_two = function
  | [] | [_] -> None
  | [ x; xs ] -> Some (x, xs)
  | x :: xs -> last_two xs;;

let rec nth n = function
  | [] -> None
  | head :: tail -> if n = 0 then Some head else nth (n - 1) tail;;

let length list =
  let rec len count = function
    | [] -> count
    | x :: xs -> len (count + 1) xs
in len 0 list;;

(* To reverse a list recusively, use an accumulator to hold the reversed list *)
let rev list =
  let rec reverse acc = function
    | [] -> acc
    | head :: tail -> reverse (head :: acc) tail
in reverse [] list;;

(* Find the reverse and compare *)
let is_palindrome list =
  rev list = list;;

(* Run Length Encoding, insert into the destination, and count remaining instances *)
let encode list =
  let rec encoder dest count = function
    | [] -> []
    | [ x ] -> (count + 1, x) :: dest
    | a :: (b :: _ as t) -> if a = b then encoder dest (count + 1) t
                       else encoder ((count + 1, a) :: dest) 0 t
  in rev (encoder [] 0 list)

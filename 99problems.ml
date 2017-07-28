(*
  99 Problems (solved) in OCaml
  https://ocaml.org/learn/tutorials/99problems.html

  Jordan Scales <scalesjordan@gmail.com>
  http://github.com/jdan/99-ocaml-problems
*)

(* 1. Write a function last : 'a list -> 'a option that returns the last element of a list *)
let rec last ls = match ls with
  | [] -> None
  | single :: [] -> Some single   (* alternatively, we can match [x] *)
  | _ :: tail -> last tail;;

assert (last [ 1 ; 2 ; 3 ] = Some 3);;
assert (last [ 10 ] = Some 10);;
assert (last [] = None);;

(* 2. Find the last but one (last and penultimate) elements of a list. (easy) *)
let rec last_two ls = match ls with
  | [] -> None
  | [x] -> None
  | [x ; y] -> Some (x, y)
  | _ :: tail -> last_two tail;;

assert (last_two [ "a" ; "b" ; "c" ; "d" ] = Some ("c", "d"));;
assert (last_two [ "a" ; "b" ] = Some ("a", "b"));;
assert (last_two [ "a" ] = None);;
assert (last_two [] = None);;

(* 3. Find the k'th element of a list. (easy) *)
let rec at idx ls = match (ls, idx) with
  | ([], _) -> None   (* cute that I can do this but maybe hard to read? *)
  | (head :: _, 1) -> Some head
  | (_ :: tail, idx) -> at (idx - 1) tail;;

assert (at 3 [ "a" ; "b"; "c"; "d"; "e" ] = Some "c");;
assert (at 3 [ "a" ] = None);;

(* 4. Find the number of elements of a list. (easy) *)
(* Bonus for a tail recursive solution. *)
let rec length ls =
  let rec inner ls acc = match ls with
    | [] -> acc
    | _ :: tail -> inner tail (acc + 1)

  in inner ls 0;;

assert (length [ "a" ; "b" ; "c"] = 3);;
assert (length [] = 0);;

(*
  99 Problems (solved) in OCaml
  https://ocaml.org/learn/tutorials/99problems.html

  Jordan Scales <scalesjordan@gmail.com>
  http://github.com/jdan/99-ocaml-problems
*)

(* 1. Write a function last : 'a list -> 'a option that returns the last element of a list *)
let rec last = function
  | [] -> None
  | single :: [] -> Some single   (* alternatively, we can match [x] *)
  | _ :: tail -> last tail;;

assert (last [ 1 ; 2 ; 3 ] = Some 3);;
assert (last [ 10 ] = Some 10);;
assert (last [] = None);;

(* 2. Find the last but one (last and penultimate) elements of a list. (easy) *)
let rec last_two = function
  | [] -> None
  | [x] -> None
  | [x ; y] -> Some (x, y)
  | _ :: tail -> last_two tail;;

assert (last_two [ "a" ; "b" ; "c" ; "d" ] = Some ("c", "d"));;
assert (last_two [ "a" ; "b" ] = Some ("a", "b"));;
assert (last_two [ "a" ] = None);;
assert (last_two [] = None);;

(* 3. Find the k'th element of a list. (easy) *)
let rec at idx = function
  | [] -> None
  | head :: tail ->
      if idx = 1
      then Some head
      else at (idx - 1) tail;;

assert (at 3 [ "a" ; "b"; "c"; "d"; "e" ] = Some "c");;
assert (at 3 [ "a" ] = None);;

(* 4. Find the number of elements of a list. (easy) *)
(* Bonus for a tail recursive solution. *)
let length ls =
  let rec inner acc = function
    | [] -> acc
    | _ :: tail -> inner (acc + 1) tail

  in inner 0 ls;;

assert (length [ "a" ; "b" ; "c"] = 3);;
assert (length [] = 0);;

(* 5. Reverse a list. (easy) *)
let rev ls =
  (* We'll pour everything into an accumulator *)
  let rec inner acc = function
    | [] -> acc
    | head :: tail -> inner (head :: acc) tail

  in inner [] ls;;

assert (rev [ "a" ; "b" ; "c" ] = [ "c" ; "b" ; "a" ]);;

(* 6. Find out whether a list is a palindrome. (easy) *)
let is_palindrome ls = (ls = rev ls);;

assert (is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ]);;
assert (not (is_palindrome [ "a" ; "b" ]));;

(* 7. Flatten a nested list structure. (medium) *)
(* There is no nested list type in OCaml, so we need to define one
    first. A node of a nested list is either an element, or a list of
    nodes. *)
type 'a node =
  | One of 'a
  | Many of 'a node list;;

let rec flatten = function
  | [] -> []
  | head :: tail -> match head with
    | One item -> item :: flatten tail
    | Many items -> List.append (flatten items) (flatten tail);;

assert (flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ] =
          [ "a" ; "b" ; "c" ; "d" ; "e" ]);;

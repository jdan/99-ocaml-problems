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
  | One item :: tail -> item :: flatten tail
  | Many items :: tail -> List.append (flatten items) (flatten tail);;

assert (flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ] =
          [ "a" ; "b" ; "c" ; "d" ; "e" ]);;

(* 8. Eliminate consecutive duplicates of list elements. (medium) *)
let compress ls =
  let rec inner last = function
    | [] -> []
    | head :: rest ->
      if head = last
      then inner last rest
      else head :: inner head rest

  in inner "" ls;;

assert (compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
          ["a"; "b"; "c"; "a"; "d"; "e"]);;

(* 9. Pack consecutive duplicates of list elements into sublists. (medium) *)
let pack ls =
  let rec inner last acc = function
    | [] -> [last :: acc]   (* flush acc *)
    | head :: rest ->
      if head = last
      then inner last (head :: acc) rest        (* add the element to the acc *)
      else (last :: acc) :: inner head [] rest  (* flush acc *)

  (* We need to trim off the initial [], added because our initial `last` is "" *)
  in match (inner "" [] ls) with
    | _ :: result -> result
    | [] -> [];;  (* shouldn't be necessary since inner won't return [] *)

assert (pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"] =
          [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
           ["e"; "e"; "e"; "e"]]);;

(* 10. Run-length encoding of a list. (easy) *)
let encode ls =
  let rec inner = function
    | [] -> []
    | (ch :: _ as head) :: tail -> (length head, ch) :: inner tail
    | _ -> []

  (* should probably just use List.map *)
  in inner (pack ls);;

assert (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
          [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]);;

(* 11. Modified run-length encoding. (easy) *)
type 'a rle =
  | One of 'a
  | Many of int * 'a;;

let encode2 ls =
  List.map
    (fun (count, ch) -> if count = 1
                        then One ch
                        else Many (count, ch))
    (encode ls);;

assert (encode2 ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
  [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
    Many (4, "e")]);;

(* 12. Decode a run-length encoded list. (medium) *)
let decode ls =
  let rec repeat item = function
    | 0 -> []
    | n -> item :: repeat item (n - 1) in

  let unpack = function
    | One ch -> [ ch ]
    | Many (count, ch) -> repeat ch count

  in List.flatten (List.map unpack ls);;

assert (decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")] =
     ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]);;

(* 13. Run-length encoding of a list (direct solution). (medium) *)
let encode3 ls =
  let rle_from_tuple = function
    | (1, ch) -> One ch
    | (n, ch) -> Many (n, ch) in

  let rec inner (n, ch) = function
    | [] -> [ rle_from_tuple (n, ch) ]   (* flush acc *)
    | head :: rest ->
      if head = ch
      then inner (n + 1, ch) rest               (* add the element to the acc *)
      else (rle_from_tuple (n, ch)) :: inner (1, head) rest  (* flush acc *)

  (* We need to trim off the initial [], added because our initial `last` is "" *)
  in match (inner (0, "") ls) with
    | _ :: result -> result
    | [] -> [];;  (* shouldn't be necessary since inner won't return [] *)

assert (encode3 ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
          [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";Many (4, "e")]);;

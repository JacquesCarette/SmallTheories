(* understanding the coding scheme for enumerating and generating theories*)

open Base
open Stdio


(*
Indcutive type for naturals: see nat.ml / nat.mli
  type Nat.t = Z | S of Nat.t
*)

(*
enumerate_theory : Nat.t -> string list
*)
(* A theory is the list of arities of its operations (after the carrier
   "a : type"). E.g. [0; 0] is

     "1(1|0)(2|0)"
     a : type
     b : a
     c : a
*)

type theory1 = Nat.t list

(* Multisorted

types (# of sorts) x names (# of ops) x arity
*)
type theoryn = Nat.t * Nat.t * Nat.t

(* size: the k-th operation (1-indexed) with arity t costs k + t + 1 *)
let size (ts : theory1) : Nat.t =
(* Haskell to ocaml (under Base library definition) translation
  sum                            $ zipWith (+)              [1..]    (map ((+1).toInteger) ts)*)
  List.sum (module Nat) ~f:Fn.id
    (List.mapi ~f:(fun i t -> let k = Nat.of_int_exn (i + 1) in Nat.(k + (t + one))) ts)

(* all theories of size exactly n *)
let enumerate (n : Nat.t) : theory1 list =
  (*
  k = position/name of the next operation,
  lo = adds order (nondecreasing) to arity doesn't duplicate,
   r = remaining size budget
   *)
  let rec construct (k : Nat.t) (lo : Nat.t) (r : Nat.t) : theory1 list =
    if Nat.(r = zero) then [ [] ]
    else if Nat.(r < k + one) then []
    else
      List.concat_map (Nat.range lo Nat.(r - k)) ~f:(fun t ->
          List.map (construct Nat.(k + one) t Nat.(r - (k + one + t))) ~f:(fun rest -> t :: rest))
  in
  construct Nat.one Nat.zero n

(* render as "1(1|t1)(2|t2)..." *)
(* leading 1 is for current 1 base type *)
let to_code (ts : theory1) : string =
  "1" ^ String.concat (List.mapi ts ~f:(fun i t -> "(" ^ Int.to_string (i + 1) ^ "|" ^ Nat.to_string t ^ ")"))

let enumerate_theory (n : Nat.t) : string list = List.map (enumerate n) ~f:to_code
let enumerates (n : Nat.t) : theory1 list  = List.concat_map (Nat.range Nat.zero Nat.(n + one)) ~f:enumerate
let enumerate_chunks (n : Nat.t) : (theory1 list) list = List.map (Nat.range Nat.zero Nat.(n + one)) ~f:enumerate
let enumerate_theories (n : Nat.t) : string list = List.concat_map (Nat.range Nat.zero Nat.(n + one)) ~f:enumerate_theory

(*
Convert to name
name : nat -> string
if x < 26 then char(ord('a')+x) else (char(ord('a')+x%26) ^ itos(x/26)))

unique name per index: a..z, then a1..z1, a2..z2, ... *)
let name (x : Nat.t) : string =
  let base = Nat.of_int_exn 26 in
  let c = String.of_char (Char.of_int_exn (Char.to_int 'a' + Nat.to_int Nat.(x % base))) in
  if Nat.(x < base) then c else c ^ Nat.to_string Nat.(x / base)

(* arity t  ->  "a -> a -> ... -> a"  (t arrows) *)
let arrows (t : Nat.t) : string =
  String.concat ~sep:" -> " (List.map (Nat.range Nat.zero Nat.(t + one)) ~f:(fun _ -> name Nat.zero))

(* index 0 is the sort; operation i gets name (i + 1)

Example enumerate 5;;
- : theory1 list = [[0; 0]; [3]]

[0; 0]
0: a : type (0)
1: b : a.   (+1+0+1)
2: c : a.   (+2+0+1)

[3]
0: a : type        (0)
1: b : a -> a -> a -> a (+1+3+1)

*)
let pretty_print (ts : theory1) : string =
  let sort = name Nat.zero ^ " : type" in
  let ops = List.mapi ~f:(fun i t -> name (Nat.of_int_exn (i + 1)) ^ " : " ^ arrows t) ts in
  String.concat ~sep:"\n" (sort :: ops)

let () =
  let default = Nat.of_int_exn 9 in
  let n =
    match Sys.get_argv () with
      (* | executable; nat |.    Maybe value; negative or non-numeric -> default *)
    | [| _; s |] -> Option.value (Nat.of_string_opt s) ~default
    | _ -> default
  in
  List.iter  ~f:(
    fun ts -> List.iter ~f:(fun t->
      printf "%s\n\n" (pretty_print t)) ts;
      printf "-------------------------------------\n\n") (enumerate_chunks n);

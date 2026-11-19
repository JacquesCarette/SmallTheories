open Base

type t = Z | S of t

let zero = Z
let one = S Z
let succ n = S n

(* conversions *)
let rec to_int = function
  | Z -> 0
  | S n -> 1 + to_int n

let of_int_exn (i : int) : t =
  if i < 0 then invalid_arg "Nat.of_int_exn: negative"
  else
    let rec go i = if i = 0 then Z else S (go (i - 1)) in
    go i

let of_int (i : int) : t option = if i < 0 then None else Some (of_int_exn i)
let to_string (n : t) : string = Int.to_string (to_int n)
let of_string_opt (s : string) : t option = Option.bind (Int.of_string_opt s) ~f:of_int

(* comparison *)
let rec compare (m : t) (n : t) : int =
  match m, n with
  | Z, Z -> 0
  | Z, S _ -> -1
  | S _, Z -> 1
  | S m', S n' -> compare m' n'

let equal m n = Int.equal (compare m n) 0

(* arithmetic *)
let rec add (m : t) (n : t) : t =
  match m with
  | Z -> n
  | S m' -> S (add m' n)

(* truncated subtraction *)
let rec monus (m : t) (n : t) : t =
  match m, n with
  | _, Z -> m
  | Z, S _ -> Z
  | S m', S n' -> monus m' n'

let rec divmod (n : t) (d : t) : t * t =
  match d with
  | Z -> invalid_arg "Nat: division by zero"
  | S _ ->
    if compare n d < 0 then (Z, n)
    else
      let q, r = divmod (monus n d) d in
      (S q, r)

let rec range (lo : t) (hi : t) : t list =
  if compare lo hi < 0 then lo :: range (S lo) hi else []

(* operators last, so nothing above accidentally uses them *)
let ( = ) = equal

let ( < ) m n = Int.( < ) (compare m n) 0
let ( <= ) m n = Int.( <= ) (compare m n) 0
let ( > ) m n = Int.( > ) (compare m n) 0
let ( >= ) m n = Int.( >= ) (compare m n) 0
let ( + ) = add
let ( - ) = monus
let ( / ) m n = fst (divmod m n)
let ( % ) m n = snd (divmod m n)

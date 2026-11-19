(*
Taking adavantage of the module system, suprised there isn't a nat module
granted it does depend on Int since things are considered as
ints when read

Inductive (unary) natural numbers with an Int-like interface,
   so code written against Int reads the same with Nat.( ... ). *)

type t = Z | S of t

val zero : t
val one : t
val succ : t -> t

(* conversions *)
val of_int : int -> t option          (* None for negatives *)
val of_int_exn : int -> t
val to_int : t -> int
val of_string_opt : string -> t option
val to_string : t -> string

(* comparison *)
val compare : t -> t -> int
val equal : t -> t -> bool
val ( = ) : t -> t -> bool
val ( < ) : t -> t -> bool
val ( <= ) : t -> t -> bool
val ( > ) : t -> t -> bool
val ( >= ) : t -> t -> bool

(* arithmetic *)
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t               (* truncated: m - n = zero when n > m *)
val ( / ) : t -> t -> t               (* raises Invalid_argument on zero divisor *)
val ( % ) : t -> t -> t

(* [lo, hi), like List.range *)
val range : t -> t -> t list

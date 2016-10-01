open String

type t = 
  | Const of bool
  | Var of string 
  | Not of t
  | Imp of (t * t)
  | Or of (t * t)
  | And of (t * t)


(** String representation of formula phi*)
val string_of_formula : t -> string

(** Transform formula phi to NNF(Negation normal form) of phi *)
val nnf_in_formula : t -> t

(** Transforms formula phi to CNF(Conjunctive Normal form) of phi *)
val cnf_in_formula : t -> t 

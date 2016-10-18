
(** Conjuctive Normal Form Type*)
type symbol = Variable of string | Constant of bool
type literal = (symbol * bool)
type clause = literal list
type t = clause list

(** Convert CNF to string*)
val string_of_cnf : t -> string

(** Removes duplicate symbols in clauses applies idempotence*)
val apply_idempotence : t -> t

(** Converts a Formula phi into a equivalent CNF*)
val cnf_of_formula : Formula.t -> t

(**Removes all clauses that contain tautologies*)
val remove_tautologies : t -> t

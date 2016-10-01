open Cnf 

type t

(**Checks if the given CNF is satisfiable*)
val sat_of_cnf : Cnf.t -> t

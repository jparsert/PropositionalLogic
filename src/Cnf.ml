open String
open Formula
open Utils

type symbol = Variable of string | Constant of bool
type literal = (symbol * bool)
type clause = literal list
type t = clause list

let rec string_of_symbol (sym:symbol) =
  match sym with
  | Variable v -> v
  | Constant c -> string_of_bool c

let rec string_of_literal ((s , b):literal) =
  if b then string_of_symbol s else not_symbol^ string_of_symbol s

let string_of_clause (_cl:clause) =
  let rec aux accum (cl:clause) =
    match cl with
    | [] -> accum
    | x::[] -> accum^" "^or_symbol^" "^(string_of_literal x)^") "
    | x::xs -> aux (accum^" "^or_symbol^" "^(string_of_literal x)) xs
  in
  match _cl with
  | x::xs -> aux (" ("^(string_of_literal x)) xs
  | _ -> "";;

let string_of_cnf (_cnf:t) =
  let rec aux accum = function
    | [] -> accum
    | x::xs -> aux (accum ^ " "^and_symbol^" "^(string_of_clause x)) xs
  in
  match _cnf with
  | x::xs -> aux (string_of_clause x) xs
  | _ -> ""

let remove_tautologies (cphi:t) =
  let rec contains_tautology = function
    | [] -> false
    | (sym , b) :: xs -> if contains (sym , not b) xs then true
        else contains_tautology xs
  in
  let rec traverse accum (_phi:t) =
    match _phi with
    | [] -> accum
    | x :: xs -> if contains_tautology x then traverse accum xs
                    else traverse (accum @ [x]) xs
  in
  traverse [] cphi

let apply_idempotence (cphi:t) =
  let remove_duplicates (phi:clause) =
    let rec aux_remove accum (p:clause) =
      match p with
      | [] -> accum
      | x::xs -> if (contains x xs) then (aux_remove accum xs)
                 else aux_remove (accum @ [x]) xs
    in aux_remove [] phi
  in map remove_duplicates cphi


let cnf_of_formula (_phi:Formula.t) =
  let rec literal_of_formula (phi:Formula.t) =
    match phi with
    | Not (Var v)-> (Variable v , false)
    | Not (Const c) -> (Constant c  , false)
    | Var v -> (Variable v , true)
    | Const c -> (Constant c , true)
    | _ -> failwith "Formula not in CNF"
  in
  let rec clause_of_formula (phi:Formula.t) =
    match phi with
    | Or(d1 , d2) -> (clause_of_formula d1) @ (clause_of_formula d2)
    | _ -> [literal_of_formula phi]
  in
  let rec aux cphi =
    match cphi with
    | And( And c1 , Or c2) ->   (aux (And c1)) @ [(clause_of_formula (Or c2))]
    | And( Or c1 , And c2 ) -> [(clause_of_formula (Or c1))] @ (aux (And c2))
    | And( And c1 , And c2) -> (aux (And c1)) @ (aux (And c2))
    | And( Or d1 , Or d2) -> [(clause_of_formula (Or d1))] @ [(clause_of_formula (Or d2))]
    | _  -> [clause_of_formula cphi]
  in
  aux (cnf_in_formula _phi)


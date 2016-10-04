open String

type t = 
  | Const of bool
  | Var of string 
  | Not of t
  | Imp of (t * t)
  | Or of (t * t)
  | And of (t * t)


let rec string_of_formula phi = 
  let  str = string_of_formula in 
  match phi with
  | Const c -> string_of_bool c
  | Var s -> s
  | Not (Var v) -> "!"^v
  | Not (Const c) -> "!"^ string_of_bool c 
  | Not fo -> ("!"^ str fo)
  | Or (a,b) -> ((str a) ^ " | " ^ (str b))
  | And (a,b) -> ((str a) ^ " & " ^ (str b) )
  | Imp (a , b) -> ( "(" ^ (str a) ^ " > " ^ (str b)^")" ) 


let nnf_in_formula form = 
  let rec aux_func t = 
    match t with
    | And (a , b) -> And(aux_func a , aux_func b)
    | Or(a ,  b) -> Or (aux_func a , aux_func b)
    | Imp(a  , b) -> Or(aux_func (Not a) , aux_func b)
    | Not (  Or(a , b )) -> And (aux_func (Not a) , aux_func (Not b) )
    | Not  ( And(a , b)) -> Or ( aux_func (Not a) , aux_func (Not b) )
    | Not ( Imp(a , b)) -> And ( aux_func a , aux_func (Not b) )
    | Not ( Not a) -> aux_func a
    | _ -> t
  in 
  aux_func form;;


let cnf_in_formula inp =  
  let rec dist =  function 
    | (And(a1,a2) , b) -> And (dist(a1 , b), dist(a2 , b)) 
    | (b , And(a1,a2)) -> And (dist(b , a1), dist(a2 , b))
    | (a, b) -> Or(a , b)
  in  
  let rec aux_cnf f = 
    match f with
    | Or(a, b) -> dist ( aux_cnf a , aux_cnf b )
    | And(a , b) -> And(aux_cnf a , aux_cnf b)
    | _ -> f
  in 
  aux_cnf (nnf_in_formula inp);;


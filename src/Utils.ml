
let not_symbol = "¬"

let and_symbol = "∧"

let or_symbol = "∨"

let implication_symbol = "→"

let is_empty = function
  | [] -> true
  | _ -> false

let head = function
  | [] -> failwith "Empty list"
  | x :: xs -> x

let remove_from_list a lst =
  let rec  aux accum = function
    |[] -> accum
    | x :: xs -> if x=a then aux accum xs else (aux (accum @ [x]) xs) 
  in
  aux [] lst

let rec contains e = function 
  | [] -> false
  | x::xs -> if e=x then true else contains e xs 

let map f lst = 
  let rec aux accum = function
    | [] -> accum 
    | x ::xs -> aux (accum @ [ f x ]) xs 
  in 
  aux [] lst 

let length lst = 
  let rec len l = function
    | [] -> l 
    | x :: xs -> len (l+1) xs 
  in 
  len 0 lst 

let drop_while f lst = 
  let rec aux accum = function
    | [] -> accum 
    | x :: xs -> if f x then aux accum xs else aux (accum @ [x]) xs
  in 
  aux [] lst

let filter f lst = 
  let rec aux accum = function 
    | [] -> accum
    | x :: xs -> if f x then aux (accum @ [x]) xs else aux accum xs
  in 
  aux [] lst

let list_to_maybe = function
  | [] -> None 
  | x :: xs -> Some x

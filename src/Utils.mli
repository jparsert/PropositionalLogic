

(**Not symbol when printing with unicode*)
val not_symbol : string

(**And symbol when printing with unicode*)
val and_symbol : string

(**Or symbol when printing with unicode*)
val or_symbol : string 

(**Implication symbol when printing with unicode*)
val implication_symbol : string

(**Checks if List is Empty. Returns true if it is empty false otherwise.*)
val is_empty : 'a list -> bool

(**typical Head function returns empty list on empty list*)
val head : 'a list -> 'a

(** Removes element a from list. And returns the list without a*)
val remove_from_list : 'a -> 'a list -> 'a list

(** Just the standard map definition*)
val map : ('a -> 'b) -> 'a list -> 'b list 

(** Determine whether a list contains an element or not*)
val contains : 'a -> 'a list -> bool

val drop_while : ('a -> bool) -> 'a list -> 'a list

val filter : ('a -> bool) -> 'a list -> 'a list 

val length : 'a list -> int

val list_to_maybe : 'a list -> 'a option 

open List

type literal = int
type clause = literal list
type problem = clause list

let negate i = i*(-1)

let propagate l p = map (fun cls -> filter (fun lit -> negate l != lit) cls)
                      (filter (fun cls -> not @@ mem l cls) p)

let rec solve = function
  | [] -> Some []
  | []::xs -> None
  | (l::cs)::xs as p -> match solve (propagate l p) with
                        | Some res -> Some (l::res)
                        | None -> match solve (propagate (negate l) p) with
                                               | Some res -> Some ((negate l)::res)
                                               | None -> None




let main =
  let example = [[1;2];[-1;-2];[-1];[-2]] in
  solve example

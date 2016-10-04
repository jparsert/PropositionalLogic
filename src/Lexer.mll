{
open Parser
}   

let white = [' ' '\t']+
let const = "true" | "false"
let var = ['a' -'z']+

rule read = 
        parse
        |white { read lexbuf}
        | const { CONST (bool_of_string (Lexing.lexeme lexbuf))} 
        | "!" { NOT }
        | "|" { OR }
        | "&" { AND }
        | "(" { LPAREN }
        | ")" { RPAREN }
        | ">" { IMP }
        | var { VAR (Lexing.lexeme lexbuf) }
        | eof { EOF }

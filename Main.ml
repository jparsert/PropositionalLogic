open Core.Std
open Formula
open Cnf
open Sat

let parse s = 
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in 
  ast


let handle_line s = 
  let parsed = parse s in
  let _ = print_endline (string_of_formula parsed) in 
  let _ = print_endline (string_of_cnf (cnf_of_formula parsed)) in 
  let _ = print_endline (string_of_cnf (apply_idempotence (cnf_of_formula parsed))) in
  let _ = print_endline (string_of_cnf (remove_tautologies (apply_idempotence (cnf_of_formula parsed))))  in 
  ()

let _ =
  (*let exp = parse "(!a & false & c > z | d | (x > !g > true))" in*)
  let file_to_read = "./Input.in" in
  let lines = In_channel.read_lines file_to_read in
  List.iter ~f:handle_line lines 

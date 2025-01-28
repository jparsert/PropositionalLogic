# PropositionalLogic and SAT

Small project that implements a highly unoptimized term structure for propositional logic terms (including parsing). I also implement a procedure to turn an arbitrary formula into CNF (but it's not Tseitin's Transformation, hence it can have exponential blowup). In addition, in the `sat.ml` file I implement the most rudimentart barebone unoptimized textbook DPLL algorithm.


## Dependencies
* Ocaml
* Menhir

## Build
* cd src
* make


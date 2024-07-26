(* acceptor to accept as much of fragment as possible *)
let accept_all string = Some string

(* testing grammar for make_matcher *)
type my_grammar_nonterminals =
  | Program | Statement | Assignment | Conditional | Expression | Value

(* homework 1 style grammar *)
let my_grammar =
  (Program,
  [
    Program, [N Statement; T ";"; N Program];
    Program, [N Statement];
    Statement, [N Assignment];
    Statement, [N Conditional];
    Assignment, [T "let"; T "x"; T "="; N Expression];
    Conditional, [T "if"; T "("; N Expression; T ")"; T "{"; N Program; T "}"; T "else"; T "{"; N Program; T "}"];
    Expression, [N Value];
    Expression, [N Value; T "||"; N Expression];
    Expression, [N Value; T "&&"; N Expression];
    Value, [T "true"];
    Value, [T "false"];
    Value, [T "x"]
    ])

(* convert to homework 2 style grammar *)
let my_grammar_func = convert_grammar my_grammar

(* accepts the valid part of the fragment and returns the invalid suffix *)
let small_frag = ["let"; "x"; "="; "true"; "||"]
let make_matcher_test = ((make_matcher my_grammar_func accept_all small_frag) = Some ["||"])

(* testing for make_parser *)

let small_frag_2 = ["let"; "x"; "="; "false"; "&&"; "true"; "||"; "false"]

let make_parser_test = match (make_parser my_grammar_func small_frag_2) with
| Some tree -> parse_tree_leaves tree = small_frag_2
| _ -> false

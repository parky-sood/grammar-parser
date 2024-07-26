type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

(* Helper function for convert_grammar, returns a pattern matcher to match rules with given nonterm *)
let rec convert_rules rules new_rules nonterm = match rules with
| [] -> new_rules
| rule::other_rules -> 
  match rule with
  | (lhs, rhs) -> 
    if (lhs = nonterm)
      then convert_rules other_rules (new_rules @ [rhs]) nonterm
    else convert_rules other_rules new_rules nonterm 

(* Implementation of convert_grammar, returns a curried pattern matcher with argument nonterm *)
let convert_grammar = function
| (start, rules) -> (start, (convert_rules rules []))

(* Implementation of parse_tree_leaves *)
let rec parse_tree_leaves = function
| Leaf x -> [x]
| Node (_, children) -> List.concat_map parse_tree_leaves children

(* Helper function to match alternative lists of nonterminals in grammar for make_matcher *)
let rec match_alt_list start_symbol production_func alt_list acceptor frag = match alt_list with
| [] -> None
| rule::other_rules ->
  match (match_rule rule production_func acceptor frag) with
  | None -> match_alt_list start_symbol production_func other_rules acceptor frag
  | x -> x

(* Helper function to match rules in alt_list for a given nonterminal for make_matcher *)
and match_rule rule production_func accept frag =
  match rule with
  | [] -> accept frag
  | symbol::other_symbols ->
    match symbol with
    | N nonterm -> 
      let new_accept = match_rule other_symbols production_func accept in
      match_alt_list nonterm production_func (production_func nonterm) new_accept frag
    | T term ->
      match frag with
      | hd::tl when (hd = term) -> match_rule other_symbols production_func accept tl
      | _ -> None

(* Implementation of make_matcher *)
let make_matcher = function
| (start, prod_func) -> 
  fun accept frag -> match_alt_list start prod_func (prod_func start) accept frag

(* Acceptor for the frag argument in a parse tree *)
let parse_tree_acceptor frag tree = match frag with
| [] -> Some tree
| _ -> None

(* Helper function for parsing alternative list of grammar parse tree *)
let rec parse_alt_list_tree start prod_func alt_list accept frag children = match alt_list with
| [] -> None
| rule::other_rules ->
  match (parse_rule_tree start prod_func rule accept frag children) with
  | None -> parse_alt_list_tree start prod_func other_rules accept frag children
  | Some x -> Some x

(* Helper function for parsing each rule in nonterminal's alternative list *)
and parse_rule_tree nonterm_node production_func rule accept frag children = match rule with
| [] -> accept frag (Node(nonterm_node, children))
| symb::symb_tl ->
  match symb with
  | N nonterm ->
    let new_accept new_frag new_tree = 
      parse_rule_tree nonterm_node production_func symb_tl accept new_frag (children @ [new_tree]) in
    parse_alt_list_tree nonterm production_func (production_func nonterm) new_accept frag []
  | T term ->
    match frag with
    | frag_h::frag_t when (frag_h = term) -> 
      parse_rule_tree nonterm_node production_func symb_tl accept frag_t (children @ [Leaf term])
    | _ -> None

(* Implementation of make_parser *)
let make_parser = function
| (start, prod_func) ->
  fun frag -> parse_alt_list_tree start prod_func (prod_func start) parse_tree_acceptor frag []
:- use_module(library(dcg/basics)).

% +---------------------------+
% | DCG for parsing the input |
% +---------------------------+

dcg_hands([])           --> eos, !.
dcg_hands([Hand|Hands]) --> dcg_hand(Hand), dcg_hands(Hands).

dcg_hand(hand(Cards, Bid)) --> dcg_cards(Cards), " ", dcg_bid(Bid), eol.

dcg_cards(Cards) --> string(Cards).

dcg_bid(Bid) --> number(Bid).

% +--------------+
% | Main program |
% +--------------+

read_input(Path, Input) :-
  phrase_from_file(dcg_hands(Input), Path).

main :-
  current_prolog_flag(argv, [Cmd|Args]),
  (
    Args = [] -> write('Usage: '), write(Cmd), writeln(' <path to input>');
    Args = [InputPath|_] ->
      read_input(InputPath, Input),
      writeln(Input)
  ).

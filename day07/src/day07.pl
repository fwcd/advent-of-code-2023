:- use_module(library(dcg/basics)).

main :-
  current_prolog_flag(argv, [Cmd|Args]),
  (
    Args = [] -> write('Usage: '), write(Cmd), writeln(' <path to input>');
    Args = [InputPath|_] -> writeln(InputPath) % TODO: Implement it!
  ).

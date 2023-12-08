:- use_module(library(dcg/basics)).
:- use_module(library(sort)).

% +-----------+
% | Utilities |
% +-----------+

% https://stackoverflow.com/questions/51968349/prolog-count-frequencies
freqs([], Out, Out).
freqs([C|Cs], Table, Out) :-
  (
    select((C, N), Table, Others),
    M is N + 1,
    freqs(Cs, [(C,M)|Others], Out),
    !
  ; freqs(Cs, [(C,1)|Table], Out)
  ).

freqs(Cs, Out) :- freqs(Cs, [], Out).

freq(C, N, Cs) :- freqs(Cs, Freqs), member((C, N), Freqs).

maximum([], Out, Out).
maximum([X|Xs], Acc, Out) :-
  (
     X > Acc
  -> maximum(Xs, X, Out)
  ;  maximum(Xs, Acc, Out)
  ).

maximum([X|Xs], Out) :- maximum(Xs, X, Out).

all_equal([]) :- !.
all_equal([_]) :- !.
all_equal([X,X|Xs]) :- all_equal([X|Xs]).

enumerate([], _, []) :- !.
enumerate([X|Xs], Index, [(Index,X)|Out]) :- Next is Index + 1, enumerate(Xs, Next, Out), !.

% +-------+
% | Hands |
% +-------+

card_value(0'A, 14) :- !.
card_value(0'K, 13) :- !.
card_value(0'Q, 12) :- !.
card_value(0'J, 11) :- !.
card_value(0'T, 10) :- !.
card_value(0'9,  9) :- !.
card_value(0'8,  8) :- !.
card_value(0'7,  7) :- !.
card_value(0'6,  6) :- !.
card_value(0'5,  5) :- !.
card_value(0'4,  4) :- !.
card_value(0'3,  3) :- !.
card_value(0'2,  2) :- !. % ' Dummy quote to fix syntax highlighting

card_values([], []).
card_values([C|Cs], [N|Ns]) :- card_value(C, N), card_values(Cs, Ns).

five_of_a_kind([C,C,C,C,C]).

four_of_a_kind(Cs, C1, C2) :-
  select(C2, Cs, [C1,C1,C1,C1]).

full_house(Cs, C1, C2) :-
  select(C1, Cs, Rest),
  select(C1, Rest, Rest2),
  select(C1, Rest2, [C2,C2]),
  !.

three_of_a_kind(Cs, C, Rest3) :-
  select(C, Cs, Rest),
  select(C, Rest, Rest2),
  select(C, Rest2, Rest3).

two_pair(Cs, C1, C2, Rest4) :-
  select(C1, Cs, Rest),
  select(C1, Rest, Rest2),
  select(C2, Rest2, Rest3),
  select(C2, Rest3, Rest4).

one_pair(Cs, C, Rest2) :-
  select(C, Cs, Rest),
  select(C, Rest, Rest2).

high_card(Cs, C) :-
  card_values(Cs, Ns),
  maximum(Ns, N),
  card_value(C, N).

hand_value(Cs, [7])    :- five_of_a_kind(Cs), !.
hand_value(Cs, [6|Ns]) :- four_of_a_kind(Cs, _, _), card_values(Cs, Ns), !.
hand_value(Cs, [5|Ns]) :- full_house(Cs, _, _), card_values(Cs, Ns), !.
hand_value(Cs, [4|Ns]) :- three_of_a_kind(Cs, _, _), card_values(Cs, Ns), !.
hand_value(Cs, [3|Ns]) :- two_pair(Cs, _, _, _), card_values(Cs, Ns), !.
hand_value(Cs, [2|Ns]) :- one_pair(Cs, _, _), card_values(Cs, Ns), !.
hand_value(Cs, [1|Ns]) :- high_card(Cs, _), card_values(Cs, Ns), !.

compare_hands(Delta, hand(Cs1, _), hand(Cs2, _)) :-
  hand_value(Cs1, V1),
  hand_value(Cs2, V2),
  compare(Delta, V1, V2).

sort_hands(Hands, Sorted) :- predsort(compare_hands, Hands, Sorted).

rank_hands(Hands, Ranked) :-
  sort_hands(Hands, Sorted),
  enumerate(Sorted, 1, Ranked).

total_winnings([], Total, Total) :- !.
total_winnings([(Rank,hand(_,Bid))|Rest], Acc, Total) :-
  Acc2 is Acc + Rank * Bid,
  total_winnings(Rest, Acc2, Total).

total_winnings(Ranked, Total) :- total_winnings(Ranked, 0, Total).

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

read_input(Path, Hands) :-
  phrase_from_file(dcg_hands(Hands), Path).

main :-
  current_prolog_flag(argv, [Cmd|Args]),
  (
    Args = [] -> write('Usage: '), write(Cmd), writeln(' <path to input>');
    Args = [InputPath|_] ->
      read_input(InputPath, Hands),
      rank_hands(Hands, Ranked),
      total_winnings(Ranked, Total),
      writeln(Ranked),
      write('Part 1: '), write(Total), nl
  ).

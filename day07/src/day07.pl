:- use_module(library(dcg/basics)).

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
% | Cards |
% +-------+

card_value1(0'A, 14).
card_value1(0'K, 13).
card_value1(0'Q, 12).
card_value1(0'J, 11).
card_value1(0'T, 10).
card_value1(0'9,  9).
card_value1(0'8,  8).
card_value1(0'7,  7).
card_value1(0'6,  6).
card_value1(0'5,  5).
card_value1(0'4,  4).
card_value1(0'3,  3).
card_value1(0'2,  2).

card_value2(0'J, 1) :- !.
card_value2(C,   N) :- card_value1(C, N).

is_card(C) :- card_value1(C, _).

% +-------+
% | Hands |
% +-------+

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

high_card(CV, Cs, C) :-
  maplist(CV, Cs, Ns),
  maximum(Ns, N),
  call(CV, C, N).

hand_type( _, Cs, 7) :- five_of_a_kind(Cs), !.
hand_type( _, Cs, 6) :- four_of_a_kind(Cs, _, _), !.
hand_type( _, Cs, 5) :- full_house(Cs, _, _), !.
hand_type( _, Cs, 4) :- three_of_a_kind(Cs, _, _), !.
hand_type( _, Cs, 3) :- two_pair(Cs, _, _, _), !.
hand_type( _, Cs, 2) :- one_pair(Cs, _, _), !.
hand_type(CV, Cs, 1) :- high_card(CV, Cs, _), !.

hand_value1(Cs, [T|Ns]) :-
  hand_type(card_value1, Cs, T),
  maplist(card_value1, Cs, Ns).

compare_hands1(Delta, hand(Cs1, _), hand(Cs2, _)) :-
  hand_value1(Cs1, V1),
  hand_value1(Cs2, V2),
  compare(Delta, V1, V2).

instantiate_joker(0'J, C) :- !, is_card(C).
instantiate_joker(C, C).

instantiate_jokers(Cs, Cs2) :- maplist(instantiate_joker, Cs, Cs2).

hand_value2_candidate(Cs, [T|Ns]) :-
  instantiate_jokers(Cs, Cs2),
  hand_type(card_value2, Cs2, T),
  maplist(card_value2, Cs, Ns).

hand_value2(Cs, MaxV) :-
  % Use `order_by` from `library(solution_sequences)`: https://stackoverflow.com/a/42593823/19890279
  findall(V, order_by([desc(V)], hand_value2_candidate(Cs, V)), [MaxV|_]).

compare_hands2(Delta, hand(Cs1, _), hand(Cs2, _)) :-
  hand_value2(Cs1, V1),
  hand_value2(Cs2, V2),
  compare(Delta, V1, V2).

rank_hands(CH, Hands, Ranked) :-
  predsort(CH, Hands, Sorted),
  enumerate(Sorted, 1, Ranked).

% +-------+
% | Parts |
% +-------+

total_winnings([], Total, Total) :- !.
total_winnings([(Rank,hand(_,Bid))|Rest], Acc, Total) :-
  Acc2 is Acc + Rank * Bid,
  total_winnings(Rest, Acc2, Total).

total_winnings(Ranked, Total) :- total_winnings(Ranked, 0, Total).

compute_result(CH, Hands, Result) :-
  rank_hands(CH, Hands, Ranked),
  total_winnings(Ranked, Result).

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

      compute_result(compare_hands1, Hands, Part1),
      write('Part 1: '), write(Part1), nl,

      compute_result(compare_hands2, Hands, Part2),
      write('Part 2: '), write(Part2), nl
  ).

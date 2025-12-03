:- module(ai, [
    ia/3,
    ia_random/3,
    possible_moves/2
]).

:- use_module(library(random)).


% possible_moves(+Board, -Moves)
% Moves = liste des colonnes valides (ex : [1,3,5])
possible_moves(Board, Moves) :-
    findall(Col, valid_move(Board, Col), Moves).

% ia_random(+Board, -Move, +Player)
ia_random(Board, Move, _Player) :-
    possible_moves(Board, Moves),
    random_member(Move, Moves).




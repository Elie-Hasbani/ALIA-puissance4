:- module(utils, [
    valid_move/2
]).

:- use_module(library(clpfd)).   % pour transpose/2 et between/3

valid_move(Board, Col) :-
    between(1, 7, Col),              % génère Col = 1..7
    transpose(Board, Transposed),
    nth1(Col, Transposed, Column),
    member(e, Column).               % la colonne contient au moins un 'e'

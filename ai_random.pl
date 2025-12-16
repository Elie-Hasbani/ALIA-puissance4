:- module(ai_random, [
    ia_random/3,
    possible_moves/2
]).

:- use_module(library(random)).
:- use_module(utils).

/*
    possible_moves(+Board, -Moves)
    Moves = liste des colonnes valides (ex : [1,3,5])
*/
possible_moves(Board, Moves) :-
    findall(Col, valid_move(Board, Col), Cols),
    sort(Cols, Moves).   % on enlève d’éventuels doublons, par sécurité

/*
    ia_random(+Board, -Move, +Player)

    Choisit un coup aléatoire parmi les colonnes valides.
*/
ia_random(Board, Move, _Player) :-
    possible_moves(Board, Moves),
    Moves \= [],               % s'assurer qu'il existe au moins un coup possible
    random_member(Move, Moves).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%         PUISSANCE 4 - DEUX JOUEURS HUMAINS        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ------------------------------
%  1- INITIALISATION DU PLATEAU
% ------------------------------

% Initialise un plateau vide (6 lignes x 7 colonnes)
% 'e' représente une case vide (empty)
init_board([
    [e, e, e, e, e, e, e],
    [e, e, e, e, e, e, e],
    [e, e, e, e, e, e, e],
    [e, e, e, e, e, e, e],
    [e, e, e, e, e, e, e],
    [e, e, e, e, e, e, e]
]).

% ------------------------------
%  2- AFFICHAGE DU PLATEAU
% ------------------------------

display_board(Board) :-
    nl,
    write('+---+---+---+---+---+---+---+'), nl,
    write('| 1 | 2 | 3 | 4 | 5 | 6 | 7 |'), nl,
    write('+---+---+---+---+---+---+---+'), nl,
    display_all_rows(Board),
    write('+---+---+---+---+---+---+---+'), nl.

display_all_rows([]).
display_all_rows([Row|Rest]) :-
    display_one_row(Row),
    display_all_rows(Rest).

display_one_row(Row) :-
    write('|'),
    display_row_cells(Row),
    nl.

display_row_cells([]).
display_row_cells([Cell|Rest]) :-
    write(' '),
    write(Cell),
    write(' |'),
    display_row_cells(Rest).

% ------------------------------
%  3- INSERTION D'UN PION
% ------------------------------

% Insère un pion dans une colonne (le pion tombe au plus bas)
insert_token(Board, Col, Player, NewBoard) :-
    Col >= 1, Col =< 7,
    transpose(Board, Transposed),
    nth1(Col, Transposed, Column),
    reverse(Column, ReversedColumn),
    insert_in_reversed_column(ReversedColumn, Player, UpdatedReversedColumn),
    reverse(UpdatedReversedColumn, UpdatedColumn),
    replace_nth(Transposed, Col, UpdatedColumn, UpdatedTransposed),
    transpose(UpdatedTransposed, NewBoard).

% Insère le pion dans la première case vide (en partant du bas)
insert_in_reversed_column([e|Rest], Player, [Player|Rest]) :- !.
insert_in_reversed_column([Cell|Rest], Player, [Cell|UpdatedRest]) :-
    Cell \= e,
    insert_in_reversed_column(Rest, Player, UpdatedRest).

% ------------------------------
%  4- VALIDATION DES COUPS
% ------------------------------

% Vérifie si une colonne est valide et non pleine
valid_move(Board, Col) :-
    integer(Col),
    Col >= 1,
    Col =< 7,
    transpose(Board, Transposed),
    nth1(Col, Transposed, Column),
    member(e, Column).

% ------------------------------
%  5- DÉTECTION DE VICTOIRE
% ------------------------------

% Un joueur gagne s'il a 4 pions alignés
win(Board, Player) :-
    (   horizontal_win(Board, Player)
    ;   vertical_win(Board, Player)
    ;   diagonal_win(Board, Player)
    ).

% Victoire horizontale (4 pions alignés sur une ligne)
horizontal_win(Board, Player) :-
    member(Row, Board),
    consecutive_four(Row, Player).

% Victoire verticale (4 pions alignés sur une colonne)
vertical_win(Board, Player) :-
    transpose(Board, Transposed),
    member(Column, Transposed),
    consecutive_four(Column, Player).

% Victoire diagonale (4 pions alignés en diagonale)
diagonal_win(Board, Player) :-
    (   diagonal_desc(Board, Player)
    ;   diagonal_asc(Board, Player)
    ).

% Vérifie 4 éléments consécutifs identiques dans une liste
consecutive_four(List, Player) :-
    append(_, [Player, Player, Player, Player|_], List).

% Diagonales descendantes (\)
diagonal_desc(Board, Player) :-
    between(1, 6, Row),
    between(1, 7, Col),
    check_diagonal_desc(Board, Row, Col, Player, 0).

check_diagonal_desc(_, Row, Col, _, 4) :- !.
check_diagonal_desc(Board, Row, Col, Player, Count) :-
    Row =< 6, Col =< 7,
    nth1(Row, Board, Line),
    nth1(Col, Line, Cell),
    Cell == Player,
    Count1 is Count + 1,
    Row1 is Row + 1,
    Col1 is Col + 1,
    check_diagonal_desc(Board, Row1, Col1, Player, Count1).

% Diagonales ascendantes (/)
diagonal_asc(Board, Player) :-
    between(1, 6, Row),
    between(1, 7, Col),
    check_diagonal_asc(Board, Row, Col, Player, 0).

check_diagonal_asc(_, Row, Col, _, 4) :- !.
check_diagonal_asc(Board, Row, Col, Player, Count) :-
    Row >= 1, Col =< 7,
    nth1(Row, Board, Line),
    nth1(Col, Line, Cell),
    Cell == Player,
    Count1 is Count + 1,
    Row1 is Row - 1,
    Col1 is Col + 1,
    check_diagonal_asc(Board, Row1, Col1, Player, Count1).

% ------------------------------
%  6- DÉTECTION DE MATCH NUL
% ------------------------------

% Le plateau est plein si aucune case vide n'existe
board_full(Board) :-
    \+ (member(Row, Board), member(e, Row)).

% ------------------------------
%  7- ALTERNANCE DES JOUEURS
% ------------------------------

next_player(x, o).
next_player(o, x).

% ------------------------------
%  8- BOUCLE PRINCIPALE DU JEU
% ------------------------------

% Démarre le jeu
play :-
    nl,
    write('========================================'), nl,
    write('     BIENVENUE AU PUISSANCE 4 !'), nl,
    write('========================================'), nl,
    write('Joueur X commence'), nl,
    init_board(Board),
    display_board(Board),
    play_turn(Board, x).

% Tour de jeu
play_turn(Board, Player) :-
    % Vérifier si match nul
    (   board_full(Board)
    ->  nl,
        write('========================================'), nl,
        write('        MATCH NUL !'), nl,
        write('========================================'), nl,
        ask_replay
    ;   % Sinon, demander le coup du joueur
        nl,
        format('--- Tour du Joueur ~w ---~n', [Player]),
        format('Joueur ~w, choisissez une colonne (1-7): ', [Player]),
        read(Col),
        (   valid_move(Board, Col)
        ->  % Coup valide
            insert_token(Board, Col, Player, NewBoard),
            nl,
            write('--- Après votre coup ---'), nl,
            display_board(NewBoard),
            (   win(NewBoard, Player)
            ->  % Victoire !
                nl,
                write('========================================'), nl,
                format('    JOUEUR ~w GAGNE !!!~n', [Player]),
                write('========================================'), nl,
                ask_replay
            ;   % Continuer le jeu
                next_player(Player, NextPlayer),
                play_turn(NewBoard, NextPlayer)
            )
        ;   % Coup invalide
            nl,
            write('!! COUP INVALIDE !! La colonne est pleine ou invalide.'), nl,
            write('Veuillez choisir une autre colonne.'), nl,
            play_turn(Board, Player)
        )
    ).

% Demander si les joueurs veulent rejouer
ask_replay :-
    nl,
    write('Voulez-vous rejouer ? (o/n): '),
    read(Choice),
    (   (Choice = o ; Choice = 'o')
    ->  play
    ;   (Choice = n ; Choice = 'n')
    ->  nl,
        write('Merci d\'avoir joué ! Au revoir.'), nl
    ;   write('Réponse invalide. Veuillez répondre par o ou n.'), nl,
        ask_replay
    ).

% ------------------------------
%  9- UTILITAIRES
% ------------------------------

% Transpose une matrice (lignes <-> colonnes)
transpose([[]|_], []) :- !.
transpose(Matrix, [Col|Cols]) :-
    maplist(head_tail, Matrix, Col, RestMatrix),
    transpose(RestMatrix, Cols).

head_tail([H|T], H, T).

% Remplace le N-ième élément d'une liste
replace_nth([_|T], 1, X, [X|T]) :- !.
replace_nth([H|T], N, X, [H|R]) :-
    N > 1,
    N1 is N - 1,
    replace_nth(T, N1, X, R).

% ------------------------------
%  10- POINT D'ENTRÉE
% ------------------------------

% Pour lancer le jeu, tapez: ?- play.

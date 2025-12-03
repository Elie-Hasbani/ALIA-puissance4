%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%         PUISSANCE 4 - DEUX JOUEURS HUMAINS        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ------------------------------
%  1- INITIALISATION DU PLATEAU
% ------------------------------

% Initialise un plateau vide (6 lignes x 7 colonnes)
% '-' représente une case vide
initialiser_plateau([
    [-, -, -, -, -, -, -],
    [-, -, -, -, -, -, -],
    [-, -, -, -, -, -, -],
    [-, -, -, -, -, -, -],
    [-, -, -, -, -, -, -],
    [-, -, -, -, -, -, -]
]).

% ------------------------------
%  2- AFFICHAGE DU PLATEAU
% ------------------------------

afficher_plateau(Plateau) :-
    nl,
    write('+---+---+---+---+---+---+---+'), nl,
    write('| 1 | 2 | 3 | 4 | 5 | 6 | 7 |'), nl,
    write('+---+---+---+---+---+---+---+'), nl,
    afficher_toutes_lignes(Plateau),
    write('+---+---+---+---+---+---+---+'), nl.

afficher_toutes_lignes([]).  %liste vide
afficher_toutes_lignes([Ligne|Reste]) :-  %Cas général : il reste au moins une ligne
    afficher_une_ligne(Ligne),   % affiche la première ligne
    afficher_toutes_lignes(Reste).   % appelle récursivement pour le reste des lignes

afficher_une_ligne(Ligne) :- 
    write('|'),          
    afficher_cellules_ligne(Ligne),  % affiche les cellules de la ligne
    nl.

afficher_cellules_ligne([]). % cellule vide
afficher_cellules_ligne([Cellule|Reste]) :-
    write(' '),
    write(Cellule),
    write(' |'),
    afficher_cellules_ligne(Reste).

% ------------------------------
%  3- INSERTION D'UN PION
% ------------------------------

% Insère un pion dans une colonne (le pion tombe au plus bas)
inserer_pion(Plateau, Col, Joueur, NouveauPlateau) :-
    Col >= 1, Col =< 7,
    transposer(Plateau, Transpose),
    nth1(Col, Transpose, Colonne),
    reverse(Colonne, ColonneInversee),
    inserer_dans_colonne_inversee(ColonneInversee, Joueur, ColonneInverseeMaj),
    reverse(ColonneInverseeMaj, ColonneMaj),
    remplacer_nieme(Transpose, Col, ColonneMaj, TransposeMaj),
    transposer(TransposeMaj, NouveauPlateau).

% Insère le pion dans la première case vide (en partant du bas)
inserer_dans_colonne_inversee([-|Reste], Joueur, [Joueur|Reste]) :- !.
inserer_dans_colonne_inversee([Cellule|Reste], Joueur, [Cellule|ResteMaj]) :-
    Cellule \= -,
    inserer_dans_colonne_inversee(Reste, Joueur, ResteMaj).

% ------------------------------
%  4- VALIDATION DES COUPS
% ------------------------------

% Vérifie si une colonne est valide et non pleine
coup_valide(Plateau, Col) :-
    integer(Col),
    Col >= 1,
    Col =< 7,
    transposer(Plateau, Transpose),
    nth1(Col, Transpose, Colonne),
    member(-, Colonne).

% ------------------------------
%  5- DÉTECTION DE VICTOIRE
% ------------------------------

% Un joueur gagne s'il a 4 pions alignés
victoire(Plateau, Joueur) :-
    (   victoire_horizontale(Plateau, Joueur)
    ;   victoire_verticale(Plateau, Joueur)
    ;   victoire_diagonale(Plateau, Joueur)
    ).

% Victoire horizontale (4 pions alignés sur une ligne)
victoire_horizontale(Plateau, Joueur) :-
    member(Ligne, Plateau),
    quatre_consecutifs(Ligne, Joueur).

% Victoire verticale (4 pions alignés sur une colonne)
victoire_verticale(Plateau, Joueur) :-
    transposer(Plateau, Transpose),
    member(Colonne, Transpose),
    quatre_consecutifs(Colonne, Joueur).

% Victoire diagonale (4 pions alignés en diagonale)
victoire_diagonale(Plateau, Joueur) :-
    (   diagonale_desc(Plateau, Joueur)
    ;   diagonale_asc(Plateau, Joueur)
    ).

% Vérifie 4 éléments consécutifs identiques dans une liste
quatre_consecutifs(Liste, Joueur) :-
    append(_, [Joueur, Joueur, Joueur, Joueur|_], Liste).

% Diagonales descendantes (\)
diagonale_desc(Plateau, Joueur) :-
    between(1, 6, Ligne),
    between(1, 7, Col),
    verifier_diagonale_desc(Plateau, Ligne, Col, Joueur, 0).

verifier_diagonale_desc(_, Ligne, Col, _, 4) :- !.
verifier_diagonale_desc(Plateau, Ligne, Col, Joueur, Compteur) :-
    Ligne =< 6, Col =< 7,
    nth1(Ligne, Plateau, LignePlateau),
    nth1(Col, LignePlateau, Cellule),
    Cellule == Joueur,
    Compteur1 is Compteur + 1,
    Ligne1 is Ligne + 1,
    Col1 is Col + 1,
    verifier_diagonale_desc(Plateau, Ligne1, Col1, Joueur, Compteur1).

% Diagonales ascendantes (/)
diagonale_asc(Plateau, Joueur) :-
    between(1, 6, Ligne),
    between(1, 7, Col),
    verifier_diagonale_asc(Plateau, Ligne, Col, Joueur, 0).

verifier_diagonale_asc(_, Ligne, Col, _, 4) :- !.
verifier_diagonale_asc(Plateau, Ligne, Col, Joueur, Compteur) :-
    Ligne >= 1, Col =< 7,
    nth1(Ligne, Plateau, LignePlateau),
    nth1(Col, LignePlateau, Cellule),
    Cellule == Joueur,
    Compteur1 is Compteur + 1,
    Ligne1 is Ligne - 1,
    Col1 is Col + 1,
    verifier_diagonale_asc(Plateau, Ligne1, Col1, Joueur, Compteur1).

% ------------------------------
%  6- DÉTECTION DE MATCH NUL
% ------------------------------

% Le plateau est plein si aucune case vide n'existe
plateau_plein(Plateau) :-
    \+ (member(Ligne, Plateau), member(-, Ligne)).

% ------------------------------
%  7- ALTERNANCE DES JOUEURS
% ------------------------------

joueur_suivant(x, o).
joueur_suivant(o, x).

% ------------------------------
%  8- BOUCLE PRINCIPALE DU JEU
% ------------------------------

% Démarre le jeu
jouer :-
    nl,
    write('========================================'), nl,
    write('     BIENVENUE AU PUISSANCE 4 !'), nl,
    write('========================================'), nl,
    write('Joueur X commence'), nl,
    initialiser_plateau(Plateau),
    afficher_plateau(Plateau),
    jouer_tour(Plateau, x).

% Tour de jeu
jouer_tour(Plateau, Joueur) :-
    % Vérifier si match nul
    (   plateau_plein(Plateau)
    ->  nl,
        write('========================================'), nl,
        write('        MATCH NUL !'), nl,
        write('========================================'), nl,
        demander_rejouer
    ;   % Sinon, demander le coup du joueur
        nl,
        format('--- Tour du Joueur ~w ---~n', [Joueur]),
        format('Joueur ~w, choisissez une colonne (1-7): ', [Joueur]),
        read(Col),
        (   coup_valide(Plateau, Col)
        ->  % Coup valide
            inserer_pion(Plateau, Col, Joueur, NouveauPlateau),
            nl,
            write('--- Après votre coup ---'), nl,
            afficher_plateau(NouveauPlateau),
            (   victoire(NouveauPlateau, Joueur)
            ->  % Victoire !
                nl,
                write('========================================'), nl,
                format('    JOUEUR ~w GAGNE !!!~n', [Joueur]),
                write('========================================'), nl,
                demander_rejouer
            ;   % Continuer le jeu
                joueur_suivant(Joueur, JoueurSuivant),
                jouer_tour(NouveauPlateau, JoueurSuivant)
            )
        ;   % Coup invalide
            nl,
            write('!! COUP INVALIDE !! La colonne est pleine ou invalide.'), nl,
            write('Veuillez choisir une autre colonne.'), nl,
            jouer_tour(Plateau, Joueur)
        )
    ).

% Demander si les joueurs veulent rejouer
demander_rejouer :-
    nl,
    write('Voulez-vous rejouer ? (o/n): '),
    read(Choix),
    (   (Choix = o ; Choix = 'o')
    ->  jouer
    ;   (Choix = n ; Choix = 'n')
    ->  nl,
        write('Merci d\'avoir joué ! Au revoir.'), nl
    ;   write('Réponse invalide. Veuillez répondre par o ou n.'), nl,
        demander_rejouer
    ).

% ------------------------------
%  9- UTILITAIRES
% ------------------------------

% Transpose une matrice (lignes <-> colonnes)
transposer([[]|_], []) :- !.
transposer(Matrice, [Col|Cols]) :-
    maplist(tete_queue, Matrice, Col, ResteMatrice),
    transposer(ResteMatrice, Cols).

tete_queue([T|Q], T, Q).

% Remplace le N-ième élément d'une liste
remplacer_nieme([_|Q], 1, X, [X|Q]) :- !.
remplacer_nieme([T|Q], N, X, [T|R]) :-
    N > 1,
    N1 is N - 1,
    remplacer_nieme(Q, N1, X, R).

% ------------------------------
%  10- POINT D'ENTRÉE
% ------------------------------

% Pour lancer le jeu, tapez: ?- jouer.

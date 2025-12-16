%   IA Alpha-Beta (Minimax) 

% charrge le fichier puissance4 pour avoir les prédicats de base
:- [puissance4].


% Choisir difficulté pour l'ia avant de commencer qui sera passé en paramètre dans la boucle de jeu
choisir_difficulte_alphabeta(Profondeur) :-
    nl,
    write('Choisissez la difficulté:'), nl,
    write('1. Facile (profondeur 4)'), nl,
    write('2. Moyen (profondeur 6)'), nl,
    write('3. Difficile (profondeur 8)'), nl,
    write('Votre choix: '),
    read(Choix),
    niveau_alphabeta(Choix, Profondeur).

niveau_alphabeta(1, 4).
niveau_alphabeta(2, 6).
niveau_alphabeta(3, 8).
niveau_alphabeta(_, 4).


%  API : meilleur coup aka meilleure colonne à jouer

best_move_alpha_beta(Plateau, Joueur, Profondeur, BestCol) :-
    % On commence par générer tous les coups possibles 
    findall(C, (between(1, 7, C), coup_valide(Plateau, C)), Moves),

    ( Moves = [] ->
        BestCol = nil
        % on trie les coups : centre → côtés
    ;   order_moves_center_first(Moves, OrderedMoves),
        % On Teste chaque coup avec Alpha-Beta et garde le meilleur score
        best_move_loop(OrderedMoves, Plateau, Joueur, Profondeur, -1000000000, nil, BestCol)
    ).

best_move_loop([], _Plateau, _Joueur, _Depth, _BestScore, BestCol, BestCol).    % il n'y a plus de coups à tester et on retourne la meilleure colonne trouvée jusqu'ici
    best_move_loop([Move|Moves], Plateau, Joueur, Depth, BestScoreSoFar, BestColSoFar, BestCol) :- % On teste le coup Move dans le but d'avoir bestscore et bestcol(récursif))
    inserer_pion(Plateau, Move, Joueur, NewPlateau), % insère le pion pour le coup testé
    joueur_suivant(Joueur, NextPlayer), % passe au joueur suivant
    D1 is Depth - 1, % on descend d'un niveau dans l'arbre de recherche
    %alphabeta calcule la valeur du coup testé
    alphabeta(NewPlateau, NextPlayer, D1, -1000000000, 1000000000, ValNeg),
    Score is -ValNeg, % on inverse le score car on utilise la technique negamax
    %comparaison avec le meilleur score jusqu'ici
    %si meilleur on met à jour bestscore et bestcol sinon on garde les anciens
    ( Score > BestScoreSoFar ->
        NewBestScore = Score,
        NewBestCol   = Move
    ;   NewBestScore = BestScoreSoFar,
        NewBestCol   = BestColSoFar
    ),
    % appel récursif pour tester les autres coups et à la fin retourner la meilleure colonne
    best_move_loop(Moves, Plateau, Joueur, Depth, NewBestScore, NewBestCol, BestCol).

%  Alpha-Beta (Negamax)
% Role : calcule le meilleur score pour un plateau donné en explorant l'arbre de jeux jusqu'au depth
alphabeta(Plateau, Joueur, Depth, Alpha, Beta, Score) :-
    ( terminal(Plateau, Depth) -> % on s'arrete si on est dans un noeud terminal ou profondeur max atteinte
        eval_board(Plateau, Joueur, Score) 
    ;   findall(C, coup_valide(Plateau, C), Moves), % sinon on génère les coups possibles
        ( Moves = [] -> % pas de coup possible
            eval_board(Plateau, Joueur, Score)
        ;   order_moves_center_first(Moves, OrderedMoves), % On teste les colonnes centrales en premier
            alphabeta_loop(OrderedMoves, Plateau, Joueur, Depth, Alpha, Beta, Alpha, Score) % appel récursif pour explorer les coups
        )
    ).

% Boucle Alpha-Beta
%Role : parcourt les coups possibles et met à jour le meilleur score trouvé
alphabeta_loop([], _Plateau, _Joueur, _Depth, _Alpha, _Beta, BestSoFar, BestSoFar). % plus de coups à tester, on retourne le meilleur score trouvé
alphabeta_loop([Move|Moves], Plateau, Joueur, Depth, Alpha, Beta, BestSoFar, Score) :- % il reste des coups à tester
    inserer_pion(Plateau, Move, Joueur, NewPlateau), % insère le pion pour le coup testé
    joueur_suivant(Joueur, NextPlayer), % passe au joueur suivant
    D1 is Depth - 1, % on descend d'un niveau dans l'arbre de recherche
    % appel récursif pour obtenir la valeur du coup testé du point de vue de l'adversaire
    alphabeta(NewPlateau, NextPlayer, D1, -Beta, -Alpha, ChildVal),
    Val is -ChildVal, % on inverse le score car on utilise la technique negamax

    NewBestSoFar is max(BestSoFar, Val), % mise à jour du meilleur score trouvé jusqu'ici
    NewAlpha is max(Alpha, Val), % mise à jour de alpha
    %test de coupure : si le coup est trop bon pour l'adversaire on coupe
    ( NewAlpha >= Beta ->
        % PRUNING : on coupe
        Score = NewBestSoFar
        % sinon on continue à tester les autres coups
    ;   alphabeta_loop(Moves, Plateau, Joueur, Depth, NewAlpha, Beta, NewBestSoFar, Score)
    ).

%  Terminal + Evaluation

terminal(Plateau, Depth) :- % sert à dire on arrete la recherche ici
    Depth =< 0 ; % profondeur max atteinte
    victoire(Plateau, x) ; 
    victoire(Plateau, o) ;
    plateau_plein(Plateau).



eval_board(Plateau, Joueur, Score) :- % évalue le plateau pour le joueur donné, plus le score est élevé mieux c'est pour le joueur
    joueur_suivant(Joueur, Adv), % identifie l'adversaire
    ( victoire(Plateau, Joueur) -> Score =  1000000 % victoire du joueur (position gagnante et score élevé pour forcer ce choix)
; victoire(Plateau, Adv)    -> Score = -1000000 % victoire de l'adversaire (position perdante et score très bas)
    ; plateau_plein(Plateau)    -> Score = 0 % match nul
    ; count_winning_moves(Plateau, Joueur, WJ), % coups gagnants pour le joueur
      count_winning_moves(Plateau, Adv,   WA), % coups gagnants pour l'adversaire (pénalité forte)
      center_count(Plateau, Joueur, CJ), % le centre est avantageux pour le joueur
      center_count(Plateau, Adv,   CA), % le centre est avantageux pour l'adversaire
      Score is 200*WJ - 220*WA + 5*CJ - 5*CA % calcul du score final
      % On pénalise plus l’adversaire que l’on ne récompense soi-même (Comportement défensif privilégié)
    ).

% Nombre de coups qui donnent une victoire immédiate
count_winning_moves(Plateau, Joueur, N) :- % 
    findall(1, 
        ( coup_valide(Plateau, Col), % on teste pour chaque colonne jouable
          inserer_pion(Plateau, Col, Joueur, P2), % on insère le pion
          victoire(P2, Joueur) % on vérifie si ça donne la victoire
        ),
        L),
    length(L, N). % nombre d'éléments dans la liste = nombre de coups gagnants

% Combien de pions dans la colonne centrale (col 4)
center_count(Plateau, Joueur, N) :-
    findall(1,
        ( member(Ligne, Plateau), % pour chaque ligne du plateau
          nth1(4, Ligne, Joueur) % on vérifie si le pion du joueur est dans la colonne centrale
        ),
        L),
    length(L, N). % nombre d'éléments dans la liste = nombre de pions du joueur dans la colonne centrale


%  Move ordering (centre d'abord)

order_moves_center_first(Moves, Ordered) :- % trie les coups pour jouer d'abord le centre puis les côtés
    preference_order([4,3,5,2,6,1,7], Pref), % ordre de préférence des colonnes
    include_in_order(Pref, Moves, Ordered). 

preference_order(L, L).

include_in_order([], _Moves, []). % plus de préférences à traiter
include_in_order([C|Cs], Moves, [C|Out]) :- % colonne prioritaire présente dans les coups possibles
    member(C, Moves), !, % on l'inclut dans le résultat et on empeche de tester les autres règles pour éviter les doublons
    include_in_order(Cs, Moves, Out). 
include_in_order([_|Cs], Moves, Out) :- % colonne prioritaire absente des coups possibles
    include_in_order(Cs, Moves, Out). % on l'ignore et récursif les autres préférences


%  INTERFACE POUR LE MENU


% Boucle de jeu Humain vs IA Alpha-Beta
jouer_humain_vs_ia_alphabeta(Plateau, Joueur, humain, Profondeur) :-
    plateau_plein(Plateau), !,
    nl, write('MATCH NUL !'), nl,
    demander_rejouer_alphabeta.

jouer_humain_vs_ia_alphabeta(Plateau, Joueur, humain, Profondeur) :-
    nl,
    format('--- Tour du Joueur ~w (VOUS) ---~n', [Joueur]),
    write('Choisissez une colonne (1-7): '),
    read(Col),
    (   coup_valide(Plateau, Col)
    ->  inserer_pion(Plateau, Col, Joueur, NouveauPlateau),
        nl, afficher_plateau(NouveauPlateau),
        (   victoire(NouveauPlateau, Joueur)
        ->  nl, format('JOUEUR ~w GAGNE !!!~n', [Joueur]),
            demander_rejouer_alphabeta
        ;   joueur_suivant(Joueur, Suivant),
            jouer_humain_vs_ia_alphabeta(NouveauPlateau, Suivant, ia, Profondeur)
        )
    ;   nl, write('!! COUP INVALIDE !!'), nl,
        jouer_humain_vs_ia_alphabeta(Plateau, Joueur, humain, Profondeur)
    ).

jouer_humain_vs_ia_alphabeta(Plateau, Joueur, ia, Profondeur) :-
    plateau_plein(Plateau), !,
    nl, write('MATCH NUL !'), nl,
    demander_rejouer_alphabeta.

jouer_humain_vs_ia_alphabeta(Plateau, Joueur, ia, Profondeur) :-
    nl,
    format('--- Tour du Joueur ~w (IA Alpha-Beta) ---~n', [Joueur]),
    write('IA réfléchit...'), nl,
    best_move_alpha_beta(Plateau, Joueur, Profondeur, Col),
    format('IA joue colonne ~w~n', [Col]),
    inserer_pion(Plateau, Col, Joueur, NouveauPlateau),
    afficher_plateau(NouveauPlateau),
    (   victoire(NouveauPlateau, Joueur)
    ->  nl, format('JOUEUR ~w GAGNE !!!~n', [Joueur]),
        demander_rejouer_alphabeta
    ;   joueur_suivant(Joueur, Suivant),
        jouer_humain_vs_ia_alphabeta(NouveauPlateau, Suivant, humain, Profondeur)
    ).

demander_rejouer_alphabeta :-
    nl,
    write('Rejouer ? (o/n): '),
    read(Choix),
    (   Choix = o
    ->  menu
    ;   Choix = n
    ->  write('Au revoir!'), nl
    ;   write('Réponse invalide'), nl,
        demander_rejouer_alphabeta
    ).

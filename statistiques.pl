% STATISTIQUES - TOURNOI ENTRE IAS

% chargement des fichiers nécessaires
:- [puissance4].
:- [ia_random].
:- [ia_heuristique].
:- [ia_alphabeta].


%  STRUCTURE DES STATISTIQUES

% On garde les victoires pour chaque IA
% stats(NomIA, Victoires, Defaites, Nuls)


%  MENU STATISTIQUES

menu_stats :-
    nl,
    write('========================================'), nl,
    write('       TOURNOI ENTRE IAS'), nl,
    write('========================================'), nl,
    nl,
    write('Choisissez le type de match:'), nl,
    write('1. IA Random vs IA Heuristique'), nl,
    write('2. IA Random vs IA Alpha-Beta'), nl,
    write('3. IA Heuristique vs IA Alpha-Beta'), nl,
    write('4. IA Alpha-Beta vs IA Alpha-Beta (profondeurs differentes)'), nl,
    write('5. Tournoi complet (toutes les IAs)'), nl,
    write('6. Retour au menu principal'), nl,
    nl,
    write('Votre choix (1-6): '),
    read(Choix),
    traiter_choix_stats(Choix).

traiter_choix_stats(1) :- % Random vs Heuristique
    nl,
    write('Combien de parties? '),
    read(N),
    jouer_tournoi(random, heuristique, N).

traiter_choix_stats(2) :-  % Random vs Alpha-Beta
    nl,
    write('Combien de parties? '),
    read(N),
    write('Profondeur pour Alpha-Beta (4/6/8)? '),
    read(Prof),
    jouer_tournoi_alphabeta(random, alphabeta, N, Prof).

traiter_choix_stats(3) :-  % Heuristique vs Alpha-Beta
    nl,
    write('Combien de parties? '),
    read(N),
    write('Profondeur pour Alpha-Beta (4/6/8)? '),
    read(Prof),
    jouer_tournoi_alphabeta(heuristique, alphabeta, N, Prof).

traiter_choix_stats(4) :- % Alpha-Beta vs Alpha-Beta
    nl,
    write('Combien de parties? '),
    read(N),
    write('Profondeur pour IA Alpha-Beta 1 (ex: 4)? '),
    read(Prof1),
    write('Profondeur pour IA Alpha-Beta 2 (ex: 6)? '),
    read(Prof2),
    jouer_tournoi_alphabeta_vs_alphabeta(alphabeta, alphabeta, N, Prof1, Prof2).

traiter_choix_stats(5) :- % Tournoi complet
    nl,
    write('Combien de parties par match? '),
    read(N),
    write('Profondeur pour Alpha-Beta (4/6/8)? '),
    read(Prof),
    tournoi_complet(N, Prof).

traiter_choix_stats(6) :- % Retour au menu principal
    nl,
    menu.

traiter_choix_stats(_) :- % Choix invalide
    nl,
    write('Choix invalide!'), nl,
    menu_stats.


%  TOURNOI COMPLET

tournoi_complet(N, Prof) :-
    nl,
    write('========================================'), nl,
    write('      TOURNOI COMPLET'), nl,
    write('========================================'), nl,
    nl,
    
    % Random vs Heuristique
    write('>>> Match 1/3: Random vs Heuristique'), nl,
    jouer_tournoi_silencieux(random, heuristique, N, V1, D1, N1),
    
    % Random vs Alpha-Beta
    nl,
    write('>>> Match 2/3: Random vs Alpha-Beta'), nl,
    jouer_tournoi_alphabeta_silencieux(random, alphabeta, N, Prof, V2, D2, N2),
    
    % Heuristique vs Alpha-Beta
    nl,
    write('>>> Match 3/3: Heuristique vs Alpha-Beta'), nl,
    jouer_tournoi_alphabeta_silencieux(heuristique, alphabeta, N, Prof, V3, D3, N3),
    
    % Calculer les totaux
    VictoiresRandom is V1 + V2,
    DefaitesRandom is D1 + D2,
    NulsRandom is N1 + N2,
    
    VictoiresHeuristique is D1 + V3,
    DefaitesHeuristique is V1 + D3,
    NulsHeuristique is N1 + N3,
    
    VictoiresAlphaBeta is D2 + D3,
    DefaitesAlphaBeta is V2 + V3,
    NulsAlphaBeta is N2 + N3,
    
    % Afficher le classement
    nl, nl,
    write('========================================'), nl,
    write('      CLASSEMENT FINAL'), nl,
    write('========================================'), nl,
    format('IA Random      : ~w victoires, ~w defaites, ~w nuls~n', [VictoiresRandom, DefaitesRandom, NulsRandom]),
    format('IA Heuristique : ~w victoires, ~w defaites, ~w nuls~n', [VictoiresHeuristique, DefaitesHeuristique, NulsHeuristique]),
    format('IA Alpha-Beta  : ~w victoires, ~w defaites, ~w nuls~n', [VictoiresAlphaBeta, DefaitesAlphaBeta, NulsAlphaBeta]),
    nl,
    
    % Déterminer le gagnant en fonction des victoires
    max_victoires([[random, VictoiresRandom], [heuristique, VictoiresHeuristique], [alphabeta, VictoiresAlphaBeta]], Champion),
    format('~n*** CHAMPION: IA ~w ***~n~n', [Champion]),
    
    menu_stats.

% Trouve l'IA avec le plus de victoires
max_victoires([[IA, V]], IA).
max_victoires([[IA1, V1], [IA2, V2]|Rest], Champion) :-
    (   V1 >= V2
    ->  max_victoires([[IA1, V1]|Rest], Champion)
    ;   max_victoires([[IA2, V2]|Rest], Champion)
    ).


%  JOUER UN TOURNOI (sans Alpha-Beta)

jouer_tournoi(IA1, IA2, N) :-
    jouer_tournoi_silencieux(IA1, IA2, N, V1, V2, Nuls),
    
    nl,
    write('========================================'), nl,
    write('      RESULTATS DU TOURNOI'), nl,
    write('========================================'), nl,
    format('Parties jouees: ~w~n', [N]),
    nl,
    format('IA ~w: ~w victoires~n', [IA1, V1]),
    format('IA ~w: ~w victoires~n', [IA2, V2]),
    format('Matchs nuls: ~w~n', [Nuls]),
    nl,
    
    (   V1 > V2
    ->  format('>>> GAGNANT: IA ~w <<<~n', [IA1])
    ;   V2 > V1
    ->  format('>>> GAGNANT: IA ~w <<<~n', [IA2])
    ;   write('>>> EGALITE <<<'), nl
    ),
    nl,
    menu_stats.

jouer_tournoi_silencieux(IA1, IA2, N, V1, V2, Nuls) :-
    jouer_n_parties(IA1, IA2, N, 0, 0, 0, V1, V2, Nuls).


%  JOUER UN TOURNOI (avec Alpha-Beta) parce que ici y a un input de plus qui est la profondeur

jouer_tournoi_alphabeta(IA1, IA2, N, Prof) :-
    jouer_tournoi_alphabeta_silencieux(IA1, IA2, N, Prof, V1, V2, Nuls),
    
    nl,
    write('========================================'), nl,
    write('      RESULTATS DU TOURNOI'), nl,
    write('========================================'), nl,
    format('Parties jouees: ~w~n', [N]),
    nl,
    format('IA ~w: ~w victoires~n', [IA1, V1]),
    format('IA ~w: ~w victoires (profondeur ~w)~n', [IA2, V2, Prof]),
    format('Matchs nuls: ~w~n', [Nuls]),
    nl,
    
    (   V1 > V2
    ->  format('>>> GAGNANT: IA ~w <<<~n', [IA1])
    ;   V2 > V1
    ->  format('>>> GAGNANT: IA ~w <<<~n', [IA2])
    ;   write('>>> EGALITE <<<'), nl
    ),
    nl,
    menu_stats.

jouer_tournoi_alphabeta_silencieux(IA1, IA2, N, Prof, V1, V2, Nuls) :-
    jouer_n_parties_alphabeta(IA1, IA2, N, Prof, 0, 0, 0, V1, V2, Nuls).


%  JOUER UN TOURNOI (Alpha-Beta vs Alpha-Beta avec profondeurs différentes)

jouer_tournoi_alphabeta_vs_alphabeta(IA1, IA2, N, Prof1, Prof2) :-
    jouer_tournoi_alphabeta_vs_alphabeta_silencieux(IA1, IA2, N, Prof1, Prof2, V1, V2, Nuls),
    
    nl,
    write('========================================'), nl,
    write('      RESULTATS DU TOURNOI'), nl,
    write('========================================'), nl,
    format('Parties jouees: ~w~n', [N]),
    nl,
    format('IA ~w (profondeur ~w): ~w victoires~n', [IA1, Prof1, V1]),
    format('IA ~w (profondeur ~w): ~w victoires~n', [IA2, Prof2, V2]),
    format('Matchs nuls: ~w~n', [Nuls]),
    nl,
    
    (   V1 > V2
    ->  format('>>> GAGNANT: IA ~w (profondeur ~w) <<<~n', [IA1, Prof1])
    ;   V2 > V1
    ->  format('>>> GAGNANT: IA ~w (profondeur ~w) <<<~n', [IA2, Prof2])
    ;   write('>>> EGALITE <<<'), nl
    ),
    nl,
    menu_stats.

jouer_tournoi_alphabeta_vs_alphabeta_silencieux(IA1, IA2, N, Prof1, Prof2, V1, V2, Nuls) :-
    jouer_n_parties_alphabeta_vs_alphabeta(IA1, IA2, N, Prof1, Prof2, 0, 0, 0, V1, V2, Nuls).


%  JOUER N PARTIES (sans Alpha-Beta)

jouer_n_parties(_, _, 0, V1, V2, Nuls, V1, V2, Nuls) :- !.
jouer_n_parties(IA1, IA2, N, V1Acc, V2Acc, NulsAcc, V1Final, V2Final, NulsFinal) :-
    N > 0,
    format('Partie ~w/~w...', [N, N]),
    initialiser_plateau(Plateau),
    jouer_partie_ia_vs_ia(Plateau, x, IA1, IA2, Resultat),
    format(' ~w~n', [Resultat]),
    
    (   Resultat = ia1_gagne
    ->  V1New is V1Acc + 1, V2New = V2Acc, NulsNew = NulsAcc
    ;   Resultat = ia2_gagne
    ->  V1New = V1Acc, V2New is V2Acc + 1, NulsNew = NulsAcc
    ;   V1New = V1Acc, V2New = V2Acc, NulsNew is NulsAcc + 1
    ),
    
    N1 is N - 1,
    jouer_n_parties(IA1, IA2, N1, V1New, V2New, NulsNew, V1Final, V2Final, NulsFinal).


%  JOUER N PARTIES (avec Alpha-Beta)

jouer_n_parties_alphabeta(_, _, 0, _, V1, V2, Nuls, V1, V2, Nuls) :- !.
jouer_n_parties_alphabeta(IA1, IA2, N, Prof, V1Acc, V2Acc, NulsAcc, V1Final, V2Final, NulsFinal) :-
    N > 0,
    format('Partie ~w/~w...', [N, N]),
    initialiser_plateau(Plateau),
    jouer_partie_ia_vs_ia_alphabeta(Plateau, x, IA1, IA2, Prof, Resultat),
    format(' ~w~n', [Resultat]),
    
    (   Resultat = ia1_gagne
    ->  V1New is V1Acc + 1, V2New = V2Acc, NulsNew = NulsAcc
    ;   Resultat = ia2_gagne
    ->  V1New = V1Acc, V2New is V2Acc + 1, NulsNew = NulsAcc
    ;   V1New = V1Acc, V2New = V2Acc, NulsNew is NulsAcc + 1
    ),
    
    N1 is N - 1,
    jouer_n_parties_alphabeta(IA1, IA2, N1, Prof, V1New, V2New, NulsNew, V1Final, V2Final, NulsFinal).


%  JOUER N PARTIES (Alpha-Beta vs Alpha-Beta avec profondeurs différentes)

jouer_n_parties_alphabeta_vs_alphabeta(_, _, 0, _, _, V1, V2, Nuls, V1, V2, Nuls) :- !.
jouer_n_parties_alphabeta_vs_alphabeta(IA1, IA2, N, Prof1, Prof2, V1Acc, V2Acc, NulsAcc, V1Final, V2Final, NulsFinal) :-
    N > 0,
    format('Partie ~w/~w...', [N, N]),
    initialiser_plateau(Plateau),
    jouer_partie_ia_vs_ia_alphabeta_double(Plateau, x, IA1, IA2, Prof1, Prof2, Resultat),
    format(' ~w~n', [Resultat]),
    
    (   Resultat = ia1_gagne
    ->  V1New is V1Acc + 1, V2New = V2Acc, NulsNew = NulsAcc
    ;   Resultat = ia2_gagne
    ->  V1New = V1Acc, V2New is V2Acc + 1, NulsNew = NulsAcc
    ;   V1New = V1Acc, V2New = V2Acc, NulsNew is NulsAcc + 1
    ),
    
    N1 is N - 1,
    jouer_n_parties_alphabeta_vs_alphabeta(IA1, IA2, N1, Prof1, Prof2, V1New, V2New, NulsNew, V1Final, V2Final, NulsFinal).


%  JOUER UNE PARTIE IA vs IA (sans Alpha-Beta)

jouer_partie_ia_vs_ia(Plateau, _, _, _, nul) :-
    plateau_plein(Plateau), !.

jouer_partie_ia_vs_ia(Plateau, Joueur, IA1, IA2, Resultat) :-
    \+ plateau_plein(Plateau),
    
    % Détermine quelle IA joue
    (   Joueur = x
    ->  IAActive = IA1, ResultatVictoire = ia1_gagne
    ;   IAActive = IA2, ResultatVictoire = ia2_gagne
    ),
    
    % L'IA choisit un coup
    choisir_coup_ia(Plateau, Joueur, IAActive, Col),
    inserer_pion(Plateau, Col, Joueur, NouveauPlateau),
    
    % Vérifie la victoire
    (   victoire(NouveauPlateau, Joueur)
    ->  Resultat = ResultatVictoire
    ;   joueur_suivant(Joueur, Suivant),
        jouer_partie_ia_vs_ia(NouveauPlateau, Suivant, IA1, IA2, Resultat)
    ).


%  JOUER UNE PARTIE IA vs IA (avec Alpha-Beta)

jouer_partie_ia_vs_ia_alphabeta(Plateau, _, _, _, _, nul) :-
    plateau_plein(Plateau), !.

jouer_partie_ia_vs_ia_alphabeta(Plateau, Joueur, IA1, IA2, Prof, Resultat) :-
    \+ plateau_plein(Plateau),
    
    % Détermine quelle IA joue
    (   Joueur = x
    ->  IAActive = IA1, ResultatVictoire = ia1_gagne
    ;   IAActive = IA2, ResultatVictoire = ia2_gagne
    ),
    
    % L'IA choisit un coup
    choisir_coup_ia_alphabeta(Plateau, Joueur, IAActive, Prof, Col),
    inserer_pion(Plateau, Col, Joueur, NouveauPlateau),
    
    % Vérifie la victoire
    (   victoire(NouveauPlateau, Joueur)
    ->  Resultat = ResultatVictoire
    ;   joueur_suivant(Joueur, Suivant),
        jouer_partie_ia_vs_ia_alphabeta(NouveauPlateau, Suivant, IA1, IA2, Prof, Resultat)
    ).


%  JOUER UNE PARTIE IA vs IA (Alpha-Beta vs Alpha-Beta avec profondeurs différentes)

jouer_partie_ia_vs_ia_alphabeta_double(Plateau, _, _, _, _, _, nul) :-
    plateau_plein(Plateau), !.

jouer_partie_ia_vs_ia_alphabeta_double(Plateau, Joueur, IA1, IA2, Prof1, Prof2, Resultat) :-
    \+ plateau_plein(Plateau),
    
    % Détermine quelle IA joue et sa profondeur
    (   Joueur = x
    ->  IAActive = IA1, ProfActive = Prof1, ResultatVictoire = ia1_gagne
    ;   IAActive = IA2, ProfActive = Prof2, ResultatVictoire = ia2_gagne
    ),
    
    % L'IA choisit un coup avec sa profondeur spécifique
    choisir_coup_ia_alphabeta(Plateau, Joueur, IAActive, ProfActive, Col),
    inserer_pion(Plateau, Col, Joueur, NouveauPlateau),
    
    % Vérifie la victoire
    (   victoire(NouveauPlateau, Joueur)
    ->  Resultat = ResultatVictoire
    ;   joueur_suivant(Joueur, Suivant),
        jouer_partie_ia_vs_ia_alphabeta_double(NouveauPlateau, Suivant, IA1, IA2, Prof1, Prof2, Resultat)
    ).


%  CHOISIR COUP SELON L'IA

choisir_coup_ia(Plateau, Joueur, random, Col) :-
    coup_ia_random(Plateau, Col).

choisir_coup_ia(Plateau, Joueur, heuristique, Col) :-
    coup_ia_heuristique(Plateau, Joueur, Col).

choisir_coup_ia_alphabeta(Plateau, Joueur, random, _, Col) :-
    coup_ia_random(Plateau, Col).

choisir_coup_ia_alphabeta(Plateau, Joueur, heuristique, _, Col) :-
    coup_ia_heuristique(Plateau, Joueur, Col).

choisir_coup_ia_alphabeta(Plateau, Joueur, alphabeta, Prof, Col) :-
    best_move_alpha_beta(Plateau, Joueur, Prof, Col).



% IA HEURISTIQUE À RÈGLES PRIORITAIRES POUR PUISSANCE 4

% charge le fichier puissance4 pour avoir les prédicats de base
:- [puissance4].


% règles priorittaires pour choisir le meilleur coup de l'IA 

% Règle 1 : si je peux gagner maintenant, je gagne
coup_ia_heuristique(Plateau, Joueur, MeilleurCol) :-
    % 1. Vérifier si on peut gagner
    trouver_coup_gagnant(Plateau, Joueur, Col),
    MeilleurCol = Col, !. % stop après avoir trouvé un coup gagnant et continue pas aux autres règles

% Règle 2 : sinon je bloque l'adversaire s'il peut gagner au prochain tour
coup_ia_heuristique(Plateau, Joueur, MeilleurCol) :-
    % 2. Bloquer l'adversaire
    joueur_suivant(Joueur, Adversaire),
    trouver_coup_gagnant(Plateau, Adversaire, Col),
    MeilleurCol = Col, !. % stop après avoir trouvé un coup pour bloquer l'adversaire et continue pas aux autres règles

% Règle 3 : sinon je joue au centre si possible puisque le centre donne le plus de possibilités d'alignement
coup_ia_heuristique(Plateau, _, MeilleurCol) :-
    coup_valide(Plateau, 4),
    MeilleurCol = 4, !.
coup_ia_heuristique(Plateau, _, MeilleurCol) :-
    % 4. Jouer aléatoirement
    findall(C, (between(1, 7, C), coup_valide(Plateau, C)), CoupsValides),
    CoupsValides \= [],
    random_member(MeilleurCol, CoupsValides).

% Trouve un coup gagnant
trouver_coup_gagnant(Plateau, Joueur, Col) :-
    between(1, 7, Col),
    coup_valide(Plateau, Col),
    inserer_pion(Plateau, Col, Joueur, NouveauPlateau),
    victoire(NouveauPlateau, Joueur).





%  BOUCLE DE JEU HUMAIN VS IA


jouer_humain_vs_ia_heuristique(Plateau, Joueur, TypeJoueur) :-
    plateau_plein(Plateau),
    nl, write('MATCH NUL !'), nl,
    demander_rejouer_heuristique.

% Tour de l'humain
jouer_humain_vs_ia_heuristique(Plateau, Joueur, humain) :-
    \+ plateau_plein(Plateau),
    nl,
    format('--- Tour du Joueur ~w (VOUS) ---~n', [Joueur]),
    write('Choisissez une colonne (1-7): '),
    read(Col),
    (   coup_valide(Plateau, Col)
    ->  inserer_pion(Plateau, Col, Joueur, NouveauPlateau),
        nl, afficher_plateau(NouveauPlateau),
        (   victoire(NouveauPlateau, Joueur)
        ->  nl, format('JOUEUR ~w GAGNE !!!~n', [Joueur]),
            demander_rejouer_heuristique
        ;   joueur_suivant(Joueur, Suivant),
            jouer_humain_vs_ia_heuristique(NouveauPlateau, Suivant, ia)
        )
    ;   nl, write('!! COUP INVALIDE !!'), nl,
        jouer_humain_vs_ia_heuristique(Plateau, Joueur, humain)
    ).

% Tour de l'IA
jouer_humain_vs_ia_heuristique(Plateau, Joueur, ia) :-
    \+ plateau_plein(Plateau),
    nl,
    format('--- Tour du Joueur ~w (IA) ---~n', [Joueur]),
    write('IA réfléchit...'), nl,
    coup_ia_heuristique(Plateau, Joueur, Col),
    format('IA joue colonne ~w~n', [Col]),
    inserer_pion(Plateau, Col, Joueur, NouveauPlateau),
    afficher_plateau(NouveauPlateau),
    (   victoire(NouveauPlateau, Joueur)
    ->  nl, format('JOUEUR ~w GAGNE !!!~n', [Joueur]),
        demander_rejouer_heuristique
    ;   joueur_suivant(Joueur, Suivant),
        jouer_humain_vs_ia_heuristique(NouveauPlateau, Suivant, humain)
    ).

% Demander si on veut rejouer
demander_rejouer_heuristique :-
    nl,
    write('Rejouer ? (o/n): '),
    read(Choix),
    (   Choix = o
    ->  menu
    ;   Choix = n
    ->  write('Au revoir!'), nl
    ;   write('Réponse invalide'), nl,
        demander_rejouer_heuristique
    ).

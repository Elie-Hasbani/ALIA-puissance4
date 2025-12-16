
:- [puissance4].
:- [ia_random].
:- [ia_heuristique].
:- [ia_alphabeta].

% 1. MENU PRINCIPAL

menu :-
    nl,
    write('========================================'), nl,
    write('          PUISSANCE 4                   '), nl,
    write('========================================'), nl,
    nl,
    write('Choisissez le mode de jeu :'), nl,
    write('1. Humain vs Humain'), nl,
    write('2. Humain vs IA Random'), nl,
    write('3. Humain vs IA Heuristique'), nl,
    write('4. Humain vs IA Alpha-Beta'), nl,
    write('5. Quitter'), nl,
    nl,
    write('Votre choix (1-5): '),
    read(Choix),
    traiter_choix(Choix).


%  2. TRAITEMENT DU CHOIX du user 


traiter_choix(1) :-
    nl,
    write('========================================'), nl,
    write('     Mode: Humain vs Humain'), nl,
    write('========================================'), nl,
    write('Joueur X commence'), nl,
    initialiser_plateau(Plateau),
    afficher_plateau(Plateau),
    jouer_tour(Plateau, x).
traiter_choix(2) :-
    nl,
    write('========================================'), nl,
    write('     Mode: Humain vs IA Random'), nl,
    write('========================================'), nl,
    write('Choisissez votre symbole:'), nl,
    write('1. X (vous commencez)'), nl,
    write('2. O (IA commence)'), nl,
    write('Votre choix: '),
    read(ChoixSymbole),
    (   ChoixSymbole = 1
    ->  initialiser_plateau(Plateau),
        afficher_plateau(Plateau),
        jouer_humain_vs_ia_random(Plateau, x, humain), ! % user is X and ai is O
    ;   ChoixSymbole = 2
    ->  initialiser_plateau(Plateau),
        afficher_plateau(Plateau),
        jouer_humain_vs_ia_random(Plateau, x, ia), ! % user is O and ai is X
    ;   write('Choix invalide, retour au menu'), nl,
        menu
    ).

traiter_choix(3) :-
    nl,
    write('========================================'), nl,
    write('     Mode: Humain vs IA Heuristique'), nl,
    write('========================================'), nl,
    nl,
    write('Choisissez votre symbole:'), nl,
    write('1. X (vous commencez)'), nl,
    write('2. O (IA commence)'), nl,
    write('Votre choix: '),
    read(ChoixSymbole),
    nl,
    (   ChoixSymbole = 1
    ->  initialiser_plateau(Plateau),
        afficher_plateau(Plateau),
        jouer_humain_vs_ia_heuristique(Plateau, x, humain), !
    ;   ChoixSymbole = 2
    ->  initialiser_plateau(Plateau),
        afficher_plateau(Plateau),
        jouer_humain_vs_ia_heuristique(Plateau, x, ia), !
    ;   write('Choix invalide, retour au menu'), nl,
        menu
    ).

traiter_choix(4) :-
    nl,
    write('========================================'), nl,
    write('     Mode: Humain vs IA Alpha-Beta'), nl,
    write('========================================'), nl,
    choisir_difficulte_alphabeta(Profondeur),
    nl,
    write('Choisissez votre symbole:'), nl,
    write('1. X (vous commencez)'), nl,
    write('2. O (IA commence)'), nl,
    write('Votre choix: '),
    read(ChoixSymbole),
    nl,
    (   ChoixSymbole = 1
    ->  initialiser_plateau(Plateau),
        afficher_plateau(Plateau),
        jouer_humain_vs_ia_alphabeta(Plateau, x, humain, Profondeur), !
    ;   ChoixSymbole = 2
    ->  initialiser_plateau(Plateau),
        afficher_plateau(Plateau),
        jouer_humain_vs_ia_alphabeta(Plateau, x, ia, Profondeur), !
    ;   write('Choix invalide, retour au menu'), nl,
        menu
    ).

traiter_choix(5) :-
    nl,
    write('Au revoir!'), nl.

traiter_choix(_) :-
    nl,
    write('Choix invalide. Veuillez choisir entre 1 et 5.'), nl,
    menu.

% ------------------------------
%  POINT D'ENTRÉE
% ------------------------------

% Pour lancer le jeu, tapez: ?- menu.
% IA RANDOM POUR PUISSANCE 4

:- [puissance4].

% Sélectionne une colonne aléatoire valide pour l'IA
coup_ia_random(Plateau, Col) :-
    %Parcourt les colonnes 1 à 7 et collecte celles qui sont valides et en choisit une au hasard
    findall(C, (between(1, 7, C), coup_valide(Plateau, C)), CoupsValides),
    random_member(Col, CoupsValides).

% Jouer Humain vs IA Random
jouer_humain_vs_ia_random(Plateau, Joueur, HumainCommence) :- % humain commence
    % Vérifier match nul
    plateau_plein(Plateau),  % si le plateau est plein
    nl,
    write('MATCH NUL !'), nl,  % afficher match nul
    demander_rejouer_ia. % demander si on veut rejouer

jouer_humain_vs_ia_random(Plateau, Joueur, HumainCommence) :-
    \+ plateau_plein(Plateau),   % si le plateau n'est pas plein
    nl,
    format('--- Tour du Joueur ~w ---~n', [Joueur]),  % afficher le tour du joueur
    
    % Savoir si c'est le tour de l'humain ou de l'IA
    ((Joueur = x, HumainCommence = humain) ; (Joueur = o, HumainCommence = ia)),
    
    % Tour humain
    format('Joueur ~w, choisissez une colonne (1-7): ', [Joueur]), % demander la colonne à l'humain
    read(Col),
    coup_valide(Plateau, Col), % vérifier si le coup est valide
    inserer_pion(Plateau, Col, Joueur, NouveauPlateau),  % insérer le pion
    afficher_plateau(NouveauPlateau),  % afficher le plateau après le coup
    
    % Vérifier victoire
    (   victoire(NouveauPlateau, Joueur) % si victoire
    ->  nl, format('JOUEUR ~w GAGNE !!!~n', [Joueur]), demander_rejouer_ia     % afficher victoire et demander rejouer
    ;   joueur_suivant(Joueur, Suivant),    % passer au joueur suivant
        jouer_humain_vs_ia_random(NouveauPlateau, Suivant, HumainCommence) % appel récursif pour le tour suivant
    ).

jouer_humain_vs_ia_random(Plateau, Joueur, HumainCommence) :-   
    \+ plateau_plein(Plateau),  % si le plateau n'est pas plein
    
    % Savoir si c'est le tour de l'IA
    ((Joueur = x, HumainCommence = ia) ; (Joueur = o, HumainCommence = humain)),
    
    % Tour IA
    write('IA réfléchit...'), nl,
    coup_ia_random(Plateau, Col), % obtenir un coup aléatoire valide
    format('IA joue colonne ~w~n', [Col]), % afficher le coup de l'IA
    inserer_pion(Plateau, Col, Joueur, NouveauPlateau),  % insérer le pion de l'IA
    afficher_plateau(NouveauPlateau),  % afficher le plateau après le coup
    
    % Vérifier victoire
    (   victoire(NouveauPlateau, Joueur) % si victoire
    ->  nl, format('JOUEUR ~w GAGNE !!!~n', [Joueur]), demander_rejouer_ia     % afficher victoire et demander rejouer
    ;   joueur_suivant(Joueur, Suivant),  % passer au joueur suivant
        jouer_humain_vs_ia_random(NouveauPlateau, Suivant, HumainCommence)  % appel récursif pour le tour suivant
    ).

% Gestion coup invalide
jouer_humain_vs_ia_random(Plateau, Joueur, HumainCommence) :-
    write('!! COUP INVALIDE !!'), nl,
    jouer_humain_vs_ia_random(Plateau, Joueur, HumainCommence).

% Demander si on veut rejouer
demander_rejouer_ia :-
    nl,
    write('Rejouer ? (o/n): '),
    read(Choix),
    (   Choix = o
    ->  menu
    ;   Choix = n
    ->  write('Au revoir!'), nl
    ;   write('Réponse invalide'), nl, demander_rejouer_ia
    ).

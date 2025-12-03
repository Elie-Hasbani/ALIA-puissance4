% 1- INITIALISATION DU PLATEAU

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


% 2- AFFICHAGE DU PLATEAU


afficher_cellules_ligne([]). % plus de cellules dans la ligne
afficher_cellules_ligne([Cellule|Reste]) :-   %Cas général : il reste au moins une cellule
    write(' '), 
    write(Cellule),  % affiche la cellule
    write(' |'),
    afficher_cellules_ligne(Reste).  % appelle récursivement pour le reste des cellules

afficher_une_ligne(Ligne) :- 
    write('|'),          
    afficher_cellules_ligne(Ligne),  % affiche les cellules de la ligne
    nl.

afficher_toutes_lignes([]).  %liste vide
afficher_toutes_lignes([Ligne|Reste]) :-  %Cas général : il reste au moins une ligne
    afficher_une_ligne(Ligne),   % affiche la première ligne
    afficher_toutes_lignes(Reste).   % appelle récursivement pour le reste des lignes

afficher_plateau(Plateau) :-
    nl,
    write('+---+---+---+---+---+---+---+'), nl,
    write('| 1 | 2 | 3 | 4 | 5 | 6 | 7 |'), nl,
    write('+---+---+---+---+---+---+---+'), nl,
    afficher_toutes_lignes(Plateau),
    write('+---+---+---+---+---+---+---+'), nl.


% 3- ALTERNANCE DES JOUEURS

joueur_suivant(x, o).
joueur_suivant(o, x).


% 4- INSERTION D'UN PION

% Remplace une case spécifique du plateau

remplacer_nieme([_|Q], 1, X, [X|Q]) :- !.    % Cas de base : N = 1, on remplace le premier élément par le nouveau
remplacer_nieme([T|Q], N, X, [T|R]) :-  % Cas général : N > 1 : on garde la tete et on remplace plus loin dans la liste
    N > 1,
    N1 is N - 1,
    remplacer_nieme(Q, N1, X, R).  % appel récursif sur la queue de la liste avec N décrémenté. On enlève à chaque appel et on remonte.


remplacer_case(Plateau, NumLigne, NumCol, Valeur, NouveauPlateau) :-
    nth1(NumLigne, Plateau, Ligne),  % récupère la ligne à modifier
    remplacer_nieme(Ligne, NumCol, Valeur, NouvelleLigne), % remplace la cellule dans la ligne
    remplacer_nieme(Plateau, NumLigne, NouvelleLigne, NouveauPlateau). % remplace de l'ancienne ligne par la nouvelle dans le plateau

% Trouve la ligne la plus basse disponible dans une colonne
trouver_ligne_libre(Plateau, Col, Ligne, Ligne) :-
    Ligne >= 1,
    nth1(Ligne, Plateau, LignePlateau), % récupère la ligne du plateau
    nth1(Col, LignePlateau, Cellule),   % récupère une cellule spécifique de la ligne
    Cellule == -, !. % Si la cellule est vide, on a trouvé la ligne libre
trouver_ligne_libre(Plateau, Col, Ligne, LigneLibre) :-  % Cas 2 : la ligne actuelle n'est pas libre
    Ligne > 1,
    LigneSuivante is Ligne - 1, % décrémente la ligne pour vérifier celle du dessus
    trouver_ligne_libre(Plateau, Col, LigneSuivante, LigneLibre). % appel récursif pour la ligne au-dessus


% Insère un pion dans une colonne (le pion tombe au plus bas)
inserer_pion(Plateau, Col, Joueur, NouveauPlateau) :-   
    Col >= 1, Col =< 7,  % vérifie que la colonne entrée est valide et entre 1 et 7
    trouver_ligne_libre(Plateau, Col, 6, LigneLibre),  % On cherche dans cette colonne la ligne libre la plus basse
    remplacer_case(Plateau, LigneLibre, Col, Joueur, NouveauPlateau).  % remplace la case libre par le pion du joueur


% 4- VALIDATION DES COUPS

% Vérifie qu'il existe au moins une case vide dans la colonne
colonne_non_pleine(Plateau, Col) :-
    member(Ligne, Plateau), % parcourt chaque ligne du plateau
    nth1(Col, Ligne, Cellule), %récupère la cellule de la colonne qu'on veut
    Cellule == -, !. % case vide

% Vérifie si une colonne est valide et non pleine
coup_valide(Plateau, Col) :-
    integer(Col),
    Col >= 1,
    Col =< 7,
    colonne_non_pleine(Plateau, Col).


% 5- DÉTECTION DE VICTOIRE


% Un joueur gagne s'il a 4 pions alignés
victoire(Plateau, Joueur) :-
    (   victoire_horizontale(Plateau, Joueur)
    ;   victoire_verticale(Plateau, Joueur)
    ;   victoire_diagonale(Plateau, Joueur)
    ).

% Vérifie 4 éléments consécutifs identiques dans une liste
quatre_consecutifs(Liste, Joueur) :-
    append(_, [Joueur, Joueur, Joueur, Joueur|_], Liste).

% Victoire horizontale (4 pions alignés sur une ligne)
victoire_horizontale(Plateau, Joueur) :-
    member(Ligne, Plateau),
    quatre_consecutifs(Ligne, Joueur).

% Extrait une colonne du plateau
extraire_colonne([], _, []). % liste vide
extraire_colonne([Ligne|ResteLignes], Col, [Cellule|ResteColonne]) :- %On parcourt chaque ligne du plateau
    nth1(Col, Ligne, Cellule), %récupère la cellule de la colonne qu'on veut
    extraire_colonne(ResteLignes, Col, ResteColonne). % appel récursif pour le reste des lignes et on construit la colonne progressivement de bas en haut

% Victoire verticale (4 pions alignés sur une colonne)
victoire_verticale(Plateau, Joueur) :-
    between(1, 7, Col),
    extraire_colonne(Plateau, Col, Colonne), % trasforme la colonne en liste
    quatre_consecutifs(Colonne, Joueur). 


% Victoire diagonale (4 pions alignés en diagonale)
victoire_diagonale(Plateau, Joueur) :-
    (   diagonale_desc(Plateau, Joueur)
    ;   diagonale_asc(Plateau, Joueur)
    ).

verifier_diagonale_desc(_, Ligne, Col, _, 4) :- !. % compteur à 4 donc victoire
verifier_diagonale_desc(Plateau, Ligne, Col, Joueur, Compteur) :-  %On analyse ligne par ligne et colonne par colonne
    Ligne =< 6, Col =< 7,  %limites du plateau
    nth1(Ligne, Plateau, LignePlateau),  %récupère la ligne du plateau
    nth1(Col, LignePlateau, Cellule),  %récupère une cellule spécifique de la ligne
    Cellule == Joueur,    %vérifie si la cellule appartient au joueur
    Compteur1 is Compteur + 1,  %Si oui, incrémente le compteur
    Ligne1 is Ligne + 1,  %passe à la ligne suivante
    Col1 is Col + 1, %passe à la colonne suivante
    verifier_diagonale_desc(Plateau, Ligne1, Col1, Joueur, Compteur1).  %appel récursif pour la diagonale descendante


% Diagonales descendantes (\)
diagonale_desc(Plateau, Joueur) :-
    between(1, 6, Ligne),
    between(1, 7, Col),
    verifier_diagonale_desc(Plateau, Ligne, Col, Joueur, 0).

verifier_diagonale_asc(_, Ligne, Col, _, 4) :- !.  % compteur à 4 donc victoire
verifier_diagonale_asc(Plateau, Ligne, Col, Joueur, Compteur) :- %On analyse ligne par ligne et colonne par colonne
    Ligne >= 1, Col =< 7,  %limites du plateau
    nth1(Ligne, Plateau, LignePlateau),  %récupère la ligne du plateau
    nth1(Col, LignePlateau, Cellule),  %récupère une cellule spécifique de la ligne
    Cellule == Joueur,      %vérifie si la cellule appartient au joueur
    Compteur1 is Compteur + 1,  %Si oui, incrémente le compteur
    Ligne1 is Ligne - 1,    %passe à la ligne précédente
    Col1 is Col + 1,    %passe à la colonne suivante
    verifier_diagonale_asc(Plateau, Ligne1, Col1, Joueur, Compteur1).   %appel récursif pour la diagonale ascendante

% Diagonales ascendantes (/)
diagonale_asc(Plateau, Joueur) :-
    between(1, 6, Ligne),
    between(1, 7, Col),
    verifier_diagonale_asc(Plateau, Ligne, Col, Joueur, 0).


% 6- DÉTECTION DE MATCH NUL

% Le plateau est plein si aucune case vide n'existe
plateau_plein(Plateau) :-
    \+ (member(Ligne, Plateau), member(-, Ligne)). % Il n'existe pas de ligne avec une case vide

% 7- BOUCLE PRINCIPALE DU JEU


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
        read(Col), % lit la colonne choisie
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
                jouer_tour(NouveauPlateau, JoueurSuivant) % appel récursif pour le tour suivant et ainsi de suite
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
    (   (Choix = o ; Choix = 'o') % oui
    ->  jouer
    ;   (Choix = n ; Choix = 'n') % non
    ->  nl,
        write('Merci d\'avoir joué ! Au revoir.'), nl
    ;   write('Réponse invalide. Veuillez répondre par o ou n.'), nl, 
        demander_rejouer % redemande
    ).


% Pour lancer le jeu, tapez: ?- jouer.

# Puissance 4

Un jeu de Puissance 4 en Prolog avec plusieurs IA.

## Lancer le jeu

```bash
swipl -s menu.pl
```

Ensuite tape `menu.` pour lancer le menu.

## Menu

Le jeu propose plusieurs modes :

- Humain vs Humain
- Humain vs IA Random
- Humain vs IA Heuristique (règles stratégiques)
- Humain vs IA Alpha-Beta (algorithme minimax)
- Statistiques (tournois entre IA)

## Statistiques / Tournois

Le menu statistiques permet de faire s'affronter les IA entre elles :

1. **IA Random vs IA Heuristique** - Compare une IA aléatoire vs une IA avec règles
2. **IA Random vs IA Alpha-Beta** - Tu choisis la profondeur pour Alpha-Beta (4/6/8)
3. **IA Heuristique vs IA Alpha-Beta** - Tu choisis la profondeur pour Alpha-Beta
4. **IA Alpha-Beta vs IA Alpha-Beta** - Compare deux profondeurs différentes (ex: 4 vs 6)
5. **Tournoi complet** - Toutes les IA s'affrontent, avec classement final

Pour chaque test, tu choisis combien de parties jouer. Le système affiche les victoires, défaites et matchs nuls de chaque IA.

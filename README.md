# Prolog Chess Game

**Joueur intelligent d'échecs avec techniques de recherche heuristique**  
*Projet IFT-2003 - Intelligence Artificielle - Université Laval*

## Installation & Lancement

```bash
swipl go.pl
```

## Architecture

**6 modules Prolog** :
- **pieces.pl** : Règles de mouvement des pièces
- **board.pl** : Représentation plateau 8x8, affichage ASCII
- **game.pl** : Gestion états, validation coups, échec/mat/pat
- **interface.pl** : Interface utilisateur française
- **ai.pl** : Algorithme négamax avec élagage alpha-beta
- **evaluation.pl** : Heuristiques de recherche (matériel + PSQT + sécurité)

## Fonctionnalités

### Jeu d'Échecs Standard
- Toutes les pièces et règles officielles
- Promotion automatique des pions
- Détection échec, mat et pat
- Interface française complète

### Intelligence Artificielle
- **Algorithme** : Négamax avec élagage alpha-beta (profondeur 2)
- **Heuristiques** : Matériel + Piece-Square Tables + sécurité pièces  
- **Tri des coups** : MVV-LVA (Most Valuable Victim - Least Valuable Attacker)
- **Performance** : Temps réel (< 1 seconde/coup)

## Usage

```
Format coups : e2e4 (notation algébrique)
Coordonnées  : colonnes a-h, rangées 1-8
Commandes    : aide, menu, quitter
```

**Modes disponibles :**
- Humain vs Humain
- Humain vs IA

## Tests

```bash
swipl -t run_tests -s tests/tests.pl
```

## Prérequis

- **SWI-Prolog** 9.x+
- **OS** : Windows, Linux, macOS

## Documentation

- [ARCHITECTURE_GUIDE_DEVELOPERS.md](docs/ARCHITECTURE_GUIDE_DEVELOPERS.md) - Architecture détaillée
- [TASKS.md](docs/TASKS.md) - État d'avancement du projet
- [PRD.md](docs/PRD.md) - Spécifications produit
# Prolog Chess Game

Jeu d'échecs complet en Prolog avec Intelligence Artificielle - Projet universitaire IFT-2003.

## Démarrage Rapide

```bash
swipl go.pl
```

## Architecture

**5 modules principaux** :
- **pieces.pl** : Règles de mouvement des pièces
- **board.pl** : Gestion plateau 8x8, coordonnées, affichage
- **game.pl** : États de jeu, validation coups, détection échec/mat
- **interface.pl** : Interface utilisateur française
- **ai.pl** : Intelligence artificielle négamax + alpha-beta

## Installation & Tests

```bash
# Lancer le jeu (inclut option tests dans le menu)
swipl go.pl

# Tests directs en ligne de commande
swipl -t run_tests -s tests/tests.pl
```

## Fonctionnalités

### Jeu d'Échecs Complet
- Toutes les pièces et règles standard
- Promotion automatique des pions
- Détection échec, mat et pat
- Interface française intuitive

### Intelligence Artificielle
- Algorithme négamax avec élagage alpha-beta
- Profondeur de recherche : 2 niveaux
- Performance : 1-4 secondes par coup

## Usage

- **Format des coups** : `e2e4` (notation algébrique)
- **Coordonnées** : colonnes a-h, rangées 1-8
- **Commandes** : `aide`, `menu`, `quitter`, `sortir`

## Compatibilité

- **SWI-Prolog** 9.x+ requis
- **OS** : Windows, Linux, macOS

## Documentation

- **Guide développeur** : [.claude/CLAUDE.md](.claude/CLAUDE.md)
- **Spécifications** : [docs/PRD.md](docs/PRD.md)
- **État d'avancement** : [docs/TASKS.md](docs/TASKS.md)

---
**Projet Universitaire IFT-2003** • **Intelligence Artificielle en Prolog**
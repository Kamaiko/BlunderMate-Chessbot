# 🏆 Prolog Chess Game

Jeu d'échecs complet en Prolog avec focus sur l'Intelligence Artificielle - Projet universitaire.

**🚀 Démarrage rapide :** `swipl go.pl`

## Architecture

4 modules principaux :
- **pieces.pl** : Règles de mouvement des pièces
- **board.pl** : Gestion du plateau et positions  
- **game.pl** : Logique de jeu et validation des coups
- **interface.pl** : Interface utilisateur française

## Installation & Tests

```bash
# Lancement du jeu
swipl go.pl

# Tests complets
swipl -t run_tests -s tests/chess_tests.pl

# Tests rapides  
swipl -s tests/quick_tests.pl
```

## Usage

- **Format coups :** `e2e4` (de e2 vers e4)
- **Notation :** colonnes a-h, rangées 1-8
- **Commandes :** `start.` pour démarrer

## Documentation

- **📋 Spécifications & Roadmap :** [PRD.md](PRD.md)
- **⚙️ Guide développeur :** [.claude/CLAUDE.md](.claude/CLAUDE.md)

---
**Projet IA** • **v5.1** • Tests exhaustifs • Architecture éducative
# 🏆 Prolog Chess Game

Jeu d'échecs complet en Prolog avec focus sur l'Intelligence Artificielle - Projet universitaire.

**🚀 Démarrage rapide :** `swipl go.pl`

## Architecture

5 modules principaux :
- **pieces.pl** : Règles de mouvement des pièces
- **board.pl** : Gestion du plateau et positions  
- **game.pl** : Logique de jeu et validation des coups
- **interface.pl** : Interface utilisateur française professionnelle
- **⚠️ ai.pl** : PROTOTYPE NON FONCTIONNEL (à éviter)

### Interface Modernisée ✅
- Menu principal ASCII professionnel
- Interface française complète (sans accents - compatibilité terminaux)
- Commandes françaises : `aide`, `quitter`, `sortir`
- Tests uniformisés avec format `[OK]` cohérent

## Installation & Tests

```bash
# Lancement du jeu (stable)
swipl go.pl

# Tests complets (regression)
swipl -t run_tests -s tests/regression_tests.pl

# Tests rapides (smoke)
swipl -s tests/smoke_tests.pl

# ⚠️ MODE IA - NON FONCTIONNEL, NE PAS UTILISER
# swipl -s src/ai.pl (PROTOTYPE DÉFAILLANT)
```

## Usage

- **Format coups :** `e2e4` (de e2 vers e4)
- **Notation :** colonnes a-h, rangées 1-8
- **Commandes :** Menu interactif, `aide` pour assistance
- **⚠️ Unicode :** Éviter accents français - problème compatibilité terminaux identifié

## Documentation

- **📋 Spécifications & Roadmap :** [PRD.md](PRD.md)
- **⚙️ Guide développeur :** [.claude/CLAUDE.md](.claude/CLAUDE.md)

---
**Projet IA** • **v5.2** • Interface modernisée • Tests exhaustifs • Architecture éducative • **⚠️ Unicode à résoudre**
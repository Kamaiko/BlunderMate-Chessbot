# 🏆 Prolog Chess Game

Jeu d'échecs complet en Prolog avec focus sur l'Intelligence Artificielle - Projet universitaire IFT-2003.

**🚀 Démarrage rapide :** `swipl go.pl`

## Statut du Projet

**Phase 2 EN COURS** : Échec et Mat ✅ COMPLET • En Passant et Promotion à implémenter

## Architecture

**5 modules principaux** (design modulaire éducatif) :
- **pieces.pl** : Règles de mouvement des pièces, logique individuelle
- **board.pl** : Gestion plateau 8x8, coordonnées, affichage ASCII  
- **game.pl** : États de jeu, validation coups, détection échec/mat
- **interface.pl** : Interface française professionnelle, menu modernisé
- **⚠️ ai.pl** : PROTOTYPE NON FONCTIONNEL (à éviter - voir documentation)

### Interface Modernisée ✅
- Menu principal ASCII professionnel avec bordures
- Interface française complète (sans accents pour compatibilité)
- Commandes intuitives : `aide`, `quitter`, `sortir`, `menu`
- Architecture de tests unifiée avec format `[OK]` cohérent

## Installation & Tests

### Démarrage Rapide
```bash
# Lancement du jeu (interface stable)
swipl go.pl

# Tests complets (33 tests en 5 catégories)
swipl -t run_tests -s tests/tests.pl

# Tests interactifs avec détails
swipl tests/tests.pl
```

### Tests de Développement
```bash
# Catégories de tests disponibles
swipl -g "consult('tests/tests'), run_basic_tests, halt."      # Tests fondamentaux
swipl -g "consult('tests/tests'), run_logic_tests, halt."     # Logique de jeu  
swipl -g "consult('tests/tests'), run_piece_tests, halt."     # Mouvements pièces
swipl -g "consult('tests/tests'), run_scenario_tests, halt."  # Séquences tactiques
swipl -g "consult('tests/tests'), run_robust_tests, halt."    # Cas limites et erreurs
```

## Fonctionnalités

### ✅ Implémenté (Phase 1 + 2 partielle)
- **Jeu complet** : Toutes pièces, mouvements de base, captures
- **Détection échec/mat** : Algorithmes complets avec scénarios complexes  
- **Interface professionnelle** : Menu modernisé, messages français
- **Tests exhaustifs** : 33 tests couvrant tous les aspects

### 🚧 En Développement (Phase 2)
- **Mouvements spéciaux** : En Passant, Promotion, Roque (à implémenter)
- **États terminaux** : Pat et règles avancées

### ⚠️ Non Fonctionnel
- **Mode IA** : `ai.pl` est un prototype défaillant - NE PAS UTILISER

## Usage Pratique

- **Format coups :** `e2e4` (de e2 vers e4) - notation algébrique standard
- **Coordonnées :** colonnes a-h, rangées 1-8
- **Commandes jeu :** `aide`, `menu`, `quitter`, `sortir`
- **Navigation :** Menu interactif avec choix numérotés (1-5)

## Documentation Technique

- **📋 Spécifications complètes :** [PRD.md](docs/PRD.md) - Vision produit et architecture
- **📝 Roadmap développement :** [TASKS.md](docs/TASKS.md) - État d'avancement détaillé  
- **⚙️ Guide développeur :** [CLAUDE.md](.claude/CLAUDE.md) - Instructions techniques complètes

## Compatibilité

- **SWI-Prolog** 9.x+ requis
- **Terminaux** : Problème Unicode identifié, interface sans accents
- **OS** : Windows, Linux, macOS (interface multi-plateforme)

---
**Projet Universitaire IA** • **Phase 2/3** • **33 Tests ✅** • **Architecture Modulaire** • **Interface Stable**
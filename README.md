# 🏆 Prolog Chess Game

Jeu d'échecs complet en Prolog avec focus sur l'Intelligence Artificielle - Projet universitaire IFT-2003.

**🚀 Démarrage rapide :** `swipl go.pl`

## Statut du Projet

**Phase 2 COMPLÈTE** ✅ : Jeu d'échecs fonctionnel avec promotion automatique des pions
**Phase 3 PRÊTE** 🚀 : Implémentation IA - Plan détaillé disponible (voir `docs/plan.md`)

## Architecture

**5 modules principaux** (design modulaire éducatif) :
- **pieces.pl** : Règles de mouvement des pièces, logique individuelle
- **board.pl** : Gestion plateau 8x8, coordonnées, affichage ASCII  
- **game.pl** : États de jeu, validation coups, détection échec/mat
- **interface.pl** : Interface française professionnelle, menu modernisé
- **ai.pl** : Module IA Phase 3 - Plan d'implémentation Minimax disponible

### Interface Modernisée ✅
- Menu principal ASCII professionnel avec bordures
- Interface française complète (sans accents pour compatibilité)
- Commandes intuitives : `aide`, `quitter`, `sortir`, `menu`
- Architecture de tests unifiée avec format `[RUN]` cohérent et alignement parfait

## Installation & Tests

### Démarrage Rapide
```bash
# Lancement du jeu (interface stable)
swipl go.pl

# Tests complets (33 tests en 5 catégories) - 100% PASS
swipl -t run_tests -s tests/tests.pl

# Tests interactifs avec détails (tous les tests passent)
swipl tests/tests.pl
```

### Tests de Développement
```bash
# Catégories de tests disponibles
swipl -g "consult('tests/tests'), run_foundation_tests, halt." # Tests fondamentaux
swipl -g "consult('tests/tests'), run_pieces_tests, halt."     # Tests des pièces (avec promotion)
swipl -g "consult('tests/tests'), run_checkmate_tests, halt."  # Tests échec et mat
swipl -g "consult('tests/tests'), run_robustness_tests, halt." # Tests de robustesse
swipl -g "consult('tests/tests'), run_integration_tests, halt." # Tests d'intégration
```

## Fonctionnalités

### ✅ Implémenté (Phase 2 COMPLÈTE)
- **Jeu complet** : Toutes pièces, mouvements de base, captures
- **Promotion automatique** : Pions promus en Dame automatiquement (7e→8e, 2e→1ère)
- **Détection échec/mat/pat** : Algorithmes complets avec scénarios complexes  
- **Interface professionnelle** : Menu modernisé, messages français, alignement parfait
- **Tests exhaustifs** : 33 tests couvrant tous les aspects (100% PASS)

### ✅ Mode IA Opérationnel (Phase 1 Terminée)
- **Mode IA disponible** : Option 2 dans menu principal (swipl go.pl)
- **Performance** : 0.5-0.6 secondes par coup (quasi-instantané)
- **Algorithme** : Minimax avec Alpha-Beta pruning, profondeur 1 optimisée
- **Tests** : Section 6 IA - 7 tests complets (40/40 PASS total)

### 🎯 Améliorations Future (Phases 2-3)
- **Profondeur 2** : Optimisation pour standard académique < 1s
- **Ouvertures** : 6-8 ouvertures essentielles intégrées
- **Polish** : Interface et documentation finale

## Usage Pratique

- **Format coups :** `e2e4` (de e2 vers e4) - notation algébrique standard
- **Coordonnées :** colonnes a-h, rangées 1-8
- **Commandes jeu :** `aide`, `menu`, `quitter`, `sortir`
- **Navigation :** Menu interactif avec choix numérotés (1-5)

## Documentation Technique

- **📋 Spécifications complètes :** [PRD.md](docs/PRD.md) - Vision produit et architecture
- **📝 Roadmap développement :** [TASKS.md](docs/TASKS.md) - État d'avancement détaillé  
- **⚙️ Guide développeur :** [CLAUDE.md](.claude/CLAUDE.md) - Instructions techniques complètes
- **🤖 Plan IA Phase 3 :** [plan.md](docs/plan.md) - Roadmap détaillée implémentation Minimax

## Compatibilité

- **SWI-Prolog** 9.x+ requis
- **Terminaux** : Problème Unicode identifié, interface sans accents
- **OS** : Windows, Linux, macOS (interface multi-plateforme)

---
**Projet Universitaire IA** • **Phase 2 COMPLÈTE ✅** • **Phase 3 PRÊTE 🚀** • **33 Tests 100% PASS** • **Plan IA Détaillé**
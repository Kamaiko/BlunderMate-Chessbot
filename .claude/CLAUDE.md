# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Important: Efficient File System Analysis

**When using /init or analyzing codebase structure:**
- ❌ AVOID `directory_tree` on project root (generates 10k+ tokens due to .git objects)
- ✅ USE `list_directory` selectively on relevant folders:
  ```
  list_directory(".")           # Project overview
  list_directory("src")         # Source code
  list_directory("tests")       # Test files
  list_directory("docs")        # Documentation
  ```
- Skip analysis of `.git`, `archive`, or other non-essential directories

# Prolog Chess Game - AI Focus Project

## Quick Context
- **Project**: Chess AI in Prolog - University AI course (IFT-2003)
- **Date remise**: 20 octobre 2025 (9h00) - Rapport PDF + Code Prolog
- **Current Phase**: Finalisation académique - Fix technique + rapport structuré  
- **Status**: ✅ **95% COMPLÉTÉ** - Objectifs apprentissage validés, fix technique prêt
- **Architecture**: 6-module design (pieces/board/game/interface/ai/evaluation)
- **Performance**: Quasi-instantanée (0.00s/coup), évaluation cohérente [EVAL] (+blanc/-noir)
- **Fix minimal**: MINIMAL_FIX_PLAN_CORRECTED.md (modification atomique 2 lignes ai.pl)
- **Timeline**: 15 minutes précises avec 5 rollback points sécurisés
- **Documentation**: Plans obsolètes supprimés, MINIMAL_FIX_PLAN_CORRECTED.md final (2025-09-08)

## Development Commands

### Testing
```bash
# Full test suite
swipl -t run_tests -s tests/tests.pl

# Single test category examples
swipl -g "consult('tests/tests'), run_basic_tests, halt."
swipl -g "consult('tests/tests'), run_logic_tests, halt."

# Interactive test session
swipl tests/tests.pl
?- run_tests.
```

### Running the Game
```bash
# Launch chess game (stable)
swipl go.pl

# Direct interface launch (stable)
swipl -s src/interface.pl -g start

# ✅ AI mode (Phase 3 - ALPHA-BETA OPTIMISÉ)  
# IA avec négamax + alpha-beta + évaluation PSQT, performance quasi-instantanée
swipl go.pl  # Option 2: IA vs Humain (alpha-beta fonctionnel)
```

### Debugging
```bash
# Enable trace mode in SWI-Prolog
?- trace.

# Debug specific predicate
?- trace, valid_move(Board, white, 2, 5, 4, 5).

# Check variable binding
?- ground(Arguments).
```

## Architecture Overview

### 6-Module Design
- **pieces.pl**: Movement rules per piece type, piece identification
- **board.pl**: 8x8 board representation, coordinates, ASCII display
- **game.pl**: Game state management, move validation, capture logic
- **interface.pl**: Interface française professionnelle, boucle de jeu, interaction utilisateur
- **ai.pl**: Intelligence artificielle négamax + alpha-beta (Phase 3 complète)
- **evaluation.pl**: Évaluation centralisée (matériel + PSQT + piece safety)

### Data Structures
```prolog
% Game State Structure
game_state(Board, CurrentPlayer, MoveCount, GameStatus, CapturedPieces)

% Board: 8x8 list of lists
% Pieces: 'P','R','N','B','Q','K' (white) / 'p','r','n','b','q','k' (black)
% Empty squares: ' ' (space) - standardisé après code review
```

### Key Predicates
- `valid_move/6`: Core movement validation (robust input validation)
- `make_move/6`: Move execution with state update and promotion handling
- `parse_algebraic_move/5`: Convert "e2e4" format
- `display_game_state/1`: ASCII board display
- `place_piece_optimized/5`: High-performance board operations (AI-ready)
- `is_promotion_move/3`: Automatic pawn promotion detection (Phase 2 ✅)
- `is_checkmate/2`: Complete checkmate detection system
- `check_path_clear/7,8`: Path validation with recursion protection (7: main, 8: depth-limited)

## Prolog Development Guidelines

### Code Conventions
- **Predicates**: snake_case (`valid_move/5`, `piece_at/3`)
- **Variables**: PascalCase (`Board`, `GameState`, `FromRow`)
- **Language**: English predicates/code, French comments (SANS ACCENTS - voir section Unicode ci-dessous)
- **Move format**: "e2e4" (not "e2-e4")
- **Git commits**: TOUJOURS en français - ce projet se déroule entièrement en français (commentaires, documentation, commits)

### Critical Validation Patterns (ENHANCED ✅)
```prolog
% ROBUST input validation - ALL predicates now follow this pattern
get_piece(Board, Row, Col, Piece) :-
    ground(Board), ground(Row), ground(Col),
    is_list(Board), length(Board, 8),
    integer(Row), integer(Col),
    valid_chess_position(Row, Col),
    % ... safe operations

% Recursion protection - ALL path checking includes depth limits  
check_path_clear(Board, Row, Col, ToRow, ToCol, RowDir, ColDir, Depth) :-
    Depth < 8,  % Maximum chess board traversal
    ground(Row), ground(Col), integer(Row), integer(Col),
    % ... safe recursion
```

### Performance Considerations (OPTIMIZED ✅)
- **Board Operations**: Use `place_piece_optimized/5` for frequent operations (AI-ready)
- **Memory**: `replace_list_element_direct/5` reduces O(n²) complexity
- Avoid `findall/3` in loops (performance killer)
- Use `once/1` for deterministic predicates
- **Security**: All coordinates validated with `ground/1` + type checks
- **Recursion**: Depth-limited with automatic protection (max 8 moves)

## Testing Strategy

### Test Structure
**tests.pl** (8 categories):
1. **Foundation tests**: Board initialization, parsing, game state
2. **Pieces tests**: Individual piece movement rules
3. **Checkmate tests**: Échec/mat/pat detection
4. **Robustness tests**: Error handling, validation
5. **Integration tests**: Complete game flows
6. **PSQT tests**: Piece-Square Tables evaluation
7. **Alpha-beta tests**: Négamax algorithm (Section 7)
8. **Defense detection tests**: Piece safety (Section 8 - nouvellement ajoutée)

### Test-Driven Workflow
1. Write failing test
2. Implement minimal code to pass
3. Run `tests.pl` for validation
4. Run full suite before commits

## Common Issues & Solutions

### Unicode & Terminal Compatibility ⚠️ **RÈGLE CRITIQUE**
**PROBLÈME PERMANENT**: Incompatibilité caractères Unicode avec terminaux Windows
- **Impact**: Pièces d'échecs Unicode, accents français s'affichent incorrectement
- **RÈGLE ABSOLUE**: **JAMAIS de caractères Unicode dans le code**
- **Interface française**: SANS ACCENTS obligatoire (é→e, à→a, è→e, ç→c, etc.)
- **Exemples interdits**: ♔♕♖♗♘♙ (pièces), "Échiquier", "déplacé", "résolu"
- **Exemples corrects**: 'K','Q','R','B','N','P' (ASCII), "Echiquier", "deplace", "resolu"

### Debugging Checklist
- **Variables unbound**: Use `ground/1` validation
- **Coordinates out of range**: Ensure 1-8 bounds
- **Move format**: Use "e2e4" not "e2-e4"
- **Infinite loops**: Check `findall/3` usage
- **Trace debugging**: `trace.` then call predicate
- **Unicode issues**: Check terminal compatibility, avoid Unicode characters

### Before Committing
```bash
# Full test suite validation (8 sections)
swipl -s tests/tests.pl -g "run_all_tests, halt."

# Specific test sections  
swipl -s tests/tests.pl -g "run_defense_detection_tests, halt."  # Section 7 (nouveau)
swipl -s tests/tests.pl -g "run_alpha_beta_tests, halt."        # IA négamax

# Quick game functionality test
swipl go.pl  # Test manual - Option 2: IA vs Humain
```

## AI Implementation Status (Phase 3) - ✅ **PLEINEMENT FONCTIONNELLE**

✅ **IA NÉGAMAX + ALPHA-BETA OPTIMISÉE**
- **Algorithme**: Négamax avec élagage alpha-beta opérationnel, profondeur 2
- **PSQT**: Piece-Square Tables ChessProgramming.org intégrées  
- **Évaluation**: Matériel + PSQT + piece safety (CORRIGÉE et active)
- **Interface**: Scores cohérents `[EVAL] Position: X (+blanc/-noir)`
- **Performance**: Quasi-instantanée (0.00s/coup), élagage fonctionnel

### ✅ **BUG DÉTECTION DÉFENSE RÉSOLU** (2025-09-07)
- **🎯 Root cause identifié** : Bug paramètre couleur dans `is_piece_defended` (evaluation.pl:311)
- **🔧 Correction appliquée** : Ajout `opposite_player()` avant appel `is_square_attacked`
- **✅ Impact résolu** : Plus de swing -855 points, détection défense fonctionnelle
- **✅ Tests validés** : Pièce défendue vs isolée correctement détectée
- **✅ Gameplay validé** : Blunders tactiques drastiquement réduits

### 🎯 **PROCHAINES OPTIMISATIONS**
- **⚠️ Dame développement précoce** : Sort encore parfois tôt (impact mineur)
- **🔧 Interface revamp** : Modernisation menu et interface jeu (frontend-designer)
- **📋 Tests restructuration** : Groupement par catégories logiques

### ✅ **TOUS BUGS CRITIQUES RÉSOLUS** (Septembre 2025)

#### **Interface Loop Bug** ✅ **RÉSOLU DÉFINITIVEMENT**
- **Status**: Plus de freeze observés sur séquence `d2d4` → `c1g5` → `g5e7`
- **Validation**: Tests multiples sessions confirment stabilité

#### **Détection Défense Bug** ✅ **RÉSOLU COMPLÈTEMENT**
- **Root cause**: Bug paramètre couleur dans `evaluation.pl:311` (`is_piece_defended`)
- **Correction**: `opposite_player(DefendingPlayer, Opponent)` avant `is_square_attacked`
- **Validation complète**: Tests unitaires + gameplay réel confirment fonctionnement
- **Impact**: Blunders tactiques éliminés, évaluation stable

### 📚 **DOCUMENTATION ACTUELLE**
- **TASKS.md**: Statut projet + solutions Option A/B détaillées (mis à jour 2025-09-07)
- **BUG_REPORT_ENTERPRISE.md**: Root cause architectural identifié (mis à jour 2025-09-07)
- **AI_V3_REFACTORING_PLAN.md**: Plan complet Option A (refactoring) + Option B (quick fix)
- **ARCHITECTURE_GUIDE_DEVELOPERS.md**: Guide complet nouveaux développeurs
- **MVV_LVA_IMPLEMENTATION_PLAN.md**: ⚠️ OBSOLÈTE - archivé (2025-09-07)


### ✅ **ÉTAT ACTUEL IA** (Septembre 2025)
- **🎯 Détection défense**: ✅ FONCTIONNELLE (bug résolu)
- **📊 Piece safety**: ✅ ACTIVE et corrigée
- **🧹 Évaluation**: ✅ STABLE (+blanc/-noir cohérent)
- **⚠️ Limitation mineure**: Dame développement occasionnellement précoce

### 🎯 **STATUS PROJET FINAL**
- **✅ Exigences techniques**: IA fonctionnelle, profondeur 2, performance optimale
- **✅ Architecture stable**: 6 modules, tests passent, interface professionnelle  
- **✅ Bugs critiques**: Tous résolus (interface loop + détection défense)
- **✅ Tests modernisés**: Section détection défense ajoutée
- **⚠️ Améliorations mineures**: Dame développement + interface revamp possibles

### 📚 RECOMMANDATIONS THÉORIQUES À IMPLÉMENTER

#### **Réponses d'Ouverture Classiques** (Priorité 1)
```prolog
% Réponse au pion roi - OBLIGATOIRE avant développement
opening_move([e2,e4], [e7,e5]).   % Ouverture ouverte (classique)
opening_move([e2,e4], [c7,c5]).   % Sicilienne
opening_move([e2,e4], [e7,e6]).   % Française
opening_move([e2,e4], [c7,c6]).   % Caro-Kann
opening_move([e2,e4], [d7,d6]).   % Pirc/Moderne
opening_move([e2,e4], [g8,f6]).   % Défense hypermoderne

% Réponse au pion dame - OBLIGATOIRE avant développement
opening_move([d2,d4], [d7,d5]).   % Classique (PRIORITÉ #1)
opening_move([d2,d4], [g8,f6]).   % Indienne
opening_move([d2,d4], [e7,e6]).   % Française pour d4
opening_move([d2,d4], [c7,c6]).   % Slav / Caro-Kann dame
opening_move([d2,d4], [d7,d6]).   % Moderne pour d4
```

#### **Checklist Évaluation Heuristique** (Profondeur 2)
```prolog
% 1. Valeur des pièces (CORRIGÉ ✅)
% Pion=100, Cavalier=320, Fou=330, Tour=500, Dame=900, Roi=10000

% 2. Contrôle du centre (À IMPLÉMENTER ❌)
% Bonus pour pions/pièces sur d4, e4, d5, e5

% 3. Sécurité du roi / Roque (À IMPLÉMENTER ❌)
% Malus pour roi exposé, bonus pour roque

% 4. Structure des pions (À IMPLÉMENTER ❌)
% Malus pions isolés/doublés, bonus chaînes de pions

% 5. Développement des pièces (PARTIELLEMENT ✅)
% Bonus cavaliers/fous actifs (fait), mais APRÈS coups centraux

% 6. Coups d'ouverture théoriques (À IMPLÉMENTER ❌)
% Énorme bonus pour réponses classiques (1.d4 d5, 1.e4 e5)
```

### 📋 État Actuel Diagnostic (Décembre 2025)
- **Algorithme IA**: ✅ OPTIMISÉ (négamax + alpha-beta profondeur 2, élagage fonctionnel)
- **Évaluation base**: ✅ STABLE (matériel + PSQT + scores cohérents)  
- **Limitations tactiques**: ⚠️ AGRESSIVE (sacrifices contre pions défendus)
- **Performance**: ✅ OPTIMALE (0.00s/coup, élagage alpha-beta opérationnel)

### 🔮 **Optimisations Futures Identifiées**
- **FEN Parser** : Implémenter parseur FEN pour tests positions spécifiques sans écrire des centaines de lignes
- **Tests Tactiques** : Système de validation positions d'échecs (mat en 1, parades forcées)
- **Quiescence Search** : Extension recherche tactique aux nœuds feuilles
- **Transposition Tables** : Cache positions évaluées pour optimisation performance
- **Opening Book** : Base réponses théoriques pour éviter dame prématurée

## File Dependencies
- interface.pl → game.pl → board.pl → pieces.pl
- ai.pl → pieces.pl, board.pl, game.pl, piece_values_sophisticated.pl
- tests.pl depends on all src modules (Section 6 IA outdated)
- go.pl is launcher (loads interface.pl directly)

## Critical Project Files  
- **📋 Tasks**: [docs/TASKS.md](../docs/TASKS.md) - Prochaines étapes: interface revamp, optimisations mineures
- **📊 Bug Report**: [docs/BUG_REPORT_ENTERPRISE.md](../docs/BUG_REPORT_ENTERPRISE.md) - Bugs résolus, status stable
- **📚 Architecture**: [docs/ARCHITECTURE_GUIDE_DEVELOPERS.md](../docs/ARCHITECTURE_GUIDE_DEVELOPERS.md) - Guide développeurs complet
- **🧪 Tests**: [tests/tests.pl](../tests/tests.pl) - Section 7 détection défense (tests propres)
- **🤖 IA**: [src/ai.pl](../src/ai.pl) - Négamax + alpha-beta opérationnel
- **⚖️ Évaluation**: [src/evaluation.pl](../src/evaluation.pl) - Piece safety corrigée (ligne 311)


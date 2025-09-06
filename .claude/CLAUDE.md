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
- **Current Phase**: Phase 3 ✅ COMPLÈTE - IA négamax + alpha-beta optimisée
- **Status**: ⚠️ Interface loop **POSSIBLEMENT RÉSOLU** + piece_safety désactivée  
- **Architecture**: 6-module design (pieces/board/game/interface/ai/evaluation)
- **Performance**: Quasi-instantanée (0.00s/coup), évaluation cohérente [EVAL] (+blanc/-noir)
- **Documentation**: TASKS.md mis à jour (2025-09-05)

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
**tests.pl** (5 categories):
1. **Basic tests**: Board initialization, display, algebraic notation parsing
2. **Logic tests**: Move validation, game state management  
3. **Piece tests**: Individual piece movement rules (pawn, knight, sliding pieces, king)
4. **Scenario tests**: Opening sequences, tactical combinations
5. **Robustness tests**: Error handling, boundary conditions, path blocking

### Test-Driven Workflow
1. Write failing test
2. Implement minimal code to pass
3. Run `tests.pl` for validation
4. Run full suite before commits

## Common Issues & Solutions

### Unicode & Terminal Compatibility ⚠️
**PROBLÈME CRITIQUE IDENTIFIÉ**: Incompatibilité des caractères Unicode avec certains terminaux Windows
- **Impact**: Pièces d'échecs Unicode, accents français ne s'affichent pas correctement
- **Solution temporaire**: ÉVITER TOUT CARACTÈRE UNICODE dans le code
- **Objectif futur**: Résoudre la compatibilité pour tous les OS
- **Règle actuelle**: Interface française SANS ACCENTS uniquement (é→e, à→a, è→e, etc.)

### Debugging Checklist
- **Variables unbound**: Use `ground/1` validation
- **Coordinates out of range**: Ensure 1-8 bounds
- **Move format**: Use "e2e4" not "e2-e4"
- **Infinite loops**: Check `findall/3` usage
- **Trace debugging**: `trace.` then call predicate
- **Unicode issues**: Check terminal compatibility, avoid Unicode characters

### Before Committing
```bash
# Full test suite validation  
swipl -t run_tests -s tests/tests.pl

# Interactive testing
swipl tests/tests.pl
```

## AI Implementation Status (Phase 3) - ⚠️ **DEBUG CRITIQUE MVV-LVA**

✅ **IA NÉGAMAX + ALPHA-BETA OPTIMISÉE**
- **Algorithme**: Négamax avec élagage alpha-beta opérationnel, profondeur 2
- **PSQT**: Piece-Square Tables ChessProgramming.org intégrées  
- **Évaluation**: Matériel + PSQT (piece safety désactivée temporairement)
- **Interface**: Scores cohérents `[EVAL] Position: X (+blanc/-noir)`
- **Performance**: Quasi-instantanée (0.00s/coup), élagage fonctionnel

### 🚨 **DÉCOUVERTE CRITIQUE - MVV-LVA DEBUG** (2025-09-06)
- **🎯 Détection défense** : **ILLUSION COMPLÈTE** - Bug paramètre couleur critique
- **Bug identifié** : `is_square_attacked(Board, Row, Col, Opponent)` → Teste mauvaise couleur
- **Impact** : IA ne détecte JAMAIS défenses réelles → Blunders tactiques persistent
- **Tests faux positifs** : Passent par accident (différence valeur pièces seulement)
- **Status critique** : **DEBUG ACTIF** - Correction paramètre Player vs Opponent requise

### 🔬 **TÂCHES PRIORITAIRES CRITIQUES**
- **🚨 IMMÉDIAT** : Fix bug couleur dans move_score_with_defense/4 (30 min)
- **⚔️ Captures forcées** : Inclure toutes captures même au-delà limite 25 coups  
- **🔍 TASK ARCH-2** : Audit architectural vs standards moteurs professionnels
- **📋 Status** : **DEBUG CRITIQUE EN COURS** - Détection défense non fonctionnelle

### ✅ **INTERFACE LOOP BUG RÉSOLU** (Septembre 2025)
- **🎉 Status**: **RÉSOLU** - Plus de freeze observés lors multiples sessions tests
- **📍 Validation**: Séquence `d2d4`, `c1g5`, `g5e7` jouable sans problème
- **🔧 Résolution**: Corrections codes antérieures ont éliminé le bug
- **📋 Monitoring**: Tests sessions MVV-LVA confirment stabilité interface

### 🚨 **BUG MVV-LVA CRITIQUE** - MISE À JOUR (Septembre 2025)  

#### **PARTIE 1: BUG PARAMÈTRE COULEUR** ✅ **PARTIELLEMENT RÉSOLU**
- **🐛 Bug initial**: `is_square_attacked(Board, Row, Col, Opponent)` paramètre inversé
- **🔧 Correction appliquée**: `Opponent` → `Player` dans ai.pl:281 (2025-09-06)
- **✅ Tests isolés**: Détection défense fonctionne (Dame×défendu -700 vs +600)

#### **🚨 PARTIE 2: DÉCOUVERTE CRITIQUE GAMEPLAY** ❌ **PROBLÈME PERSISTE**
- **📍 Evidence**: IA blunder dame a5→a2 coup 5 en jeu réel malgré correction
- **🎯 Réalité**: Tests isolés passent ≠ Comportement jeu réel
- **📋 Status**: **INVESTIGATION PIPELINE COMPLET REQUISE**
- **🔍 Hypothèses**: generate_opening_moves bypass MVV-LVA, limitation coups, ou autre bug

**CONCLUSION**: Bug plus complexe que paramètre couleur - Blunders persistent en partie réelle

### 🔍 ANALYSE QUALITÉ CODE COMPLÈTE (Janvier 2025)


#### **📚 Documentation Créée**
- **BUG_REPORT_ENTERPRISE.md**: Analyse complète bugs + problèmes qualité
- **AI_TEST_SUITE_PROPOSAL.md**: 83 tests IA structurés (Section 7)  
- **ARCHITECTURE_GUIDE_DEVELOPERS.md**: Guide complet nouveaux développeurs


### ⚠️ Limitations Tactiques Identifiées (Décembre 2025)  
- **🎯 Aggressivité excessive**: IA sacrifie fous/cavaliers contre pions défendus
- **🐛 Détection défenses faible**: Ne calcule pas toujours recaptures importantes  
- **📊 Piece safety désactivée**: is_square_attacked partiellement fonctionnel
- **🧹 Scores EVAL corrigés**: Cohérents (+blanc/-noir) mais IA reste tactiquement faible

### 🎯 Status Académique 
- **✅ Exigences techniques**: IA fonctionnelle, profondeur 2, performance acceptable
- **✅ Architecture stable**: 6 modules, tests passent, interface professionnelle  
- **✅ Interface stable**: Bug loop résolu, plus de freeze observés
- **🚨 Bug critique MVV-LVA**: Détection défense non fonctionnelle (paramètre couleur)
- **⚠️ Recommandation**: Fix bug détection défense pour éliminer blunders tactiques

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
- **🚨 MVV-LVA Plan**: [docs/MVV_LVA_IMPLEMENTATION_PLAN.md](../docs/MVV_LVA_IMPLEMENTATION_PLAN.md) - Plan détection défense + découvertes critiques
- **📋 Tasks**: [docs/TASKS.md](../docs/TASKS.md) - **PRIORITÉ CRITIQUE** : Debug MVV-LVA paramètre couleur  
- **📊 Bug Report**: [docs/BUG_REPORT_ENTERPRISE.md](../docs/BUG_REPORT_ENTERPRISE.md) - Archive interface loop résolu
- **🧪 Tests**: [tests/tests.pl](../tests/tests.pl) - Section 7B MVV-LVA (tests faux positifs identifiés)
- **🤖 IA**: [src/ai.pl](../src/ai.pl) - Négamax + alpha-beta + MVV-LVA avec bug couleur critique


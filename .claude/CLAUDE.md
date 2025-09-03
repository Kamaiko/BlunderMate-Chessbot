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
- **Current Phase**: Phase 3 ❌ DÉFAILLANTE - IA fait blunders tactiques constants
- **Critical Issue**: Minimax implémenté MAIS donne matériel gratuitement
- **Architecture**: 5-module design (pieces/board/game/interface/ai) + système complet
- **Status**: Interface fonctionnelle, tests IA outdated, objectif TP1 NON ATTEINT

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

# ❌ AI mode (Phase 3 - DÉFAILLANTE)  
# IA fait blunders constants, voir docs/AI_STATUS_HANDOFF.md
swipl go.pl  # Option 2: IA vs Humain (non recommandé - blunders)
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

### 5-Module Design
- **pieces.pl**: Movement rules per piece type, piece identification
- **board.pl**: 8x8 board representation, coordinates, ASCII display
- **game.pl**: Game state management, move validation, capture logic
- **interface.pl**: Interface française professionnelle, boucle de jeu, interaction utilisateur
- **ai.pl**: Intelligence artificielle (Phase 3 - voir docs/plan.md pour implémentation)

### Data Structures
```prolog
% Game State Structure
game_state(Board, CurrentPlayer, MoveCount, GameStatus, CapturedPieces)

% Board: 8x8 list of lists
% Pieces: 'P','R','N','B','Q','K' (white) / 'p','r','n','b','q','k' (black)
% Empty squares: '.'
```

### Key Predicates
- `valid_move/5`: Core movement validation (robust input validation)
- `make_move/5`: Move execution with state update and promotion handling
- `parse_algebraic_move/5`: Convert "e2e4" format
- `display_game_state/1`: ASCII board display
- `place_piece_optimized/5`: High-performance board operations (AI-ready)
- `is_promotion_move/3`: Automatic pawn promotion detection (Phase 2 ✅)
- `is_checkmate/2`: Complete checkmate detection system
- `check_path_clear/7`: Path validation with recursion protection

## Prolog Development Guidelines

### Code Conventions
- **Predicates**: snake_case (`valid_move/5`, `piece_at/3`)
- **Variables**: PascalCase (`Board`, `GameState`, `FromRow`)
- **Language**: English predicates/code, French comments (SANS ACCENTS - voir section Unicode ci-dessous)
- **Move format**: "e2e4" (not "e2-e4")
- **Comments**: NEVER add status/update comments in source code, readme.md or prd.md (e.g. "Status completed", "New implementation")

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

## AI Implementation Status (Phase 3) - ÉCHEC TP1 ❌

❌ **OBJECTIF TP1 NON ATTEINT - CORRECTION REQUISE**
- **Statut critique**: IA défaillante + tests IA outdated ne passent pas
- **Problème 1**: Blunders tactiques constants (donne matériel gratuitement)  
- **Problème 2**: Section 6 tests IA complètement outdated
- **Impact**: Jeu non utilisable, deadline TP1 (20 oct 2025) en danger
- **Documentation**: `docs/AI_STATUS_HANDOFF.md` contient diagnostic complet avec 4 théories

### ✅ Corrections Partielles Appliquées (Insuffisantes)
- **🚨 BUG g8h6 CORRIGÉ**: Génération coups avec priorités
- **⚡ Génération ouverture AMÉLIORÉE**: Développement vs pions latéraux  
- **📊 Tables piece-square**: Implémentées mais possiblement problématiques
- **🧹 Cleanup projet**: 14 fichiers debug supprimés

### ❌ Problèmes Persistants NON RÉSOLUS
- **🚨 BLUNDERS CONSTANTS**: IA donne matériel gratuitement (Nxd4 sans voir Qxd4)
- **📊 Évaluation défaillante**: Séquence perte cavalier évaluée comme gain (+5 au lieu de -220)
- **🧪 Tests IA outdated**: Section 6 ne passe pas, complètement obsolète
- **⚡ Minimax défaillant**: Algorithme structure OK mais résultats incorrects

### 📋 État Actuel Diagnostic (Septembre 2025)
- **Logique captures**: ✅ FONCTIONNELLE (testée: d4xe5, Nc6xd4, Qd1xd4)
- **Algorithme minimax**: ❌ DÉFAILLANT (structure OK, propagation problématique)
- **Tables évaluation**: ❌ SUSPECTES (bonus positionnels masquent pertes)
- **Tests validation**: ❌ OUTDATED (Section 6 IA à refaire complètement)

## File Dependencies
- interface.pl → game.pl → board.pl → pieces.pl
- ai.pl → pieces.pl, board.pl, game.pl, piece_values_sophisticated.pl
- tests.pl depends on all src modules (Section 6 IA outdated)
- go.pl is launcher (loads interface.pl directly)

## Critical Project Files  
- **🚨 AI Status**: [docs/AI_STATUS_HANDOFF.md](../docs/AI_STATUS_HANDOFF.md) - Diagnostic complet + 4 théories
- **📋 Tasks**: [docs/TASKS.md](../docs/TASKS.md) - Status réaliste TP1 (échec)
- **📊 Plan**: [docs/plan.md](../docs/plan.md) - Découverte critique intégrée
- **🧪 Tests**: [tests/tests.pl](../tests/tests.pl) - Section 6 IA À REFAIRE
- **🤖 IA**: [src/ai.pl](../src/ai.pl) - Algorithme défaillant à debugger
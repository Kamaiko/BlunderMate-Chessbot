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
- **Current Phase**: Phase 1 COMPLETE ✅ - Ready for Phase 2 (advanced rules) or Phase 3 (AI)
- **Architecture**: 4-module design (pieces/board/game/interface) + optimized operations
- **Status**: Hardened foundations, security fixes applied, performance optimized, AI-ready

## Development Commands

### Testing
```bash
# Full test suite (5 categories)
swipl -t run_tests -s tests/chess_tests.pl

# Quick validation tests
swipl -s tests/quick_tests.pl

# Single test category examples
swipl -g "consult('tests/chess_tests'), run_basic_tests, halt."
swipl -g "consult('tests/chess_tests'), run_logic_tests, halt."

# Interactive test session
swipl tests/chess_tests.pl
?- run_tests.
```

### Running the Game
```bash
# Launch chess game (stable)
swipl go.pl

# Direct interface launch (stable)
swipl -s src/interface.pl -g start

# ⚠️ AI mode (PROTOTYPE - NON FONCTIONNEL)
# NE PAS UTILISER - peut planter le système
swipl -s src/ai.pl -g ai_vs_human_mode
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

### 4-Module Design
- **pieces.pl**: Movement rules per piece type, piece identification
- **board.pl**: 8x8 board representation, coordinates, ASCII display
- **game.pl**: Game state management, move validation, capture logic
- **interface.pl**: French UI, game loop, user interaction

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
- `execute_move/6`: Move execution with state update  
- `parse_algebraic_move/5`: Convert "e2e4" format
- `display_game_state/1`: ASCII board display
- `place_piece_optimized/5`: High-performance board operations (AI-ready)
- `check_path_clear/7`: Path validation with recursion protection

## Prolog Development Guidelines

### Code Conventions
- **Predicates**: snake_case (`valid_move/5`, `piece_at/3`)
- **Variables**: PascalCase (`Board`, `GameState`, `FromRow`)
- **Language**: English predicates/code, French comments (no accents)
- **Move format**: "e2e4" (not "e2-e4")

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
**chess_tests.pl** (5 categories):
1. **Basic tests**: Board initialization, display, algebraic notation parsing
2. **Logic tests**: Move validation, game state management  
3. **Piece tests**: Individual piece movement rules (pawn, knight, sliding pieces, king)
4. **Scenario tests**: Opening sequences, tactical combinations
5. **Robustness tests**: Error handling, boundary conditions, path blocking

**quick_tests.pl** (rapid validation):
- System initialization and basic moves (e2e4, e7e5)
- Component validation (board display, move validation)
- Demo functions for interactive testing

### Test-Driven Workflow
1. Write failing test
2. Implement minimal code to pass
3. Run `quick_tests.pl` for fast validation
4. Run full suite before commits

## Common Issues & Solutions

### Debugging Checklist
- **Variables unbound**: Use `ground/1` validation
- **Coordinates out of range**: Ensure 1-8 bounds
- **Move format**: Use "e2e4" not "e2-e4"
- **Infinite loops**: Check `findall/3` usage
- **Trace debugging**: `trace.` then call predicate

### Before Committing
```bash
# Run quick validation
swipl -s tests/quick_tests.pl

# Full test suite validation  
swipl -t run_tests -s tests/chess_tests.pl

# Interactive testing
swipl tests/chess_tests.pl
```

## AI Implementation Status (Phase 3)

⚠️ **IMPORTANT: ai.pl est un PROTOTYPE NON FONCTIONNEL**
- **Statut**: Code expérimental, non testé, potentiellement défaillant
- **Utilisation**: À des fins éducatives et de référence uniquement
- **Recommandation**: Considérer une réécriture complète si implémentation IA nécessaire

### Contenu du Prototype ai.pl
- Algorithme minimax avec alpha-beta (théorique)
- Évaluation de position basique (non validée)
- Interface IA vs Humain (probablement bugguée)
- **NE PAS UTILISER EN PRODUCTION**

### Implémentation IA Future (si nécessaire)
- Réécrire avec architecture modulaire propre
- Tests unitaires complets avant intégration
- Validation algorithme avec positions connues
- Profiling performance réel

## File Dependencies
- interface.pl → game.pl → board.pl → pieces.pl
- All test files depend on all src modules
- go.pl is launcher (loads interface.pl directly)

## Project Context Links
- **User Guide**: [README.md](../README.md)
- **Test Files**: [tests/](../tests/)
- **Source Code**: [src/](../src/)
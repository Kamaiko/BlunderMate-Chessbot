# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

# Prolog Chess Game - AI Focus Project

## Quick Context
- **Project**: Chess AI in Prolog - University AI course (IFT-2003)
- **Current Phase**: Ready for Phase 3 (AI implementation) after solid foundations
- **Architecture**: 4-module design (pieces/board/game/interface)
- **Status**: Complete foundation, comprehensive tests, ready for minimax+alpha-beta

## Development Commands

### Testing
```bash
# Full test suite (6 sections)
swipl -t run_tests -s tests/chess_tests.pl

# Quick validation tests
swipl -s tests/quick_tests.pl

# Single test section (replace section1 with desired section)
swipl -g "consult('tests/chess_tests'), run_test(section1), halt."
```

### Running the Game
```bash
# Launch chess game
swipl go.pl

# Direct interface launch
swipl -s src/interface.pl -g start
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
- `valid_move/5`: Core movement validation
- `execute_move/6`: Move execution with state update  
- `parse_algebraic_move/5`: Convert "e2e4" format
- `display_game_state/1`: ASCII board display

## Prolog Development Guidelines

### Code Conventions
- **Predicates**: snake_case (`valid_move/5`, `piece_at/3`)
- **Variables**: PascalCase (`Board`, `GameState`, `FromRow`)
- **Language**: English predicates/code, French comments (no accents)
- **Move format**: "e2e4" (not "e2-e4")

### Critical Validation Patterns
```prolog
% Always validate arguments
valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol) :-
    ground(Board), ground(Player),
    ground(FromRow), ground(FromCol), ground(ToRow), ground(ToCol),
    % ... rest of validation
```

### Performance Considerations
- Avoid `findall/3` in loops (performance killer)
- Use `once/1` for deterministic predicates
- Validate coordinates are 1-8 range
- Prevent variable unification issues

## Testing Strategy

### Test Structure (6 sections in chess_tests.pl)
1. **Board basics**: Initialization, display, coordinates
2. **Algebraic notation**: Move parsing, validation
3. **Piece movements**: Individual piece rules
4. **Game logic**: Move validation, state updates
5. **Path blocking**: Collision detection
6. **Integration**: Full game scenarios

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
# Validate all tests pass
swipl -s tests/quick_tests.pl

# Check git remotes
git remote -v

# Run specific test section if issues
swipl -g "consult('tests/chess_tests'), run_test(section3), halt."
```

## AI Implementation Roadmap (Phase 3)

### Minimax Implementation
- Implement `minimax/4` with depth limiting
- Add `alpha_beta/6` pruning optimization
- Create `evaluate_position/3` heuristic function

### Position Evaluation Factors
- Material count (piece values)
- Piece mobility and activity
- King safety assessment
- Pawn structure evaluation

### Expected File Structure for AI
```
src/
├── ai/
│   ├── minimax.pl      % Minimax algorithm
│   ├── evaluation.pl   % Position evaluation
│   └── search.pl       % Search optimizations
```

## File Dependencies
- interface.pl → game.pl → board.pl → pieces.pl
- All test files depend on all src modules
- go.pl is launcher (loads interface.pl directly)

## Project Context Links
- **Specifications & Roadmap**: [PRD.md](../PRD.md)
- **User Guide**: [README.md](../README.md)
- **Test Files**: [tests/](../tests/)
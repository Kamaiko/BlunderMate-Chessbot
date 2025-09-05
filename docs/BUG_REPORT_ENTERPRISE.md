# BUG REPORT - Interface Loop on g5e7 Move

**Priority**: High  
**Status**: Open  
**Reporter**: Development Team  
**Assigned**: Next Developer  
**Date**: 2025-01-21  

---

## EXECUTIVE SUMMARY

Critical interface bug causing infinite loop when human player executes specific chess move `g5e7` in AI vs Human mode. Application becomes unresponsive and requires forced termination.

## ENVIRONMENT

| Component | Value |
|-----------|-------|
| **OS** | Windows |
| **Runtime** | SWI-Prolog 9.x+ |
| **Build** | Latest |
| **Mode** | IA vs Humain (AI vs Human) |
| **Affected Files** | `src/interface.pl`, potentially `src/ai.pl` |

## REPRODUCTION STEPS

### Prerequisites
- Launch `swipl go.pl`
- Select "2. Mode IA vs Humain"

### Exact Sequence
1. **Move 1**: Player enters `d2d4` → IA responds with `c7c6` ✅
2. **Move 2**: Player enters `c1g5` → IA responds with `d7d5` ✅ 
3. **Move 3**: Player enters `g5e7` → **BUG TRIGGERED** ❌

### Expected vs Actual Behavior

| Expected | Actual |
|----------|--------|
| Move executed, bishop captures pawn e7, IA plays next move | Interface displays move multiple times, enters infinite loop, no state change |

## SYMPTOMS

### Console Output (Actual)
```
Joueur blanc (tapez "aide")> g5e7
Mouvement joue: g5e7

IA reflechit (noir, minimax alpha-beta)...
Mouvement joue: g5e7

IA reflechit (noir, minimax alpha-beta)...
Mouvement joue: g5e7

IA reflechit (noir, minimax alpha-beta)...
Mouvement joue: g5e7

IA reflechit (noir, minimax alpha-beta)...
Mouvement joue: c1g5              ← Incorrect old move display

IA reflechit (noir, minimax alpha-beta)...
IA joue : 7454 (0.00 sec)

[Board shows bishop still on g5, not e7]
```

### Critical Observations
- ✅ Message "Mouvement joue: g5e7" appears multiple times
- ❌ **Board state never updates** (bishop remains on g5)
- ❌ Inconsistent move display (shows old moves)
- ❌ IA gets same position repeatedly, causing confusion

## TECHNICAL ANALYSIS

### Root Cause Assessment

#### Investigation Results
| Component | Status | Details |
|-----------|---------|---------|
| `valid_move(Board, white, 5, 7, 7, 5)` | ✅ **PASS** | Move validation works correctly |
| `execute_move(...)` | ✅ **PASS** | Board update logic functional |
| `advance_game_state(...)` | ✅ **PASS** | Game state transitions work |
| `make_move(GameState, 5, 7, 7, 5, NewGameState)` | ✅ **PASS** (isolated) | Function works in isolation |
| Interface integration | ❌ **FAIL** | State not propagated through interface |

#### **Root Cause**: Interface State Management Failure

**Detailed Analysis**:

1. **Move Execution Layer** ✅ **FUNCTIONAL**
   ```prolog
   % Tests confirmés - Ces fonctions marchent parfaitement:
   valid_move(Board, white, 5, 7, 7, 5).     % ✅ SUCCÈS
   execute_move(Board, from(5,7), to(7,5), NewBoard, [], NewCaptured).  % ✅ SUCCÈS  
   advance_game_state(white, 4, NewBoard, NewCaptured, FinalState).     % ✅ SUCCÈS
   make_move(GameState, 5, 7, 7, 5, NewGameState).                     % ✅ SUCCÈS isolé
   ```
   
   **Preuve**: Test isolé montre fou blanc se déplaçant correctement g5→e7, pion noir capturé.

2. **Interface State Propagation** ❌ **DÉFAILLANTE**
   
   **Flow Analysis**:
   ```
   unified_game_loop(UnifiedGameState)
     ├─ extract_game_state(UnifiedGameState, GameState)  ✅ OK
     ├─ handle_player_turn(human) calls:
     │   ├─ process_game_input(Input, GameState, NewGameState)
     │   │   └─ attempt_move(GameState, 5,7,7,5, TempGameState)  
     │   │       ├─ make_move(GameState, 5,7,7,5, TempGameState)  ✅ WORKS
     │   │       └─ NewGameState = TempGameState                  ❓ SUSPECT #1
     │   └─ update_unified_game_state(UnifiedGameState, NewGameState, NewUnified)  ❓ SUSPECT #2
     └─ unified_game_loop(NewUnified)  ❌ RECEIVES STALE/WRONG STATE
   ```

3. **Prolog Backtracking Issue** ❌ **CRITICAL**
   
   **Hypothesis**: When IA fails to find move on stale position, Prolog backtracks through:
   ```
   handle_player_turn(UnifiedGameState, black, ai, NewUnifiedGameState) :-
       choose_ai_move(GameState, AIMove),  % FAILS on repeated position
       % Prolog backtracks to previous choice point
   ```
   
   **This causes**:
   - Interface to retry same move multiple times
   - Console shows "Mouvement joue: g5e7" repeatedly  
   - Board never updates because state unification fails
   - IA receives same position indefinitely

4. **State Structure Mismatch** ❓ **POSSIBLE**
   
   **Analysis Required**:
   ```prolog
   % Expected format:
   unified_game_state(Board, Player, MoveCount, Status, CapturedPieces, PlayerTypes)
   
   % vs Actual after move:
   game_state(NewBoard, NewPlayer, NewMoveCount, NewStatus, NewCapturedPieces)
   ```
   
   **Potential Issue**: `update_unified_game_state/3` may not handle all state transitions correctly.

### **Critical Code Paths to Debug**

#### Primary Suspect: `interface.pl:41-45`
```prolog
update_unified_game_state(unified_game_state(_, _, _, _, _, PlayerTypes), 
                          game_state(Board, CurrentPlayer, MoveCount, GameStatus, CapturedPieces),
                          unified_game_state(Board, CurrentPlayer, MoveCount, GameStatus, CapturedPieces, PlayerTypes)).
```

**Debug Required**: Verify this unification succeeds and propagates correctly.

#### Secondary Suspect: `interface.pl:477-480`
```prolog
(make_move(GameState, FromRow, FromCol, ToRow, ToCol, TempGameState) ->
    (coordinates_to_algebraic(FromRow, FromCol, ToRow, ToCol, MoveStr),
     display_message(move_played), write(MoveStr), nl, nl,
     NewGameState = TempGameState)
```

**Debug Required**: Confirm `NewGameState = TempGameState` unification occurs and persists.

#### Tertiary Suspect: `interface.pl:294-295`
```prolog
handle_player_turn(UnifiedGameState, Player, PlayerType, NewUnifiedGameState),
unified_game_loop(NewUnifiedGameState)
```

**Debug Required**: Verify `NewUnifiedGameState` contains updated board position.

### Call Stack Analysis
```
unified_game_loop(UnifiedGameState)
  ├─ handle_player_turn(UnifiedGameState, white, human, NewUnifiedGameState)
  │   ├─ attempt_move(GameState, 5, 7, 7, 5, NewGameState)
  │   │   ├─ make_move(GameState, 5, 7, 7, 5, TempGameState) ✅ SUCCESS
  │   │   └─ NewGameState = TempGameState ❌ STATE LOSS HERE?
  │   └─ update_unified_game_state(...) ❌ POTENTIAL FAILURE POINT
  └─ unified_game_loop(NewUnifiedGameState) ❌ RECEIVES STALE STATE
```

## IMPACT ASSESSMENT

### Severity: **HIGH**
- **Functionality**: Core game loop completely blocked
- **User Experience**: Application freeze, requires force quit
- **Data Integrity**: No data loss, but game unplayable
- **Reproducibility**: 100% reproducible with exact sequence

### Business Impact
- **Testing**: Manual QA blocked for this game path
- **Demo**: Cannot demonstrate AI vs Human mode reliably
- **Delivery**: May delay milestone if not resolved

## FAILED SOLUTIONS ATTEMPTED

### Attempt #1: IA Timeout Protection
- **Approach**: Added `call_with_time_limit(8.0, ...)` to IA
- **Result**: ❌ Failed - Function not available in Windows SWI-Prolog
- **Code**: `src/ai.pl:108-114` (reverted)

### Attempt #2: Emergency Move System  
- **Approach**: `choose_emergency_move/3` with `catch/3` error handling
- **Result**: ⚠️ Partial - Prevents crashes but doesn't fix root cause
- **Code**: `src/ai.pl:116-128` (retained)

### Attempt #3: Limited Depth IA
- **Approach**: Reduced minimax depth from 2 to 1
- **Result**: ❌ Ineffective - Problem is interface, not IA
- **Code**: Reverted to depth 2 per user request

## RECOMMENDED NEXT STEPS

### Priority 1: Interface State Debugging
1. **Add debug traces** to `unified_game_loop` and `handle_player_turn`
2. **Verify state propagation** through `update_unified_game_state`
3. **Check Prolog unification** in interface predicates

### Priority 2: AI Critical Code Issues **[MULTIPLE NEWLY IDENTIFIED]**

#### **Issue A: Piece Safety Disabled**
1. **Root Cause**: `evaluate_piece_safety` always returns 0 (`src/ai.pl:372-373`)
2. **Problem**: Comment states "is_square_attacked ne fonctionne pas encore"
3. **Impact**: AI cannot detect hanging pieces → causes sacrificial moves vs defended pawns

#### **Issue B: Function Name Inconsistency** 
1. **Problem**: `display_position_evaluation` calls `evaluate_tactical_safety` but actual AI uses `evaluate_piece_safety`
2. **Location**: Lines 34-35 vs 274-275 in `src/ai.pl`
3. **Impact**: Debug display and real evaluation use different functions

#### **Issue C: CRITICAL - Empty Square Handling Bug**
1. **Root Cause**: `generate_regular_moves` only checks `Piece \= '.'` but not `Piece \= ' '`
2. **Location**: `src/ai.pl:754` in `generate_regular_moves`
3. **Problem**: Calls `get_piece_color(' ', Player)` on space characters → **FAILS**
4. **Evidence**: `is_empty_square(Piece)` accepts both `' '` and `'.'` but generation only excludes `'.'`
5. **Impact**: **LIKELY CAUSE OF INFINITE LOOP** - Move generation fails on boards with spaces
6. **Fix Required**: 
   ```prolog
   % CURRENT (broken):
   Piece \= '.',
   get_piece_color(Piece, Player),
   
   % FIX:
   \+ is_empty_square(Piece),
   get_piece_color(Piece, Player),
   ```

#### **Issue D: Evaluation Function Calls Non-existent Function**
1. **Location**: `evaluate_center_control` calls `piece_attacks_square` 
2. **Status**: Function exists but may have performance issues

### Priority 3: Specific Investigations  
1. **Position dependency**: Why only `g5e7` triggers this?
2. **Memory state**: Are there stale variable bindings?
3. **Backtracking**: Is Prolog retrying failed operations?
4. **AI evaluation**: Why does broken `piece_safety` not cause more interface issues?

### Priority 4: Defensive Programming
1. **Add state validation** checkpoints in interface loop
2. **Implement circuit breaker** for repeated moves
3. **Add logging** for state transitions

## WORKAROUND

### Temporary Solution
- **Avoid sequence**: Don't play `d2d4`, `c1g5`, `g5e7` 
- **Alternative testing**: Use different opening sequences
- **Mode switch**: Use Humain vs Humain mode for testing

## FILES TO EXAMINE

### Primary Suspects
- `src/interface.pl:472-484` - `attempt_move/6` function
- `src/interface.pl:265-295` - `unified_game_loop/1` main loop  
- `src/interface.pl:41-45` - `update_unified_game_state/3` state management

### Secondary
- `src/game.pl:83-87` - `make_move/6` integration points
- `src/ai.pl:90-114` - Modified IA error handling

## LOGS & DIAGNOSTICS

### Debug Commands
```bash
# Test isolated move
swipl -s debug_test.pl -g "test_g5e7_isolated"

# Check SWI-Prolog version
swipl --version

# Verify file compilation
swipl -g "consult('src/interface'), halt"
```

### State Dump Required
- GameState before `attempt_move`
- NewGameState after `attempt_move`  
- UnifiedGameState before `unified_game_loop` recursion
- Board representation at each step

---

**Next Developer Notes**: This bug requires systematic interface debugging. The core chess logic works perfectly in isolation. Focus investigation on state propagation through the interface layer, particularly around game state unification and Prolog backtracking behavior.
# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Contexte Projet
- **Project**: BlunderMate Chessbot - Patrick Patenaude
- **Date projet**: 20 septembre 2025 - Moteur d'echecs avec IA
- **Current Phase**: Diagnostic critique post-fix - Détection attaque défaillante identifiée
- **Statut** : projet fige depuis septembre 2025. Voir docs/RAPPORT_TECHNIQUE.md pour l'etat reel.
- **Architecture**: 6-module design (pieces/board/game/interface/ai/evaluation)
- **Performance**: Quasi-instantanée (0.00s/coup), évaluation cohérente [EVAL] (+blanc/-noir)
- **Fix minimal** : modification atomique de 2 lignes dans ai.pl
- **Approche** : modification atomique avec points de retour sécurisés


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

### Code Cleanup & Implementation Best Practices

#### **📋 RÈGLE CRITIQUE : NETTOYAGE POST-IMPLÉMENTATION**
**Après chaque implémentation de fonctionnalité, TOUJOURS :**

1. **Supprimer code obsolète** - Fonctions, variables, imports non utilisés
2. **Nettoyer code debug** - Traces, prints temporaires, commentaires debug  
3. **Vérifier cohérence** - S'assurer que tous les appels de fonction sont définis
4. **Valider compilation** - `swipl -s file.pl -g "halt."` sans erreurs
5. **Tester régression** - Suite de tests complète avant commit

#### **Pattern de Nettoyage**
```bash
# Chercher code mort
grep -r "% TODO\|% FIXME\|% DEBUG\|% TEMP" src/ --include="*.pl"

# Chercher fonctions non définies  
grep -r "predicate_name" src/ --include="*.pl"

# Nettoyer traces debug
grep -i "debug\|print\|writeln\|trace" src/ --include="*.pl"
```

**⚠️ JAMAIS laisser du code expérimental ou des bonus/malus éparpillés dans le codebase sans documentation claire.**

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

### ⚠️ **RÈGLES CRITIQUES DE DÉVELOPPEMENT**
- **❌ JAMAIS reconstituer board positions sans validation développeur**
- **✅ TOUJOURS demander validation positions avant tests**
- **📋 Reconstruction board = source d'erreurs constante**
- **🎯 Focus sur logique/algorithmes plutôt que positions spécifiques**

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

## Implementation de l'IA (phase 3)

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

### Bugs critiques resolus

#### **Interface Loop Bug** ✅ **RÉSOLU DÉFINITIVEMENT**
- **Status**: Plus de freeze observés sur séquence `d2d4` → `c1g5` → `g5e7`
- **Validation**: Tests multiples sessions confirment stabilité

#### **Détection Défense Bug** ✅ **RÉSOLU COMPLÈTEMENT**
- **Root cause**: Bug paramètre couleur dans `evaluation.pl:311` (`is_piece_defended`)
- **Correction**: `opposite_player(DefendingPlayer, Opponent)` avant `is_square_attacked`
- **Validation complète**: Tests unitaires + gameplay réel confirment fonctionnement
- **Impact**: Blunders tactiques éliminés, évaluation stable

### Documentation

- **ARCHITECTURE_GUIDE_DEVELOPERS.md**: Guide complet nouveaux développeurs

- **RAPPORT_TECHNIQUE.md**: Spécifications détaillées du projet

### Etat de l'IA apres refactorisation
- **✅ Architecture MVV-LVA**: REFACTORISÉE (captures unifiées, tri immédiat)
- **✅ Détection défense**: CORRIGÉE (bug `opposite_player()` résolu)
- **✅ Pénalités adaptatives**: Dame x1.5, Tour x1.2 pour éviter blunders
- **⚠️ PROBLÈME RÉSIDUEL**: Recaptures Dame contexte GameState vs tests isolés

### Statut du projet
- **✅ Exigences techniques**: IA fonctionnelle, profondeur 2, performance <0.1s/coup
- **✅ Architecture robuste**: 6 modules, suite de tests en place, interface professionnelle
- **✅ Stabilité**: Aucun crash, freeze interface résolu définitivement

- **🎯 Amélioration mesurable**: Knight recapture tardive (6 coups), progrès tactique

### Diagnostic
- **Algorithme IA**: ✅ OPTIMISÉ (négamax + alpha-beta profondeur 2, élagage fonctionnel)
- **Évaluation base**: ✅ STABLE (matériel + PSQT + scores cohérents)  
- **Limitations tactiques**: ⚠️ AGRESSIVE (sacrifices contre pions défendus)
- **Performance**: ✅ OPTIMALE (0.00s/coup, élagage alpha-beta opérationnel)

## File Dependencies
- interface.pl → game.pl → board.pl → pieces.pl
- ai.pl depend de pieces.pl, board.pl, game.pl
- tests.pl depends on all src modules (Section 6 IA outdated)
- go.pl is launcher (loads interface.pl directly)

## Critical Project Files  


- **📚 Architecture**: [docs/ARCHITECTURE_GUIDE_DEVELOPERS.md](../docs/ARCHITECTURE_GUIDE_DEVELOPERS.md) - Guide développeurs complet
- **🧪 Tests**: [tests/tests.pl](../tests/tests.pl) - Section 7 détection défense (tests propres)
- **🤖 IA**: [src/ai.pl](../src/ai.pl) - Négamax + alpha-beta opérationnel
- **⚖️ Évaluation**: [src/evaluation.pl](../src/evaluation.pl) - Piece safety corrigée (ligne 311)









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
- **Current Phase**: Phase 3 ✅ IMPLÉMENTÉE - IA négamax + alpha-beta profondeur 2
- **Status**: IA fonctionnelle mais tactiquement agressive (sacrifie pièces vs pions défendus)
- **Architecture**: 5-module design (pieces/board/game/interface/ai) + psqt_tables.pl  
- **Performance**: 1-4 secondes/coup acceptable, évaluation cohérente [EVAL] (+blanc/-noir)

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

# ✅ AI mode (Phase 3 - ALPHA-BETA FONCTIONNEL)  
# IA avec négamax + alpha-beta + évaluation PSQT simple
swipl go.pl  # Option 2: IA vs Humain (alpha-beta implémenté)
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

## AI Implementation Status (Phase 3) - ⚠️ FONCTIONNELLE AVEC BUG CRITIQUE

✅ **IA NÉGAMAX + ALPHA-BETA FONCTIONNELLE**
- **Algorithme**: Négamax avec élagage alpha-beta, profondeur 2 stable
- **Tri MVV-LVA**: Most Valuable Victim - Least Valuable Attacker + limitation 25 coups
- **PSQT**: Piece-Square Tables ChessProgramming.org intégrées  
- **Évaluation**: Matériel + PSQT (piece safety désactivée temporairement)
- **Interface**: Scores cohérents `[EVAL] Position: X (+blanc/-noir)`
- **Performance**: 1-4 secondes/coup acceptable pour profondeur 2

### 🚨 BUG CRITIQUE IDENTIFIÉ (Janvier 2025)
- **🐛 Interface Loop**: Boucle infinie après mouvement spécifique `g5e7`
- **📍 Position**: Séquence `d2d4`, `c1g5`, `g5e7` cause freeze total
- **🔍 Root Cause**: Échec propagation d'état dans `unified_game_loop`
- **📋 Status**: Bug report détaillé dans `docs/BUG_REPORT_ENTERPRISE.md`
- **🛠️ Correctifs Appliqués**: Gestion d'erreur IA + profondeur 2 restaurée

### ⚠️ PROBLÈMES CRITIQUES IA IDENTIFIÉS (Janvier 2025)

#### **🎯 Cause des Sacrifices - Piece Safety Désactivé**
- **Problème**: `evaluate_piece_safety` **DÉSACTIVÉ** (retourne toujours 0)
- **Raison**: Commentaire "is_square_attacked ne fonctionne pas encore"
- **Impact**: IA ne détecte pas ses pièces en danger → sacrifices contre pions
- **Code**: `src/ai.pl:372-373` - SafetyValue = 0 forcé

#### **🚨 CAUSE PROBABLE BOUCLE INFINIE - Empty Square Bug** 
- **Problème CRITIQUE**: `generate_regular_moves` ne vérifie que `Piece \= '.'` 
- **Bug**: Appelle `get_piece_color(' ', Player)` sur les espaces → **ÉCHEC**
- **Localisation**: `src/ai.pl:754` dans génération de coups
- **Impact**: **CAUSE PROBABLE de la boucle infinie g5e7**
- **Correction**: Remplacer par `\+ is_empty_square(Piece)`

#### **📋 Incohérences Code**
- **Fonctions différentes**: `display_position_evaluation` vs `evaluate_pure_reference` 
- **Noms incohérents**: `evaluate_tactical_safety` vs `evaluate_piece_safety`

### **🛠️ CORRECTIFS PRIORITAIRES**
1. **Critique**: Fixer empty square handling dans `generate_regular_moves`
2. **Important**: Décider si garder ou retirer `piece_safety` 
3. **Nettoyage**: Uniformiser les noms de fonctions d'évaluation

### ⚠️ Limitations Tactiques Identifiées (Décembre 2025)  
- **🎯 Aggressivité excessive**: IA sacrifie fous/cavaliers contre pions défendus
- **🐛 Détection défenses faible**: Ne calcule pas toujours recaptures importantes  
- **📊 Piece safety désactivée**: is_square_attacked partiellement fonctionnel
- **🧹 Scores EVAL corrigés**: Cohérents (+blanc/-noir) mais IA reste tactiquement faible

### 🎯 Status Académique 
- **✅ Exigences techniques**: IA fonctionnelle, profondeur 2, performance acceptable
- **✅ Architecture stable**: 5 modules, tests passent, interface professionnelle  
- **❌ Bug bloquant**: Interface freeze sur certains mouvements
- **⚠️ Recommandation**: Debug interface requis avant démo finale

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
- **Algorithme IA**: ✅ IMPLÉMENTÉ (négamax + alpha-beta profondeur 2)
- **Évaluation base**: ✅ STABLE (matériel + PSQT + scores cohérents)  
- **Limitations tactiques**: ⚠️ AGRESSIVE (sacrifices contre pions défendus)
- **Performance**: ✅ ACCEPTABLE (1-4s/coup, standard académique atteint)

## File Dependencies
- interface.pl → game.pl → board.pl → pieces.pl
- ai.pl → pieces.pl, board.pl, game.pl, piece_values_sophisticated.pl
- tests.pl depends on all src modules (Section 6 IA outdated)
- go.pl is launcher (loads interface.pl directly)

## Critical Project Files  
- **🚨 Bug Report**: [docs/BUG_REPORT_ENTERPRISE.md](../docs/BUG_REPORT_ENTERPRISE.md) - Bug critique interface détaillé
- **📋 Tasks**: [docs/TASKS.md](../docs/TASKS.md) - Roadmap structuré Phase 3 finalisation
- **📊 Plan**: [docs/plan.md](../docs/plan.md) - Découverte critique intégrée
- **🧪 Tests**: [tests/tests.pl](../tests/tests.pl) - Section 6 IA À REFAIRE
- **🤖 IA**: [src/ai.pl](../src/ai.pl) - Négamax + alpha-beta + gestion erreur implémenté

## Session Debug Summary (21 Janvier 2025)

### **Modifications Code**
- **src/ai.pl** (lignes 90-163): Ajout gestion d'erreur sécurisée
- **src/game.pl** (ligne 187): Correction warning singleton variable

### **Fichiers Supprimés**
- `HANDOFF_TO_NEXT_AI.md` - Obsolète
- `archive/AI_STATUS_HANDOFF.md` - Obsolète  
- `archive/ai_v1_defaillante.pl` - Obsolète
- `archive/tests_backup.pl` - Obsolète

### **Diagnostic Réalisé**
- ✅ Logique métier fonctionne (make_move, valid_move, execute_move)
- ❌ Interface state propagation défaillante
- 📋 Bug report complet pour debugging suivant

### **Recommandation Prioritaire**
**FOCUS**: Debug `interface.pl` lignes 41-45, 294-295, 477-480 pour résoudre propagation d'état.
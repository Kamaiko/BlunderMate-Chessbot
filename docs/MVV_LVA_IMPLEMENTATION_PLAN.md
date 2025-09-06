# 🎯 PLAN IMPLÉMENTATION MVV-LVA - DÉTECTION DÉFENSE

**Date**: 2025-09-06  
**Priorité**: CRITIQUE  
**Effort estimé**: 90-120 minutes  
**Objectif**: Corriger détection défense manquante dans système MVV-LVA  

---

## 🚨 PROBLÈME IDENTIFIÉ

### Root Cause
- **Localisation**: `src/evaluation.pl:307-310` - `is_piece_defended/4`
- **Problème**: Fonction fait `fail.` systématiquement (ultra-conservateur)
- **Impact**: IA fait captures perdantes (ex: Dame vs Pion défendu = perte -400 points)

### MVV-LVA Base (Correcte) ✅
```prolog
% ai.pl:252-262 - Implémentation correcte
Score is AbsTargetVal - AbsAttackerVal + 1000  % MVV-LVA basique
```

### Infrastructure Disponible ✅
- `is_square_attacked/4` fonctionnel (game.pl:456)
- Pipeline tri `order_moves/4` → `keysort_desc` opérationnel
- **Seule défaillance**: Détection défense après capture

---

## 📚 STANDARDS PROFESSIONNELS (Context7)

### Python-Chess (Trust 9.6)
```python
# Standards identifiés
board.is_attacked_by(color, square)
board.attackers(color, square)  
```

### Approche Théorique Stockfish
- **Simulation coup** + vérification défense
- **Ajustement score**: `Score_défendue = BaseScore - AttackerValue`

---

## 🛠️ PLAN IMPLÉMENTATION SÉQUENTIEL

### **PHASE 1: CORRECTION IMMÉDIATE is_piece_defended/4** (10 min)

**Fichier**: `src/evaluation.pl:307-310`

**AVANT** (Défaillant):
```prolog
is_piece_defended(_GameState, _Row, _Col, _DefendingPlayer) :-
    fail.  % Systématiquement faux
```

**APRÈS** (Fonctionnel):
```prolog
is_piece_defended(GameState, Row, Col, DefendingPlayer) :-
    GameState = game_state(Board, _, _, _, _),
    is_square_attacked(Board, Row, Col, DefendingPlayer).
```

**Validation**: Teste `evaluate_piece_safety/3` ne retourne plus 0 systématiquement.

---

### **PHASE 2: AMÉLIORATION move_score/4 AVEC DÉTECTION DÉFENSE** (45 min)

**Fichier**: `src/ai.pl:252-262`

**Nouveau prédicat** `move_score_with_defense/4`:
```prolog
% move_score_with_defense(+Board, +Player, +Move, -Score)
% Score MVV-LVA avec détection défense après simulation
move_score_with_defense(Board, Player, [FromRow, FromCol, ToRow, ToCol], Score) :-
    get_piece(Board, ToRow, ToCol, TargetPiece),
    (   \+ is_empty_square(TargetPiece) ->
        % CAPTURE: Calculer score base MVV-LVA
        get_piece(Board, FromRow, FromCol, AttackingPiece),
        piece_value(TargetPiece, TargetVal),
        piece_value(AttackingPiece, AttackerVal),
        AbsTargetVal is abs(TargetVal),
        AbsAttackerVal is abs(AttackerVal),
        BaseScore is AbsTargetVal - AbsAttackerVal + 1000,
        
        % SIMULATION + DÉTECTION DÉFENSE
        make_move_simulation(Board, FromRow, FromCol, ToRow, ToCol, NewBoard),
        opposite_player(Player, Opponent),
        (   is_square_attacked(NewBoard, ToRow, ToCol, Opponent) ->
            % Défendue: ajuster score négativement
            AdjustedScore is BaseScore - AbsAttackerVal,
            Score = AdjustedScore
        ;   % Sûre: score base
            Score = BaseScore
        )
    ;   % NON-CAPTURE
        Score = 0
    ).
```

**Prédicat support** `make_move_simulation/6`:
```prolog
% make_move_simulation(+Board, +FromRow, +FromCol, +ToRow, +ToCol, -NewBoard)
% Simule un coup sans modifier GameState complet (optimisé)
make_move_simulation(Board, FromRow, FromCol, ToRow, ToCol, NewBoard) :-
    get_piece(Board, FromRow, FromCol, Piece),
    place_piece_optimized(Board, FromRow, FromCol, ' ', TempBoard),
    place_piece_optimized(TempBoard, ToRow, ToCol, Piece, NewBoard).
```

---

### **PHASE 3: AJOUT DÉTECTIONS PROMOTIONS + ÉCHECS** (20 min)

**Enrichissement move_score/4** avec détections manquantes:

```prolog
% Détection promotion
detect_promotion_bonus([FromRow, FromCol, ToRow, ToCol], Board, Player, Bonus) :-
    get_piece(Board, FromRow, FromCol, Piece),
    (   (Piece = 'P', ToRow = 8) ; (Piece = 'p', ToRow = 1) ->
        Bonus = 90  % Score promotion Dame
    ;   Bonus = 0
    ).

% Détection échec 
detect_check_bonus(Board, Player, [FromRow, FromCol, ToRow, ToCol], Bonus) :-
    make_move_simulation(Board, FromRow, FromCol, ToRow, ToCol, NewBoard),
    opposite_player(Player, Opponent),
    (   is_in_check(game_state(NewBoard, Opponent, 0, ongoing, []), Opponent) ->
        Bonus = 50  % Score échec forçant
    ;   Bonus = 0
    ).
```

**Integration dans move_score/4**:
```prolog
% move_score/4 ENRICHI (remplace l'ancien)
move_score(Board, Player, Move, FinalScore) :-
    % Score base MVV-LVA avec détection défense
    move_score_with_defense(Board, Player, Move, BaseScore),
    
    % Bonus promotions
    detect_promotion_bonus(Move, Board, Player, PromotionBonus),
    
    % Bonus échecs  
    detect_check_bonus(Board, Player, Move, CheckBonus),
    
    % Score final
    FinalScore is BaseScore + PromotionBonus + CheckBonus.
```

---

## 🧪 TESTS MVV-LVA SECTION 7b (45 min)

### Stratégie Validation

**Défi**: Comment prouver que système tri fonctionne (tri invisible) ?  
**Solution**: Tests positions tactiques précises + validation ordre résultant

### Structure Tests

**Fichier**: `tests/tests.pl` - Nouvelle section 7b après tests alpha-beta

```prolog
% =============================================================================
% SECTION 7B: TESTS MVV-LVA - SYSTÈME TRI CAPTURES
% =============================================================================

test_mvv_lva_system :-
    write('=== Tests MVV-LVA - Système Tri Captures ==='), nl,
    test_mvv_lva_capture_ordering,
    test_mvv_lva_defense_detection,  % NOUVEAU - CRITIQUE
    test_mvv_lva_promotion_priority,
    test_mvv_lva_check_priority,
    write('Tous les tests MVV-LVA: PASSÉS'), nl.
```

### **TEST 1: Ordre Captures Basique**
```prolog
test_mvv_lva_capture_ordering :-
    % Position: Blanc peut capturer Dame(900), Tour(500), Fou(330)
    setup_multi_capture_board(Board),
    GameState = game_state(Board, white, 10, ongoing, []),
    
    generate_moves_simple(GameState, white, AllMoves),
    order_moves(GameState, white, AllMoves, OrderedMoves),
    
    % VALIDATION: Captures triées par valeur décroissante
    validate_capture_order_mvv_lva(OrderedMoves),
    write('Test ordre captures MVV-LVA: PASSÉ'), nl.

% Position test manuel
setup_multi_capture_board([
    ['r', 'n', 'b', 'q', '.', '.', '.', 'r'],  % Dame q, Tour r disponibles
    ['.', '.', '.', '.', '.', 'b', '.', '.'],  % Fou b disponible
    ['.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', 'Q', '.', '.', '.', '.'],  % Dame blanche peut tout capturer
    ['.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.'],
    ['R', 'N', 'B', '.', 'K', '.', '.', 'R']
]).
```

### **TEST 2: Détection Défense** ✨ **NOUVEAU - CRITIQUE**
```prolog
test_mvv_lva_defense_detection :-
    % Position: Dame peut prendre pion défendu vs Tour libre
    setup_defended_capture_board(Board),
    
    move_score(Board, white, [4,4,7,4], ScoreDefended),  % Dame×pion défendu
    move_score(Board, white, [4,4,8,4], ScoreSafe),     % Dame×tour libre
    
    % VALIDATION CRITIQUE: ScoreSafe > ScoreDefended
    (   ScoreSafe > ScoreDefended ->
        write('Test détection défense: PASSÉ'), nl
    ;   write('ÉCHEC: Détection défense non fonctionnelle'), nl, fail
    ).

% Position: pion d7 défendu par pion c6, tour h8 libre
setup_defended_capture_board([
    ['r', 'n', 'b', 'q', 'k', '.', '.', 'r'],  % Tour h8 libre
    ['.', '.', 'p', 'p', '.', '.', '.', '.'],  % pion d7, c7
    ['.', '.', 'p', '.', '.', '.', '.', '.'],  % pion c6 défend d7
    ['.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', 'Q', '.', '.', '.', '.'],  % Dame blanche
    ['.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.'],
    ['R', 'N', 'B', '.', 'K', '.', '.', 'R']
]).
```

### **TEST 3: Promotions Priorisées**
```prolog
test_mvv_lva_promotion_priority :-
    % Position: Promotion disponible + captures mineures
    setup_promotion_board(Board),
    GameState = game_state(Board, white, 50, ongoing, []),
    
    order_moves(GameState, white, AllMoves, OrderedMoves),
    
    % VALIDATION: Premier coup = promotion
    OrderedMoves = [FirstMove|_],
    is_promotion_move(FirstMove, Board),
    write('Test priorité promotions: PASSÉ'), nl.
```

### **TEST 4: Échecs Priorisés**
```prolog
test_mvv_lva_check_priority :-
    % Position: Échec possible + coups neutres
    setup_check_available_board(Board),
    
    move_score(Board, white, [3,3,7,7], CheckScore),    % Coup échec
    move_score(Board, white, [2,2,3,3], NeutralScore),  % Coup neutre
    
    % VALIDATION: CheckScore > NeutralScore
    CheckScore > NeutralScore,
    write('Test priorité échecs: PASSÉ'), nl.
```

---

## ⚡ VALIDATION RÉGRESSION

### Tests Existants (10 min)
```bash
# Vérifier que corrections n'impactent pas tests actuels
swipl -t run_tests -s tests/tests.pl
```

**Si échecs**: Ajuster tests existants pour nouveaux scores MVV-LVA.

---

## 📊 MÉTRIQUES SUCCÈS

### **Avant Correction** (État actuel)
- `is_piece_defended/4`: Fail systématique ❌
- IA: Dame prématurée, captures perdantes ❌
- Tests MVV-LVA: Inexistants ❌

### **Après Correction** (Objectif)
- `is_piece_defended/4`: Détection fonctionnelle ✅
- IA: Captures sûres privilégiées ✅
- Tests MVV-LVA: 4 tests section 7b ✅
- Blunders tactiques: Réduction significative ✅

---

## 🚨 RISQUES IDENTIFIÉS

### **Performance Impact** ⚠️
- **Problème**: Simulation coup pour chaque `move_score/4`
- **Mitigation**: Limiter aux captures importantes (>300 points)

### **Compatibilité Tests** ⚠️  
- **Problème**: Tests existants avec anciens scores
- **Mitigation**: Tests isolés section 7b + validation régression

### **Complexité Positions** ⚠️
- **Problème**: Positions d'échecs manuelles difficiles
- **Mitigation**: Positions simplifiées mais tactiquement correctes

---

## 📅 SÉQUENCE EXÉCUTION

1. **[10 min]** Corriger `is_piece_defended/4`
2. **[45 min]** Implémenter détection défense `move_score/4`  
3. **[20 min]** Ajouter promotions + échecs scoring
4. **[45 min]** Créer tests section 7b
5. **[10 min]** Validation régression

**Total**: 90-120 min ✅

---

## 🎯 IMPACT ATTENDU

Cette implémentation résoudra **directement** les problèmes critiques identifiés:
- ✅ **Blunders tactiques** (captures perdantes éliminées)
- ✅ **Dame prématurée** (promotions priorisées)  
- ✅ **Hanging pieces** (détection défense fonctionnelle)

**ROOT CAUSE CORRIGÉE** → **IA tactiquement cohérente**
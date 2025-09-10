# 🔧 PLAN DE REFACTORING - ARCHITECTURE GÉNÉRATION DE COUPS

**Date**: 2025-01-09  
**Statut**: ✅ **RÉSOLU PARTIELLEMENT** - 2025-01-10  
**Objectif**: ~~Résoudre les recaptures manquées~~ ✅ RÉSOLU (problème coordonnées)  
**Problème initial**: Dame noire ne recapture pas Fou blanc sur d6 après séquence `d2d4→b1c3→c1f4→e2e3→f1d3→f4d6`  
**Root cause trouvée**: ✅ Confusion coordonnées Row=8,Col=4 vs Row=1,Col=4 pour Dame d8

---

## 📊 ANALYSE ARCHITECTURALE CRITIQUE

### ✅ **FONCTIONNALITÉS DÉJÀ IMPLÉMENTÉES ET FONCTIONNELLES**

Après analyse approfondie du codebase, ces fonctionnalités critiques **existent déjà** et fonctionnent correctement:

- ✅ `is_in_check/2` - Détection d'échec (game.pl:451)
- ✅ `is_checkmate/2`, `is_stalemate/2` - Détection mat/pat (game.pl:589, 596)
- ✅ `is_promotion_move/3` - Gestion promotions (pieces.pl:147-148)
- ✅ `is_square_attacked/4` - Détection attaque case (game.pl:461)
- ✅ `is_piece_defended/4` - Détection défense pièces (evaluation.pl:306)
- ✅ `move_leaves_king_in_check/6` - Validation légalité (game.pl:642)
- ✅ `valid_move/6` avec validation échec complète (game.pl:45)
- ✅ `simulate_move/6` - Simulation coups (game.pl:651)

### ❌ **PROBLÈME ARCHITECTURAL RÉEL IDENTIFIÉ**

Le problème **N'EST PAS** dans les fonctionnalités manquantes mais dans l'architecture défaillante de `generate_structured_moves/3` (ai.pl:405-548) qui:

1. **Sépare artificiellement captures/développement** → L'IA ne voit pas les recaptures dans le contexte global
2. **Applique restrictions géographiques hardcodées** → Bloque recaptures légitimes (d6) pendant 15 coups
3. **Trie tardivement et inefficacement** → MVV-LVA appliqué sur liste fragmentée
4. **Utilise logique de priorisation rigide** → Dame bloquée 6 premiers coups indépendamment du contexte

---

## 🎯 PLAN DE REFACTORING DÉTAILLÉ

### **PHASE 1: UNIFICATION DE LA GÉNÉRATION DE COUPS** 
**Priorité**: 🔴 **CRITIQUE**  
**Durée estimée**: 45 minutes  
**Fichiers**: `src/ai.pl`

#### Problème actuel (lignes 405-548):
```prolog
% 0. CAPTURES PRIORITAIRES - SÉPARÉES
findall([FromRow, FromCol, ToRow, ToCol], (
    % ... logique capture isolée
), AllCaptureMoves),

% 1. DÉVELOPPEMENT PRIORITAIRE - SÉPARÉ  
findall([FromRow, FromCol, ToRow, ToCol], (
    % ... logique développement isolée
), DevelopmentMoves),
```

#### Solution unifiée:
```prolog
generate_unified_moves(GameState, Player, Moves) :-
    GameState = game_state(Board, _, MoveCount, _, _),
    
    % ÉTAPE 1: Génération unifiée de TOUS les coups légaux
    findall([FromRow, FromCol, ToRow, ToCol], (
        between(1, 8, FromRow),
        between(1, 8, FromCol),
        get_piece(Board, FromRow, FromCol, Piece),
        \+ is_empty_square(Piece),
        get_piece_color(Piece, Player),
        between(1, 8, ToRow),
        between(1, 8, ToCol),
        valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol)  % DÉJÀ valide légalement
    ), AllLegalMoves),
    
    % ÉTAPE 2: Classification tactique immédiate
    classify_moves_tactically(GameState, Player, AllLegalMoves, ClassifiedMoves),
    
    % ÉTAPE 3: Tri MVV-LVA immédiat 
    tactical_sort(ClassifiedMoves, SortedMoves),
    
    % ÉTAPE 4: Limitation adaptative intelligente
    adaptive_move_limit(MoveCount, Limit),
    take_first_n_simple(SortedMoves, Limit, Moves).
```

### **PHASE 2: CLASSIFICATION TACTIQUE INTELLIGENTE**
**Priorité**: 🔴 **CRITIQUE**  
**Durée estimée**: 30 minutes

#### Classification unifiée par priorité:
```prolog
classify_single_move(GameState, Player, [FromRow, FromCol, ToRow, ToCol], Priority-Move) :-
    GameState = game_state(Board, _, _, _, _),
    get_piece(Board, ToRow, ToCol, TargetPiece),
    get_piece(Board, FromRow, FromCol, AttackingPiece),
    Move = [FromRow, FromCol, ToRow, ToCol],
    
    (   % 1. PROMOTION (priorité absolue)
        is_promotion_move(Player, FromRow, ToRow) -> Priority = 1500
    ;   % 2. CAPTURES MVV-LVA (inclut Qd8xd6)
        \+ is_empty_square(TargetPiece) ->
        piece_value(TargetPiece, TargetVal),
        AbsTargetVal is abs(TargetVal),
        (   AbsTargetVal >= 900 -> Priority = 1000  % Dame capturée
        ;   AbsTargetVal >= 500 -> Priority = 800   % Tour capturée  
        ;   AbsTargetVal >= 300 -> Priority = 700   % Fou/Cavalier capturés ⭐ CORRIGE Qd8xd6
        ;   Priority = 600  % Pion capturé
        )
    ;   % 3. DÉVELOPPEMENT INTELLIGENT
        member(AttackingPiece, ['N','n','B','b']) -> Priority = 400
    ;   member(AttackingPiece, ['P','p']) -> Priority = 300  
    ;   % 4. DAME/TOUR/ROI (contexte adaptatif)
        Priority = 200
    ).
```

### **PHASE 3: SUPPRESSION DES RESTRICTIONS HARDCODÉES**
**Priorité**: 🟡 **HAUTE**  
**Durée estimée**: 15 minutes

#### Restrictions actuelles à supprimer:
```prolog
% ❌ SUPPRIMÉ: Restrictions géographiques fixes
(MoveCount =< 15 ->
    (ToRow >= 3, ToRow =< 6, ToCol >= 3, ToCol =< 6)  % Bloque d6!
;   true
)

% ❌ SUPPRIMÉ: Blocage Dame rigide  
(MoveCount =< 6 ->
    member(Piece, ['R','r','K','k'])      % Pas de Dame!
;   member(Piece, ['R','r','K','k','Q','q'])
)
```

#### Solution adaptative intelligente:
```prolog
% ✅ NOUVEAU: Limitations adaptatives basées sur valeur tactique
adaptive_move_limit(MoveCount, Limit) :-
    (   MoveCount =< 10 -> Limit = 25      % Ouverture: plus de choix tactiques
    ;   MoveCount =< 20 -> Limit = 20      % Milieu: équilibré  
    ;   Limit = 15                         % Fin: plus focalisé
    ).
```

### **PHASE 4: VALIDATION ET TESTS**
**Priorité**: 🟡 **HAUTE**  
**Durée estimée**: 30 minutes

#### Tests de validation:
1. **Test séquence critique**: `d2d4→b1c3→c1f4→e2e3→f1d3→f4d6`
   - Vérifier que `Qd8xd6` (Priority=700) > `Qd8-d7` (Priority=200)
2. **Tests régression**: Suite complète `tests/tests.pl`
3. **Tests performance**: Timing <0.1s/coup maintenu

---

## 📚 FONCTIONNALITÉS AVANCÉES (OPTIONNELLES)

Ces fonctionnalités sont **déjà implémentées** ou **non critiques** pour résoudre les recaptures:

### ✅ **Déjà implémentées** (ne nécessitent pas de refactoring):
- Détection échec donné (via `is_in_check` après simulation)
- Défense pièces attaquées (via `is_piece_defended`)  
- Validation légalité complète (via `valid_move` + `move_leaves_king_in_check`)
- Gestion promotions (via `is_promotion_move`)

### 🟢 **Améliorations futures** (non critiques actuellement):
- Détection pièces clouées spécialisée
- Roque et en passant (règles avancées)  
- Contrôle du centre avancé
- Détection répétition positions
- Opening book théorique

---

## ⚠️ **POINTS CRITIQUES D'IMPLÉMENTATION**

### **1. Préservation des fonctionnalités existantes**
- **NE PAS MODIFIER** `valid_move/6` (fonctionne parfaitement)
- **NE PAS MODIFIER** les systèmes de détection échec/mat (fonctionnels)
- **RÉUTILISER** les fonctions existantes dans la nouvelle architecture

### **2. Migration progressive**
```prolog
% Phase de transition - dual support
generate_moves(GameState, Player, Moves) :-
    % Mode unifié (nouveau)
    (   use_unified_generation ->
        generate_unified_moves(GameState, Player, Moves)
    ;   % Mode legacy (ancien) - fallback sécurisé
        generate_structured_moves(GameState, Player, Moves)
    ).
```

### **3. Tests de régression critiques**
- ✅ Tous les tests existants doivent passer
- ✅ Performance maintenue <0.1s/coup  
- ✅ Pas de nouveaux blunders introduits
- ✅ Recapture `Qd8xd6` correctement priorisée

---

## 🎯 **RÉSULTATS ATTENDUS**

Après implémentation complète:

### **Séquence test `d2d4→b1c3→c1f4→e2e3→f1d3→f4d6`**:
1. **Génération**: Tous les coups légaux incluent `Qd8xd6`
2. **Classification**: `Qd8xd6` → Priority=700 (Fou capturé)
3. **Tri**: `Qd8xd6` classée avant `Qd8-d7` (Priority=200)
4. **Sélection**: IA choisit la recapture correcte

### **Performance globale**:
- ✅ Recaptures Dame/Roi/Fous fonctionnelles
- ✅ Architecture extensible et maintenable  
- ✅ Suppression des comportements hardcodés
- ✅ Logique tactique cohérente et intelligente

---

## 📋 **CHECKLIST D'IMPLÉMENTATION**

- [ ] **Phase 1**: Implémenter `generate_unified_moves/3`
- [ ] **Phase 2**: Implémenter `classify_moves_tactically/4`
- [ ] **Phase 3**: Supprimer restrictions hardcodées
- [ ] **Phase 4**: Tests validation séquence critique
- [ ] **Validation**: Suite tests complète passe
- [ ] **Performance**: Timing <0.1s maintenu
- [ ] **Documentation**: Mise à jour CLAUDE.md et TASKS.md

**Durée totale estimée**: ~~2 heures~~ **ÉVALUATION POST-IMPLÉMENTATION**  
**Impact**: ~~Résolution définitive des recaptures manquées~~ ✅ RÉSOLU différemment

---

## 🎯 **RÉSULTATS POST-IMPLÉMENTATION** (2025-01-10)

### **✅ IMPLÉMENTATIONS RÉALISÉES**
- ✅ **Phase 1 complétée**: `generate_unified_moves/3` implémentée et fonctionnelle
- ✅ **Phase 2 complétée**: `classify_moves_tactically/4` avec priorités MVV-LVA
- ✅ **Phase 3 complétée**: Restrictions hardcodées supprimées
- ✅ **Phase 4 partiellement**: Tests validation séquence critique réussis

### **🎯 ROOT CAUSE DÉCOUVERTE**
**Le problème N'ÉTAIT PAS architectural** mais dans la **convention coordonnées**:
- `get_piece(Board, Row, Col)` utilise conversion `9-Row`
- Dame d8 = **Row=8,Col=4** (pas Row=1,Col=4)
- Recapture `Qd8xd6` = `[8,4,6,4]` fonctionne avec Priority=1000

### **📋 ÉTAT ACTUEL**
- ✅ **Architecture unifiée** fonctionne mais n'était pas nécessaire
- ✅ **Recaptures Dame** résolues via correction coordonnées
- ❌ **NOUVEAUX BUGS** découverts plus critiques (validation coups f2f3)

### **🎯 RECOMMANDATIONS**
1. **Garder architecture unifiée** (amélioration qualité code)
2. **Focus nouveaux bugs critiques** (validation coups illégaux)
3. **Marquer ce document** comme référence historique mais priorités changées
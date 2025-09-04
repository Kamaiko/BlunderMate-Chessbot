# TASKS - Projet Échecs IA (IFT-2003)

> **Travail universitaire TP1** - 10% note finale IFT-2003

## 🎯 STATUS PROJET - Vue d'ensemble

| Phase | Status | Contenu | Tests |
|-------|--------|---------|-------|
| **Phase 1** | ✅ **COMPLÈTE** | Fondations, mouvements base, interface | 28 tests |
| **Phase 2** | ✅ **COMPLÈTE** | Échec/mat, promotion pions | +7 tests (35 total) |
| **Phase 3** | ⚡ **EN FINALISATION** | IA alpha-beta + PSQT éducative | +7 tests (42 total) |

### ✅ Situation Actuelle (Septembre 2025)
- **Architecture IA** : Alpha-beta négamax fonctionnel
- **Interface** : Mode IA vs Humain opérationnel  
- **Tests** : 42/42 PASS - Moteur de jeu robuste
- **À finaliser** : Piece-Square Tables + tests tactiques

---

## 🧠 ARCHITECTURE IA CONFIRMÉE

### ✅ Algorithmes Implémentés
- [x] **Négamax (variante minimax)** : `minimax_ab/5` avec alpha-beta et élagage
- [x] **Tri MVV-LVA** : Most Valuable Victim - Least Valuable Attacker  
- [x] **Détection terminale** : Mat (-100000), Pat (0)
- [ ] **Coups d'ouverture** : ~~Caro-Kann fixes~~ → **SUPPRIMER pour IA authentique**

### 📊 Évaluation (Architecture Éducative)
- ✅ **Matériel** : Valeurs centipawns (P:100, N:320, B:330, R:500, Q:900)
- ⚠️ **PSQT à implémenter** : Piece-Square Tables (ChessProgramming.org)
- ❌ **SEE abandonnée** : Trop complexe pour niveau universitaire
- ✅ **Mobilité** : Nombre coups légaux par joueur

---

## 🎯 PLAN SESSION DEMAIN (2025-09-05)

### 📋 Checklist Critique (105 minutes) - CONSISTENCY + SUPPRESSION COUPS FIXES

#### 🚨 PHASE 0 : Consistency Fixes + IA Pure (45 min) - NOUVEAU  
- [ ] **🎯 PRIORITÉ 0: Supprimer coups fixes** : IA négamax pur dès coup 1 (ÉDUCATIF)
- [ ] **🚨 PRIORITÉ 1: Tests cassés** : Ajouter `run_all_tests` dans tests.pl
- [ ] **🚨 PRIORITÉ 2: Cases vides** : `' '` seulement, supprimer `'.'` inconsistant
- [ ] **Validation Menu** : Option 3 "Tests" fonctionne sans erreur
- [ ] **Test IA authentique** : 1.e4 → IA répond via PSQT (pas scripts)

#### 🔥 PHASE 1 : Piece-Square Tables (30 min)
- [ ] **Implémenter PSQT** selon ChessProgramming.org Simplified
- [ ] **Remplacer évaluation complexe** par PSQT simple
- [ ] **Valeurs centrales** : Cavaliers centre bon, bords mauvais  
- [ ] **Test intégration** : Vérifier évaluation fonctionne

#### 🧪 PHASE 2 : Tests Structurés (30 min)  
- [ ] **Test mat en 1** : IA choisit coup gagnant unique
- [ ] **Test parade** : IA joue seule défense anti-mat
- [ ] **Test recaptures** : e4xd5 → c6xd5 automatique
- [ ] **Validation alpha-beta** : Même résultats que minimax

#### ⚡ PHASE 3 : Finalisation (15 min)
- [ ] **Performance** : <10s profondeur 2-3  
- [ ] **Documentation** : Commit status final avec consistency fixes

---

## 🔧 Corrections Techniques Requises (Cross-File Consistency Check)

### 🚨 Priorité 1 - Consistency Fixes (CRITIQUE)
1. **🚨 Tests cassés** : Ajouter `run_all_tests` manquant dans tests.pl
2. **🚨 Cases vides** : Standardiser `' '` vs `'.'` (pieces.pl, board.pl, ai.pl inconsistant)
3. **Implémenter PSQT** : ChessProgramming.org Simplified Evaluation

### ⚠️ Priorité 2 - Architecture IA
1. **Enlever SEE** : Remplacer par évaluation positionnelle basique
2. **Bonus développement** : Réduire 100→30 points max
3. **Génération coups** : Garde-fou si roi en échec

### 🔧 Priorité 3 - Optimisation
1. **Validation** : `fast_get_piece/4` optimisée IA
2. **Performance** : Cache positions pour éviter boucles 8x8

---

## 📊 TESTS STRUCTURÉS PLANIFIÉS

### Phase 0 : Consistency Tests (NOUVEAU)
```prolog
% Test cases vides cohérentes
test_empty_cell_consistency :-
    % Vérifier que tous modules utilisent ' ' pour cases vides
    create_empty_board(Board),
    get_piece(Board, 1, 1, EmptyCell),
    EmptyCell = ' '.  % DOIT être espace, pas point

% Test run_all_tests existe
test_run_all_tests_exists :-
    % Vérifier que le prédicat appelé par interface.pl existe
    current_predicate(run_all_tests/0).
```

### Phase 1 : Tests Unitaires Contrôlés  
```prolog
% Test mat en 1 : IA trouve coup gagnant unique
test_mate_in_one :-
    % Position où seul Qd8# mate, autres perdent
    setup_mate_in_one_position(GameState),
    choose_ai_move(GameState, Move),
    assert_is_checkmate_move(Move).
    
% Test parade : IA évite mat imminent  
test_avoid_mate :-
    % Position où seul coup évite défaite
    setup_must_defend_position(GameState),
    choose_ai_move(GameState, Move),
    assert_avoids_checkmate(Move).
```

### Phase 2 : Validation Alpha-Beta
```prolog
% Consistency : alpha-beta = minimax sur positions test
test_alphabeta_consistency :-
    forall(test_position(GS), (
        minimax_no_pruning(GS, Move1, Value1),
        minimax_ab(GS, Move2, Value2),
        assert_same_evaluation(Value1, Value2)
    )).
```

### Phase 3 : Anti-Blunder Tactique
- **Recaptures obligatoires** : e4xd5 → c6xd5
- **Menaces détectées** : Cavalier f6 attaqué par e5  
- **Pas de sacrifices** : Éviter dame vs pion

---

## 🧩 Architecture 5 Modules

### Modules Stables ✅
- **pieces.pl** : Logique mouvement, validation pièces
- **board.pl** : Représentation 8x8, affichage, conversion
- **game.pl** : État jeu, validation coups, échec/mat
- **interface.pl** : Interface française, modes de jeu

### Module En Finalisation ⚡
- **ai.pl** : Alpha-beta + PSQT à compléter

### Prédicats Clés
- `minimax_ab/5` : Alpha-beta négamax fonctionnel
- `choose_ai_move/2` : Interface IA principale  
- `evaluate_position/3` : À enrichir avec PSQT
- `generate_moves_simple/3` : Génération coups optimisée

---

## 🎓 Objectif Éducatif

### Niveau Approprié TP1
- **Algorithme moderne** : Négamax + alpha-beta
- **Évaluation simple** : Matériel + position basique  
- **Code commenté** : Compréhensible niveau universitaire
- **Performance raisonnable** : <10s acceptable

### Extensions Futures (Post-TP1)
- [ ] **En passant + roque** : Compléter règles échecs
- [ ] **Opening book** : Base théorique ouvertures
- [ ] **Quiescence search** : Éviter horizons tactiques
- [ ] **Interface avancée** : Choix difficulté/couleur

---

## 📚 Ressources & Commandes

**Documentation** : [CLAUDE.md](../.claude/CLAUDE.md) • [plan.md](plan.md)  
**Tests** : `swipl -g "consult('tests/tests'), run_tests, halt."`  
**Jeu** : `swipl go.pl` (Option 2: IA vs Humain)

**Ressource PSQT** : [ChessProgramming.org Simplified Evaluation](https://www.chessprogramming.org/Simplified_Evaluation_Function)

---

## ⚡ Décisions Architecturales Finales

### ✅ Confirmées
- **Négamax** : Variante symétrique minimax (`Value is -OpponentValue`)
- **Alpha-beta** : Élagage implémenté avec négamax
- **Terminologie** : "Minimax négamax" = algorithme implémenté
- **PSQT** : Compatible avec négamax, niveau éducatif parfait
- **Abandon SEE** : Trop complexe, pas nécessaire TP1

### 🎯 Session Demain - Objectif
**Implémenter Piece-Square Tables pour compléter l'évaluation négamax**

**Note Terminologique** : Votre algorithme EST négamax (minimax symétrique) - plus simple à implémenter !

Terminer avec une IA robuste, performante et appropriée niveau universitaire !
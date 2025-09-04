# 🎯 Plan de Développement IA d'Échecs - IFT-2003

## 📋 Vue d'Ensemble Projet

| Aspect | Détail |
|--------|--------|
| **🎓 Contexte** | TP1 Universitaire IFT-2003 - Intelligence Artificielle |
| **⏰ Deadline** | 20 octobre 2025 |
| **🎯 Objectif** | IA d'échecs éducative avec négamax + alpha-beta |
| **🏗️ Architecture** | 5 modules Prolog (pieces/board/game/interface/ai) |
| **✅ Acquis** | Négamax + alpha-beta + tri MVV-LVA fonctionnels |
| **🚨 Problème** | Blunders tactiques constants, évaluation position incomplète |

## 🎯 Statut Actuel (Audit Septembre 2025)

### ✅ Fonctionnalités Complètes
- **Algorithme**: Négamax avec élagage alpha-beta (profondeur 2)
- **Tri des coups**: MVV-LVA (Most Valuable Victim - Least Valuable Attacker)  
- **Détection terminale**: Mat/Pat avec scores appropriés
- **Interface**: Mode Humain vs IA fonctionnel
- **Tests**: 42/42 tests passent
- **Ouverture**: ~~Coups fixes Caro-Kann~~ → **À SUPPRIMER pour IA pure**

### ❌ Problèmes Identifiés (Cross-File Consistency Check - Sept 2025)
1. **🎯 Coups fixes Caro-Kann** → **SUPPRIMER pour IA authentique** (ÉDUCATIF)
2. **Évaluation position manquante** → Implémenter Piece-Square Tables
3. **🚨 Cases vides inconsistantes** → Standardiser `' '` vs `'.'` (CRITIQUE)  
4. **🚨 Tests cassés** → `run_all_tests` manquant dans tests.pl (MAJEUR)
5. **Blunders tactiques** → Recaptures manquées, sacrifices involontaires
6. **Bonus développement excessif** → 100 points vs 30 recommandés
7. **Tests structurés manquants** → Mat en 1, parade obligatoire

## 📅 Phase 1: Fondations Évaluatives (Session 1 - 105 min)

### 🎯 Objectif: Implémenter évaluation positionnelle éducative simple

#### ⚡ Tâche 1.1: Piece-Square Tables ChessProgramming.org (45 min)
- [ ] **Définir valeurs centipawns standard**: P:100, N:320, B:330, R:500, Q:900
- [ ] **Implémenter PSQT pions**: Avant-postes bonus, centre encouragé
- [ ] **PSQT cavaliers**: Bords -50pts, cases centrales +bonus
- [ ] **PSQT fous**: Diagonales longues privilégiées
- [ ] **PSQT tours**: Colonnes ouvertes, 7ème rangée
- [ ] **PSQT dame**: Éviter développement prématuré
- [ ] **PSQT roi**: Sécurité milieu de partie, activité finale

**Critères validation**:
- Cavalier a6 pénalisé vs cavalier d4 avantagé
- Pion e4 > pion a4 (contrôle centre)
- Code éducatif simple, commenté pour compréhension

#### 🔧 Tâche 1.2: Simplification Architecture + Consistency Fix (45 min)
- [ ] **🎯 PRIORITÉ 0: Supprimer coups fixes** → IA pure négamax dès coup 1 (ÉDUCATIF)
- [ ] **🚨 PRIORITÉ 1: Fixer `run_all_tests`** → Ajouter prédicat manquant tests.pl
- [ ] **🚨 PRIORITÉ 2: Standardiser cases vides** → `' '` partout (vs `'.' inconsistent`)
- [ ] **Enlever SEE complètement**: Trop complexe niveau universitaire
- [ ] **Réduire bonus développement**: 100 → 30 points max
- [ ] **Nettoyer évaluation**: Matériel + PSQT + mobilité basique seulement

**Critères validation**:
- **IA authentique**: 1.e4 → IA répond e5/c5 via PSQT (pas scripts fixes)
- Menu Tests (Option 3) fonctionne sans erreur  
- Cases vides cohérentes: pieces.pl, board.pl, ai.pl alignés
- Code plus lisible, commentaires éducatifs
- Performance maintenue <10s profondeur 2-3

#### 📖 Tâche 1.3: Documentation Architecture (15 min)
- [ ] **Commenter algorithme négamax**: Chaque étape expliquée
- [ ] **Documenter évaluation**: Justification choix PSQT
- [ ] **Guide utilisation**: Comment modifier profondeur/évaluation

## 📅 Phase 2: Tests Structurés (Session 2 - 90 min)

### 🎯 Objectif: Validation robustesse avec tests exhaustifs

#### 🧪 Tâche 2.1: Tests Mat en 1 (30 min)
- [ ] **Position test 1**: Dame en h8, mat Qd8# obligatoire
- [ ] **Position test 2**: Tour sacrifice, seul coup gagnant  
- [ ] **Position test 3**: Mat étouffé, cavalier forcé
- [ ] **Validation**: IA choisit LE coup gagnant à chaque fois

**Template test**:
```prolog
test_mate_in_one_basic :-
    % Position où seul Qd8# gagne
    setup_mate_position(Board),
    choose_ai_move(game_state(Board, white, 10, active, []), Move),
    Move = [_, _, 8, 4].  % Doit être vers d8
```

#### ⚔️ Tâche 2.2: Tests Parade Obligatoire (30 min)  
- [ ] **Éviter mat**: Seul coup défensif doit être joué
- [ ] **Blocage échec**: Interposition obligatoire  
- [ ] **Capture attaquant**: Éliminer menace immédiate
- [ ] **Validation**: IA évite mat systématiquement

#### 🔄 Tâche 2.3: Validation Alpha-Beta (30 min)
- [ ] **Test consistency**: Négamax = négamax avec élagage
- [ ] **Positions aléatoires**: 100 positions test
- [ ] **Profondeurs variables**: Depth 1, 2, 3 cohérentes
- [ ] **Performance**: Élagage effectif (temps réduit)

**Template validation**:
```prolog
test_alpha_beta_consistency :-
    generate_random_position(Board),
    minimax_no_pruning(Board, white, 2, Score1),
    minimax_ab(Board, white, 2, -9999, 9999, Score2),
    Score1 =:= Score2.
```

## 📅 Phase 3: Corrections Tactiques (Session 3 - 90 min)

### 🎯 Objectif: Éliminer blunders, améliorer jeu tactique

#### 🛡️ Tâche 3.1: Recaptures Automatiques (45 min)
- [ ] **Détecter échanges**: Si adversaire capture, considérer recapture en priorité
- [ ] **Tri amélioré**: Recaptures avant autres coups dans MVV-LVA
- [ ] **Test e4xd5 c6xd5**: Validation recapture systématique
- [ ] **Bonus recapture**: +50 points pour équilibrer échange

**Logique implémentation**:
```prolog
% Priorité recapture si coup adverse était capture
prioritize_recaptures(LastMove, Moves, PrioritizedMoves) :-
    (was_capture(LastMove) ->
        find_recaptures(LastMove, Moves, Recaptures),
        append(Recaptures, OtherMoves, PrioritizedMoves)
    ; PrioritizedMoves = Moves).
```

#### 🎯 Tâche 3.2: Détection Menaces Basique (30 min)
- [ ] **Pièces en prise**: Bonus si pièce adversaire non défendue  
- [ ] **Pièces attaquées**: Malus si nos pièces attaquées sans défense
- [ ] **Validation tactique**: IA protège cavalier f6 attaqué par e5
- [ ] **Test blunder**: IA évite de donner matériel gratuitement

#### 🔧 Tâche 3.3: Optimisations Performance (15 min)
- [ ] **fast_get_piece/4**: Version IA sans validation redondante
- [ ] **Cache pièces**: Pré-calculer positions pour éviter boucles 8x8
- [ ] **Mesure performance**: Profiling temps de calcul
- [ ] **Target <5s**: Profondeur 2 sous 5 secondes

## 📅 Phase 4: Finalisation Éducative (Session Optionnelle - 60 min)

### 🎯 Objectif: Polish interface, documentation, présentation

#### 📚 Tâche 4.1: Documentation Complète (30 min)
- [ ] **Guide algorithmes**: Explication négamax, alpha-beta, PSQT
- [ ] **Exemples commentés**: Positions typiques avec raisonnement
- [ ] **Instructions utilisation**: Modifier profondeur, évaluation
- [ ] **Résultats tests**: Benchmark performances, taux succès

#### 🎮 Tâche 4.2: Interface Éducative (30 min)  
- [ ] **Affichage évaluation**: Score position visible
- [ ] **Temps réflexion**: Afficher temps calcul IA
- [ ] **Coup choisi**: Justification simple du choix
- [ ] **Mode debug**: Trace minimax optionnelle

## 🏁 Critères de Validation Finaux

### ✅ Objectifs Techniques
- [ ] **Tests 100% PASS**: Tous tests unitaires et intégration
- [ ] **Performance <10s**: Profondeur 2-3 acceptable  
- [ ] **Zéro blunder**: Tests tactiques basiques réussis
- [ ] **Code éducatif**: Comments, architecture claire

### 🎓 Objectifs Académiques  
- [ ] **Négamax implémenté**: Algorithme correct avec élagage
- [ ] **Évaluation complète**: Matériel + position + mobilité
- [ ] **Interface française**: Fonctionnelle et professionnelle  
- [ ] **Documentation**: Explications algorithmes pour évaluation

## 📊 Estimation Temps Total

| Phase | Temps Estimé | Priorité | Résultat Attendu |
|-------|-------------|----------|------------------|
| **Phase 1** | 90 min | 🔥 Critique | Évaluation PSQT fonctionnelle |
| **Phase 2** | 90 min | 🔥 Critique | Tests structurés validés |
| **Phase 3** | 90 min | ⚡ Important | Blunders éliminés |
| **Phase 4** | 60 min | ⭐ Bonus | Polish éducatif |

**Total: 4h30 sur 2-3 sessions de travail**

## 🎯 Prochaine Session Immédiate

### 📅 Session Demain (105 min) - Focus Phase 1 + Suppression Coups Fixes
1. **0-45min**: Implémenter Piece-Square Tables selon ChessProgramming.org
2. **45-90min**: **CONSISTENCY + PURETÉ IA** - Supprimer coups fixes + fixer `run_all_tests` + cases vides
3. **90-105min**: Tests IA authentique + documentation négamax pur

### 🎪 Résultat Attendu Session 1
- **🎯 IA Authentique**: Négamax pur dès coup 1 (finies les béquilles Caro-Kann!)
- IA évalue positions avec PSQT (cavalier centre > bord)
- **Menu Tests fonctionnel** (Option 3 interface.pl → run_all_tests)
- **Cases vides cohérentes** (`' '` standardisé partout)
- Code simplifié, éducatif, commenté
- **Démonstration TP1**: Algorithme robuste sans scripts préprogrammés

---

## 📚 Ressources Références

- **ChessProgramming.org**: Simplified Evaluation Function, Piece-Square Tables
- **Architecture actuelle**: 5 modules Prolog robustes et testés
- **Documentation**: [PRD.md](PRD.md), [TASKS.md](TASKS.md), [CLAUDE.md](../.claude/CLAUDE.md)
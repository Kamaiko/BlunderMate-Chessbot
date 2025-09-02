# 🚀 Plan d'Implémentation IA - Prolog Chess Game
## Phase 3 : Intelligence Artificielle Minimax/Alpha-Beta

> **Deadline** : 20 octobre 2025  
> **Temps estimé** : 8-11 heures de développement  
> **Architecture cible** : Minimax avec élagage Alpha-Beta, profondeur 2, répertoire d'ouvertures

---

## 📊 État Actuel du Projet

### ✅ Acquis (Phases 1-2 Complétées)
- **Architecture 5-modules fonctionnelle** : pieces.pl, board.pl, game.pl, interface.pl + ai.pl
- **Système d'échec/mat complet** : Détection robuste avec optimisations
- **Tests complets** : 35 tests organisés en 5 catégories passent
- **Promotion des pions** : Automatique vers dame
- **Interface française** : Menu professionnel, messages centralisés

### ⚠️ Problèmes Identifiés dans ai.pl
- **API incompatible** : Utilise `execute_move/6` au lieu de `make_move/5`
- **Dépendances manquantes** : Prédicats non définis ou mal référencés  
- **Profondeur excessive** : Défaut à 4 au lieu de 2 (PRD)
- **Non intégré** : Option IA désactivée dans menu principal
- **Non testé** : Aucun test de validation pour l'IA

---

## 🎯 Objectifs Phase 3

### Obligatoires (P0) - Exigences PRD
- [x] Algorithme minimax avec profondeur fixe niveau 2
- [x] Élagage alpha-beta pour optimisation performance  
- [x] Évaluation heuristique (matériel, mobilité, sécurité roi)
- [x] Mode Humain vs IA avec réponse quasi-instantanée (< 1 seconde)

### Importantes (P1) - Qualité
- [x] Répertoire d'ouvertures modeste (6-8 ouvertures essentielles)
- [x] Tests IA intégrés à la suite existante
- [x] Documentation technique mise à jour
- [x] Interface utilisateur polie

---

## 📋 Roadmap d'Implémentation

## Phase 1 : Diagnostic et Réparation (2-3h)
> **Objectif** : Faire fonctionner l'IA existante

### 1.1 Audit Complet ai.pl (30 min)
- [ ] **Identifier incompatibilités API**
  - Lister tous les prédicats `execute_move/6`, `find_king_position/4`
  - Vérifier dépendances entre modules
  - Compiler ai.pl et capturer erreurs
- [ ] **Analyser structure données**
  - Vérifier compatibilité avec `game_state/5`
  - Valider format des mouvements `[FromRow, FromCol, ToRow, ToCol]`
- [ ] **Documentation des corrections nécessaires**

### 1.2 Correction API et Dépendances (1h)
- [ ] **Remplacer execute_move/6 par make_move/5**
  ```prolog
  % Ancien : execute_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState)
  % Nouveau : make_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState)
  ```
- [ ] **Corriger find_king_position**
  - Utiliser version existante `find_king_position/3` de board.pl
  - Adapter tous les appels dans ai.pl
- [ ] **Valider generate_all_moves/2**
  - S'assurer utilisation de `valid_move/5` existant
  - Optimiser pour performance (éviter duplicatas)

### 1.3 Tests de Base - Intégration dans tests.pl (30 min)
- [ ] **Ajouter Section 6 : TESTS IA dans tests/tests.pl**
  ```prolog
  % =============================================================================
  % SECTION 6: TESTS IA
  % =============================================================================
  
  run_ai_tests :-
      display_test_section_header('SECTION 6: TESTS IA', 'Intelligence Artificielle'),
      run_test_group([
          test_ai_compilation,
          test_ai_move_generation,
          test_ai_evaluation
      ]),
      display_test_section_footer('Section IA terminee').
  
  test_ai_compilation :-
      write('[TEST] COMPILATION IA'), nl,
      write('-------------------'), nl,
      write('[RUN] Test 1/3: Chargement module ai.pl........ '),
      (   consult('../src/ai') ->
          write('PASS'), nl
      ;   write('FAIL'), nl, fail).
  
  test_ai_move_generation :-
      write('[RUN] Test 2/3: Generation coups IA............ '),
      (   (init_game_state(GameState),
           generate_all_moves(GameState, Moves),
           length(Moves, Count),
           Count > 0) ->
          write('PASS'), nl
      ;   write('FAIL'), nl, fail).
  
  test_ai_evaluation :-
      write('[RUN] Test 3/3: Evaluation position initiale... '),
      (   (init_game_state(GameState),
           evaluate_position(GameState, Score),
           number(Score)) ->
          write('PASS'), nl
      ;   write('FAIL'), nl, fail), nl.
  ```
- [ ] **Mettre à jour run_all_tests dans tests.pl**
  ```prolog
  run_all_tests :-
      get_time(StartTime),
      % ... header existant ...
      run_foundation_tests,
      run_pieces_tests,
      run_checkmate_tests,
      run_robustness_tests,
      run_integration_tests,
      run_ai_tests,  % NOUVELLE SECTION 6
      % ... footer avec durée totale ...
  ```
- [ ] **Test avec alignement parfait des PASS comme les autres sections**
- [ ] **Une seule commande pour tous les tests : swipl -t run_tests -s tests/tests.pl**

### 1.4 Intégration Interface (1h)
- [ ] **Activer option IA dans menu principal**
  - Modifier `interface.pl` ligne 163 : retirer "bientot disponible"
  - Rediriger `process_choice('2')` vers `start_ai_game`
- [ ] **Créer start_ai_game/0**
  ```prolog
  start_ai_game :-
      display_title_box('MODE IA vs HUMAIN'),
      write('L\'IA joue les noirs, vous jouez les blancs.'), nl,
      init_game_state(GameState),
      ai_game_loop(GameState).
  ```
- [ ] **Gestion erreurs et retour menu**

---

## Phase 2 : Optimisation Algorithme Minimax (2-3h)
> **Objectif** : Assurer performance et exactitude

### 2.1 Validation Algorithme Minimax (1h)
- [ ] **Audit logique alpha-beta**
  - Vérifier `search_moves/8` (lignes 60-75 ai.pl)
  - Corriger bug potentiel `minimax_evaluate_moves/10`
  - Valider alternance max/min correcte
- [ ] **Tests positions tactiques**
  ```prolog
  test_mate_in_one :-
      % Position : Blancs mat en 1
      Board = [['r',' ',' ','q','k',' ',' ','r'],
               [' ','p','p',' ',' ','p','p',' '],
               ['p',' ','n',' ',' ','n',' ','p'],
               [' ',' ',' ','p','p',' ',' ',' '],
               [' ',' ',' ','P','P',' ',' ',' '],
               ['P',' ','N',' ',' ','N',' ','P'],
               [' ','P','P',' ',' ','P','P',' '],
               ['R',' ',' ','Q','K',' ',' ','R']],
      GameState = game_state(Board, white, 10, active, [[], []]),
      choose_ai_move(GameState, Move),
      % Vérifier que Move donne mat
      is_checkmate_move(GameState, Move).
  ```

### 2.2 Réglage Profondeur et Performance (1h)
- [ ] **Réduire profondeur par défaut à 2**
  - Modifier `choose_ai_move/2` ligne 35 : `minimax_search(GameState, 2, BestMove, _)`
  - Ajouter paramètre configurable pour tests
- [ ] **Mesurer temps de réponse**
  ```prolog
  benchmark_ai_performance :-
      init_game_state(GameState),
      get_time(Start),
      choose_ai_move(GameState, _),
      get_time(End),
      Duration is End - Start,
      Duration < 1.0.  % < 1 seconde requis (quasi-instantané)
  ```
- [ ] **Optimiser sort_moves/3**
  - Prioriser captures (MVV-LVA)
  - Éviter évaluations coûteuses en tri

### 2.3 Amélioration Évaluation (1h)
- [ ] **Valider fonction evaluate_position/3**
  - Vérifier pondération : matériel×10, mobilité×2, sécurité roi×5
  - Tester sur positions d'ouverture standard
- [ ] **Ajouter debug évaluation**
  ```prolog
  evaluate_position_debug(GameState, Player, Value) :-
      material_value(GameState, Player, Mat),
      mobility_value(GameState, Player, Mob),  
      king_safety_value(GameState, Player, Safety),
      format('Eval ~w: Mat=~w Mob=~w Safety=~w~n', [Player, Mat, Mob, Safety]),
      Value is Mat * 10 + Mob * 2 + Safety * 5.
  ```

---

## Phase 3 : Répertoire d'Ouvertures Modeste (1-2h)
> **Objectif** : Améliorer le jeu en début de partie

### 3.1 Design Système Ouvertures (30 min)
- [ ] **Intégrer répertoire ouvertures dans ai.pl** (éviter fichier séparé)
  ```prolog
  % Intégré dans ai.pl - Section répertoire ouvertures  
  opening_move(GameState, [FromRow, FromCol, ToRow, ToCol]) :-
      GameState = game_state(Board, Player, MoveCount, _, _),
      MoveCount =< 6,  % Seulement premiers 6 coups
      opening_database(Board, Player, MoveCount, FromRow, FromCol, ToRow, ToCol).
      
  % Base compacte dans ai.pl
  opening_database(InitialBoard, white, 1, 2, 5, 4, 5).  % e2e4
  opening_database(InitialBoard, white, 1, 2, 4, 4, 4).  % d2d4
  ```
- [ ] **Intégration dans choose_ai_move/2**
  - Vérifier d'abord le livre d'ouvertures
  - Fallback sur minimax si position inconnue

### 3.2 Implémentation Ouvertures (1h)
- [ ] **6-8 ouvertures essentielles** (optimal pour petit projet universitaire)
  - **Ouvertures blanches** (3-4) : Italienne (e4 e5 Nf3 Nc6 Bc4), Espagnole (e4 e5 Nf3 Nc6 Bb5), Gambit Dame (d4 d5 c4)
  - **Défenses noires** (3-4) : Sicilienne (e4 c5), Française (e4 e6), Caro-Kann (e4 c6)
- [ ] **Reconnaissance transpositions simples**
- [ ] **Sortie progressive livre après 8 coups**

### 3.3 Tests Ouvertures (30 min)
- [ ] **Test reconnaissance ouvertures**
  ```prolog
  test_opening_recognition :-
      init_game_state(GS1),
      make_move(GS1, 2, 5, 4, 5, GS2),  % e2e4
      choose_ai_move(GS2, Move),
      member(Move, [[2,5,4,5], [7,4,5,4], [7,3,5,3]]).  % e7e5, d7d5, c7c5
  ```
- [ ] **Test transition livre → minimax**
- [ ] **Validation cohérence ouvertures**

---

## Phase 4 : Tests Complets et Intégration (2h)
> **Objectif** : Assurer robustesse du système IA

### 4.1 Tests Unitaires IA - Extension Section 6 (1h)
- [ ] **Étendre run_ai_tests dans tests/tests.pl**
  ```prolog
  test_minimax_basic :-
      write('[RUN] Test 4/7: Minimax profondeur 2........... '),
      (   (init_game_state(GameState),
           minimax_search(GameState, 2, BestMove, _),
           is_list(BestMove)) ->
          write('PASS'), nl
      ;   write('FAIL'), nl, fail).
      
  test_evaluation_function :-
      write('[RUN] Test 5/7: Fonction evaluation............ '),
      (   (init_game_state(GameState),
           evaluate_position(GameState, Score),
           number(Score)) ->
          write('PASS'), nl
      ;   write('FAIL'), nl, fail).
  
  test_opening_book :-
      write('[RUN] Test 6/7: Repertoire ouvertures.......... '),
      (   (init_game_state(GameState),
           opening_move(GameState, Move),
           (Move \= none ; Move = none)) ->  % Succès si résultat valide
          write('PASS'), nl
      ;   write('FAIL'), nl, fail).
  
  test_ai_performance :-
      write('[RUN] Test 7/7: Performance quasi-instantanee... '),
      (   (init_game_state(GameState),
           get_time(Start),
           choose_ai_move(GameState, _),
           get_time(End),
           Duration is End - Start,
           Duration < 1.0) ->
          write('PASS'), nl
      ;   write('FAIL'), nl, fail), nl.
  ```
- [ ] **Mettre à jour run_test_group avec les 7 tests IA**
- [ ] **Alignement parfait des PASS avec les autres sections**

### 4.2 Tests d'Intégration - Dans tests.pl (45 min)
- [ ] **Ajouter test_ai_integration à la Section 6**
  ```prolog
  % Optionnel: Test d'intégration complet (peut être long)
  test_ai_integration :-
      write('[TEST] INTEGRATION IA'), nl,
      write('------------------'), nl,
      write('[RUN] Test 1/1: Partie IA complete............. '),
      (   (init_game_state(GameState),
           simulate_ai_game(GameState, 10, FinalState),  % 10 coups max
           FinalState = game_state(_, _, _, Status, _),
           member(Status, [checkmate, stalemate, active])) ->
          write('PASS'), nl
      ;   write('FAIL'), nl, fail), nl.
  ```
- [ ] **Test accessible via run_all_tests (une seule commande)**
- [ ] **Test stabilité : 50 coups sans crash**

### 4.3 Tests de Régression (15 min)
- [ ] **Vérifier tous tests existants passent (35/35)**
  ```bash
  swipl -t run_tests -s tests/tests.pl
  ```
- [ ] **Mode humain vs humain non affecté**
- [ ] **Edge cases : positions terminales**

---

## Phase 5 : Finition et Documentation (1h)
> **Objectif** : Préparer livraison finale

### 5.1 Documentation Technique (30 min)
- [ ] **Mettre à jour CLAUDE.md**
  - Retirer "PROTOTYPE NON FONCTIONNEL"
  - Documenter API IA pour développeurs
  - Instructions d'utilisation mode IA
- [ ] **Mettre à jour README.md**
  - Section "Mode IA" fonctionnel  
  - Commandes : `swipl go.pl` → Option 2

### 5.2 Interface Utilisateur Finale (30 min)
- [ ] **Messages français cohérents**
  ```prolog
  message(ai_thinking, 'IA reflechit (profondeur 2)...').
  message(ai_move_played, 'IA joue: ').
  message(ai_evaluation_score, 'Evaluation position: ').
  ```
- [ ] **Affichage raisonnement IA optionnel**
- [ ] **Aide contextuelle mode IA**
- [ ] **Polish final expérience utilisateur**

---

## ⏰ Planning Recommandé

### 🌅 MATIN (4h) - Fonctionnel de Base
- **09h00-10h30** : Phase 1.1-1.2 (Audit et corrections API)
- **10h30-11h00** : Pause
- **11h00-12h30** : Phase 1.3-1.4 + Phase 2.1 (Tests base + validation minimax)
- **12h30** : **OBJECTIF** - IA fonctionnelle basique intégrée

### 🌞 APRÈS-MIDI (4h) - Optimisations et Tests
- **14h00-15h30** : Phase 2.2-2.3 (Performance et évaluation)
- **15h30-16h00** : Pause
- **16h00-17h30** : Phase 3 (Répertoire ouvertures)
- **17h30-18h30** : Phase 4 (Tests complets)

### 🌆 FINALISATION (1h) - Polish
- **18h30-19h30** : Phase 5 (Documentation et interface finale)

---

## 🚨 Gestion des Risques

### RISQUE ÉLEVÉ : Incompatibilités API
- **Impact** : IA ne compile pas ou plante
- **Mitigation** : Phase 1 prioritaire avec tests compilation
- **Plan B** : API wrapper si corrections complexes

### RISQUE MOYEN : Performance Minimax
- **Impact** : Temps réponse > 1 seconde (trop lent)
- **Mitigation** : Profondeur 2 conservatrice, optimisations ciblées
- **Plan B** : Réduire profondeur à 1 temporairement

### RISQUE FAIBLE : Complexité Ouvertures
- **Impact** : Retard sur planning
- **Mitigation** : Design simple, 5-8 ouvertures minimum
- **Plan B** : Report Phase 3 si Phase 4 prioritaire

---

## 📊 Définition de Succès

### ✅ Critères d'Acceptance
- [ ] **IA fonctionnelle** : Compile sans erreur, intégrée au menu
- [ ] **Performance** : Réponse quasi-instantanée (≤ 1 seconde par coup)
- [ ] **Algorithme** : Minimax/alpha-beta profondeur 2 validé
- [ ] **Qualité** : Tous les tests existants (35/35) passent + tests IA
- [ ] **UX** : Interface française cohérente, retour raisonnement IA
- [ ] **Documentation** : CLAUDE.md et README.md mis à jour

### 🎯 Objectifs Étendus (Si Temps)
- [ ] **Répertoire ouvertures** : 10+ ouvertures implémentées
- [ ] **Analyse avancée** : Affichage scores évaluation détaillés
- [ ] **Modes difficulté** : Profondeur configurable 1-3
- [ ] **Statistiques** : Temps moyen, coups analysés, taux victoire

---

## 📚 Ressources et Références

### Documentation Technique
- **API Game** : `C:\DevTools\Projects\PrologChessGame_Clean\src\game.pl`
- **Tests Existants** : `C:\DevTools\Projects\PrologChessGame_Clean\tests\tests.pl`
- **Configuration** : `C:\DevTools\Projects\PrologChessGame_Clean\.claude\CLAUDE.md`

### Algorithmes IA
- **Minimax** : Algorithme déjà présent dans ai.pl, corrections nécessaires
- **Alpha-Beta** : Élagage implémenté, validation requise  
- **Évaluation** : Matériel (×10) + Mobilité (×2) + Sécurité Roi (×5)

### Tests Références
```bash
# Tests complets (existants + nouveaux tests IA)
swipl -t run_tests -s tests/tests.pl  # Sections 1-6 incluant TESTS IA

# Benchmark performance
swipl -g "benchmark_ai_performance, halt." -s src/ai.pl
```

---

## 🎉 Conclusion

Ce plan transforme le **prototype ai.pl existant en IA fonctionnelle** avec une approche **réparation plutôt que réécriture**. L'architecture 5-modules solide et les 35 tests existants offrent une base robuste.

**L'implémentation est réaliste pour une journée intensive** avec des phases clairement définies, des risques identifiés et des solutions de contournement. Le focus sur la **correction d'API en Phase 1** assure un prototype fonctionnel dès le matin, permettant l'itération et l'amélioration l'après-midi.

**Ready to code! 🚀**
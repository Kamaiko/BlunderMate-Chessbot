# Plan de Développement - Implémentation Promotion des Pions

## Aperçu du Projet
**Projet** : Jeu d'échecs en Prolog avec architecture modulaire 5 modules (pieces.pl, board.pl, game.pl, interface.pl, ai.pl)  
**Fonctionnalité cible** : Implémentation de la promotion automatique des pions en dame  
**Architecture actuelle** : Système complet avec validation de mouvements, détection échec/mat, interface française, tests unitaires

## 1. Analyse Architecture Existante ✅

### Points d'Intégration Identifiés
- **pieces.pl** : Règles de mouvement des pions (`can_white_pawn_move/5`, `can_black_pawn_move/5`)
- **game.pl** : Exécution des mouvements (`execute_move/6`, `perform_move/5`)
- **game.pl** : Validation des mouvements (`valid_move/6`)
- **tests.pl** : Structure modulaire pour ajouter tests de promotion

### Structure de Données Actuelle
```prolog
game_state(Board, CurrentPlayer, MoveCount, GameStatus, CapturedPieces)
% Board: Liste 8x8 avec pièces 'P'/'p' (pions), 'Q'/'q' (dames)
% Coordonnées: 1-8 (rangée 8 = promotion blancs, rangée 1 = promotion noirs)
```

## 2. Modification Validation Mouvements Pions

### 2.1 Amélioration Détection Conditions Promotion
- **Fichier**: `C:\DevTools\Projects\PrologChessGame_Clean\src\pieces.pl`
- **Predicat cible**: `can_white_pawn_move/5`, `can_black_pawn_move/5`

#### Tâches Spécifiques:
- [ ] Ajouter prédicat `is_promotion_move/5`
  - Vérifier si pion blanc atteint rangée 8 (ToRow =:= 8)
  - Vérifier si pion noir atteint rangée 1 (ToRow =:= 1)
- [ ] Marquer les mouvements de promotion pour traitement spécial
- [ ] Conserver compatibilité avec mouvements normaux de pions

#### Code à Ajouter:
```prolog
% is_promotion_move(+Piece, +FromRow, +FromCol, +ToRow, +ToCol)
% Détecte si un mouvement de pion nécessite une promotion
is_promotion_move('P', _, _, 8, _) :- !.  % Pion blanc vers rangée 8
is_promotion_move('p', _, _, 1, _) :- !.  % Pion noir vers rangée 1
is_promotion_move(_, _, _, _, _) :- fail.
```

### 2.2 Intégration dans Validation Existante
- [ ] Modifier `can_white_pawn_move/5` pour accepter mouvements vers rangée 8
- [ ] Modifier `can_black_pawn_move/5` pour accepter mouvements vers rangée 1
- [ ] Conserver logique existante (simple, double, capture)

## 3. Implémentation Logique Promotion

### 3.1 Modification execute_move
- **Fichier**: `C:\DevTools\Projects\PrologChessGame_Clean\src\game.pl`
- **Predicat cible**: `execute_move/6`

#### Tâches Spécifiques:
- [ ] Détecter mouvements de promotion avant exécution
- [ ] Remplacer automatiquement pion par dame lors de l'exécution
- [ ] Conserver logique capture existante
- [ ] Maintenir compatibilité avec system d'état de jeu

#### Logique d'Implémentation:
```prolog
% perform_move_with_promotion(+Board, +Piece, +FromPos, +ToPos, -NewBoard)
% Exécute mouvement avec promotion automatique si nécessaire
perform_move_with_promotion(Board, Piece, from(FromRow, FromCol), to(ToRow, ToCol), NewBoard) :-
    (   is_promotion_move(Piece, FromRow, FromCol, ToRow, ToCol) ->
        get_promoted_piece(Piece, PromotedPiece),
        place_single_piece(Board, FromRow, FromCol, ' ', TempBoard),
        place_single_piece(TempBoard, ToRow, ToCol, PromotedPiece, NewBoard)
    ;   perform_move(Board, Piece, from(FromRow, FromCol), to(ToRow, ToCol), NewBoard)
    ).

% get_promoted_piece(+Pawn, -Queen)
% Convertit pion en dame pour promotion
get_promoted_piece('P', 'Q').  % Pion blanc -> Dame blanche
get_promoted_piece('p', 'q').  % Pion noir -> Dame noire
```

### 3.2 Modification Perform_move
- [ ] Refactoriser `perform_move/5` pour déléguer à `perform_move_with_promotion/5`
- [ ] Conserver signature existante pour compatibilité
- [ ] Ajouter gestion d'erreurs pour cas edge

## 4. Tests Unitaires Complets

### 4.1 Structure Tests Promotion
- **Fichier**: `C:\DevTools\Projects\PrologChessGame_Clean\tests\tests.pl`
- **Section**: Nouvelle section "Tests Promotion"

#### Tests Fondamentaux:
- [ ] **Test détection promotion pion blanc**
  ```prolog
  test_white_pawn_promotion :-
      % Pion blanc en 7ème rangée -> 8ème rangée = promotion
      % Vérifier conversion 'P' -> 'Q'
  ```

- [ ] **Test détection promotion pion noir**  
  ```prolog
  test_black_pawn_promotion :-
      % Pion noir en 2ème rangée -> 1ère rangée = promotion
      % Vérifier conversion 'p' -> 'q'
  ```

#### Tests Scénarios:
- [ ] **Promotion par mouvement simple**
- [ ] **Promotion par capture diagonale**
- [ ] **Promotion avec détection échec/mat résultant**
- [ ] **Intégration game_state après promotion**

#### Tests Robustesse:
- [ ] **Validation coordonnées promotion**
- [ ] **Pas de promotion si pion n'atteint pas dernière rangée**
- [ ] **Conservation pieces capturées lors promotion**

### 4.2 Intégration Suite Tests Existante
- [ ] Ajouter `run_promotion_tests/0` dans structure modulaire
- [ ] Intégrer appel dans `run_tests/0` principal
- [ ] Utiliser utilities affichage existants
- [ ] Respecter format de sortie cohérent

## 5. Validation Intégration Système

### 5.1 Tests d'Intégration End-to-End
- [ ] **Partie complète avec promotions**
  - Initialiser jeu standard
  - Jouer séquence menant à promotion
  - Vérifier état final correct

- [ ] **Interface utilisateur**  
  - Tester affichage dames promues
  - Vérifier messages utilisateur appropriés
  - Conserver expérience utilisateur fluide

### 5.2 Tests Performance et Stabilité
- [ ] **Benchmark mouvements avec promotion**
- [ ] **Test memory leaks potentiels**
- [ ] **Validation avec suite tests complète existante**

## 6. Documentation et Maintenance

### 6.1 Documentation Code
- [ ] Commenter nouveaux prédicats (français sans accents)
- [ ] Mettre à jour headers de modules modifiés
- [ ] Documenter exemples utilisation promotion

### 6.2 Tests Régression
- [ ] Exécuter suite tests complète après modifications
- [ ] Vérifier aucune régression sur fonctionnalités existantes
- [ ] Valider détection échec/mat avec dames promues

## 7. Contraintes Techniques Respectées

### Architecture Modulaire ✅
- Conservation structure 5 modules existante
- Modifications minimales, extensions propres
- Compatibilité ascendante préservée

### Conventions Code ✅  
- Prédicats snake_case (`is_promotion_move/5`)
- Variables PascalCase (`PromotedPiece`)
- Validation `ground/1` systématique
- Commentaires français sans accents

### Performance ✅
- Utilisation `place_single_piece/5` optimisé
- Pas de `findall/3` dans boucles critiques
- Détection promotion en O(1)

## 8. Points d'Attention Critique

### Sécurité
- **Validation coordonnées**: Tous nouveaux prédicats doivent valider `ground/1` et bornes 1-8
- **Protection récursion**: Aucun risque de boucle infinie dans détection promotion
- **Type safety**: Vérification types pieces avant promotion

### Compatibilité
- **Tests existants**: Aucun test existant ne doit être cassé
- **Interface**: Messages utilisateur cohérents avec français existant
- **AI module**: Préparation pour future intégration AI avec promotions

### Edge Cases
- **Promotion avec échec**: Dame promue peut donner échec/mat immédiatement
- **Multiple promotions**: Plusieurs pions peuvent être promus dans même partie
- **Capture + promotion**: Combinaison des deux mécaniques

---

**Estimation temps**: 4-6 heures développement + tests
**Priorité**: Haute (complète fonctionnalités essentielles échecs)
**Risque**: Faible (modifications localisées, architecture stable)
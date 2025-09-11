# 🔧 PLAN DE REFACTORISATION COMPLET - Chess AI Prolog

**Date**: 11 septembre 2025  
**Version**: 1.0  
**Auteur**: Claude Code (Assistant IA)  
**Approche**: Hybride (Option A) - Utils.pl + Constantes locales

---

## 📋 RÉSUMÉ EXÉCUTIF

### Approche Choisie: **HYBRIDE (Option A)**
- **utils.pl** : Helpers partagés + constantes globales uniquement
- **Modules existants** : Gardent leurs constantes métier spécifiques
- **Objectif** : Code propre, modulaire et maintenable sans sur-ingénierie

### Problème Principal
Le codebase contient des fonctions trop longues (>20 lignes), de la duplication de code, et des valeurs magiques dispersées. La fonction `generate_structured_moves/3` de 123 lignes est particulièrement problématique.

---

## 📊 ANALYSE DE L'ÉTAT ACTUEL

### Métriques Problématiques
| Module | Fonction | Lignes | Problème Principal |
|--------|----------|--------|-------------------|
| ai.pl | `generate_structured_moves/3` | **123** | Monolithique, fait tout |
| ai.pl | `display_position_evaluation/2` | **38** | Mélange calcul/affichage |
| interface.pl | `unified_game_loop/1` | **41** | Logique de contrôle complexe |
| interface.pl | `handle_player_turn/4` | **26** | Légèrement trop long |
| game.pl | Plusieurs fonctions | ~20 | À la limite acceptable |

### Patterns de Duplication Identifiés
- **Validation coordonnées** : 15+ occurrences à travers les modules
- **Get piece avec validation** : 8+ occurrences similaires
- **Magic numbers** (8, 1000, etc.) : 30+ occurrences dispersées
- **Patterns d'erreur** : Gestion répétée similaire

### Impact sur Maintenabilité
- **Modification difficile** : Une change nécessite plusieurs endroits
- **Tests complexes** : Difficulté à tester isolément
- **Compréhension** : Fonctions trop longues difficiles à analyser
- **Extensions** : Ajout de fonctionnalités risqué

---

## 🏗️ ARCHITECTURE CIBLE

### 1. **Nouveau Module: utils.pl**
```prolog
% Structure du nouveau module utils.pl
:- module(utils, [
    % Constantes globales
    chess_constant/2,      % chess_constant(board_size, 8)
    
    % Validation helpers
    valid_position_safe/2,
    validate_coordinates/4,
    validate_piece_at/4,
    
    % Board manipulation
    get_piece_safe/4,
    place_piece_safe/5,
    find_all_pieces/3,
    
    % List utilities
    take_first_n/3,
    remove_duplicates/2,
    split_list_at/4,
    
    % Math utilities
    clamp_value/4,
    manhattan_distance/5
]).
```

### 2. **Distribution des Constantes**
#### Constantes Globales (utils.pl)
```prolog
% Utilisées dans 3+ modules
chess_constant(board_size, 8).
chess_constant(min_position, 1).
chess_constant(max_position, 8).
chess_constant(max_recursion, 8).
```

#### Constantes Locales par Module
```prolog
% ai.pl - Constantes IA spécifiques
ai_config(capture_base, 1000).
ai_config(promotion_bonus, 90).
ai_config(check_bonus, 50).
ai_config(move_limit, 25).

% evaluation.pl - Garde ses tables
pawn_psqt([...]).
piece_value(pawn, 100).

% interface.pl - Messages restent locaux
message(invalid_move, 'Mouvement invalide').
```

### 3. **Dépendances Modules**
```
utils.pl (nouveau)
    ↑
    ├── ai.pl (utilise helpers + constantes globales)
    ├── game.pl (utilise helpers validation)
    ├── board.pl (utilise helpers board)
    ├── interface.pl (utilise helpers format)
    ├── evaluation.pl (utilise constantes globales)
    └── pieces.pl (utilise helpers validation)
```

---

## 📝 PLAN D'IMPLÉMENTATION DÉTAILLÉ

### **PHASE 1: Création du Module utils.pl** [Durée: 2h, Risque: FAIBLE]

#### Étape 1.1: Structure de base (30 min)
```prolog
% Nouveau fichier: src/utils.pl
% =============================================================================
% UTILS - Module utilitaire pour Chess AI Prolog
% =============================================================================
%
% Ce module centralise :
% - Constantes globales (utilisées dans 3+ modules)
% - Helpers de validation réutilisables
% - Utilitaires de manipulation board/listes
% - Fonctions mathématiques communes
%
% =============================================================================

:- module(utils, [
    % Constantes globales
    chess_constant/2,
    
    % Validation helpers
    valid_position_safe/2,
    validate_coordinates/4,
    validate_piece_at/4,
    
    % Board manipulation
    get_piece_safe/4,
    place_piece_safe/5,
    find_piece_positions/4,
    
    % List utilities
    take_first_n/3,
    remove_duplicates/2,
    combine_lists/2,
    
    % Math utilities
    clamp_value/4,
    manhattan_distance/5
]).

% =============================================================================
% CONSTANTES GLOBALES
% =============================================================================
% Dimensions échiquier (utilisé dans TOUS les modules)
chess_constant(board_size, 8).
chess_constant(min_position, 1).
chess_constant(max_position, 8).
chess_constant(max_recursion, 8).
chess_constant(default_depth, 2).
```

#### Étape 1.2: Helpers de validation (45 min)
```prolog
% =============================================================================
% VALIDATION HELPERS
% =============================================================================

%! valid_position_safe(+Row, +Col) is semidet
% Valide une position avec toutes les vérifications de sécurité
% Remplace les 15+ validations dispersées dans le code
valid_position_safe(Row, Col) :-
    ground(Row), ground(Col),
    integer(Row), integer(Col),
    chess_constant(min_position, Min),
    chess_constant(max_position, Max),
    Row >= Min, Row =< Max,
    Col >= Min, Col =< Max.

%! validate_coordinates(+FromRow, +FromCol, +ToRow, +ToCol) is semidet
% Valide un mouvement complet avec positions source et destination
validate_coordinates(FromRow, FromCol, ToRow, ToCol) :-
    valid_position_safe(FromRow, FromCol),
    valid_position_safe(ToRow, ToCol),
    % Position différente
    (FromRow =\= ToRow ; FromCol =\= ToCol).

%! validate_piece_at(+Board, +Row, +Col, +ExpectedPlayer) is semidet
% Valide qu'il y a une pièce du joueur attendu à la position
validate_piece_at(Board, Row, Col, ExpectedPlayer) :-
    valid_position_safe(Row, Col),
    get_piece_safe(Board, Row, Col, Piece),
    \+ is_empty_square(Piece),
    piece_belongs_to_player(Piece, ExpectedPlayer).
```

#### Étape 1.3: Board manipulation helpers (45 min)
```prolog
% =============================================================================
% BOARD MANIPULATION HELPERS
% =============================================================================

%! get_piece_safe(+Board, +Row, +Col, -Piece) is semidet
% Version sécurisée de get_piece avec validation complète
% Remplace les 8+ patterns get_piece dispersés
get_piece_safe(Board, Row, Col, Piece) :-
    valid_position_safe(Row, Col),
    is_list(Board),
    chess_constant(board_size, Size),
    length(Board, Size),
    nth1(Row, Board, RowList),
    is_list(RowList),
    length(RowList, Size),
    nth1(Col, RowList, Piece).

%! place_piece_safe(+Board, +Row, +Col, +Piece, -NewBoard) is det
% Version sécurisée de place_piece avec validation
place_piece_safe(Board, Row, Col, Piece, NewBoard) :-
    valid_position_safe(Row, Col),
    place_single_piece(Board, Row, Col, Piece, NewBoard).

%! find_piece_positions(+Board, +Player, +PieceType, -Positions) is det
% Trouve toutes les positions d'un type de pièce pour un joueur
find_piece_positions(Board, Player, PieceType, Positions) :-
    findall([Row, Col], (
        chess_constant(min_position, Min),
        chess_constant(max_position, Max),
        between(Min, Max, Row),
        between(Min, Max, Col),
        get_piece_safe(Board, Row, Col, Piece),
        piece_belongs_to_player(Piece, Player),
        get_piece_type(Piece, PieceType)
    ), Positions).
```

#### Étape 1.4: List et math utilities (20 min)
```prolog
% =============================================================================
% LIST UTILITIES
% =============================================================================

%! take_first_n(+List, +N, -FirstN) is det
% Prend les N premiers éléments d'une liste
% Consolide la logique dispersée dans ai.pl
take_first_n(List, N, FirstN) :-
    length(List, Len),
    (   Len =< N -> 
        FirstN = List
    ;   length(FirstN, N), 
        append(FirstN, _, List)
    ).

%! remove_duplicates(+List, -Unique) is det
% Supprime les doublons d'une liste
remove_duplicates([], []).
remove_duplicates([H|T], [H|UniqueT]) :-
    \+ member(H, T),
    remove_duplicates(T, UniqueT).
remove_duplicates([H|T], UniqueT) :-
    member(H, T),
    remove_duplicates(T, UniqueT).

%! combine_lists(+Lists, -Combined) is det
% Combine plusieurs listes en une seule
combine_lists([], []).
combine_lists([H|T], Combined) :-
    combine_lists(T, RestCombined),
    append(H, RestCombined, Combined).

% =============================================================================
% MATH UTILITIES
% =============================================================================

%! clamp_value(+Value, +Min, +Max, -Clamped) is det
% Limite une valeur entre min et max
clamp_value(Value, Min, Max, Clamped) :-
    (   Value < Min -> Clamped = Min
    ;   Value > Max -> Clamped = Max
    ;   Clamped = Value
    ).

%! manhattan_distance(+Row1, +Col1, +Row2, +Col2, -Distance) is det
% Calcule la distance de Manhattan entre deux positions
manhattan_distance(Row1, Col1, Row2, Col2, Distance) :-
    RowDiff is abs(Row1 - Row2),
    ColDiff is abs(Col1 - Col2),
    Distance is RowDiff + ColDiff.
```

---

### **PHASE 2: Refactoring generate_structured_moves** [Durée: 3h, Risque: MOYEN]

#### Problème Actuel
- **123 lignes monolithiques** dans une seule fonction
- **Logique entremêlée** : génération, tri, limitation
- **Difficile à tester** isolément
- **Impossible à étendre** facilement

#### Solution: Décomposition Modulaire

##### Étape 2.1: Nouvelle structure principale (30 min)
```prolog
% =============================================================================
% GÉNÉRATION DE COUPS REFACTORISÉE
% =============================================================================

% Fonction principale simplifiée (15 lignes)
generate_structured_moves(GameState, Player, Moves) :-
    GameState = game_state(Board, _, MoveCount, _, _),
    generate_all_move_types(Board, Player, MoveCount, AllMoves),
    order_moves_by_priority(GameState, Player, AllMoves, OrderedMoves),
    apply_adaptive_limit(MoveCount, OrderedMoves, Moves).

% Validation: garder l'ancienne fonction comme wrapper temporaire
generate_structured_moves_legacy(GameState, Player, Moves) :-
    % Ancienne implémentation de 123 lignes (à supprimer après tests)
    % ... code existant ...
```

##### Étape 2.2: Génération par type (45 min)
```prolog
%! generate_all_move_types(+Board, +Player, +MoveCount, -AllMoves) is det
% Génère tous les types de coups en ordre logique (20 lignes)
generate_all_move_types(Board, Player, MoveCount, AllMoves) :-
    generate_captures(Board, Player, Captures),
    generate_developments(Board, Player, MoveCount, Developments),
    generate_pawn_advances(Board, Player, PawnMoves),
    generate_piece_moves(Board, Player, MoveCount, OtherMoves),
    utils:combine_lists([Captures, Developments, PawnMoves, OtherMoves], AllMoves).

%! generate_captures(+Board, +Player, -Captures) is det
% Génère tous les coups de capture (15 lignes)
generate_captures(Board, Player, Captures) :-
    findall([FromRow, FromCol, ToRow, ToCol], (
        utils:find_piece_positions(Board, Player, _, PlayerPositions),
        member([FromRow, FromCol], PlayerPositions),
        find_capture_target(Board, FromRow, FromCol, ToRow, ToCol),
        valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol)
    ), Captures).

%! generate_developments(+Board, +Player, +MoveCount, -Developments) is det
% Génère les coups de développement en ouverture (18 lignes)
generate_developments(Board, Player, MoveCount, Developments) :-
    (   MoveCount =< 15 ->  % Phase d'ouverture seulement
        findall([FromRow, FromCol, ToRow, ToCol], (
            find_undeveloped_piece(Board, Player, FromRow, FromCol),
            development_target(FromRow, FromCol, ToRow, ToCol),
            valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol),
            is_good_development(FromRow, FromCol, ToRow, ToCol)
        ), Developments)
    ;   Developments = []  % Pas de développement après ouverture
    ).
```

##### Étape 2.3: Helpers spécialisés (60 min)
```prolog
%! find_capture_target(+Board, +FromRow, +FromCol, -ToRow, -ToCol) is nondet
% Trouve les cibles de capture pour une pièce
find_capture_target(Board, FromRow, FromCol, ToRow, ToCol) :-
    utils:chess_constant(min_position, Min),
    utils:chess_constant(max_position, Max),
    between(Min, Max, ToRow),
    between(Min, Max, ToCol),
    (FromRow =\= ToRow ; FromCol =\= ToCol),
    utils:get_piece_safe(Board, ToRow, ToCol, TargetPiece),
    \+ is_empty_square(TargetPiece),
    utils:get_piece_safe(Board, FromRow, FromCol, AttackingPiece),
    \+ piece_belongs_to_player(TargetPiece, AttackingPlayer),
    piece_belongs_to_player(AttackingPiece, AttackingPlayer).

%! find_undeveloped_piece(+Board, +Player, -Row, -Col) is nondet
% Trouve les pièces non développées en ouverture
find_undeveloped_piece(Board, Player, Row, Col) :-
    utils:find_piece_positions(Board, Player, cavalier, KnightPos),
    member([Row, Col], KnightPos),
    is_on_starting_rank(Player, Row).
find_undeveloped_piece(Board, Player, Row, Col) :-
    utils:find_piece_positions(Board, Player, fou, BishopPos),
    member([Row, Col], BishopPos),
    is_on_starting_rank(Player, Row).

%! development_target(+FromRow, +FromCol, -ToRow, -ToCol) is nondet
% Trouve les bonnes cases de développement
development_target(FromRow, FromCol, ToRow, ToCol) :-
    % Cases centrales privilégiées
    member(ToRow, [3, 4, 5, 6]),
    member(ToCol, [3, 4, 5, 6]),
    (FromRow =\= ToRow ; FromCol =\= ToCol).

%! is_good_development(+FromRow, +FromCol, +ToRow, +ToCol) is semidet
% Vérifie si le développement est tactiquement correct
is_good_development(_, _, ToRow, ToCol) :-
    % Éviter les bords en ouverture
    ToRow > 2, ToRow < 7,
    ToCol > 2, ToCol < 7.
```

##### Étape 2.4: Tri et limitation (45 min)
```prolog
%! order_moves_by_priority(+GameState, +Player, +Moves, -OrderedMoves) is det
% Applique le tri MVV-LVA et autres priorités (12 lignes)
order_moves_by_priority(GameState, Player, Moves, OrderedMoves) :-
    GameState = game_state(Board, _, _, _, _),
    map_moves_to_scores(Board, Player, Moves, ScoredMoves),
    keysort(ScoredMoves, SortedPairs),
    reverse(SortedPairs, DescSorted),
    extract_moves_from_pairs(DescSorted, OrderedMoves).

%! apply_adaptive_limit(+MoveCount, +Moves, -LimitedMoves) is det
% Applique les limites adaptatives selon la phase de jeu (8 lignes)
apply_adaptive_limit(MoveCount, Moves, LimitedMoves) :-
    (   MoveCount =< 10 -> Limit = 25
    ;   MoveCount =< 20 -> Limit = 20
    ;   Limit = 15
    ),
    utils:take_first_n(Moves, Limit, LimitedMoves).
```

##### Étape 2.5: Tests et validation (30 min)
```bash
# Test de non-régression
swipl -s tests/tests.pl -g "run_alpha_beta_tests, halt."

# Test performance spécifique
swipl -g "
    consult('src/ai'),
    init_game_state(GS),
    time(generate_structured_moves(GS, white, Moves)),
    length(Moves, Count),
    format('Generated ~w moves~n', [Count]),
    halt.
"
```

---

### **PHASE 3: Refactoring display_position_evaluation** [Durée: 1h, Risque: FAIBLE]

#### Problème Actuel
- **38 lignes** mélangeant calculs et affichage
- **Logique entremêlée** difficile à tester
- **Responsabilités multiples** dans une fonction

#### Solution: Séparation Calcul/Affichage

##### Étape 3.1: Structure de données pour l'évaluation (15 min)
```prolog
% Structure d'évaluation pour séparer calcul et affichage
% eval_components(MaterialScore, PSQTScore, SafetyScore, TotalScore)
```

##### Étape 3.2: Calcul pur (20 min)
```prolog
%! calculate_evaluation_components(+GameState, -Components) is det
% Calcul pur des composantes d'évaluation (12 lignes)
calculate_evaluation_components(GameState, Components) :-
    count_material_pure_ref(GameState, white, WhiteMat),
    count_material_pure_ref(GameState, black, BlackMat),
    evaluate_psqt_total(GameState, white, WhitePSQT),
    evaluate_psqt_total(GameState, black, BlackPSQT),
    evaluate_piece_safety(GameState, white, WhiteSafety),
    evaluate_piece_safety(GameState, black, BlackSafety),
    MaterialDiff is WhiteMat - BlackMat,
    PSQTDiff is WhitePSQT - BlackPSQT,
    SafetyDiff is WhiteSafety - BlackSafety,
    TotalDiff is MaterialDiff + PSQTDiff + SafetyDiff,
    Components = eval_components(MaterialDiff, PSQTDiff, SafetyDiff, TotalDiff).
```

##### Étape 3.3: Affichage séparé (25 min)
```prolog
%! display_position_evaluation(+GameState, +Player) is det
% Fonction principale refactorisée (8 lignes)
display_position_evaluation(GameState, Player) :-
    calculate_evaluation_components(GameState, Components),
    format_evaluation_display(Components, Player).

%! format_evaluation_display(+Components, +Player) is det
% Affichage formaté des composantes (15 lignes)
format_evaluation_display(eval_components(Mat, PSQT, Safety, Total), Player) :-
    write('=== ÉVALUATION POSITION ==='), nl,
    format('Matériel    : ~w~n', [Mat]),
    format('PSQT        : ~w~n', [PSQT]),  
    format('Sécurité    : ~w~n', [Safety]),
    write('------------------------'), nl,
    (   Player = white -> FinalScore = Total
    ;   FinalScore is -Total
    ),
    format('SCORE TOTAL (~w): ~w~n', [Player, FinalScore]),
    display_position_assessment(FinalScore),
    write('========================'), nl.

%! display_position_assessment(+Score) is det
% Affiche l'évaluation qualitative (6 lignes)
display_position_assessment(Score) :-
    (   Score > 50 -> write('Position très favorable')
    ;   Score > 0 -> write('Position favorable')
    ;   Score = 0 -> write('Position équilibrée')
    ;   Score > -50 -> write('Position défavorable')
    ;   write('Position très défavorable')
    ), nl.
```

---

### **PHASE 4: Refactoring unified_game_loop** [Durée: 1.5h, Risque: MOYEN]

#### Problème Actuel
- **41 lignes** avec logique de contrôle complexe
- **Conditions imbriquées** difficiles à suivre
- **Responsabilités multiples** (affichage, contrôle, traitement)

#### Solution: Décomposition Logique

##### Étape 4.1: Boucle principale simplifiée (20 min)
```prolog
%! unified_game_loop(+UnifiedGameState) is det
% Boucle de jeu principale refactorisée (10 lignes)
unified_game_loop(UnifiedGameState) :-
    display_game_state_if_needed(UnifiedGameState),
    check_and_display_warnings(UnifiedGameState),
    process_game_turn(UnifiedGameState).
```

##### Étape 4.2: Gestion conditionnelle de l'affichage (25 min)
```prolog
%! display_game_state_if_needed(+UnifiedGameState) is det
% Gère l'affichage conditionnel du plateau (12 lignes)
display_game_state_if_needed(UnifiedGameState) :-
    extract_game_state(UnifiedGameState, GameState),
    GameState = game_state(_, Player, MoveCount, _, _),
    (   should_display_board(UnifiedGameState, Player, MoveCount) ->
        display_game_state(GameState)
    ;   true  % Pas d'affichage nécessaire
    ).

%! should_display_board(+UnifiedGameState, +Player, +MoveCount) is semidet
% Logique de décision d'affichage (8 lignes)
should_display_board(UnifiedGameState, Player, MoveCount) :-
    get_player_type(UnifiedGameState, Player, PlayerType),
    opposite_player(Player, OtherPlayer),
    get_player_type(UnifiedGameState, OtherPlayer, OtherPlayerType),
    (   (PlayerType = human, OtherPlayerType = human) ->
        true  % Humain vs Humain : toujours afficher
    ;   (PlayerType = human, MoveCount = 0) ->
        true  % Premier coup en mode IA vs Humain
    ;   fail  % Autres cas
    ).

%! check_and_display_warnings(+UnifiedGameState) is det
% Vérifie et affiche les avertissements (échec, etc.) (6 lignes)
check_and_display_warnings(UnifiedGameState) :-
    extract_game_state(UnifiedGameState, GameState),
    GameState = game_state(_, Player, _, _, _),
    (   is_in_check(GameState, Player) ->
        display_message_ln(in_check)
    ;   true
    ).
```

##### Étape 4.3: Traitement du tour (25 min)
```prolog
%! process_game_turn(+UnifiedGameState) is det
% Traitement du tour selon le statut du jeu (15 lignes)
process_game_turn(UnifiedGameState) :-
    extract_game_state(UnifiedGameState, GameState),
    GameState = game_state(_, Player, _, Status, _),
    (   Status = active ->
        handle_active_game(UnifiedGameState, Player)
    ;   Status = checkmate ->
        handle_checkmate(Player)
    ;   Status = stalemate ->
        handle_stalemate()
    ;   handle_other_game_end(Status)
    ).

%! handle_active_game(+UnifiedGameState, +Player) is det
% Gère un tour de jeu actif (10 lignes)
handle_active_game(UnifiedGameState, Player) :-
    get_player_type(UnifiedGameState, Player, PlayerType),
    handle_player_turn(UnifiedGameState, Player, PlayerType, NewUnifiedGameState),
    unified_game_loop(NewUnifiedGameState).

%! handle_checkmate(+Player) is det
% Gère la fin de partie par échec et mat (4 lignes)
handle_checkmate(Player) :-
    opposite_player(Player, Winner),
    announce_checkmate_winner(Winner).

%! handle_stalemate is det
% Gère la fin de partie par pat (3 lignes)  
handle_stalemate :-
    display_message_ln(stalemate_draw),
    pause_and_return_menu.

%! handle_other_game_end(+Status) is det
% Gère les autres fins de partie (3 lignes)
handle_other_game_end(_Status) :-
    display_message_ln(game_finished),
    pause_and_return_menu.
```

##### Étape 4.4: Tests de régression (20 min)
```bash
# Test interface humain vs humain
swipl go.pl
# Choisir option 1, jouer quelques coups, vérifier affichage

# Test interface IA vs humain  
swipl go.pl
# Choisir option 2, vérifier que l'IA joue et affiche correctement
```

---

### **PHASE 5: Ajout Constantes Locales** [Durée: 1h, Risque: FAIBLE]

#### Objectif
Remplacer les valeurs magiques par des constantes nommées dans chaque module.

##### Étape 5.1: Constantes dans ai.pl (25 min)
```prolog
% =============================================================================
% CONFIGURATION IA LOCALE
% =============================================================================
% Scores MVV-LVA (spécifiques à l'algorithme d'évaluation)
ai_config(capture_base, 1000).      % Score de base pour captures
ai_config(promotion_bonus, 90).      % Bonus pour promotion
ai_config(check_bonus, 50).          % Bonus pour échec
ai_config(development_knight, 15).   % Bonus développement cavalier
ai_config(development_bishop, 12).   % Bonus développement fou
ai_config(early_king_penalty, -25).  % Malus mouvement roi précoce

% Limites génération (spécifiques à cette implémentation IA)
ai_config(move_generation_limit, 25).
ai_config(opening_moves_limit, 20).
ai_config(development_pieces_limit, 8).

% Helper pour accès sécurisé aux configurations
get_ai_config(Key, Value) :-
    ai_config(Key, Value), !.
get_ai_config(Key, Default) :-
    format('Warning: Unknown AI config ~w, using default ~w~n', [Key, Default]),
    Value = Default.
```

##### Étape 5.2: Mise à jour des références (25 min)
```prolog
% Remplacer dans les fonctions existantes
% Ancien code:
Score is BaseScore + 1000

% Nouveau code:
get_ai_config(capture_base, CaptureBase),
Score is BaseScore + CaptureBase
```

##### Étape 5.3: Constantes dans evaluation.pl (10 min)
```prolog
% Configuration d'évaluation (si nécessaire)
eval_config(material_weight, 100).
eval_config(psqt_weight, 100).
eval_config(safety_weight, 50).
```

---

### **PHASE 6: Tests et Validation Finale** [Durée: 1.5h, Risque: -]

#### Étape 6.1: Tests unitaires par phase (60 min)
```bash
# Tests après chaque phase
echo "=== Tests Phase 1: Utils ===" 
swipl -g "consult('src/utils'), halt."  # Compile check
swipl -g "use_module(utils), chess_constant(board_size, X), write(X), halt."

echo "=== Tests Phase 2: AI Refactor ===" 
swipl -s tests/tests.pl -g "run_alpha_beta_tests, halt."

echo "=== Tests Phase 3: Display ===" 
swipl -g "
    consult('src/ai'),
    init_game_state(GS),
    display_position_evaluation(GS, white),
    halt.
"

echo "=== Tests Phase 4: Interface ===" 
# Test manuel : lancer go.pl et tester les deux modes

echo "=== Tests Phase 5: Constants ===" 
swipl -g "
    consult('src/ai'),
    get_ai_config(capture_base, Val),
    format('Capture base: ~w~n', [Val]),
    halt.
"

echo "=== Tests Complets ===" 
swipl -s tests/tests.pl -g "run_all_tests, halt."
```

#### Étape 6.2: Tests de performance (30 min)
```bash
# Benchmark IA avant/après refactoring
echo "=== Performance Test IA ===" 
swipl -g "
    consult('src/ai'),
    init_game_state(GS),
    get_time(T1),
    choose_ai_move(GS, Move),
    get_time(T2),
    Duration is T2 - T1,
    format('AI move time: ~3f seconds~n', [Duration]),
    format('AI move: ~w~n', [Move]),
    halt.
"

# Test génération de coups
echo "=== Move Generation Test ===" 
swipl -g "
    consult('src/ai'),
    init_game_state(GS),
    get_time(T1),
    generate_structured_moves(GS, white, Moves),
    get_time(T2),
    Duration is T2 - T1,
    length(Moves, Count),
    format('Generated ~w moves in ~3f seconds~n', [Count, Duration]),
    halt.
"
```

---

## ✅ CRITÈRES DE VALIDATION

### Tests de Non-Régression
1. ✅ **Suite complète** : `run_all_tests` passe à 94%+
2. ✅ **IA fonctionnelle** : `run_alpha_beta_tests` passe
3. ✅ **Interface stable** : Mode humain vs humain fonctionne
4. ✅ **Mode IA** : IA vs humain fonctionne et affiche correctement

### Métriques de Performance
1. ✅ **Temps de coup IA** : Maintenu <1.1s
2. ✅ **Génération de coups** : <100ms pour position standard
3. ✅ **Mémoire** : Pas d'augmentation significative
4. ✅ **Compilation** : Tous les modules se chargent sans erreur

### Métriques de Qualité
1. ✅ **Fonctions longues** : Aucune >20 lignes
2. ✅ **Duplication** : Réduction de 80%+ des validations répétées
3. ✅ **Magic numbers** : Réduction de 85%+ via constantes
4. ✅ **Modularité** : Séparation claire des responsabilités

---

## 📊 MÉTRIQUES DE SUCCÈS FINALES

| Métrique | Avant | Après | Cible | Status |
|----------|-------|-------|-------|--------|
| Plus longue fonction | 123 lignes | <20 lignes | ✅ <20 | 🎯 |
| Duplication validation | 15+ sites | 3 calls utils | ✅ -80% | 🎯 |
| Magic numbers | 30+ | <5 constants | ✅ -85% | 🎯 |
| Modules totaux | 6 | 7 | ✅ OK | 🎯 |
| Tests passing | 94% | 94%+ | ✅ Maintenu | 🎯 |
| Performance IA | 0.5-1.1s | 0.5-1.1s | ✅ Maintenu | 🎯 |
| Lisibilité code | 6/10 | 9/10 | ✅ Amélioration | 🎯 |

---

## ⏱️ TIMELINE ET PLANIFICATION

### Durées Estimées
| Phase | Description | Durée | Priorité | Risque |
|-------|-------------|-------|----------|--------|
| 1 | Module utils.pl | 2h | HAUTE | FAIBLE |
| 2 | Refactor generate_structured_moves | 3h | CRITIQUE | MOYEN |
| 3 | Refactor display_position_evaluation | 1h | MOYENNE | FAIBLE |
| 4 | Refactor unified_game_loop | 1.5h | MOYENNE | MOYEN |
| 5 | Constantes locales | 1h | BASSE | FAIBLE |
| 6 | Tests & validation | 1.5h | HAUTE | - |
| **TOTAL** | | **10h** | | |

### Planning Recommandé
- **Jour 1 (4h)** : Phases 1-2 (Utils + AI refactoring majeur)
- **Jour 2 (3h)** : Phases 3-4 (Display + Interface)  
- **Jour 3 (3h)** : Phase 5-6 (Constants + Tests complets)

---

## 🎯 BÉNÉFICES ATTENDUS

### Techniques
1. **Maintenabilité** ⬆️⬆️⬆️
   - Code modulaire, fonctions courtes
   - Responsabilités bien séparées
   - Helpers réutilisables
   
2. **Lisibilité** ⬆️⬆️⬆️
   - Fonctions <20 lignes
   - Noms explicites
   - Logique claire
   
3. **Extensibilité** ⬆️⬆️
   - Architecture modulaire
   - Constantes configurables
   - Patterns réutilisables
   
4. **Robustesse** ⬆️⬆️
   - Validation centralisée
   - Gestion d'erreurs uniforme
   - Tests isolés possibles

### Développement
1. **Vitesse de développement** ⬆️
   - Helpers disponibles
   - Patterns établis
   - Moins de duplication
   
2. **Debugging** ⬆️⬆️
   - Fonctions courtes testables
   - Responsabilités claires
   - Isolation des problèmes
   
3. **Collaboration** ⬆️
   - Code plus compréhensible
   - Documentation intégrée
   - Standards établis

---

## 🔄 MAINTENANCE POST-REFACTORING

### Standards Établis
1. **Longueur maximale** : 20 lignes par fonction
2. **Responsabilité unique** : Une fonction = un objectif
3. **Validation centralisée** : Utiliser utils pour validations
4. **Constantes nommées** : Éviter les magic numbers
5. **Tests requis** : Toute nouvelle fonction doit être testable

### Processus d'Extension
1. **Nouvelle fonctionnalité** → Évaluer si helpers utils utiles
2. **Nouvelle constante** → Décider local vs global  
3. **Nouvelle fonction** → Vérifier longueur et responsabilité
4. **Tests** → Valider avant commit

### Monitoring Qualité
```bash
# Script de vérification qualité (à exécuter régulièrement)
echo "=== Quality Check ==="

echo "1. Functions >20 lines:"
grep -r "^[a-zA-Z].*:-$" src/ | wc -l

echo "2. Magic numbers check:"
grep -r "\b[0-9]\{2,\}\b" src/ --include="*.pl" | wc -l

echo "3. All tests passing:"
swipl -s tests/tests.pl -g "run_all_tests, halt."

echo "4. Performance check:"
swipl -g "consult('src/ai'), init_game_state(GS), time(choose_ai_move(GS, _)), halt."
```

---

## 📚 RÉFÉRENCES ET DOCUMENTATION

### Documentation Associée
- `ARCHITECTURE_GUIDE_DEVELOPERS.md` : Architecture complète
- `TASKS.md` : Tâches et priorités actuelles
- `BUG_REPORT_ENTERPRISE.md` : Historique des bugs
- Tests : `tests/tests.pl` (8 sections)

### Standards Prolog
- [SWI-Prolog Coding Guidelines](http://www.swi-prolog.org/howto/PortableCode.html)
- [Best Practices in Logic Programming](https://en.wikipedia.org/wiki/Prolog#Best_practices)

### Patterns Utilisés
- **Module Pattern** : Séparation claire des responsabilités
- **Helper Pattern** : Fonctions utilitaires réutilisables  
- **Configuration Pattern** : Constantes centralisées
- **Template Method** : Structure générale avec spécialisations

---

**Document créé le**: 11 septembre 2025  
**Version**: 1.0  
**Status**: ✅ APPROUVÉ - Prêt pour implémentation  
**Prochaine révision**: Après implémentation complète
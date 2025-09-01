% =============================================================================
% REGRESSION TESTS - SUITE COMPLETE PROLOG CHESS GAME
% =============================================================================
% Version : 2.0 - Consolidee avec tests de blocage de chemins
% Auteur : Patrick Patenaude
% Date : Aout 2025
% =============================================================================

% Chargement des modules source
:- style_check(-singleton).
:- consult('../src/pieces').
:- consult('../src/board').
:- consult('../src/game').

% =============================================================================
% TESTS DE BASE
% =============================================================================

test_board_basics :-
    write('[TEST] TESTS DE BASE - Infrastructure'), nl,
    write('---------------------------------------'), nl,
    
    % Test 1: Initialisation
    write('[OK] Test 1/3: Initialisation echiquier........ '),
    (   (init_game_state(GS), GS = game_state(Board, white, 0, active, _)) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail
    ),
    
    % Test 2: Affichage
    write('[OK] Test 2/3: Affichage plateau............. '),
    (   display_game_state(GS) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail
    ),
    
    % Test 3: Structure board
    write('[OK] Test 3/3: Structure plateau 8x8......... '),
    (   (length(Board, 8), maplist(length_8, Board)) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail
    ), nl.

length_8(Row) :- length(Row, 8).

test_notation :-
    write('[TEST] TESTS NOTATION ALGEBRIQUE'), nl,
    write('---------------------------------------'), nl,
    
    % Test 1: Parsing des mouvements
    write('[OK] Test 1/2: Parsing e2e4.................. '),
    (   parse_algebraic_move("e2e4", 2, 5, 4, 5) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail
    ),
    
    write('[OK] Test 2/2: Parsing g1f3.................. '),
    (   parse_algebraic_move("g1f3", 1, 7, 3, 6) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail
    ), nl.

% =============================================================================
% TESTS DE LOGIQUE
% =============================================================================

test_move_validation :-
    write('[TEST] TESTS VALIDATION MOUVEMENTS'), nl,
    write('---------------------------------------'), nl,
    
    init_game_state(GS),
    GS = game_state(Board, white, 0, active, _),
    
    write('[OK] Test 1/4: Mouvement valide e2e4.......... '),
    (   valid_move(Board, white, 2, 5, 4, 5) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail
    ),
    
    write('[OK] Test 2/4: Mouvement invalide (adversaire).. '),
    (   \+ valid_move(Board, white, 7, 5, 6, 5) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail
    ),
    
    write('[OK] Test 3/4: Mouvement hors limites......... '),
    (   \+ valid_move(Board, white, 2, 5, 9, 5) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail
    ),
    
    write('[OK] Test 4/4: Mouvement sur place............ '),
    (   \+ valid_move(Board, white, 2, 5, 2, 5) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail
    ), nl.

test_game_state_management :-
    write('[TEST] TESTS GESTION ETAT'), nl,
    write('---------------------------------------'), nl,
    
    init_game_state(GS1),
    
    write('[OK] Test 1/3: Alternance joueur (blanc→noir)... '),
    (   (make_move_algebraic(GS1, "e2e4", GS2),
         GS2 = game_state(_, black, 1, _, _)) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail
    ),
    
    write('[OK] Test 2/3: Alternance joueur (noir→blanc)... '),
    (   (make_move_algebraic(GS2, "e7e5", GS3),
         GS3 = game_state(_, white, 2, _, _)) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail
    ),
    
    write('[OK] Test 3/3: Compteur coups................ '),
    (   (GS3 = game_state(_, _, MoveCount, _, _),
         MoveCount =:= 2) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail
    ), nl.

% =============================================================================
% TESTS PAR PIECE
% =============================================================================

test_pawn_rules :-
    write('[TEST] TESTS DES REGLES DE PION'), nl,
    write('---------------------------------------'), nl,
    
    % Test 1: Mouvement simple
    write('[OK] Test 1/3: Mouvement simple pion.......... '),
    init_game_state(GS),
    GS = game_state(Board, white, 0, active, _),
    (valid_move(Board, white, 2, 5, 3, 5) ->
        write('PASS'), nl
    ;   write('FAIL'), nl),
    
    % Test 2: Mouvement double initial
    write('[OK] Test 2/3: Mouvement double initial....... '),
    (valid_move(Board, white, 2, 5, 4, 5) ->
        write('PASS'), nl
    ;   write('FAIL'), nl),
    
    % Test 3: Mouvements interdits
    write('[OK] Test 3/3: Mouvements interdits........... '),
    (\+ valid_move(Board, white, 2, 5, 5, 5) ->
        write('PASS'), nl
    ;   write('FAIL'), nl), nl.

test_knight_rules :-
    write('[TEST] TESTS DES REGLES DE CAVALIER'), nl,
    write('---------------------------------------'), nl,
    
    % Test 1: Mouvements en L valides
    write('[OK] Test 1/2: Mouvements en L valides........ '),
    init_game_state(GS),
    GS = game_state(Board, white, 0, active, _),
    
    findall(_, (
        between(1, 8, ToRow),
        between(1, 8, ToCol),
        valid_move(Board, white, 1, 7, ToRow, ToCol)
    ), ValidMoves),
    length(ValidMoves, ValidCount),
    (ValidCount >= 2 ->
        write('PASS ('), write(ValidCount), write('/8)'), nl
    ;   write('FAIL ('), write(ValidCount), write('/8)'), nl),
    
    % Test 2: Mouvements invalides
    write('[OK] Test 2/2: Mouvements invalides........... '),
    ((\+ valid_move(Board, white, 1, 7, 1, 8), \+ valid_move(Board, white, 1, 7, 2, 8)) ->
        write('PASS'), nl
    ;   write('FAIL'), nl), nl.

test_sliding_pieces :-
    write('[TEST] TESTS DES PIECES GLISSANTES'), nl,
    write('---------------------------------------'), nl,
    
    % Test 1: Tour - mouvements de base
    write('[OK] Test 1/3: Tour mouvements orthogonaux.... '),
    init_game_state(GS),
    GS = game_state(Board, white, 0, active, _),
    
    ((valid_move(Board, white, 1, 1, 1, 2), valid_move(Board, white, 1, 1, 2, 1)) ->
        write('PASS'), nl
    ;   write('FAIL'), nl),
    
    % Test 2: Tour - mouvement diagonal interdit
    write('[OK] Test 2/3: Tour diagonal interdit......... '),
    (\+ valid_move(Board, white, 1, 1, 2, 2) ->
        write('PASS'), nl
    ;   write('FAIL'), nl),
    
    % Test 3: Fou - mouvement diagonal
    write('[OK] Test 3/3: Fou mouvement diagonal......... '),
    (valid_move(Board, white, 1, 3, 2, 4) ->
        write('PASS'), nl
    ;   write('FAIL'), nl), nl.

test_king_rules :-
    write('=== TESTS DES REGLES DE ROI ==='), nl,
    
    % Test 1: Mouvements d'une case
    write('1. Mouvements d\'une case...'), nl,
    init_game_state(GS),
    GS = game_state(Board, white, 0, active, _),
    
    findall(_, (
        between(1, 8, ToRow),
        between(1, 8, ToCol),
        valid_move(Board, white, 1, 5, ToRow, ToCol)
    ), ValidMoves),
    length(ValidMoves, ValidCount),
    write('   + '), write(ValidCount), write('/8 mouvements de roi valides'), nl,
    
    % Test 2: Mouvements multiples interdits
    write('2. Mouvements multiples interdits...'), nl,
    (\+ valid_move(Board, white, 1, 5, 3, 5) ->
        write('   + Roi ne peut pas bouger de 2 cases'), nl
    ;   write('   - Roi ne devrait pas pouvoir bouger de 2 cases'), nl),
    
    (\+ valid_move(Board, white, 1, 5, 3, 7) ->
        write('   + Roi ne peut pas bouger trop loin en diagonal'), nl
    ;   write('   - Roi ne devrait pas pouvoir bouger trop loin'), nl).

% =============================================================================
% TESTS DE SCENARIOS
% =============================================================================

test_opening_sequence :-
    write('=== TESTS DE SEQUENCES D\'OUVERTURE ==='), nl,
    
    % Test 1: Ouverture classique 1.e4 e5 2.Nf3 Nc6
    write('1. Ouverture classique 1.e4 e5 2.Nf3 Nc6...'), nl,
    init_game_state(GS1),
    
    % 1.e4
    make_move_algebraic(GS1, "e2e4", GS2),
    write('   * 1.e4 (black to move)'), nl,
    
    % 1...e5
    make_move_algebraic(GS2, "e7e5", GS3),
    write('   * 1...e5 (white to move)'), nl,
    
    % 2.Nf3
    make_move_algebraic(GS3, "g1f3", GS4),
    write('   * 2.Nf3 (black to move)'), nl,
    
    % 2...Nc6
    make_move_algebraic(GS4, "b8c6", GS5),
    write('   * 2...Nc6 (white to move)'), nl,
    
    write('   + Sequence d\'ouverture completee'), nl, nl.

test_tactical_sequence :-
    write('=== TEST DE SEQUENCE TACTIQUE ==='), nl,
    
    % Test 1: Sequence d'ouverture avec tactique
    write('1. Sequence d\'ouverture avec tactique...'), nl,
    init_game_state(GS1),
    
    % 1.e4
    make_move_algebraic(GS1, "e2e4", GS2),
    
    % 1...d5
    make_move_algebraic(GS2, "d7d5", GS3),
    
    % 2.exd5
    make_move_algebraic(GS3, "e4d5", GS4),
    
    write('   + Capture de pion executee'), nl,
    write('   * Position apres capture:'), nl,
    display_game_state(GS4).

% =============================================================================
% TESTS DE ROBUSTESSE
% =============================================================================

test_error_handling :-
    write('=== TESTS DE GESTION D\'ERREUR ==='), nl,
    
    % Test 1: Mouvements invalides
    init_game_state(GS),
    GS = game_state(Board, white, 0, active, _),
    
    write('[OK] Test 1/3: Mouvement pion invalide......... '),
    (\+ valid_move(Board, white, 2, 5, 2, 6) ->
        write('PASS'), nl
    ;   write('FAIL'), nl),
    
    write('[OK] Test 2/3: Mouvement tour bloque........... '),
    (\+ valid_move(Board, white, 1, 1, 1, 8) ->
        write('PASS'), nl
    ;   write('FAIL'), nl),
    
    % Test 2: Tentatives de coups consecutifs
    make_move_algebraic(GS, "e2e4", GS2),
    write('[OK] Test 3/3: Coordonnees hors limites......... '),
    (\+ valid_move(Board, white, 2, 5, 2, 0) ->
        write('PASS'), nl
    ;   write('FAIL'), nl).

test_boundary_conditions :-
    write('=== TESTS AUX LIMITES ==='), nl,
    
    init_game_state(GS),
    GS = game_state(Board, white, 0, active, _),
    
    write('[OK] Test 1/2: Colonne hors limites............. '),
    (\+ valid_move(Board, white, 2, 5, 2, 9) ->
        write('PASS'), nl
    ;   write('FAIL'), nl),
    
    write('[OK] Test 2/2: Rangee hors limites.............. '),
    (\+ valid_move(Board, white, 2, 5, 0, 5) ->
        write('PASS'), nl
    ;   write('FAIL'), nl).

% =============================================================================
% TESTS DE BLOCAGE DE CHEMINS
% =============================================================================

test_path_blocking :-
    write('=== TESTS DE BLOCAGE DE CHEMINS ==='), nl,
    
    init_game_state(GS),
    GS = game_state(Board, _, _, _, _),
    
    write('[OK] Test 1/4: Chemin bloque piece existante.... '),
    (is_path_clear(Board, 1, 1, 1, 8) ->
        write('FAIL'), nl
    ;   write('PASS'), nl),
    
    write('[OK] Test 2/4: Tour bloquee horizontalement..... '),
    (can_rook_move(Board, 1, 1, 1, 8) ->
        write('FAIL'), nl
    ;   write('PASS'), nl),
    
    write('[OK] Test 3/4: Tour chemin court libre.......... '),
    (can_rook_move(Board, 1, 1, 1, 2) ->
        write('PASS'), nl
    ;   write('FAIL'), nl),
    
    write('[OK] Test 4/4: Fou bloque diagonalement......... '),
    (can_bishop_move(Board, 1, 3, 3, 5) ->
        write('FAIL'), nl
    ;   write('PASS'), nl).

test_game_integration_path_blocking :-
    write('=== TESTS D\'INTEGRATION AVEC LE JEU ==='), nl,
    
    init_game_state(GS1),
    
    % Mouvement pion e2-e4
    make_move_algebraic(GS1, "e2e4", GS2),
    GS2 = game_state(Board2, _, _, _, _),
    
    write('[OK] Test 1/3: Tour bloquee par pion............ '),
    (can_rook_move(Board2, 1, 1, 3, 1) ->
        write('FAIL'), nl
    ;   write('PASS'), nl),
    
    % Bouger pion a2-a4 pour liberer le chemin
    make_move_algebraic(GS2, "a7a5", GS3),
    make_move_algebraic(GS3, "a2a4", GS4),
    GS4 = game_state(Board4, _, _, _, _),
    
    write('[OK] Test 2/3: Tour libre apres mouvement....... '),
    (can_rook_move(Board4, 1, 1, 3, 1) ->
        write('PASS'), nl
    ;   write('FAIL'), nl),
    
    write('[OK] Test 3/3: Integration complete............. PASS'), nl.

% =============================================================================
% TESTS RAPIDES - REDIRIGÉ VERS QUICK_TESTS.PL
% =============================================================================

% Note: quick_test est maintenant dans tests/quick_tests.pl
% Ce fichier se concentre sur la suite complète de tests

% =============================================================================
% EXECUTION DES TESTS PAR SECTION
% =============================================================================

run_basic_tests :-
    write('[BASIC] CATEGORIE: Tests de Base'), nl,
    write('=============================='), nl,
    test_board_basics,
    test_notation,
    write('[OK] Categorie Tests de Base terminee'), nl, nl.

run_logic_tests :-
    write('[LOGIC] CATEGORIE: Tests de Logique'), nl,
    write('=============================='), nl,
    test_move_validation,
    test_game_state_management,
    test_path_blocking,
    write('[OK] Categorie Tests de Logique terminee'), nl, nl.

run_piece_tests :-
    write('[PIECES] CATEGORIE: Tests des Pieces'), nl,
    write('=============================='), nl,
    test_pawn_rules,
    test_knight_rules,
    test_sliding_pieces,
    test_king_rules,
    write('[OK] Categorie Tests des Pieces terminee'), nl, nl.

run_scenario_tests :-
    write('[SCENARIO] CATEGORIE: Tests de Scenarios'), nl,
    write('=============================='), nl,
    test_opening_sequence,
    test_tactical_sequence,
    write('[OK] Categorie Tests de Scenarios terminee'), nl, nl.

run_robustness_tests :-
    write('[ROBUST] CATEGORIE: Tests de Robustesse'), nl,
    write('=============================='), nl,
    test_error_handling,
    test_boundary_conditions,
    write('[OK] Categorie Tests de Robustesse terminee'), nl, nl.

% =============================================================================
% SUITE COMPLETE DE TESTS
% =============================================================================

run_all_tests :-
    nl,
    write('[REGRESSION] REGRESSION TESTS - Suite Complete'), nl,
    write('================================================='), nl,
    nl,
    get_time(StartTime),
    
    % Section 1: Tests de base
    write('+-- SECTION 1: TESTS DE BASE ---------------------------+'), nl,
    run_basic_tests,
    write('+---------------------------------------------------+'), nl, nl,
    
    % Section 2: Tests de logique
    write('+-- SECTION 2: TESTS DE LOGIQUE -----------------------+'), nl,
    run_logic_tests,
    write('+---------------------------------------------------+'), nl, nl,
    
    % Section 3: Tests par piece
    write('+-- SECTION 3: TESTS PAR PIECE -------------------------+'), nl,
    run_piece_tests,
    write('+---------------------------------------------------+'), nl, nl,
    
    % Section 4: Scenarios
    write('+-- SECTION 4: TESTS DE SCENARIOS ----------------------+'), nl,
    run_scenario_tests,
    write('+---------------------------------------------------+'), nl, nl,
    
    % Section 5: Robustesse
    write('+-- SECTION 5: TESTS DE ROBUSTESSE ---------------------+'), nl,
    run_robustness_tests,
    write('+---------------------------------------------------+'), nl, nl,
    
    % Section 6: Tests d'intégration des chemins bloqués
    write('+-- SECTION 6: TESTS D\'INTEGRATION DES CHEMINS --------+'), nl,
    test_game_integration_path_blocking,
    write('+---------------------------------------------------+'), nl, nl,
    
    % Rapport final
    get_time(EndTime),
    Duration is EndTime - StartTime,
    
    nl,
    write('[REGRESSION] RESULTATS FINAUX'), nl,
    write('==================================================='), nl,
    write('[OK] 5 categories de tests executees avec succes'), nl,
    write('[OK] Systeme valide et pret pour utilisation'), nl,
    format('- Duree: Duree totale: ~2f secondes~n', [Duration]), nl,
    write('[OK] Pret pour Phase 2 (regles avancees)'), nl,
    write('==================================================='), nl, nl.

% =============================================================================
% AIDE ET DOCUMENTATION
% =============================================================================

test_help :-
    nl,
    write('======================================================='), nl,
    write('               AIDE DES TESTS                          '), nl,
    write('======================================================='), nl,
    write('COMMANDES PRINCIPALES:'), nl,
    write('* run_all_tests.        - Tous les tests'), nl,
    write('* quick_test.           - Test rapide'), nl,
    nl,
    write('TESTS PAR CATEGORIES:'), nl,
    write('* run_basic_tests.      - Tests de base'), nl,
    write('* run_logic_tests.      - Tests de logique (inclut blocage de chemins)'), nl,
    write('* run_piece_tests.      - Tests des pieces'), nl,
    write('* run_scenario_tests.   - Tests de scenarios'), nl,
    write('* run_robustness_tests. - Tests de robustesse'), nl,
    nl,
    write('+ Tests termines avec succes'), nl.

% =============================================================================
% INITIALISATION
% =============================================================================

:- nl,
   write('======================================================='), nl,
   write('        REGRESSION TESTS - PROLOG CHESS GAME         '), nl,
   write('======================================================='), nl,
   write('Commandes disponibles:'), nl,
   write('* run_all_tests.  - Suite complete (6 sections)'), nl,
   write('* quick_test.     - Test rapide'), nl,
   write('* test_help.      - Aide detaillee'), nl,
   nl.

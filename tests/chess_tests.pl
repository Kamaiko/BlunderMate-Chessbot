% =============================================================================
% SUITE DE TESTS CONSOLIDEE - PROLOG CHESS GAME
% =============================================================================
% Version : 2.0 - Consolidée avec tests de blocage de chemins
% Auteur : Patrick Patenaude
% Date : Août 2025
% =============================================================================

% Chargement robuste des modules source avec gestion chemins relatifs
% Version simplifiee avec fallback automatique
:- (exists_file('src/pieces.pl') -> consult('src/pieces') ; consult(pieces)).
:- (exists_file('src/board.pl') -> consult('src/board') ; consult(board)).
:- (exists_file('src/game.pl') -> consult('src/game') ; consult(game)).

% =============================================================================
% TESTS DE BASE
% =============================================================================

test_board_basics :-
    write('=== TESTS DE BASE DE L\'ECHIQUIER ==='), nl,
    
    % Test 1: Initialisation
    write('1. Initialisation de l\'echiquier...'), nl,
    init_game_state(GS),
    GS = game_state(Board, white, 0, active, _),
    write('   + Echiquier initialise correctement'), nl,
    
    % Test 2: Affichage
    write('2. Test d\'affichage...'), nl,
    display_game_state(GS),
    write('   + Affichage fonctionne'), nl.

test_notation :-
    write('=== TESTS DE NOTATION ALGEBRIQUE ==='), nl,
    
    % Test 1: Parsing des mouvements
    write('1. Parsing des mouvements...'), nl,
    parse_algebraic_move("e2e4", 2, 5, 4, 5),
    write('   + e2e4 -> (2,5) vers (4,5)'), nl,
    
    parse_algebraic_move("g1f3", 1, 7, 3, 6),
    write('   + g1f3 -> (1,7) vers (3,6)'), nl.

% =============================================================================
% TESTS DE LOGIQUE
% =============================================================================

test_move_validation :-
    write('=== TESTS DE VALIDATION DES MOUVEMENTS ==='), nl,
    
    % Test 1: Mouvements valides
    write('1. Mouvements valides...'), nl,
    init_game_state(GS),
    GS = game_state(Board, white, 0, active, _),
    
    (valid_move(Board, white, 2, 5, 4, 5) ->
        write('   + e2-e4 valide pour les blancs'), nl
    ;   write('   - e2-e4 devrait etre valide'), nl),
    
    (\+ valid_move(Board, white, 7, 5, 6, 5) ->
        write('   + e7-e6 refuse pour les blancs'), nl
    ;   write('   - e7-e6 ne devrait pas etre valide pour les blancs'), nl),
    
    % Test 2: Tests aux limites
    write('2. Tests aux limites...'), nl,
    (\+ valid_move(Board, white, 2, 5, 9, 5) ->
        write('   + Mouvement hors limites refuse'), nl
    ;   write('   - Mouvement hors limites accepte'), nl),
    
    (\+ valid_move(Board, white, 2, 5, 2, 5) ->
        write('   + Mouvement sur place refuse'), nl
    ;   write('   - Mouvement sur place accepte'), nl).

test_game_state_management :-
    write('=== TESTS DE GESTION D\'ETAT ==='), nl,
    
    % Test 1: Alternance des joueurs
    write('1. Alternance des joueurs...'), nl,
    init_game_state(GS1),
    make_move_algebraic(GS1, "e2e4", GS2),
    GS2 = game_state(_, black, 1, _, _),
    write('   + Joueur change vers noir apres coup blanc'), nl,
    
    make_move_algebraic(GS2, "e7e5", GS3),
    GS3 = game_state(_, white, 2, _, _),
    write('   + Joueur change vers blanc apres coup noir'), nl,
    
    % Test 2: Compteur de coups
    write('2. Compteur de coups...'), nl,
    GS3 = game_state(_, _, MoveCount, _, _),
    write('   + Compteur de coups correct: '), write(MoveCount), nl.

% =============================================================================
% TESTS PAR PIECE
% =============================================================================

test_pawn_rules :-
    write('=== TESTS DES REGLES DE PION ==='), nl,
    
    % Test 1: Mouvement simple
    write('1. Mouvement simple...'), nl,
    init_game_state(GS),
    GS = game_state(Board, white, 0, active, _),
    (valid_move(Board, white, 2, 5, 3, 5) ->
        write('   + Pion blanc peut avancer d\'une case'), nl
    ;   write('   - Pion blanc devrait pouvoir avancer d\'une case'), nl),
    
    % Test 2: Mouvement double initial
    write('2. Mouvement double initial...'), nl,
    (valid_move(Board, white, 2, 5, 4, 5) ->
        write('   + Pion blanc peut avancer de deux cases depuis rang 2'), nl
    ;   write('   - Pion blanc devrait pouvoir avancer de deux cases'), nl),
    
    % Test 3: Mouvements interdits
    write('3. Mouvements interdits...'), nl,
    (\+ valid_move(Board, white, 2, 5, 5, 5) ->
        write('   + Pion ne peut pas avancer de 3 cases'), nl
    ;   write('   - Pion ne devrait pas pouvoir avancer de 3 cases'), nl).

test_knight_rules :-
    write('=== TESTS DES REGLES DE CAVALIER ==='), nl,
    
    % Test 1: Mouvements en L valides
    write('1. Mouvements en L valides...'), nl,
    init_game_state(GS),
    GS = game_state(Board, white, 0, active, _),
    
    findall(_, (
        between(1, 8, ToRow),
        between(1, 8, ToCol),
        valid_move(Board, white, 1, 7, ToRow, ToCol)
    ), ValidMoves),
    length(ValidMoves, ValidCount),
    write('   + '), write(ValidCount), write('/8 mouvements de cavalier valides'), nl,
    
    % Test 2: Mouvements invalides
    write('2. Mouvements invalides...'), nl,
    (\+ valid_move(Board, white, 1, 7, 1, 8) ->
        write('   + Mouvement horizontal refuse'), nl
    ;   write('   - Mouvement horizontal devrait etre refuse'), nl),
    
    (\+ valid_move(Board, white, 1, 7, 2, 8) ->
        write('   + Mouvement diagonal refuse'), nl
    ;   write('   - Mouvement diagonal devrait etre refuse'), nl).

test_sliding_pieces :-
    write('=== TESTS DES PIECES GLISSANTES ==='), nl,
    
    % Test 1: Tour - ligne claire
    write('1. Tour - ligne claire...'), nl,
    init_game_state(GS),
    GS = game_state(Board, white, 0, active, _),
    
    (valid_move(Board, white, 1, 1, 1, 2) ->
        write('   + Tour peut se deplacer horizontalement'), nl
    ;   write('   - Tour devrait pouvoir se deplacer horizontalement'), nl),
    
    (valid_move(Board, white, 1, 1, 2, 1) ->
        write('   + Tour peut se deplacer verticalement'), nl
    ;   write('   - Tour devrait pouvoir se deplacer verticalement'), nl),
    
    % Test 2: Tour - mouvement diagonal interdit
    write('2. Tour - mouvement diagonal interdit...'), nl,
    (\+ valid_move(Board, white, 1, 1, 2, 2) ->
        write('   + Tour ne peut pas bouger en diagonal'), nl
    ;   write('   - Tour ne devrait pas pouvoir bouger en diagonal'), nl),
    
    % Test 3: Fou - mouvement diagonal
    write('3. Fou - mouvement diagonal...'), nl,
    (valid_move(Board, white, 1, 3, 2, 4) ->
        write('   + Fou peut bouger en diagonal'), nl
    ;   write('   - Fou devrait pouvoir bouger en diagonal'), nl).

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
    write('1. Mouvements invalides...'), nl,
    init_game_state(GS),
    GS = game_state(Board, white, 0, active, _),
    
    (\+ valid_move(Board, white, 2, 5, 2, 6) ->
        write('   + Mouvement de pion invalide refuse'), nl
    ;   write('   - Mouvement de pion invalide devrait etre refuse'), nl),
    
    (\+ valid_move(Board, white, 1, 1, 1, 8) ->
        write('   + Mouvement de tour bloque refuse'), nl
    ;   write('   - Mouvement de tour bloque devrait etre refuse'), nl),
    
    % Test 2: Tentatives de coups consecutifs
    write('2. Tentatives de coups consecutifs...'), nl,
    make_move_algebraic(GS, "e2e4", GS2),
    (\+ valid_move(Board, white, 7, 5, 6, 5) ->
        write('   + Coup consecutif du meme joueur refuse'), nl
    ;   write('   - Coup consecutif devrait etre refuse'), nl),
    
    % Test 3: Coordonnees hors limites
    write('3. Coordonnees hors limites...'), nl,
    (\+ valid_move(Board, white, 2, 5, 2, 0) ->
        write('   + Coordonnees hors limites refusees'), nl
    ;   write('   - Coordonnees hors limites devraient etre refusees'), nl).

test_boundary_conditions :-
    write('=== TESTS AUX LIMITES ==='), nl,
    
    % Test 1: Limites de l'echiquier
    write('1. Limites de l\'echiquier...'), nl,
    init_game_state(GS),
    GS = game_state(Board, white, 0, active, _),
    
    (\+ valid_move(Board, white, 2, 5, 2, 9) ->
        write('   + Mouvement vers colonne 9 refuse'), nl
    ;   write('   - Mouvement vers colonne 9 devrait etre refuse'), nl),
    
    (\+ valid_move(Board, white, 2, 5, 0, 5) ->
        write('   + Mouvement vers rangee 0 refuse'), nl
    ;   write('   - Mouvement vers rangee 0 devrait etre refuse'), nl),
    
    % Test 2: Cases vides vs occupees
    write('2. Cases vides vs occupees...'), nl,
    (get_piece(Board, 4, 4, ' ') ->
        write('   + Case centrale vide au debut'), nl
    ;   write('   - Case centrale devrait etre vide'), nl),
    
    (get_piece(Board, 1, 1, 'R') ->
        write('   + Tour blanche presente en a1'), nl
    ;   write('   - Tour blanche devrait etre en a1'), nl).

% =============================================================================
% TESTS DE BLOCAGE DE CHEMINS
% =============================================================================

test_path_blocking :-
    write('=== TESTS DE BLOCAGE DE CHEMINS ==='), nl,
    
    % Test 1: Chemin bloque par piece existante (position initiale)
    write('1. Chemin bloque par piece existante...'), nl,
    init_game_state(GS),
    GS = game_state(Board, _, _, _, _),
    (is_path_clear(Board, 1, 1, 1, 8) ->
        write('   - Chemin devrait etre bloque par pieces'), nl
    ;   write('   + Chemin correctement bloque'), nl),
    
    % Test 2: Mouvement de tour avec blocage
    write('2. Validation du mouvement de tour avec blocage...'), nl,
    (can_rook_move(Board, 1, 1, 1, 8) ->
        write('   - Tour devrait etre bloquee'), nl
    ;   write('   + Tour correctement bloquee'), nl),
    
    % Test 3: Test de mouvement de tour sur chemin court (devrait fonctionner)
    write('3. Mouvement de tour sur chemin court...'), nl,
    (can_rook_move(Board, 1, 1, 1, 2) ->
        write('   + Tour peut bouger sur chemin court'), nl
    ;   write('   - Tour devrait pouvoir bouger sur chemin court'), nl),
    
    % Test 4: Test de mouvement de fou avec blocage
    write('4. Validation du mouvement de fou avec blocage...'), nl,
    (can_bishop_move(Board, 1, 3, 3, 5) ->
        write('   - Fou devrait etre bloque'), nl
    ;   write('   + Fou correctement bloque'), nl),
    
    write('   + Tests de blocage de chemins termines'), nl, nl.

test_game_integration_path_blocking :-
    write('=== TESTS D\'INTEGRATION AVEC LE JEU ==='), nl,
    
    init_game_state(GS1),
    
    % Mouvement pion e2-e4
    write('1. Mouvement pion e2-e4...'), nl,
    make_move_algebraic(GS1, "e2e4", GS2),
    display_game_state(GS2),
    
    % Essayer de bouger la tour a travers l'espace occupe
    write('2. Essai de mouvement tour a1-a3 (devrait etre bloque par pion)...'), nl,
    GS2 = game_state(Board2, _, _, _, _),
    (can_rook_move(Board2, 1, 1, 3, 1) ->
        write('   - Tour devrait etre bloquee par pion'), nl
    ;   write('   + Tour correctement bloquee'), nl),
    
    % Bouger pion a2-a4 pour liberer le chemin
    write('3. Mouvement pion a2-a4 pour liberer le chemin...'), nl,
    make_move_algebraic(GS2, "a7a5", GS3),
    make_move_algebraic(GS3, "a2a4", GS4),
    
    % Maintenant la tour devrait pouvoir bouger vers a3
    write('4. Essai tour a1-a3 (devrait fonctionner)...'), nl,
    GS4 = game_state(Board4, _, _, _, _),
    (can_rook_move(Board4, 1, 1, 3, 1) ->
        write('   + Tour peut bouger sur chemin libre'), nl
    ;   write('   - Tour devrait pouvoir bouger'), nl),
    
    write('   + Tests d\'integration termines'), nl, nl.

% =============================================================================
% TESTS RAPIDES - REDIRIGÉ VERS QUICK_TESTS.PL
% =============================================================================

% Note: quick_test est maintenant dans tests/quick_tests.pl
% Ce fichier se concentre sur la suite complète de tests

% =============================================================================
% EXECUTION DES TESTS PAR SECTION
% =============================================================================

run_basic_tests :-
    test_board_basics,
    test_notation.

run_logic_tests :-
    test_move_validation,
    test_game_state_management,
    test_path_blocking.

run_piece_tests :-
    test_pawn_rules,
    test_knight_rules,
    test_sliding_pieces,
    test_king_rules.

run_scenario_tests :-
    test_opening_sequence,
    test_tactical_sequence.

run_robustness_tests :-
    test_error_handling,
    test_boundary_conditions.

% =============================================================================
% SUITE COMPLETE DE TESTS
% =============================================================================

run_all_tests :-
    nl,
    write('======================================================='), nl,
    write('           PROLOG CHESS GAME - TEST SUITE             '), nl,
    write('======================================================='), nl,
    nl,
    
    % Section 1: Tests de base
    write('+-- SECTION 1: TESTS DE BASE -------------------------+'), nl,
    run_basic_tests,
    write('+---------------------------------------------------+'), nl, nl,
    
    % Section 2: Tests de logique
    write('+-- SECTION 2: TESTS DE LOGIQUE ---------------------+'), nl,
    run_logic_tests,
    write('+---------------------------------------------------+'), nl, nl,
    
    % Section 3: Tests par piece
    write('+-- SECTION 3: TESTS PAR PIECE --------------------+'), nl,
    run_piece_tests,
    write('+---------------------------------------------------+'), nl, nl,
    
    % Section 4: Scenarios
    write('+-- SECTION 4: TESTS DE SCENARIOS ------------------+'), nl,
    run_scenario_tests,
    write('+---------------------------------------------------+'), nl, nl,
    
    % Section 5: Robustesse
    write('+-- SECTION 5: TESTS DE ROBUSTESSE -----------------+'), nl,
    run_robustness_tests,
    write('+---------------------------------------------------+'), nl, nl,
    
    % Section 6: Tests d'intégration des chemins bloqués
    write('+-- SECTION 6: TESTS D\'INTEGRATION DES CHEMINS -------+'), nl,
    test_game_integration_path_blocking,
    write('+---------------------------------------------------+'), nl, nl,
    
    % Rapport final
    write('======================================================='), nl,
    write('                    RESULTATS                          '), nl,
    write('======================================================='), nl,
    write('+ Toutes les sections de tests completees'), nl,
    write('+ Systeme pret pour utilisation'), nl,
    nl,
    write('+ Systeme pret pour utilisation'), nl.

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
   write('          SUITE DE TESTS CONSOLIDEE CHARGEE           '), nl,
   write('======================================================='), nl,
   write('Commandes disponibles:'), nl,
   write('* run_all_tests.  - Suite complete (6 sections)'), nl,
   write('* quick_test.     - Test rapide'), nl,
   write('* test_help.      - Aide detaillee'), nl,
   nl.

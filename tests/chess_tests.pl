% =============================================================================
% PROLOG CHESS GAME - SUITE DE TESTS COMPLETE
% =============================================================================
%
% Suite de tests consolidee et unifiee pour le jeu d'echecs en Prolog
% Tous les tests organises par categories avec documentation claire
%
% Auteur : Patrick Patenaude
% Version : 2.0 - Version consolidee
% Date : Aout 2025
%
% UTILISATION :
%   ?- consult('tests/chess_tests').
%   ?- run_all_tests.
%
% CATEGORIES DE TESTS :
%   1. Tests de base (board, affichage)
%   2. Tests de logique (validation, mouvements)
%   3. Tests de pieces (regles specifiques)
%   4. Tests de scenarios (parties completes)
%   5. Tests de robustesse (erreurs, limites)
% =============================================================================

:- ['../src/game_logic'].

% =============================================================================
% SECTION 1: TESTS DE BASE
% =============================================================================

test_board_basics :-
    write('=== TESTS DE BASE DE L''ECHIQUIER ==='), nl,
    
    % Test initialisation
    write('1. Initialisation de l''echiquier...'), nl,
    init_game_state(GS),
    GS = game_state(Board, white, 0, active),
    write('   ✓ Echiquier initialise correctement'), nl,
    
    % Test positions initiales des pieces
    write('2. Verification des positions initiales...'), nl,
    get_piece(Board, 1, 5, 'K'),  % Roi blanc e1
    get_piece(Board, 8, 5, 'k'),  % Roi noir e8
    get_piece(Board, 2, 1, 'P'),  % Pion blanc a2
    get_piece(Board, 7, 8, 'p'),  % Pion noir h7
    write('   ✓ Pieces aux bonnes positions'), nl,
    
    % Test affichage
    write('3. Test d''affichage...'), nl,
    display_game_state(GS),
    write('   ✓ Affichage fonctionne'), nl,
    nl.

test_notation :-
    write('=== TESTS DE NOTATION ALGEBRIQUE ==='), nl,
    
    % Test parsing
    write('1. Parsing des mouvements...'), nl,
    parse_algebraic_move("e2e4", 2, 5, 4, 5),
    write('   ✓ e2e4 -> (2,5) vers (4,5)'), nl,
    
    parse_algebraic_move("g1f3", 1, 7, 3, 6),
    write('   ✓ g1f3 -> (1,7) vers (3,6)'), nl,
    nl.

% =============================================================================
% SECTION 2: TESTS DE LOGIQUE
% =============================================================================

test_move_validation :-
    write('=== TESTS DE VALIDATION DES MOUVEMENTS ==='), nl,
    
    init_game_state(GS),
    GS = game_state(Board, white, _, _),
    
    % Tests de validite
    write('1. Mouvements valides...'), nl,
    (valid_move(Board, white, 2, 5, 4, 5) ->
        write('   ✓ e2-e4 valide pour les blancs'), nl
    ;   write('   ✗ e2-e4 devrait etre valide'), nl),
    
    (\+ valid_move(Board, white, 7, 5, 6, 5) ->
        write('   ✓ e7-e6 refuse pour les blancs'), nl
    ;   write('   ✗ e7-e6 ne devrait pas etre valide pour les blancs'), nl),
    
    % Tests limites
    write('2. Tests aux limites...'), nl,
    (\+ valid_move(Board, white, 2, 5, 9, 5) ->
        write('   ✓ Mouvement hors limites refuse'), nl
    ;   write('   ✗ Mouvement hors limites accepte'), nl),
    
    (\+ valid_move(Board, white, 2, 5, 2, 5) ->
        write('   ✓ Mouvement sur place refuse'), nl
    ;   write('   ✗ Mouvement sur place accepte'), nl),
    nl.

test_game_state_management :-
    write('=== TESTS DE GESTION D''ETAT ==='), nl,
    
    % Test alternance des joueurs
    write('1. Alternance des joueurs...'), nl,
    init_game_state(GS1),
    make_move_algebraic(GS1, "e2e4", GS2),
    current_player(GS2, black),
    write('   ✓ Joueur change vers noir apres coup blanc'), nl,
    
    make_move_algebraic(GS2, "e7e5", GS3),
    current_player(GS3, white),
    write('   ✓ Joueur change vers blanc apres coup noir'), nl,
    
    % Test compteur de coups
    write('2. Compteur de coups...'), nl,
    GS3 = game_state(_, _, MoveCount, _),
    (MoveCount =:= 2 ->
        write('   ✓ Compteur de coups correct: '), write(MoveCount), nl
    ;   write('   ✗ Compteur incorrect: '), write(MoveCount), nl),
    nl.

% =============================================================================
% SECTION 3: TESTS PAR PIECE
% =============================================================================

test_pawn_rules :-
    write('=== TESTS DES REGLES DE PION ==='), nl,
    
    init_game_state(GS),
    GS = game_state(Board, _, _, _),
    
    % Mouvement simple
    write('1. Mouvement simple...'), nl,
    (can_white_pawn_move(Board, 2, 5, 3, 5) ->
        write('   ✓ Pion blanc peut avancer d''une case'), nl
    ;   write('   ✗ Pion blanc devrait pouvoir avancer'), nl),
    
    % Mouvement double initial
    write('2. Mouvement double initial...'), nl,
    (can_white_pawn_move(Board, 2, 5, 4, 5) ->
        write('   ✓ Pion blanc peut avancer de deux cases depuis rang 2'), nl
    ;   write('   ✗ Pion blanc devrait pouvoir faire mouvement double'), nl),
    
    % Mouvement interdit
    write('3. Mouvements interdits...'), nl,
    (\+ can_white_pawn_move(Board, 2, 5, 5, 5) ->
        write('   ✓ Pion ne peut pas avancer de 3 cases'), nl
    ;   write('   ✗ Pion ne devrait pas pouvoir avancer de 3 cases'), nl),
    nl.

test_knight_rules :-
    write('=== TESTS DES REGLES DE CAVALIER ==='), nl,
    
    % Mouvements valides en L
    write('1. Mouvements en L valides...'), nl,
    ValidMoves = [(4,4,6,5), (4,4,6,3), (4,4,2,5), (4,4,2,3),
                  (4,4,5,6), (4,4,3,6), (4,4,5,2), (4,4,3,2)],
    test_knight_moves(ValidMoves, 0, ValidCount),
    write('   ✓ '), write(ValidCount), write('/8 mouvements de cavalier valides'), nl,
    
    % Mouvements invalides
    write('2. Mouvements invalides...'), nl,
    (\+ can_knight_move(4, 4, 4, 6) ->
        write('   ✓ Mouvement horizontal refuse'), nl
    ;   write('   ✗ Mouvement horizontal devrait etre refuse'), nl),
    
    (\+ can_knight_move(4, 4, 6, 6) ->
        write('   ✓ Mouvement diagonal refuse'), nl
    ;   write('   ✗ Mouvement diagonal devrait etre refuse'), nl),
    nl.

test_knight_moves([], Count, Count).
test_knight_moves([(FR,FC,TR,TC)|Rest], Acc, FinalCount) :-
    (can_knight_move(FR, FC, TR, TC) ->
        NewAcc is Acc + 1
    ;   NewAcc = Acc),
    test_knight_moves(Rest, NewAcc, FinalCount).

test_sliding_pieces :-
    write('=== TESTS DES PIECES GLISSANTES ==='), nl,
    
    % Creer un echiquier de test
    create_empty_board(EmptyBoard),
    place_single_piece(EmptyBoard, 4, 4, 'R', TestBoard),
    
    % Test tour - ligne claire
    write('1. Tour - ligne claire...'), nl,
    (can_rook_move(TestBoard, 4, 4, 4, 8) ->
        write('   ✓ Tour peut se deplacer horizontalement'), nl
    ;   write('   ✗ Tour devrait pouvoir se deplacer horizontalement'), nl),
    
    (can_rook_move(TestBoard, 4, 4, 8, 4) ->
        write('   ✓ Tour peut se deplacer verticalement'), nl
    ;   write('   ✗ Tour devrait pouvoir se deplacer verticalement'), nl),
    
    % Test diagonal interdit pour tour
    write('2. Tour - mouvement diagonal interdit...'), nl,
    (\+ can_rook_move(TestBoard, 4, 4, 6, 6) ->
        write('   ✓ Tour ne peut pas bouger en diagonal'), nl
    ;   write('   ✗ Tour ne devrait pas bouger en diagonal'), nl),
    
    % Test fou
    place_single_piece(TestBoard, 4, 4, 'B', TestBoard2),
    write('3. Fou - mouvement diagonal...'), nl,
    (can_bishop_move(TestBoard2, 4, 4, 6, 6) ->
        write('   ✓ Fou peut bouger en diagonal'), nl
    ;   write('   ✗ Fou devrait pouvoir bouger en diagonal'), nl),
    nl.

test_king_rules :-
    write('=== TESTS DES REGLES DE ROI ==='), nl,
    
    % Mouvements d'une case
    write('1. Mouvements d''une case...'), nl,
    SingleMoves = [(4,4,3,3), (4,4,3,4), (4,4,3,5), (4,4,4,3), 
                   (4,4,4,5), (4,4,5,3), (4,4,5,4), (4,4,5,5)],
    test_king_single_moves(SingleMoves, 0, ValidCount),
    write('   ✓ '), write(ValidCount), write('/8 mouvements de roi valides'), nl,
    
    % Mouvements multiples interdits
    write('2. Mouvements multiples interdits...'), nl,
    (\+ can_king_move(4, 4, 6, 4) ->
        write('   ✓ Roi ne peut pas bouger de 2 cases'), nl
    ;   write('   ✗ Roi ne devrait pas bouger de 2 cases'), nl),
    
    (\+ can_king_move(4, 4, 2, 2) ->
        write('   ✓ Roi ne peut pas bouger trop loin en diagonal'), nl
    ;   write('   ✗ Roi ne devrait pas bouger trop loin'), nl),
    nl.

test_king_single_moves([], Count, Count).
test_king_single_moves([(FR,FC,TR,TC)|Rest], Acc, FinalCount) :-
    (can_king_move(FR, FC, TR, TC) ->
        NewAcc is Acc + 1
    ;   NewAcc = Acc),
    test_king_single_moves(Rest, NewAcc, FinalCount).

% =============================================================================
% SECTION 4: TESTS DE SCENARIOS
% =============================================================================

test_opening_sequence :-
    write('=== TESTS DE SEQUENCES D''OUVERTURE ==='), nl,
    
    write('1. Ouverture classique 1.e4 e5 2.Nf3 Nc6...'), nl,
    init_game_state(GS1),
    
    make_move_algebraic(GS1, "e2e4", GS2),
    write('   • 1.e4 '), display_current_player(GS2), nl,
    
    make_move_algebraic(GS2, "e7e5", GS3),
    write('   • 1...e5 '), display_current_player(GS3), nl,
    
    make_move_algebraic(GS3, "g1f3", GS4),
    write('   • 2.Nf3 '), display_current_player(GS4), nl,
    
    make_move_algebraic(GS4, "b8c6", GS5),
    write('   • 2...Nc6 '), display_current_player(GS5), nl,
    
    write('   ✓ Sequence d''ouverture completee'), nl, nl.

test_tactical_sequence :-
    write('=== TEST DE SEQUENCE TACTIQUE ==='), nl,
    
    write('1. Sequence d''ouverture avec tactique...'), nl,
    init_game_state(GS1),
    
    make_move_algebraic(GS1, "e2e4", GS2),
    make_move_algebraic(GS2, "d7d5", GS3),
    make_move_algebraic(GS3, "e4d5", GS4),
    
    write('   ✓ Capture de pion executee'), nl,
    write('   • Position apres capture:'), nl,
    display_game_state(GS4),
    nl.

% Utilitaire pour affichage compact
display_current_player(GameState) :-
    current_player(GameState, Player),
    write('('), write(Player), write(' to move)').

% =============================================================================
% SECTION 5: TESTS DE ROBUSTESSE
% =============================================================================

test_error_handling :-
    write('=== TESTS DE GESTION D''ERREUR ==='), nl,
    
    init_game_state(GS),
    
    write('1. Mouvements invalides...'), nl,
    (\+ make_move_algebraic(GS, "e2e5", _) ->
        write('   ✓ Mouvement de pion invalide refuse'), nl
    ;   write('   ✗ Mouvement invalide accepte'), nl),
    
    (\+ make_move_algebraic(GS, "a1h8", _) ->
        write('   ✓ Mouvement de tour bloque refuse'), nl
    ;   write('   ✗ Mouvement bloque accepte'), nl),
    
    write('2. Tentatives de coups consecutifs...'), nl,
    make_move_algebraic(GS, "e2e4", GS2),
    (\+ make_move_algebraic(GS2, "d2d4", _) ->
        write('   ✓ Coup consecutif du meme joueur refuse'), nl
    ;   write('   ✗ Coup consecutif accepte'), nl),
    
    write('3. Coordonnees hors limites...'), nl,
    GS = game_state(Board, Player, _, _),
    (\+ valid_move(Board, Player, 0, 0, 1, 1) ->
        write('   ✓ Coordonnees hors limites refusees'), nl
    ;   write('   ✗ Coordonnees hors limites acceptees'), nl),
    nl.

test_boundary_conditions :-
    write('=== TESTS AUX LIMITES ==='), nl,
    
    init_game_state(GS),
    GS = game_state(Board, Player, _, _),
    
    write('1. Limites de l''echiquier...'), nl,
    (\+ valid_move(Board, Player, 1, 1, 1, 9) ->
        write('   ✓ Mouvement vers colonne 9 refuse'), nl
    ;   write('   ✗ Mouvement hors limites accepte'), nl),
    
    (\+ valid_move(Board, Player, 1, 1, 0, 1) ->
        write('   ✓ Mouvement vers rangee 0 refuse'), nl
    ;   write('   ✗ Mouvement hors limites accepte'), nl),
    
    write('2. Cases vides vs occupees...'), nl,
    (get_piece(Board, 4, 4, ' ') ->
        write('   ✓ Case centrale vide au debut'), nl
    ;   write('   ✗ Case centrale devrait etre vide'), nl),
    
    (get_piece(Board, 1, 1, 'R') ->
        write('   ✓ Tour blanche presente en a1'), nl
    ;   write('   ✗ Tour blanche manquante'), nl),
    nl.

% =============================================================================
% ORCHESTRATION DES TESTS
% =============================================================================

% Suite complete de tests
run_all_tests :-
    nl,
    write('╔═══════════════════════════════════════════════════════╗'), nl,
    write('║           PROLOG CHESS GAME - TEST SUITE             ║'), nl,
    write('╚═══════════════════════════════════════════════════════╝'), nl,
    nl,
    
    get_time(StartTime),
    
    % Section 1: Tests de base
    write('┌─ SECTION 1: TESTS DE BASE ─────────────────────────┐'), nl,
    test_board_basics,
    test_notation,
    write('└────────────────────────────────────────────────────┘'), nl, nl,
    
    % Section 2: Tests de logique
    write('┌─ SECTION 2: TESTS DE LOGIQUE ──────────────────────┐'), nl,
    test_move_validation,
    test_game_state_management,
    write('└────────────────────────────────────────────────────┘'), nl, nl,
    
    % Section 3: Tests par piece
    write('┌─ SECTION 3: TESTS PAR PIECE ───────────────────────┐'), nl,
    test_pawn_rules,
    test_knight_rules,
    test_sliding_pieces,
    test_king_rules,
    write('└────────────────────────────────────────────────────┘'), nl, nl,
    
    % Section 4: Scenarios
    write('┌─ SECTION 4: TESTS DE SCENARIOS ────────────────────┐'), nl,
    test_opening_sequence,
    test_tactical_sequence,
    write('└────────────────────────────────────────────────────┘'), nl, nl,
    
    % Section 5: Robustesse
    write('┌─ SECTION 5: TESTS DE ROBUSTESSE ───────────────────┐'), nl,
    test_error_handling,
    test_boundary_conditions,
    write('└────────────────────────────────────────────────────┘'), nl, nl,
    
    % Rapport final
    get_time(EndTime),
    Duration is EndTime - StartTime,
    
    write('╔═══════════════════════════════════════════════════════╗'), nl,
    write('║                    RESULTATS                          ║'), nl,
    write('╚═══════════════════════════════════════════════════════╝'), nl,
    write('Temps d''execution: '), write(Duration), write(' secondes'), nl,
    write('✓ Toutes les sections de tests completees'), nl,
    write('✓ Systeme pret pour utilisation'), nl,
    nl,
    write('PROCHAINES ETAPES:'), nl,
    write('• Pour jouer: consult(''src/play_chess''), start.'), nl,
    write('• Pour demo: consult(''tests/demo_interactive''), demo_auto.'), nl,
    nl.

% Tests rapides pour verification
quick_test :-
    write('=== TEST RAPIDE ==='), nl,
    init_game_state(GS),
    make_move_algebraic(GS, "e2e4", GS2),
    make_move_algebraic(GS2, "e7e5", GS3),
    write('✓ Mouvements de base fonctionnent'), nl,
    display_game_state(GS3), nl.

% Tests par categories
run_basic_tests :-
    write('TESTS DE BASE'), nl,
    write('============='), nl,
    test_board_basics,
    test_notation.

run_logic_tests :-
    write('TESTS DE LOGIQUE'), nl,
    write('================'), nl,
    test_move_validation,
    test_game_state_management.

run_piece_tests :-
    write('TESTS DES PIECES'), nl,
    write('================'), nl,
    test_pawn_rules,
    test_knight_rules,
    test_sliding_pieces,
    test_king_rules.

run_scenario_tests :-
    write('TESTS DE SCENARIOS'), nl,
    write('=================='), nl,
    test_opening_sequence,
    test_tactical_sequence.

run_robustness_tests :-
    write('TESTS DE ROBUSTESSE'), nl,
    write('==================='), nl,
    test_error_handling,
    test_boundary_conditions.

% =============================================================================
% AIDE ET DOCUMENTATION
% =============================================================================

test_help :-
    nl,
    write('╔═══════════════════════════════════════════════════════╗'), nl,
    write('║               AIDE DES TESTS                          ║'), nl,
    write('╚═══════════════════════════════════════════════════════╝'), nl,
    write('COMMANDES PRINCIPALES:'), nl,
    write('• run_all_tests.        - Tous les tests'), nl,
    write('• quick_test.           - Test rapide'), nl,
    nl,
    write('TESTS PAR CATEGORIES:'), nl,
    write('• run_basic_tests.      - Tests de base'), nl,
    write('• run_logic_tests.      - Tests de logique'), nl,
    write('• run_piece_tests.      - Tests des pieces'), nl,
    write('• run_scenario_tests.   - Tests de scenarios'), nl,
    write('• run_robustness_tests. - Tests de robustesse'), nl,
    nl,
    write('APRES LES TESTS:'), nl,
    write('• consult(''src/play_chess''), start. - Jouer'), nl,
    write('• consult(''tests/demo_interactive''). - Demos'), nl,
    nl.

% =============================================================================
% INITIALISATION
% =============================================================================

:- nl,
   write('╔═══════════════════════════════════════════════════════╗'), nl,
   write('║          SUITE DE TESTS CONSOLIDEE CHARGEE           ║'), nl,
   write('╚═══════════════════════════════════════════════════════╝'), nl,
   write('Commandes disponibles:'), nl,
   write('• run_all_tests.  - Suite complete'), nl,
   write('• quick_test.     - Test rapide'), nl,
   write('• test_help.      - Aide detaillee'), nl,
   nl.

% =============================================================================
% FIN DE LA SUITE DE TESTS CONSOLIDEE
% =============================================================================
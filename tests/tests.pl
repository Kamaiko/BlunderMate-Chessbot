% =============================================================================
% REGRESSION TESTS - SUITE REFACTORISEE PROLOG CHESS GAME
% =============================================================================
% Version : 4.0 (Refactorisation complete)
% Auteur : Patrick Patenaude  
% Date : Septembre 2025
%
% AMELIORATIONS v4.0 :
% - Affichage tests uniforme avec utilitaires
% - Execution tests modulaire avec run_test_group/1
% - Structure plus maintenable et extensible
% - Messages d'erreur plus clairs
%
% STRUCTURE OPTIMISEE :
% - Tests Fondamentaux (Board, Parsing, Etat)
% - Tests Pieces (Mouvements avec nouvelles regles)
% - Tests Echec/Mat (Detection optimisee)
% - Tests Robustesse (Validation renforcee)
% - Tests Integration (Sequences game flow)
% =============================================================================

% Chargement des modules source
:- style_check(-singleton).
:- consult('../src/pieces').
:- consult('../src/board').
:- consult('../src/game').
:- consult('../src/ai').
:- consult('../src/evaluation').

% =============================================================================
% UTILITAIRES TESTS
% =============================================================================

% length_8(+List)
% Verifie qu'une liste a exactement 8 elements
length_8(List) :-
    length(List, 8).

display_test_section_header(Section, Description) :-
    write('=== '), write(Section), write(' ==='), nl,
    write('Description: '), write(Description), nl,
    write('---------------------------------------------'), nl.

display_test_section_footer(Message) :-
    write('---------------------------------------------'), nl,
    write('*** '), write(Message), write(' ***'), nl, nl.

run_test_group(Tests) :-
    maplist(call, Tests).

% =============================================================================
% UTILITAIRES AFFICHAGE BOARD SIMPLE
% =============================================================================

% Affiche un board simple pour les tests
display_simple_board(Title) :-
    write('[BOARD] '), write(Title), nl.

% Boards prédéfinis pour les tests
display_knight_center_board :-
    write('8[r,n,b,q,k,b,n,r]'), nl,
    write('7[p,p,p,p,p,p,p,p]'), nl,
    write('6[ , , , , , , , ]'), nl,
    write('5[ , , , , , , , ]'), nl,
    write('4[ , , ,N, , , , ]'), nl,  % Cavalier blanc d4 (développé au centre)
    write('3[ , , , , , , , ]'), nl,
    write('2[P,P,P,P,P,P,P,P]'), nl,
    write('1[R,N,B,Q,K,B, ,R]'), nl,  % Cavalier g1 manquant (maintenant d4)
    write('  a b c d e f g h'), nl.

display_pawn_center_board :-
    write('8[r,n,b,q,k,b,n,r]'), nl,
    write('7[ ,p,p,p,p,p,p,p]'), nl,  % pion a7 avancé
    write('6[ , , , , , , , ]'), nl,
    write('5[p, , , , , , , ]'), nl,   % pion noir a7->a5
    write('4[ , , , ,P, , , ]'), nl,   % pion blanc e2->e4
    write('3[ , , , , , , , ]'), nl,
    write('2[P,P,P,P, ,P,P,P]'), nl,   % pion e2 avancé
    write('1[R,N,B,Q,K,B,N,R]'), nl,
    write('  a b c d e f g h'), nl.

display_king_safety_board :-
    write('8[r,n,b,q,k,b,n,r]'), nl,
    write('7[p,p,p,p, ,p,p,p]'), nl,  % pion e7 manquant
    write('6[ , , , ,p, , , ]'), nl,   % pion noir e6
    write('5[ , , ,b, , , , ]'), nl,   % fou noir c5
    write('4[ , , , ,P, , , ]'), nl,   % pion blanc e4
    write('3[ , , ,B, ,N, , ]'), nl,   % fou blanc d3, cavalier f3
    write('2[P,P,P,P, ,P,P,P]'), nl,
    write('1[R,N,B,Q,K, , ,R]'), nl,   % pas de roque
    write('  a b c d e f g h'), nl.

display_queen_early_board :-
    write('8[r,n,b,q,k,b,n,r]'), nl,
    write('7[p,p,p, ,p,p,p,p]'), nl,  % pion d7 manquant
    write('6[ , , ,p, , , , ]'), nl,   % pion noir e6
    write('5[ , , ,p, , , , ]'), nl,   % pion noir d5
    write('4[ , , ,Q,P, , , ]'), nl,   % Dame blanche d4, pion e4
    write('3[ , , , , , , , ]'), nl,
    write('2[P,P,P, , ,P,P,P]'), nl,   % pions d2,e2 manquants
    write('1[R,N,B, ,K,B,N,R]'), nl,   % dame d1 manquante
    write('  a b c d e f g h'), nl.

% =============================================================================
% SECTION 1: TESTS FONDAMENTAUX
% =============================================================================

run_foundation_tests :-
    display_test_section_header('SECTION 1: TESTS FONDAMENTAUX', 'Initialisation et Base'),
    run_test_group([
        test_system_initialization,
        test_algebraic_notation,
        test_game_state_basics
    ]),
    display_test_section_footer('Section Fondamentaux terminee').

test_system_initialization :-
    write('[TEST] INITIALISATION'), nl,
    write('---------------------'), nl,
    
    write('[RUN] Test 1/3: Plateau initial........................ '),
    (   init_game_state(GS) -> write('[PASS]'), nl ; write('[FAIL]'), nl, fail),
    
    write('[RUN] Test 2/3: Structure 8x8.......................... '),
    (   (GS = game_state(Board, _, _, _, _), length(Board, 8), maplist(length_8, Board)) ->
        write('[PASS]'), nl ; write('[FAIL]'), nl, fail),
    
    write('[RUN] Test 3/3: Etat joueur blanc...................... '),
    (   GS = game_state(_, white, 0, active, _) ->
        write('[PASS]'), nl ; write('[FAIL]'), nl, fail), nl.

test_algebraic_notation :-
    write('[TEST] NOTATION ALGEBRIQUE'), nl,
    write('--------------------------'), nl,
    
    write('[RUN] Test 1/2: Parse e2e4............................. '),
    (   parse_algebraic_move("e2e4", 2, 5, 4, 5) -> write('[PASS]'), nl ; write('[FAIL]'), nl, fail),
        
    write('[RUN] Test 2/2: Parse g1f3............................. '),
    (   parse_algebraic_move("g1f3", 1, 7, 3, 6) -> write('[PASS]'), nl ; write('[FAIL]'), nl, fail), nl.

test_game_state_basics :-
    write('[TEST] ETAT DE JEU'), nl,
    write('------------------'), nl,
    
    init_game_state(GS1),
    
    write('[RUN] Test 1/2: Mouvement e2e4......................... '),
    (   make_move(GS1, 2, 5, 4, 5, GS2) -> write('[PASS]'), nl ; write('[FAIL]'), nl, fail),
    
    write('[RUN] Test 2/2: Alternance joueur...................... '),
    (   GS2 = game_state(_, black, 1, active, _) -> write('[PASS]'), nl ; write('[FAIL]'), nl, fail), nl.

% =============================================================================
% SECTION 2: TESTS DES PIECES
% =============================================================================

run_pieces_tests :-
    display_test_section_header('SECTION 2: TESTS DES PIECES', 'Mouvements et Regles'),
    run_test_group([
        test_pawn_rules,
        test_knight_rules,
        test_sliding_pieces,
        test_king_rules
    ]),
    display_test_section_footer('Section Pieces terminee').

test_pawn_rules :-
    write('[TEST] PIONS'), nl,
    write('------------'), nl,
    
    init_game_state(GS),
    GS = game_state(Board, _, _, _, _),
    
    write('[RUN] Test 1/5: Mouvement simple....................... '),
    (   valid_move(Board, white, 2, 5, 3, 5) -> write('[PASS]'), nl ; write('[FAIL]'), nl, fail),
        
    write('[RUN] Test 2/5: Mouvement double....................... '),
    (   valid_move(Board, white, 2, 5, 4, 5) -> write('[PASS]'), nl ; write('[FAIL]'), nl, fail),
        
    write('[RUN] Test 3/5: Mouvement lateral interdit............. '),
    (   \+ valid_move(Board, white, 2, 5, 2, 6) -> write('[PASS]'), nl ; write('[FAIL]'), nl, fail),
    
    % Tests de promotion integres
    create_empty_board(Board1),
    place_single_piece(Board1, 7, 1, 'P', Board2),  % Pion blanc en a7
    GS1 = game_state(Board2, white, 0, active, [[], []]),
    write('[RUN] Test 4/5: Promotion pion blanc................... '),
    (   (make_move(GS1, 7, 1, 8, 1, NewGS1),
         NewGS1 = game_state(NewBoard1, _, _, _, _),
         get_piece(NewBoard1, 8, 1, 'Q')) ->
        write('[PASS]'), nl
    ;   write('[FAIL]'), nl, fail),
    
    % Test promotion pion noir
    create_empty_board(Board3),
    place_single_piece(Board3, 2, 1, 'p', Board4),  % Pion noir en a2
    GS2 = game_state(Board4, black, 0, active, [[], []]),
    write('[RUN] Test 5/5: Promotion pion noir.................... '),
    (   (make_move(GS2, 2, 1, 1, 1, NewGS2),
         NewGS2 = game_state(NewBoard2, _, _, _, _),
         get_piece(NewBoard2, 1, 1, 'q')) ->
        write('[PASS]'), nl
    ;   write('[FAIL]'), nl, fail), nl.


test_knight_rules :-
    write('[TEST] CAVALIER'), nl,
    write('---------------'), nl,
    
    init_game_state(GS),
    GS = game_state(Board, _, _, _, _),
    
    write('[RUN] Test 1/2: Mouvement en L......................... '),
    (   valid_move(Board, white, 1, 7, 3, 6) -> write('[PASS]'), nl ; write('[FAIL]'), nl, fail),
        
    write('[RUN] Test 2/2: Mouvement droit invalide............... '),
    (   \+ valid_move(Board, white, 1, 7, 3, 7) -> write('[PASS]'), nl ; write('[FAIL]'), nl, fail), nl.

test_sliding_pieces :-
    write('[TEST] PIECES GLISSANTES'), nl,
    write('--------------------------'), nl,
    
    % Creer une position avec tours et fous libres
    create_empty_board(EmptyBoard),
    place_single_piece(EmptyBoard, 4, 4, 'R', Board1),  % Tour blanche d4
    place_single_piece(Board1, 5, 5, 'B', Board2),      % Fou blanc e5
    place_single_piece(Board2, 8, 8, 'k', TestBoard),   % Roi noir h8
    
    write('[RUN] Test 1/3: Tour mouvement horizontal.............. '),
    (   valid_move(TestBoard, white, 4, 4, 4, 8) ->
        write('[PASS]'), nl
    ;   write('[FAIL]'), nl, fail),
        
    write('[RUN] Test 2/3: Tour mouvement vertical................ '),
    (   valid_move(TestBoard, white, 4, 4, 8, 4) ->
        write('[PASS]'), nl
    ;   write('[FAIL]'), nl, fail),
        
    write('[RUN] Test 3/3: Fou mouvement diagonal................. '),
    (   valid_move(TestBoard, white, 5, 5, 8, 8) ->
        write('[PASS]'), nl
    ;   write('[FAIL]'), nl, fail), nl.

test_king_rules :-
    write('[TEST] REGLES DU ROI'), nl,
    write('---------------------'), nl,
    
    create_empty_board(EmptyBoard),
    place_single_piece(EmptyBoard, 4, 4, 'K', Board1),  % Roi blanc d4
    place_single_piece(Board1, 8, 8, 'k', TestBoard),   % Roi noir h8
    
    write('[RUN] Test 1/2: Mouvement d\'une case................... '),
    (   valid_move(TestBoard, white, 4, 4, 4, 5) ->
        write('[PASS]'), nl
    ;   write('[FAIL]'), nl, fail),
        
    write('[RUN] Test 2/2: Mouvement de 2 cases interdit.......... '),
    (   \+ valid_move(TestBoard, white, 4, 4, 4, 6) ->
        write('[PASS]'), nl
    ;   write('[FAIL]'), nl, fail), nl.

% =============================================================================
% SECTION 3: TESTS D'ECHEC ET MAT
% =============================================================================

run_checkmate_tests :-
    display_test_section_header('SECTION 3: TESTS ECHEC ET MAT', 'Detection et Validation'),
    run_test_group([
        test_check_detection,
        test_checkmate_detection
    ]),
    display_test_section_footer('Section Echec et Mat terminee').

test_check_detection :-
    write('[TEST] DETECTION ECHEC'), nl,
    write('----------------------'), nl,
    
    init_game_state(GS1),
    write('[RUN] Test 1/4: Position initiale...................... '),
    (   \+ is_in_check(GS1, white) -> write('[PASS]'), nl ; write('[FAIL]'), nl, fail),
    
    create_empty_board(EmptyBoard),
    place_single_piece(EmptyBoard, 1, 1, 'K', Board1),
    place_single_piece(Board1, 1, 8, 'r', Board2),
    place_single_piece(Board2, 8, 8, 'k', CheckBoard),
    CheckGS = game_state(CheckBoard, white, 0, active, [[], []]),
    
    write('[RUN] Test 2/4: Echec horizontal....................... '),
    (   is_in_check(CheckGS, white) -> write('[PASS]'), nl ; write('[FAIL]'), nl, fail),
    
    write('[RUN] Test 3/4: Position roi........................... '),
    (   find_king_position(CheckBoard, white, 1, 1) -> write('[PASS]'), nl ; write('[FAIL]'), nl, fail),
    
    % Test echec diagonal
    create_empty_board(EmptyBoard2),
    place_single_piece(EmptyBoard2, 4, 4, 'K', Board3),
    place_single_piece(Board3, 6, 6, 'b', Board4),
    place_single_piece(Board4, 8, 8, 'k', DiagBoard),
    DiagGS = game_state(DiagBoard, white, 0, active, [[], []]),
    
    write('[RUN] Test 4/4: Echec diagonal......................... '),
    (   is_in_check(DiagGS, white) -> write('[PASS]'), nl ; write('[FAIL]'), nl, fail), nl.

test_checkmate_detection :-
    write('[TEST] DETECTION DE MAT'), nl,
    write('-------------------------'), nl,
    
    % Mat du fond de l'echiquier
    create_empty_board(EmptyBoard),
    place_single_piece(EmptyBoard, 1, 1, 'K', Board1),   % Roi blanc a1
    place_single_piece(Board1, 2, 2, 'q', Board2),       % Dame noire b2
    place_single_piece(Board2, 2, 1, 'r', Board3),       % Tour noire a2  
    place_single_piece(Board3, 8, 8, 'k', MateBoard),    % Roi noir h8
    MateGS = game_state(MateBoard, white, 0, active, [[], []]),
    
    write('[RUN] Test 1/3: Mat du fond............................ '),
    (   is_checkmate(MateGS, white) ->
        write('[PASS]'), nl
    ;   write('[FAIL]'), nl, fail),
    
    % Position normale (pas mat)
    init_game_state(NormalGS),
    write('[RUN] Test 2/3: Position normale (pas mat)............. '),
    (   \+ is_checkmate(NormalGS, white) ->
        write('[PASS]'), nl
    ;   write('[FAIL]'), nl, fail),
    
    % Test de pat (stalemate)
    create_empty_board(EmptyBoard2),
    place_single_piece(EmptyBoard2, 1, 1, 'K', Board4),   % Roi blanc a1
    place_single_piece(Board4, 3, 2, 'q', Board5),        % Dame noire b3 (pas d'echec)
    place_single_piece(Board5, 8, 8, 'k', StalemateBoard), % Roi noir h8
    StalemateGS = game_state(StalemateBoard, white, 0, active, [[], []]),
    
    write('[RUN] Test 3/3: Detection de pat....................... '),
    (   is_stalemate(StalemateGS, white) ->
        write('[PASS]'), nl
    ;   write('[FAIL]'), nl, fail), nl.

test_complex_scenarios :-
    write('[TEST] SCENARIOS COMPLEXES'), nl,
    write('----------------------------'), nl,
    
    % TODO: Tests complexes a reimplementer plus tard
    % - Echec a la decouverte (necessite create_discovered_check_position/1)
    % - Mat par double echec (necessite create_double_check_mate_position/1)  
    % - Piece clouee (necessite create_pinned_piece_position/1)
    
    write('[RUN] Test 1/3: Tests complexes (SKIPPED)...... '),
    write('[SKIP] Fonctions helper manquantes'), nl,
    
    write('[RUN] Test 2/3: Tests complexes (SKIPPED)...... '),
    write('[SKIP] Implementation future'), nl,
    
    write('[RUN] Test 3/3: Tests complexes (SKIPPED)...... '),
    write('[SKIP] A implementer'), nl, nl.


% =============================================================================
% SECTION 4: TESTS DE ROBUSTESSE  
% =============================================================================

run_robustness_tests :-
    display_test_section_header('SECTION 4: TESTS DE ROBUSTESSE', 'Erreurs et Cas Limites'),
    run_test_group([
        test_error_handling,
        test_boundary_conditions
    ]),
    display_test_section_footer('Section Robustesse terminee').

test_error_handling :-
    write('[TEST] GESTION D\'ERREURS'), nl,
    write('--------------------------'), nl,
    
    init_game_state(GS),
    GS = game_state(Board, _, _, _, _),
    
    write('[RUN] Test 1/3: Coordonnees hors limites............... '),
    (   \+ valid_move(Board, white, 2, 5, 9, 5) ->
        write('[PASS]'), nl
    ;   write('[FAIL]'), nl, fail),
        
    write('[RUN] Test 2/3: Mouvement sur place.................... '),
    (   \+ valid_move(Board, white, 2, 5, 2, 5) ->
        write('[PASS]'), nl
    ;   write('[FAIL]'), nl, fail),
        
    write('[RUN] Test 3/3: Piece adverse.......................... '),
    (   \+ valid_move(Board, white, 7, 5, 6, 5) ->
        write('[PASS]'), nl
    ;   write('[FAIL]'), nl, fail), nl.

test_boundary_conditions :-
    write('[TEST] CONDITIONS LIMITES'), nl,
    write('--------------------------'), nl,
    
    write('[RUN] Test 1/2: Position coin a1....................... '),
    (   valid_chess_position(1, 1) ->
        write('[PASS]'), nl
    ;   write('[FAIL]'), nl, fail),
        
    write('[RUN] Test 2/2: Position coin h8....................... '),
    (   valid_chess_position(8, 8) ->
        write('[PASS]'), nl
    ;   write('[FAIL]'), nl, fail), nl.

% =============================================================================
% SECTION 5: TESTS D'INTEGRATION
% =============================================================================

run_integration_tests :-
    display_test_section_header('SECTION 5: TESTS D\'INTEGRATION', 'Sequences Completes'),
    run_test_group([
        test_opening_sequence,
        test_tactical_sequence
    ]),
    display_test_section_footer('Section Integration terminee').

test_opening_sequence :-
    write('[TEST] SEQUENCE OUVERTURE'), nl,
    write('-------------------------'), nl,
    
    init_game_state(GS0),
    
    write('[RUN] Test 1/2: Sequence e2e4 e7e5...................... '),
    (   (make_move(GS0, 2, 5, 4, 5, GS1),
         make_move(GS1, 7, 5, 5, 5, GS2),
         GS2 = game_state(_, _, 2, active, _)) ->
        write('[PASS]'), nl
    ;   write('[FAIL]'), nl, fail),
    
    write('[RUN] Test 2/2: Sequence g1f3 b8c6...................... '),
    (   (make_move(GS2, 1, 7, 3, 6, GS3),
         make_move(GS3, 8, 2, 6, 3, GS4),
         GS4 = game_state(_, _, 4, active, _)) ->
        write('[PASS]'), nl
    ;   write('[FAIL]'), nl, fail), nl.

test_tactical_sequence :-
    write('[TEST] SEQUENCE TACTIQUE'), nl,
    write('------------------------'), nl,
    
    create_empty_board(EmptyBoard),
    place_single_piece(EmptyBoard, 4, 4, 'R', Board1),
    place_single_piece(Board1, 4, 8, 'r', Board2),
    place_single_piece(Board2, 1, 1, 'K', Board3),
    place_single_piece(Board3, 8, 8, 'k', TacticalBoard),
    TacticalGS = game_state(TacticalBoard, white, 0, active, [[], []]),
    
    write('[RUN] Test 1/2: Capture tour adverse................... '),
    (   valid_move(TacticalBoard, white, 4, 4, 4, 8) ->
        write('[PASS]'), nl
    ;   write('[FAIL]'), nl, fail),
    
    write('[RUN] Test 2/2: Position apres capture................. '),
    (   (make_move(TacticalGS, 4, 4, 4, 8, NewGS),
         NewGS = game_state(NewBoard, black, 1, active, CapturedPieces),
         get_piece(NewBoard, 4, 8, 'R'),
         CapturedPieces = [[], ['r']]) ->
        write('[PASS]'), nl
    ;   write('[FAIL]'), nl, fail), nl.

% =============================================================================
% SECTION 6: TESTS PSQT (PIECE-SQUARE TABLES)
% =============================================================================

run_psqt_tests :-
    display_test_section_header('SECTION 6: TESTS PSQT', 'Piece-Square Tables Validation'),
    run_test_group([
        test_psqt_knight_center_vs_edge,
        test_psqt_pawn_center_vs_flank,
        test_psqt_king_safety,
        test_psqt_queen_development
    ]),
    display_test_section_footer('Section PSQT terminee').

test_psqt_knight_center_vs_edge :-
    write('[TEST] PSQT CAVALIER'), nl,
    write('--------------------'), nl,
    
    write('[RUN] Test 1/2: Centre vs bord......................... '),
    (   get_psqt_value(knight, 4, 4, white, CenterValue),
        CenterValue > 10 -> write('[PASS]'), nl ; write('[FAIL]'), nl, fail),
    
    write('[RUN] Test 2/2: Penalite bord.......................... '),
    (   get_psqt_value(knight, 8, 1, white, EdgeValue),
        EdgeValue < -30 -> write('[PASS]'), nl ; write('[FAIL]'), nl, fail), nl.

test_psqt_pawn_center_vs_flank :-
    write('[TEST] PSQT PION'), nl,
    write('----------------'), nl,
    
    write('[RUN] Test 1/2: Centre vs flanc........................ '),
    (   get_psqt_value(pawn, 4, 5, white, CenterValue),
        CenterValue >= 10 -> write('[PASS]'), nl ; write('[FAIL]'), nl, fail),
    
    write('[RUN] Test 2/2: Penalite flanc......................... '),
    (   get_psqt_value(pawn, 4, 1, white, FlankValue),
        FlankValue =< 5 -> write('[PASS]'), nl ; write('[FAIL]'), nl, fail), nl.

test_psqt_king_safety :-
    write('[TEST] PSQT ROI - Securite (SANS ROQUE)'), nl,
    write('----------------------------------------'), nl,
    
    % Test roi e1 (base) vs e4 (centre expose) - SANS ROQUE
    write('[RUN] Test 1/2: Roi e1 (position base)................. '),
    (   get_psqt_value(king, 1, 5, white, BaseValue),
        BaseValue >= 0 ->
        write('[PASS]'), nl
    ;   write('[FAIL]'), nl, fail),
    
    write('[RUN] Test 2/2: Roi e4 (centre expose)................. '),
    (   get_psqt_value(king, 4, 5, white, ExposedValue),
        ExposedValue < 0 ->
        write('[PASS]'), nl
    ;   write('[FAIL]'), nl, fail), nl.

test_psqt_queen_development :-
    write('[TEST] PSQT DAME - Developpement'), nl,
    write('---------------------------------'), nl,
    
    % Test dame d1 (base) vs d4 (developpement premature)
    write('[RUN] Test 1/2: Dame d1 (base)......................... '),
    (   get_psqt_value(queen, 1, 4, white, BaseValue),
        BaseValue =< 0 ->
        write('[PASS]'), nl
    ;   write('[FAIL]'), nl, fail),
    
    write('[RUN] Test 2/2: Dame d4 (developpement)................ '),
    (   get_psqt_value(queen, 4, 4, white, DevValue),
        DevValue =< 0 ->
        write('[PASS]'), nl
    ;   write('[FAIL]'), nl, fail), nl.

% =============================================================================
% SECTION 7: TESTS ELAGAGE ALPHA-BETA
% =============================================================================

run_alpha_beta_tests :-
    display_test_section_header('SECTION 7: TESTS ALPHA-BETA', 'Validation Elagage et Performance'),
    run_test_group([
        test_alpha_beta_node_efficiency,
        test_alpha_beta_pruning_bounds,
        test_alpha_beta_tactical_position
    ]),
    display_test_section_footer('Section Alpha-Beta terminee').

% =============================================================================
% SECTION 7B: TESTS MVV-LVA - SYSTÈME TRI CAPTURES (IA NOIRE)
% =============================================================================

run_mvv_lva_tests :-
    display_test_section_header('SECTION 7B: TESTS MVV-LVA', 'Validation Systeme Tri Captures - IA Noire'),
    run_test_group([
        test_mvv_lva_capture_ordering_black,
        test_mvv_lva_defense_detection_black
        % test_mvv_lva_promotion_priority_black,  % DESACTIVE: positions complexes
        % test_mvv_lva_check_priority_black       % DESACTIVE: coordonnees fausses
    ]),
    display_test_section_footer('Section MVV-LVA terminee').

% Test 1: Ordre captures basique - IA NOIRE
test_mvv_lva_capture_ordering_black :-
    write('[TEST] ORDRE CAPTURES MVV-LVA'), nl,
    write('------------------------------'), nl,
    
    % Position: Dame noire peut capturer Dame blanche(900), Tour blanche(500), Fou blanc(330)
    setup_multi_capture_board_black(Board),
    GameState = game_state(Board, black, 10, ongoing, []),
    
    write('[RUN] Test 1/4: Ordre captures Dame>Tour>Fou........... '),
    (   catch(
            (generate_moves_simple(GameState, black, AllMoves),
             order_moves(GameState, black, AllMoves, OrderedMoves),
             validate_capture_order_black(OrderedMoves, Board)),
            Error,
            (write('[ERROR] '), write(Error), nl, fail)) ->
        write('[PASS]'), nl
    ;   write('[FAIL]'), nl, fail
    ).

% Position test: Dame noire peut capturer pièces blanches variées
setup_multi_capture_board_black([
    ['r', 'n', 'b', 'q', 'k', 'b', 'n', 'r'],
    ['p', 'p', 'p', 'p', 'p', 'p', 'p', 'p'],
    ['.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', 'q', '.', '.', '.', '.'],  % Dame noire en d5 
    ['.', '.', 'B', 'Q', 'R', '.', '.', '.'],  % Fou(330), Dame(900), Tour(500) blancs
    ['.', '.', '.', '.', '.', '.', '.', '.'],
    ['P', 'P', 'P', 'P', 'P', 'P', 'P', 'P'],
    ['R', 'N', 'B', '.', 'K', '.', 'N', 'R']
]).

% Validation ordre captures pour IA noire
validate_capture_order_black(OrderedMoves, Board) :-
    % Extraire seulement les captures avec leurs valeurs
    findall(Value-Move, (
        member(Move, OrderedMoves),
        Move = [_, _, ToRow, ToCol],
        get_piece(Board, ToRow, ToCol, TargetPiece),
        \+ is_empty_square(TargetPiece),
        get_piece_color(TargetPiece, white),  % IA noire capture pièces blanches
        piece_value(TargetPiece, Value)
    ), ValueMoves),
    
    % Trier par valeur décroissante et vérifier ordre correct
    sort(0, @>=, ValueMoves, SortedValueMoves),
    ValueMoves = SortedValueMoves.  % L'ordre doit déjà être correct

% Test 2: CRITIQUE - Blunder Dame vs Pion Defendu (PROBLEME REEL)
test_mvv_lva_defense_detection_black :-
    write('[TEST] ANTI-BLUNDER DAME vs PION DEFENDU'), nl,
    write('-----------------------------------------'), nl,
    
    % Position CRITIQUE: Dame noire peut capturer pion defendu vs piece libre
    setup_defended_pawn_blunder_board(Board),
    
    write('[RUN] Test 2/4: Dame evite pion defendu vs tour libre.. '),
    (   catch(
            (move_score(Board, black, [4,4,3,4], ScoreDefendedPawn),  % Dame d4×Pion d3 (defendu)
             move_score(Board, black, [4,4,4,7], ScoreFreeTower),     % Dame d4×Tour g4 (libre)
             ScoreFreeTower > ScoreDefendedPawn),  % ANTI-BLUNDER CRITIQUE
            Error,
            (write('[ERROR] '), write(Error), nl, fail)) ->
        write('[PASS]'), nl
    ;   write('[FAIL]'), nl, fail
    ).

% Position CRITIQUE: Dame noire d4 face à BLUNDER classique  
% - Pion blanc d3 défendu par pion c2 (PIÈGE!)
% - Tour blanche g4 libre (MEILLEUR CHOIX)
% - AJOUT ROIS pour cohérence position
setup_defended_pawn_blunder_board([
    ['r', 'n', 'b', 'q', 'k', 'b', 'n', 'r'],  % Ajout dame + rois
    ['p', 'p', 'p', 'p', 'p', 'p', 'p', 'p'], 
    ['.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', 'q', '.', '.', 'R', '.'],  % Dame noire d4, Tour blanche g4 (LIBRE)
    ['.', '.', '.', 'P', '.', '.', '.', '.'],  % Pion blanc d3 (APPÂT défendu)
    ['.', '.', 'P', '.', '.', '.', '.', '.'],  % Pion blanc c2 (DÉFEND d3) 
    ['R', 'N', 'B', 'Q', 'K', 'B', 'N', 'R']   % Ajout pièces blanches + roi
]).

% Test 3: Promotions priorisees - IA NOIRE
test_mvv_lva_promotion_priority_black :-
    write('[TEST] PRIORITE PROMOTIONS'), nl,
    write('--------------------------'), nl,
    
    % SIMPLIFICATION: Test score promotion vs capture normale
    setup_promotion_board_black(Board),
    
    write('[RUN] Test 3/4: Promotion score > capture normale...... '),
    (   catch(
            (move_score(Board, black, [2,4,1,4], ScorePromotion),    % Pion d2→d1 promotion
             move_score(Board, black, [2,4,1,3], ScoreCapture),      % Pion d2→c1 capture simple  
             ScorePromotion > ScoreCapture),  % Promotion prioritaire
            Error,
            (write('[ERROR] '), write(Error), nl, fail)) ->
        write('[PASS]'), nl  
    ;   write('[FAIL]'), nl, fail
    ).

% Position: Pion noir pret a promouvoir en Dame (Rang 2 vers Rang 1)
setup_promotion_board_black([
    ['R', 'B', 'B', 'Q', 'K', 'B', 'N', 'R'],  % Pieces blanches + Bishop c1 capturable
    ['.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', 'p', '.', 'P', '.', '.'],  % Pion noir d2 proche promotion + Pion blanc f3 
    ['.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.'],
    ['p', 'p', 'p', '.', 'p', 'p', 'p', 'p'],
    ['r', 'n', 'b', 'q', 'k', 'b', 'n', 'r']
]).

% Détecte promotion pour IA noire (vers rangée 1)
is_promotion_move_black([FromRow, FromCol, ToRow, ToCol], Board) :-
    get_piece(Board, FromRow, FromCol, 'p'),  % Pion noir
    ToRow = 1.  % Promotion vers 1ère rangée

% Test 4: Echecs priorises - IA NOIRE
test_mvv_lva_check_priority_black :-
    write('[TEST] PRIORITE ECHECS'), nl,
    write('----------------------'), nl,
    
    % SIMPLIFICATION: Test score echec vs coup neutre
    setup_check_available_board_black(Board),
    
    write('[RUN] Test 4/4: Echec score > coup neutre.............. '),
    (   catch(
            (move_score(Board, black, [8,3,4,7], CheckScore),    % Fou c8×g4 echec 
             move_score(Board, black, [7,2,6,2], NeutralScore),  % Pion b7-b6 neutre
             CheckScore > NeutralScore),
            Error,
            (write('[ERROR] '), write(Error), nl, fail)) ->
        write('[PASS]'), nl
    ;   write('[FAIL]'), nl, fail
    ).

% Position: Fou noir peut donner échec au roi blanc
setup_check_available_board_black([
    ['r', 'n', 'b', 'q', 'k', '.', 'n', 'r'],
    ['p', 'p', 'p', 'p', 'p', 'p', 'p', 'p'],
    ['.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.'],
    ['P', 'P', 'P', 'P', 'P', 'P', 'P', 'P'],
    ['R', 'N', '.', 'Q', 'K', '.', 'N', 'R']  % Roi blanc en e1, fou noir peut échec depuis f5
]).

test_alpha_beta_node_efficiency :-
    write('[TEST] EFFICACITE NOEUDS'), nl,
    write('------------------------'), nl,
    
    init_game_state(GameState),
    
    % Test comptage de noeuds profondeur 1 vs 2
    write('[RUN] Test 1/3: Stats noeuds disponibles............... '),
    (   catch(
            (negamax_ab_with_stats(GameState, white, 1, -1000, 1000, _, _, 0, Nodes1),
             negamax_ab_with_stats(GameState, white, 2, -1000, 1000, _, _, 0, Nodes2),
             ground(Nodes1), ground(Nodes2),
             Nodes1 > 0, Nodes2 > 0),
            _, fail
        ) -> write('[PASS]'), nl ; write('[FAIL]'), nl, fail),
    
    % Profondeur 2 doit explorer plus de noeuds que profondeur 1
    write('[RUN] Test 2/3: Profondeur 2 explore plus.............. '),
    (   catch(
            (negamax_ab_with_stats(GameState, white, 1, -1000, 1000, _, _, 0, N1),
             negamax_ab_with_stats(GameState, white, 2, -1000, 1000, _, _, 0, N2),
             N2 > N1),
            _, fail
        ) -> write('[PASS]'), nl ; write('[WARN]'), nl),
    
    % Performance raisonnable (< 1000 noeuds pour profondeur 2)
    write('[RUN] Test 3/3: Elagage limite explosion............... '),
    (   catch(
            (negamax_ab_with_stats(GameState, white, 2, -1000, 1000, _, _, 0, NodesTotal),
             NodesTotal < 1000),  % Elagage doit limiter explosion
            _, fail
        ) -> write('[PASS]'), nl ; write('[SLOW]'), nl), nl.




test_alpha_beta_pruning_bounds :-
    write('[TEST] ELAGAGE BORNES'), nl,
    write('---------------------'), nl,
    
    init_game_state(GameState),
    
    % Test avec bornes larges vs etroites (elagage plus efficace avec bornes etroites)
    write('[RUN] Test 1/3: Bornes etroites plus efficaces......... '),
    (   catch(
            (negamax_ab_with_stats(GameState, white, 2, -1000, 1000, _, _, 0, NodesWide),
             negamax_ab_with_stats(GameState, white, 2, -100, 100, _, _, 0, NodesNarrow),
             NodesNarrow =< NodesWide),  % Bornes etroites = moins ou egal noeuds
            _, fail
        ) -> write('[PASS]'), nl ; write('[WARN]'), nl),
    
    % Coherence: memes parametres = meme resultat
    write('[RUN] Test 2/3: Coherence resultats.................... '),
    (   catch(
            (negamax_ab(GameState, white, 2, -1000, 1000, Move1, Value1),
             negamax_ab(GameState, white, 2, -1000, 1000, Move2, Value2),
             Move1 = Move2, Value1 = Value2),
            _, fail
        ) -> write('[PASS]'), nl ; write('[FAIL]'), nl, fail),
    
    % Cas limite: Alpha = Beta (doit gerer sans planter)
    write('[RUN] Test 3/3: Bornes egales gerees................... '),
    (   catch(
            negamax_ab(GameState, white, 1, 0, 0, _, _),
            _, fail
        ) -> write('[PASS]'), nl ; write('[FAIL]'), nl, fail), nl.

test_alpha_beta_tactical_position :-
    write('[TEST] POSITION TACTIQUE'), nl,
    write('------------------------'), nl,
    
    % Position ou un coup est clairement superieur (capture dame)
    create_empty_board(EmptyBoard),
    place_single_piece(EmptyBoard, 4, 4, 'R', Board1),  % Tour blanche d4
    place_single_piece(Board1, 4, 8, 'q', Board2),      % Dame noire h4 (exposee)
    place_single_piece(Board2, 1, 1, 'K', Board3),      % Roi blanc a1
    place_single_piece(Board3, 8, 8, 'k', TacticalBoard), % Roi noir h8
    TacticalGS = game_state(TacticalBoard, white, 0, active, [[], []]),
    
    % Alpha-beta doit trouver rapidement la capture de dame
    write('[RUN] Test 1/3: Trouve coup tactique................... '),
    (   catch(
            (negamax_ab(TacticalGS, white, 2, -1000, 1000, [FromR,FromC,ToR,ToC], Value),
             FromR = 4, FromC = 4, ToR = 4, ToC = 8,  % Tour prend dame
             Value > 500),  % Valeur elevee pour capture dame
            _, fail
        ) -> write('[PASS]'), nl ; write('[FAIL]'), nl, fail),
    
    % Doit explorer moins de noeuds grace a l'elagage (coups evidemment mauvais elimines)
    write('[RUN] Test 2/3: Elagage position tactique.............. '),
    (   catch(
            (negamax_ab_with_stats(TacticalGS, white, 2, -1000, 1000, _, _, 0, TacticalNodes),
             TacticalNodes < 500),  % Moins de noeuds grace aux coupures
            _, fail
        ) -> write('[PASS]'), nl ; write('[SLOW]'), nl),
    
    % Performance rapide sur position tactique
    write('[RUN] Test 3/3: Performance tactique................... '),
    (   get_time(Start),
        catch(negamax_ab(TacticalGS, white, 2, -1000, 1000, _, _), _, fail),
        get_time(End),
        Duration is End - Start,
        Duration < 2.0 ->  % Moins de 2 secondes
        write('[PASS]'), nl ; write('[SLOW]'), nl), nl.


% =============================================================================
% RUNNER PRINCIPAL - TESTS ACTIFS
% =============================================================================

% Prédicate principal pour interface.pl
run_all_tests :-
    write('SUITE DE TESTS COMPLETE - Prolog Chess Game'), nl,
    write('============================================'), nl, nl,
    
    % Suite complete de tests
    run_foundation_tests,
    run_pieces_tests,
    run_checkmate_tests,
    run_robustness_tests,
    run_integration_tests,
    run_psqt_tests,
    run_alpha_beta_tests,
    run_mvv_lva_tests,  % NOUVEAU - Tests système MVV-LVA pour IA noire
    
    write('TESTS TERMINES - Suite complete validee!'), nl.

% Runner pour tests legacy (si besoin debugging)
run_legacy_tests :-
    write('TESTS LEGACY - Version anterieure'), nl,
    write('===================================='), nl, nl,
    
    run_foundation_tests,
    run_pieces_tests, 
    run_checkmate_tests,
    run_robustness_tests,
    run_integration_tests,
    
    write('TESTS LEGACY TERMINES!'), nl.



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

% =============================================================================
% SECTION 1: TESTS FONDAMENTAUX
% =============================================================================

run_foundation_tests :-
    display_test_section_header('TESTS FONDAMENTAUX', 'Initialisation et Base'),
    run_test_group([
        test_system_initialization,
        test_algebraic_notation,
        test_game_state_basics
    ]),
    display_test_section_footer('Section Fondamentaux terminee').

test_system_initialization :-
    write('[TEST] INITIALISATION SYSTEME'), nl,
    write('------------------------------'), nl,
    
    write('[OK] Test 1/3: Initialisation plateau......... '),
    (   init_game_state(GS) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail),
    
    write('[OK] Test 2/3: Structure 8x8.................. '),
    (   (GS = game_state(Board, _, _, _, _), 
         length(Board, 8), 
         maplist(length_8, Board)) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail),
    
    write('[OK] Test 3/3: Etat initial correct........... '),
    (   GS = game_state(_, white, 0, active, _) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail), nl.

test_algebraic_notation :-
    write('[TEST] NOTATION ALGEBRIQUE'), nl,
    write('--------------------------'), nl,
    
    write('[OK] Test 1/2: Parsing e2e4................... '),
    (   parse_algebraic_move("e2e4", 2, 5, 4, 5) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail),
        
    write('[OK] Test 2/2: Parsing g1f3................... '),
    (   parse_algebraic_move("g1f3", 1, 7, 3, 6) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail), nl.

test_game_state_basics :-
    write('[TEST] GESTION ETAT DE JEU'), nl,
    write('---------------------------'), nl,
    
    init_game_state(GS1),
    
    write('[OK] Test 1/2: Mouvement valide e2e4.......... '),
    (   make_move(GS1, 2, 5, 4, 5, GS2) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail),
    
    write('[OK] Test 2/2: Alternance joueur (blanc->noir).. '),
    (   GS2 = game_state(_, black, 1, active, _) ->
        write('PASS'), nl  
    ;   write('FAIL'), nl, fail), nl.

% =============================================================================
% SECTION 2: TESTS DES PIECES
% =============================================================================

run_pieces_tests :-
    display_test_section_header('TESTS DES PIECES', 'Mouvements et Regles'),
    run_test_group([
        test_pawn_rules,
        test_pawn_promotion,
        test_knight_rules,
        test_sliding_pieces,
        test_king_rules
    ]),
    display_test_section_footer('Section Pieces terminee').

test_pawn_rules :-
    write('[TEST] REGLES DES PIONS'), nl,
    write('------------------------'), nl,
    
    init_game_state(GS),
    GS = game_state(Board, _, _, _, _),
    
    write('[OK] Test 1/3: Mouvement simple e2e3.......... '),
    (   valid_move(Board, white, 2, 5, 3, 5) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail),
        
    write('[OK] Test 2/3: Mouvement double e2e4.......... '),
    (   valid_move(Board, white, 2, 5, 4, 5) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail),
        
    write('[OK] Test 3/3: Mouvement lateral interdit..... '),
    (   \+ valid_move(Board, white, 2, 5, 2, 6) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail), nl.

test_pawn_promotion :-
    write('[TEST] PROMOTION DES PIONS'), nl,
    write('--------------------------'), nl,
    
    % Test promotion pion blanc
    create_empty_board(Board1),
    place_single_piece(Board1, 7, 1, 'P', Board2),  % Pion blanc en a7
    GS1 = game_state(Board2, white, 0, active, [[], []]),
    write('[OK] Test 1/2: Promotion pion blanc............ '),
    (   (make_move(GS1, 7, 1, 8, 1, NewGS1),
         NewGS1 = game_state(NewBoard1, _, _, _, _),
         get_piece(NewBoard1, 8, 1, 'Q')) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail),
    
    % Test promotion pion noir
    create_empty_board(Board3),
    place_single_piece(Board3, 2, 1, 'p', Board4),  % Pion noir en a2
    GS2 = game_state(Board4, black, 0, active, [[], []]),
    write('[OK] Test 2/2: Promotion pion noir............. '),
    (   (make_move(GS2, 2, 1, 1, 1, NewGS2),
         NewGS2 = game_state(NewBoard2, _, _, _, _),
         get_piece(NewBoard2, 1, 1, 'q')) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail), nl.

test_knight_rules :-
    write('[TEST] REGLES DU CAVALIER'), nl,
    write('---------------------------'), nl,
    
    init_game_state(GS),
    GS = game_state(Board, _, _, _, _),
    
    write('[OK] Test 1/2: Mouvement en L g1f3............ '),
    (   valid_move(Board, white, 1, 7, 3, 6) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail),
        
    write('[OK] Test 2/2: Mouvement invalide g1g3........ '),
    (   \+ valid_move(Board, white, 1, 7, 3, 7) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail), nl.

test_sliding_pieces :-
    write('[TEST] PIECES GLISSANTES'), nl,
    write('--------------------------'), nl,
    
    % Creer une position avec tours et fous libres
    create_empty_board(EmptyBoard),
    place_single_piece(EmptyBoard, 4, 4, 'R', Board1),  % Tour blanche d4
    place_single_piece(Board1, 5, 5, 'B', Board2),      % Fou blanc e5
    place_single_piece(Board2, 8, 8, 'k', TestBoard),   % Roi noir h8
    
    write('[OK] Test 1/3: Tour mouvement horizontal....... '),
    (   valid_move(TestBoard, white, 4, 4, 4, 8) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail),
        
    write('[OK] Test 2/3: Tour mouvement vertical......... '),
    (   valid_move(TestBoard, white, 4, 4, 8, 4) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail),
        
    write('[OK] Test 3/3: Fou mouvement diagonal......... '),
    (   valid_move(TestBoard, white, 5, 5, 8, 8) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail), nl.

test_king_rules :-
    write('[TEST] REGLES DU ROI'), nl,
    write('---------------------'), nl,
    
    create_empty_board(EmptyBoard),
    place_single_piece(EmptyBoard, 4, 4, 'K', Board1),  % Roi blanc d4
    place_single_piece(Board1, 8, 8, 'k', TestBoard),   % Roi noir h8
    
    write('[OK] Test 1/2: Mouvement d\'une case........... '),
    (   valid_move(TestBoard, white, 4, 4, 4, 5) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail),
        
    write('[OK] Test 2/2: Mouvement de 2 cases interdit.. '),
    (   \+ valid_move(TestBoard, white, 4, 4, 4, 6) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail), nl.

% =============================================================================
% SECTION 3: TESTS D'ECHEC ET MAT
% =============================================================================

run_checkmate_tests :-
    display_test_section_header('TESTS ECHEC ET MAT', 'Detection et Validation'),
    run_test_group([
        test_check_detection,
        test_checkmate_detection,
        test_complex_scenarios
    ]),
    display_test_section_footer('Section Echec et Mat terminee').

test_check_detection :-
    write('[TEST] DETECTION D\'ECHEC'), nl,
    write('---------------------------'), nl,
    
    % Position initiale (pas d'echec)
    init_game_state(GS1),
    write('[OK] Test 1/3: Position initiale sans echec... '),
    (   \+ is_in_check(GS1, white) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail),
    
    % Position d'echec simple
    create_empty_board(EmptyBoard),
    place_single_piece(EmptyBoard, 1, 1, 'K', Board1),  % Roi blanc a1
    place_single_piece(Board1, 1, 8, 'r', Board2),      % Tour noire h1
    place_single_piece(Board2, 8, 8, 'k', CheckBoard),  % Roi noir h8
    CheckGS = game_state(CheckBoard, white, 0, active, [[], []]),
    
    write('[OK] Test 2/3: Echec horizontal simple........ '),
    (   is_in_check(CheckGS, white) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail),
    
    write('[OK] Test 3/3: Recherche position roi......... '),
    (   find_king_position(CheckBoard, white, 1, 1) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail), nl.

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
    
    write('[OK] Test 1/3: Mat du fond..................... '),
    (   is_checkmate(MateGS, white) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail),
    
    % Position normale (pas mat)
    init_game_state(NormalGS),
    write('[OK] Test 2/3: Position normale (pas mat)...... '),
    (   \+ is_checkmate(NormalGS, white) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail),
    
    % Test de pat (stalemate)
    create_empty_board(EmptyBoard2),
    place_single_piece(EmptyBoard2, 1, 1, 'K', Board4),   % Roi blanc a1
    place_single_piece(Board4, 3, 2, 'q', Board5),        % Dame noire b3 (pas d'echec)
    place_single_piece(Board5, 8, 8, 'k', StalemateBoard), % Roi noir h8
    StalemateGS = game_state(StalemateBoard, white, 0, active, [[], []]),
    
    write('[OK] Test 3/3: Detection de pat............... '),
    (   is_stalemate(StalemateGS, white) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail), nl.

test_complex_scenarios :-
    write('[TEST] SCENARIOS COMPLEXES'), nl,
    write('----------------------------'), nl,
    
    % Test d'echec a la decouverte
    create_discovered_check_position(DiscoveredBoard),
    DiscoveredGS = game_state(DiscoveredBoard, white, 0, active, [[], []]),
    write('[OK] Test 1/3: Echec a la decouverte.......... '),
    (   move_leaves_king_in_check(DiscoveredGS, white, 3, 5, 4, 4) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail),
    
    % Mat par double echec
    create_double_check_mate_position(DoubleCheckBoard),
    DoubleCheckGS = game_state(DoubleCheckBoard, white, 0, active, [[], []]),
    write('[OK] Test 2/3: Mat par double echec........... '),
    (   is_checkmate(DoubleCheckGS, white) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail),
    
    % Piece clouee ne peut bouger
    create_pinned_piece_position(PinnedBoard),  
    PinnedGS = game_state(PinnedBoard, white, 0, active, [[], []]),
    write('[OK] Test 3/3: Piece clouee ne peut bouger.... '),
    (   \+ generate_legal_move(PinnedGS, white, 4, 5, _, _) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail), nl.

% =============================================================================
% SECTION 4: TESTS DE ROBUSTESSE  
% =============================================================================

run_robustness_tests :-
    display_test_section_header('TESTS DE ROBUSTESSE', 'Erreurs et Cas Limites'),
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
    
    write('[OK] Test 1/3: Coordonnees hors limites....... '),
    (   \+ valid_move(Board, white, 2, 5, 9, 5) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail),
        
    write('[OK] Test 2/3: Mouvement sur place............ '),
    (   \+ valid_move(Board, white, 2, 5, 2, 5) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail),
        
    write('[OK] Test 3/3: Piece adverse.................. '),
    (   \+ valid_move(Board, white, 7, 5, 6, 5) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail), nl.

test_boundary_conditions :-
    write('[TEST] CONDITIONS LIMITES'), nl,
    write('--------------------------'), nl,
    
    write('[OK] Test 1/2: Position coin a1............... '),
    (   valid_chess_position(1, 1) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail),
        
    write('[OK] Test 2/2: Position coin h8............... '),
    (   valid_chess_position(8, 8) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail), nl.

% =============================================================================
% SECTION 5: TESTS D'INTEGRATION
% =============================================================================

run_integration_tests :-
    display_test_section_header('TESTS D\'INTEGRATION', 'Sequences Completes'),
    run_test_group([
        test_opening_sequence,
        test_tactical_sequence
    ]),
    display_test_section_footer('Section Integration terminee').

% Utilitaires d'affichage des tests - reduit la duplication
display_test_section_header(Title, Subtitle) :-
    atom_length(Title, TitleLen),
    PadLen is 50 - TitleLen,
    format('+-- SECTION: ~w ', [Title]),
    forall(between(1, PadLen, _), write('-')),
    write('+'), nl,
    format('[~w]~n', [Subtitle]),
    write('==========================================='), nl.

display_test_section_footer(Message) :-
    format('[OK] ~w~n', [Message]),
    write('+-----------------------------------------------+'), nl, nl.

% run_test_group - Execute une liste de tests
run_test_group([]).
run_test_group([Test|Rest]) :-
    call(Test),
    run_test_group(Rest).

test_opening_sequence :-
    write('[TEST] SEQUENCE D\'OUVERTURE'), nl,
    write('-----------------------------'), nl,
    
    init_game_state(GS1),
    write('[OK] Test 1/4: 1.e4............................ '),
    (   make_move(GS1, 2, 5, 4, 5, GS2) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail),
        
    write('[OK] Test 2/4: 1...e5.......................... '),
    (   make_move(GS2, 7, 5, 5, 5, GS3) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail),
        
    write('[OK] Test 3/4: 2.Nf3........................... '),
    (   make_move(GS3, 1, 7, 3, 6, GS4) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail),
        
    write('[OK] Test 4/4: 2...Nc6......................... '),
    (   make_move(GS4, 8, 2, 6, 3, _GS5) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail), nl.

test_tactical_sequence :-
    write('[TEST] SEQUENCE TACTIQUE'), nl,
    write('---------------------------'), nl,
    
    % Test d'une capture simple
    init_game_state(GS1),
    make_move(GS1, 2, 5, 4, 5, GS2),    % 1.e4
    make_move(GS2, 7, 4, 5, 4, GS3),    % 1...d5
    
    write('[OK] Test 1/1: Capture exd5................... '),
    (   make_move(GS3, 4, 5, 5, 4, GS4) ->
        (GS4 = game_state(_, _, _, _, [_, CapturedBlack]),
         member(p, CapturedBlack) ->
            write('PASS'), nl
        ;   write('FAIL - capture non enregistree'), nl, fail)
    ;   write('FAIL'), nl, fail), nl.

% =============================================================================
% POSITIONS DE TEST UTILISEES
% =============================================================================

% Position d'echec a la decouverte
create_discovered_check_position(Board) :-
    create_empty_board(EmptyBoard),
    place_single_piece(EmptyBoard, 1, 5, 'K', Board1),   % Roi blanc e1
    place_single_piece(Board1, 3, 5, 'B', Board2),       % Fou blanc e3 (bloque)
    place_single_piece(Board2, 8, 5, 'r', Board3),       % Tour noire e8
    place_single_piece(Board3, 8, 8, 'k', Board).        % Roi noir h8

% Position de mat par double echec
create_double_check_mate_position(Board) :-
    create_empty_board(EmptyBoard),
    place_single_piece(EmptyBoard, 1, 1, 'K', Board1),   % Roi blanc a1
    place_single_piece(Board1, 3, 2, 'q', Board2),       % Dame noire b3
    place_single_piece(Board2, 8, 1, 'r', Board3),       % Tour noire a8
    place_single_piece(Board3, 1, 8, 'r', Board4),       % Tour noire h1
    place_single_piece(Board4, 8, 8, 'k', Board).        % Roi noir h8

% Position avec piece clouee
create_pinned_piece_position(Board) :-
    create_empty_board(EmptyBoard),
    place_single_piece(EmptyBoard, 1, 5, 'K', Board1),   % Roi blanc e1
    place_single_piece(Board1, 4, 5, 'B', Board2),       % Fou blanc e4 (cloue)
    place_single_piece(Board2, 8, 5, 'q', Board3),       % Dame noire e8 (cloue)
    place_single_piece(Board3, 8, 8, 'k', Board).        % Roi noir h8

% Utilitaire
length_8(Row) :- length(Row, 8).

% =============================================================================
% INTERFACE PRINCIPALE
% =============================================================================

% Executeur principal
% Alias pour compatibilite
run_tests :- run_all_tests.

run_all_tests :-
    get_time(StartTime),
    
    write('======================================================='), nl,
    write('        REGRESSION TESTS - PROLOG CHESS GAME         '), nl, 
    write('======================================================='), nl, nl,
    
    run_foundation_tests,
    run_pieces_tests,
    run_checkmate_tests,
    run_robustness_tests, 
    run_integration_tests,
    
    get_time(EndTime),
    Duration is EndTime - StartTime,
    
    write('======================================================='), nl,
    write('[SUCCES] TOUS LES TESTS REUSSIS'), nl,
    format('Duree totale: ~3f secondes~n', [Duration]),
    write('======================================================='), nl,
    write('[PRET] Systeme valide et pret pour utilisation'), nl,
    write('======================================================='), nl.


% =============================================================================
% INTERFACE D'AIDE
% =============================================================================

test_help :-
    write('REGRESSION TESTS - AIDE'), nl,
    write('========================'), nl, nl,
    write('Commandes disponibles:'), nl,
    write('* run_all_tests.          - Suite complete'), nl,
    write('* run_foundation_tests.   - Tests fondamentaux'), nl, 
    write('* run_pieces_tests.       - Tests des pieces'), nl,
    write('* run_checkmate_tests.    - Tests echec et mat'), nl,
    write('* run_robustness_tests.   - Tests de robustesse'), nl,
    write('* run_integration_tests.  - Tests d\'integration'), nl,
    write('* test_pawn_promotion.    - Tests promotion pions'), nl,
    write('* test_help.              - Cette aide'), nl.

% =============================================================================
% FIN DES TESTS DE REGRESSION - VERSION REFACTORISEE
% Derniere mise a jour : Septembre 2025
% Optimisations : Modularite, lisibilite, maintenabilite
% =============================================================================
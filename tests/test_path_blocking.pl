% =============================================================================
% TEST PATH BLOCKING FUNCTIONALITY
% =============================================================================
%
% Auteur : Patrick Patenaude
% Dernière mise à jour : Août 2025

:- [src/game_logic].

% Test path blocking functionality
test_path_blocking :-
    write('=== TESTING PATH BLOCKING ==='), nl,
    
    % Initialize game state
    init_game_state(GameState),
    GameState = game_state(Board, _, _, _),
    
    % Test 1: Path should be clear on empty board between unoccupied squares
    write('Test 1: Clear path on empty board...'), nl,
    create_empty_board(EmptyBoard),
    (is_path_clear(EmptyBoard, 1, 1, 1, 8) ->
        write('OK - Horizontal path clear'), nl
    ;   write('FAIL - Should be clear'), nl),
    
    % Test 2: Path blocked by existing pawn
    write('Test 2: Path blocked by existing piece...'), nl,
    (is_path_clear(Board, 1, 1, 1, 8) ->
        write('FAIL - Path should be blocked by pieces'), nl
    ;   write('OK - Path correctly blocked'), nl),
    
    % Test 3: Diagonal path clear
    write('Test 3: Diagonal path...'), nl,
    (is_path_clear(EmptyBoard, 1, 1, 3, 3) ->
        write('OK - Diagonal path clear'), nl
    ;   write('FAIL - Should be clear'), nl),
    
    % Test 4: Rook movement with blocking
    write('Test 4: Rook movement validation...'), nl,
    (can_rook_move(Board, 1, 1, 1, 8) ->
        write('FAIL - Rook should be blocked'), nl
    ;   write('OK - Rook correctly blocked'), nl),
    
    % Test 5: Rook movement clear path
    (can_rook_move(EmptyBoard, 1, 1, 1, 8) ->
        write('OK - Rook can move on clear path'), nl
    ;   write('FAIL - Rook should be able to move'), nl),
    
    write('Path blocking tests completed!'), nl, nl.

% Test integration with game
test_game_integration :-
    write('=== TESTING GAME INTEGRATION ==='), nl,
    
    init_game_state(GS1),
    
    % Move pawn e2-e4
    write('Moving pawn e2-e4...'), nl,
    make_move_algebraic(GS1, "e2e4", GS2),
    display_game_state(GS2),
    
    % Try to move rook through occupied space
    write('Trying to move rook a1-a3 (should be blocked by pawn)...'), nl,
    GS2 = game_state(Board2, _, _, _),
    (can_rook_move(Board2, 1, 1, 3, 1) ->
        write('FAIL - Rook should be blocked by pawn'), nl
    ;   write('OK - Rook correctly blocked'), nl),
    
    % Move pawn a2-a4 to clear path
    write('Moving pawn a2-a4 to clear path...'), nl,
    make_move_algebraic(GS2, "a7a5", GS3),
    make_move_algebraic(GS3, "a2a4", GS4),
    
    % Now rook should be able to move to a3
    write('Now trying rook a1-a3 (should work)...'), nl,
    GS4 = game_state(Board4, _, _, _),
    (can_rook_move(Board4, 1, 1, 3, 1) ->
        write('OK - Rook can move on clear path'), nl
    ;   write('FAIL - Rook should be able to move'), nl),
    
    write('Game integration tests completed!'), nl, nl.

% Main test runner
run_path_tests :-
    test_path_blocking,
    test_game_integration,
    write('ALL PATH BLOCKING TESTS COMPLETED!'), nl.

% Auto-run message
:- write('Path blocking test loaded! Run: run_path_tests.'), nl.
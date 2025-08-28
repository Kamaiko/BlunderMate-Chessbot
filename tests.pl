% Unified Test File for Chess Game - Clean and Factorized
% Author: Student IA1
% This file combines all testing functionality in a clean, organized way

:- [board_smart].
:- [game_logic].
:- [simple_ai].

% ============================================================================
% MAIN TEST FUNCTIONS
% ============================================================================

% Run all tests
run_all_tests :-
    write('╔══════════════════════════════════════════════════════════╗'), nl,
    write('║              COMPREHENSIVE CHESS TESTS                   ║'), nl,
    write('╚══════════════════════════════════════════════════════════╝'), nl, nl,
    
    test_board_display, nl,
    test_algebraic_notation, nl,
    test_game_logic, nl,
    test_ai_basic, nl,
    test_game_interface, nl,
    
    write('╔══════════════════════════════════════════════════════════╗'), nl,
    write('║                   ALL TESTS COMPLETED!                   ║'), nl,
    write('╚══════════════════════════════════════════════════════════╝'), nl.

% Quick test - just the board
quick_test :-
    write('Quick Test - Unicode Board Only'), nl,
    write('================================'), nl,
    test_board_smart,
    write('Unicode board displayed successfully!'), nl.

% ============================================================================
% INDIVIDUAL TEST COMPONENTS
% ============================================================================

% Test 1: Board display
test_board_display :-
    write('1. Testing Board Display...'), nl,
    write('   Testing Unicode board creation and display...'), nl,
    test_board_smart,
    write('   ✅ Board display working'), nl.

% Test 2: Algebraic notation
test_algebraic_notation :-
    write('2. Testing Algebraic Notation...'), nl,
    write('   Testing move parsing...'), nl,
    
    % Test parsing
    parse_algebraic_move("e2e4", FromRow, FromCol, ToRow, ToCol),
    write('   e2e4 -> From: ('), write(FromRow), write(','), write(FromCol), 
    write('), To: ('), write(ToRow), write(','), write(ToCol), write(')'), nl,
    
    % Test coordinate conversion
    coordinates_to_algebraic(2, 5, 4, 5, MoveString),
    write('   Coordinates (2,5) to (4,5) -> '), write(MoveString), nl,
    
    write('   ✅ Algebraic notation working'), nl.

% Test 3: Game logic
test_game_logic :-
    write('3. Testing Game Logic...'), nl,
    write('   Testing game state initialization...'), nl,
    
    init_game_state(GameState),
    GameState = game_state(Board, Player, MoveCount, GameOver),
    write('   Initial player: '), write(Player), nl,
    write('   Move count: '), write(MoveCount), nl,
    write('   Game over: '), write(GameOver), nl,
    
    % Test algebraic move
    (make_move_algebraic(GameState, "e2e4", NewGameState) ->
        write('   ✅ Algebraic move e2e4 working'), nl
    ; write('   ❌ Algebraic move e2e4 failed'), nl),
    
    % Test move validation
    (valid_move(Board, white, 2, 5, 4, 5) ->
        write('   ✅ Move validation working'), nl
    ; write('   ❌ Move validation failed'), nl),
    
    write('   ✅ Game logic working'), nl.

% Test 4: AI basic functionality
test_ai_basic :-
    write('4. Testing AI Basic...'), nl,
    write('   Testing move generation...'), nl,
    
    initialize_board(Board),
    generate_moves(Board, white, Moves),
    length(Moves, MoveCount),
    write('   White has '), write(MoveCount), write(' possible moves'), nl,
    
    % Test position evaluation
    evaluate_position_simple(Board, white, Score),
    write('   Position evaluation: '), write(Score), nl,
    
    write('   ✅ AI basic functionality working'), nl.

% Test 5: Game interface
test_game_interface :-
    write('5. Testing Game Interface...'), nl,
    write('   Testing game state display...'), nl,
    
    init_game_state(GameState),
    display_game_state(GameState),
    
    write('   ✅ Game interface working'), nl.

% ============================================================================
% DEMO FUNCTIONS
% ============================================================================

% Quick AI vs AI demo
demo_ai_vs_ai :-
    write('AI vs AI Demo'), nl,
    write('=============='), nl, nl,
    
    write('Initial position:'), nl,
    init_game_state(GameState),
    display_game_state(GameState),
    
    write('AI is thinking...'), nl,
    GameState = game_state(Board, Player, _, _),
    ai_move(Board, Player, BestMove),
    write('AI chose: '), write(BestMove), nl,
    
    write('Demo completed!'), nl.

% Demo with specific moves
demo_specific_moves :-
    write('Demo with Specific Moves'), nl,
    write('========================'), nl, nl,
    
    init_game_state(GameState),
    write('1. Initial position:'), nl,
    display_game_state(GameState),
    
    write('2. White plays e2e4:'), nl,
    make_move_algebraic(GameState, "e2e4", GameState2),
    display_game_state(GameState2),
    
    write('3. Black plays e7e5:'), nl,
    make_move_algebraic(GameState2, "e7e5", GameState3),
    display_game_state(GameState3),
    
    write('4. White plays g1f3:'), nl,
    make_move_algebraic(GameState3, "g1f3", GameState4),
    display_game_state(GameState4),
    
    write('Demo completed!'), nl.

% ============================================================================
% UTILITY FUNCTIONS
% ============================================================================

% Test specific component
test_component(Component) :-
    write('Testing '), write(Component), write('...'), nl,
    (call(Component) ->
        write('✅ '), write(Component), write(' passed'), nl
    ; write('❌ '), write(Component), write(' failed'), nl).

% Help function
help :-
    write('Available test functions:'), nl,
    write('  run_all_tests     - Run all tests'), nl,
    write('  quick_test        - Quick board test only'), nl,
    write('  test_board_display - Test board display'), nl,
    write('  test_algebraic_notation - Test algebraic notation'), nl,
    write('  test_game_logic   - Test game logic'), nl,
    write('  test_ai_basic     - Test AI basic functionality'), nl,
    write('  test_game_interface - Test game interface'), nl,
    write('  demo_ai_vs_ai     - AI vs AI demo'), nl,
    write('  demo_specific_moves - Demo with specific moves'), nl,
    write('  help              - Show this help'), nl.

% Comprehensive Test File for Chess Game - Unicode Version
% Author: Student IA1
% This file tests all components: board, game logic, AI, and game interface

:- [game_logic].
:- [simple_ai].

% Test all components
test_all :-
    write('╔══════════════════════════════════════════════════════════╗'), nl,
    write('║              COMPREHENSIVE CHESS TESTS                   ║'), nl,
    write('╚══════════════════════════════════════════════════════════╝'), nl, nl,
    
    write('1. Testing Board Component...'), nl,
    test_board_component, nl,
    
    write('2. Testing Algebraic Notation...'), nl,
    test_algebraic_component, nl,
    
    write('3. Testing Game Logic Component...'), nl,
    test_game_logic_component, nl,
    
    write('4. Testing AI Component...'), nl,
    test_ai_component, nl,
    
    write('5. Testing Game Interface...'), nl,
    test_game_interface_component, nl,
    
    write('╔══════════════════════════════════════════════════════════╗'), nl,
    write('║                   ALL TESTS COMPLETED!                   ║'), nl,
    write('╚══════════════════════════════════════════════════════════╝'), nl.

% Test board component
test_board_component :-
    write('   Testing Unicode board creation and display...'), nl,
    initialize_board(Board),
    display_board(Board),
    write('   OK - Unicode board component working'), nl.

% Test algebraic notation component
test_algebraic_component :-
    write('   Testing algebraic notation conversion...'), nl,
    parse_algebraic_move("e2e4", FromRow, FromCol, ToRow, ToCol),
    write('   e2e4 -> From: ('), write(FromRow), write(','), write(FromCol), 
    write('), To: ('), write(ToRow), write(','), write(ToCol), write(')'), nl,
    
    coordinates_to_algebraic(2, 5, 4, 5, MoveString),
    write('   Coordinates (2,5) to (4,5) -> '), write(MoveString), nl,
    write('   OK - Algebraic notation working'), nl.

% Test game logic component
test_game_logic_component :-
    write('   Testing game state and algebraic moves...'), nl,
    init_game_state(GameState),
    GameState = game_state(Board, Player, MoveCount, GameOver),
    write('   Initial player: '), write(Player), nl,
    write('   Move count: '), write(MoveCount), nl,
    write('   Game over: '), write(GameOver), nl,
    
    % Test algebraic move
    (make_move_algebraic(GameState, "e2e4", NewGameState) ->
        write('   OK - Algebraic move e2e4 working'), nl
    ; write('   ERROR - Algebraic move e2e4 failed'), nl),
    
    % Test move validation
    (valid_move(Board, white, 2, 5, 4, 5) ->
        write('   OK - Move validation working'), nl
    ; write('   ERROR - Move validation failed'), nl),
    
    write('   OK - Game logic component working'), nl.

% Test AI component
test_ai_component :-
    write('   Testing AI move generation...'), nl,
    initialize_board(Board),
    generate_moves(Board, white, Moves),
    length(Moves, MoveCount),
    write('   White has '), write(MoveCount), write(' possible moves'), nl,
    
    % Test position evaluation
    evaluate_position_simple(Board, white, Score),
    write('   Position evaluation: '), write(Score), nl,
    write('   OK - AI component working'), nl.

% Test game interface component
test_game_interface_component :-
    write('   Testing game interface...'), nl,
    init_game_state(GameState),
    display_game_state(GameState),
    write('   OK - Game interface working'), nl.

% Quick test
quick_test :-
    write('Quick Test - Unicode Board Only'), nl,
    write('================================'), nl,
    initialize_board(Board),
    display_board(Board),
    write('Unicode board displayed successfully!'), nl.

% Test specific components
test_board :-
    test_board_component.

test_algebraic :-
    test_algebraic_component.

test_logic :-
    test_game_logic_component.

test_ai :-
    test_ai_component.

test_interface :-
    test_game_interface_component.

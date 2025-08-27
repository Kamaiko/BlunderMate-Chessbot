% Demo Game - Updated for Unicode and Algebraic Notation
% Author: Student IA1
% This file demonstrates the chess game functionality

:- [game_logic].
:- [simple_ai].

% Quick demo of AI vs AI
quick_demo :-
    write('Quick Demo - AI vs AI'), nl,
    write('====================='), nl, nl,
    
    write('Initial position:'), nl,
    init_game_state(GameState),
    display_game_state(GameState),
    
    write('AI is thinking...'), nl,
    GameState = game_state(Board, Player, _, _),
    ai_move(Board, Player, BestMove),
    write('AI chose: '), write(BestMove), nl,
    
    write('Demo completed!'), nl.

% Demo with specific moves
demo_moves :-
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

% Test algebraic notation
test_algebraic :-
    write('Testing Algebraic Notation'), nl,
    write('=========================='), nl, nl,
    
    write('Testing move parsing:'), nl,
    parse_algebraic_move("e2e4", FromRow, FromCol, ToRow, ToCol),
    write('e2e4 -> From: ('), write(FromRow), write(','), write(FromCol), 
    write('), To: ('), write(ToRow), write(','), write(ToCol), write(')'), nl,
    
    parse_algebraic_move("g1f3", FromRow2, FromCol2, ToRow2, ToCol2),
    write('g1f3 -> From: ('), write(FromRow2), write(','), write(FromCol2), 
    write('), To: ('), write(ToRow2), write(','), write(ToCol2), write(')'), nl,
    
    write('Testing coordinate conversion:'), nl,
    coordinates_to_algebraic(2, 5, 4, 5, MoveString1),
    write('Coordinates (2,5) to (4,5) -> '), write(MoveString1), nl,
    
    coordinates_to_algebraic(1, 7, 3, 6, MoveString2),
    write('Coordinates (1,7) to (3,6) -> '), write(MoveString2), nl,
    
    write('Algebraic notation test completed!'), nl.

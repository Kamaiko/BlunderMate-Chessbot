% Simple Chess Game - Updated for Unicode and Algebraic Notation
% Author: Student IA1
% This file provides the main game interface

:- [game_logic].
:- [simple_ai].

% Main game loop
play_chess :-
    write('╔══════════════════════════════════════════════════════════╗'), nl,
    write('║                    CHESS GAME vs AI                      ║'), nl,
    write('╚══════════════════════════════════════════════════════════╝'), nl,
    write('You play as WHITE, AI plays as BLACK'), nl,
    write('Enter moves in algebraic notation (e.g., e2e4, g1f3)'), nl,
    write('Type "quit" to exit'), nl,
    write('╔══════════════════════════════════════════════════════════╗'), nl, nl,

    init_game_state(GameState),
    game_loop(GameState).

% Game loop
game_loop(GameState) :-
    GameState = game_state(Board, Player, MoveCount, GameOver),

    (GameOver ->
        write('Game Over!'), nl,
        display_final_result(Board)
    ;
        display_game_state(GameState),
        (Player = white ->
            get_human_move_algebraic(Board, MoveString),
            (MoveString = "quit" ->
                write('Game ended by user.'), nl
            ;
                make_move_algebraic(GameState, MoveString, NewGameState),
                game_loop(NewGameState)
            )
        ;
            write('AI is thinking...'), nl,
            ai_move(Board, Player, BestMove),
            BestMove = move(FromRow, FromCol, ToRow, ToCol),
            write('AI plays: '),
            display_move(FromRow, FromCol, ToRow, ToCol), nl, nl,
            make_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState),
            game_loop(NewGameState)
        )
    ).

% Get human move in algebraic notation
get_human_move_algebraic(Board, MoveString) :-
    write('Your move (e.g., e2e4): '),
    read_line_to_codes(user_input, Codes),
    atom_codes(Atom, Codes),
    atom_string(Atom, MoveString),
    (MoveString = "quit" -> true
    ; validate_algebraic_move(MoveString) ->
        write('Move: '), write(MoveString), nl
    ;
        write('Invalid move format! Use: e2e4, g1f3, etc.'), nl,
        get_human_move_algebraic(Board, MoveString)
    ).

% Validate algebraic move format
validate_algebraic_move(MoveString) :-
    string_length(MoveString, 4),
    string_chars(MoveString, [Col1, Row1, Col2, Row2]),
    valid_col(Col1), valid_row(Row1),
    valid_col(Col2), valid_row(Row2).

valid_col(Col) :- member(Col, ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']).
valid_row(Row) :- member(Row, ['1', '2', '3', '4', '5', '6', '7', '8']).

% Display game state
display_game_state(GameState) :-
    GameState = game_state(Board, Player, MoveCount, GameOver),
    display_board(Board),
    write('Current Player: '), write(Player), nl,
    write('Move Count: '), write(MoveCount), nl,
    write('Game Over: '), write(GameOver), nl, nl.

% Display final result
display_final_result(Board) :-
    display_board(Board),
    write('Final position displayed above.'), nl.

% Test the game
test_game :-
    write('Testing Simple Chess Game'), nl,
    write('=========================='), nl, nl,
    
    write('1. Testing game initialization...'), nl,
    init_game_state(GameState),
    display_game_state(GameState),
    
    write('2. Testing move execution...'), nl,
    make_move_algebraic(GameState, "e2e4", NewGameState),
    write('After pawn move:'), nl,
    display_game_state(NewGameState),
    
    write('3. Testing AI move...'), nl,
    NewGameState = game_state(Board, Player, _, _),
    ai_move(Board, Player, BestMove),
    write('AI chose: '), write(BestMove), nl,
    
    write('OK - Game tests completed!'), nl.

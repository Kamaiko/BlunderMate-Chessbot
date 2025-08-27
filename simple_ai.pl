% Simple AI for Chess Game - Updated for Unicode
% Author: Student IA1
% This file implements a simple one-ply lookahead AI

:- [game_logic].

% Smart AI that evaluates moves simply
smart_ai_move(Board, Player, BestMove) :-
    generate_moves(Board, Player, Moves),
    Moves \= [],
    evaluate_moves_simple(Board, Player, Moves, BestMove).

% Evaluate moves by looking one move ahead
evaluate_moves_simple(Board, Player, [Move], Move).
evaluate_moves_simple(Board, Player, [Move|Moves], BestMove) :-
    evaluate_moves_simple(Board, Player, Moves, RestBestMove),
    evaluate_move_simple(Board, Player, Move, MoveScore),
    evaluate_move_simple(Board, Player, RestBestMove, RestScore),
    (Player = white ->
        % White maximizes
        (MoveScore >= RestScore ->
            BestMove = Move
        ; BestMove = RestBestMove)
    ; % Black minimizes
        (MoveScore =< RestScore ->
            BestMove = Move
        ; BestMove = RestBestMove)
    ).

% Simple move evaluation: material gain + position
evaluate_move_simple(Board, Player, Move, Score) :-
    Move = move(FromRow, FromCol, ToRow, ToCol),
    execute_move(Board, FromRow, FromCol, ToRow, ToCol, NewBoard),
    evaluate_position_simple(NewBoard, Player, Score).

% Simple position evaluation
evaluate_position_simple(Board, Player, Score) :-
    count_pieces(Board, Player, PlayerPieces),
    (Player = white -> Opponent = black ; Opponent = white),
    count_pieces(Board, Opponent, OpponentPieces),
    Score is PlayerPieces - OpponentPieces.

% AI move function
ai_move(Board, Player, BestMove) :-
    write('AI is thinking...'), nl,
    smart_ai_move(Board, Player, BestMove),
    BestMove = move(FromRow, FromCol, ToRow, ToCol),
    coordinates_to_algebraic(FromRow, FromCol, ToRow, ToCol, MoveString),
    write('AI chooses: '), write(MoveString), nl.

% Display move in algebraic notation
display_move(FromRow, FromCol, ToRow, ToCol) :-
    coordinates_to_algebraic(FromRow, FromCol, ToRow, ToCol, MoveString),
    write(MoveString).

% Convert column number to letter for display
col_to_letter(Col, Letter) :-
    (Col = 1 -> Letter = 'a'
    ; Col = 2 -> Letter = 'b'
    ; Col = 3 -> Letter = 'c'
    ; Col = 4 -> Letter = 'd'
    ; Col = 5 -> Letter = 'e'
    ; Col = 6 -> Letter = 'f'
    ; Col = 7 -> Letter = 'g'
    ; Col = 8 -> Letter = 'h').

% Test the simple AI
test_simple_ai :-
    write('Testing Simple AI'), nl,
    write('=================='), nl, nl,
    
    write('1. Creating board...'), nl,
    initialize_board(Board),
    display_board(Board),
    
    write('2. Testing move generation...'), nl,
    generate_moves(Board, white, Moves),
    length(Moves, MoveCount),
    write('White has '), write(MoveCount), write(' possible moves'), nl,
    
    write('3. Testing AI move selection...'), nl,
    ai_move(Board, white, BestMove),
    write('Best move found: '), write(BestMove), nl,
    
    write('4. Testing position evaluation...'), nl,
    evaluate_position_simple(Board, white, Score),
    write('Position score: '), write(Score), nl,
    
    write('OK - Simple AI tests completed!'), nl.

% Chess Game Logic - Updated for Unicode and Algebraic Notation
% Author: Student IA1
% This file contains the basic game rules and move generation

:- [board_smart].

% Game state representation
% game_state(Board, CurrentPlayer, MoveCount, GameOver)
% CurrentPlayer: 'white' or 'black'
% MoveCount: number of moves made
% GameOver: true if game is finished

% Initialize game state
init_game_state(GameState) :-
    initialize_board(Board),
    GameState = game_state(Board, white, 0, false).

% Get current player
current_player(game_state(_, Player, _, _), Player).

% Switch player
switch_player(white, black).
switch_player(black, white).

% Make a move using algebraic notation
make_move_algebraic(GameState, MoveString, NewGameState) :-
    parse_algebraic_move(MoveString, FromRow, FromCol, ToRow, ToCol),
    make_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState).

% Make a move using coordinates
make_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState) :-
    GameState = game_state(Board, Player, MoveCount, false),
    valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol),
    execute_move(Board, FromRow, FromCol, ToRow, ToCol, NewBoard),
    switch_player(Player, NewPlayer),
    NewMoveCount is MoveCount + 1,
    is_checkmate(NewBoard, NewPlayer, IsCheckmate),
    NewGameState = game_state(NewBoard, NewPlayer, NewMoveCount, IsCheckmate).

% Basic move validation
valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol) :-
    % Check bounds
    FromRow >= 1, FromRow =< 8,
    FromCol >= 1, FromCol =< 8,
    ToRow >= 1, ToRow =< 8,
    ToCol >= 1, ToCol =< 8,
    
    % Check if source has a piece
    get_piece(Board, FromRow, FromCol, Piece),
    (Player = white -> is_white_piece(Piece) ; is_black_piece(Piece)),
    
    % Check if destination is different
    (FromRow \= ToRow ; FromCol \= ToCol),
    
    % Check if destination is empty or enemy piece
    get_piece(Board, ToRow, ToCol, DestPiece),
    (DestPiece = ' ' ; 
     (Player = white -> is_black_piece(DestPiece) ; is_white_piece(DestPiece))).

% Execute move
execute_move(Board, FromRow, FromCol, ToRow, ToCol, NewBoard) :-
    get_piece(Board, FromRow, FromCol, Piece),
    place_single_piece(Board, FromRow, FromCol, ' ', Board1),
    place_single_piece(Board1, ToRow, ToCol, Piece, NewBoard).

% Check if piece is white (Unicode chess symbols)
is_white_piece(Piece) :-
    member(Piece, ['♙', '♖', '♘', '♗', '♕', '♔']).

% Check if piece is black (Unicode chess symbols)
is_black_piece(Piece) :-
    member(Piece, ['♟', '♜', '♞', '♝', '♛', '♚']).

% Check if square is empty
is_empty_square(Board, Row, Col) :-
    get_piece(Board, Row, Col, Piece),
    Piece = ' '.

% Check if square is occupied by enemy
is_enemy_square(Board, Row, Col, Player) :-
    get_piece(Board, Row, Col, Piece),
    (Player = white -> is_black_piece(Piece) ; is_white_piece(Piece)).

% Check if square is occupied by ally
is_ally_square(Board, Row, Col, Player) :-
    get_piece(Board, Row, Col, Piece),
    (Player = white -> is_white_piece(Piece) ; is_black_piece(Piece)).

% Check if square is free (empty or enemy)
is_free_square(Board, Row, Col, Player) :-
    is_empty_square(Board, Row, Col) ; is_enemy_square(Board, Row, Col, Player).

% Basic checkmate detection (simplified)
is_checkmate(Board, Player, false) :-
    % For now, just check if king is present
    find_king(Board, Player, _, _).
is_checkmate(_, _, true).

% Find king position
find_king(Board, Player, Row, Col) :-
    (Player = white -> King = '♔' ; King = '♚'),
    find_piece(Board, King, Row, Col).

% Find piece position
find_piece(Board, Piece, Row, Col) :-
    find_piece(Board, Piece, 1, 1, Row, Col).

find_piece(Board, Piece, Row, Col, Row, Col) :-
    get_piece(Board, Row, Col, Piece).

find_piece(Board, Piece, Row, Col, FoundRow, FoundCol) :-
    (Col < 8 ->
        NextCol is Col + 1,
        find_piece(Board, Piece, Row, NextCol, FoundRow, FoundCol)
    ; Row < 8 ->
        NextRow is Row + 1,
        find_piece(Board, Piece, NextRow, 1, FoundRow, FoundCol)
    ).

% Generate all valid moves for a player
generate_moves(Board, Player, Moves) :-
    findall(move(FromRow, FromCol, ToRow, ToCol),
            (between(1, 8, FromRow),
             between(1, 8, FromCol),
             between(1, 8, ToRow),
             between(1, 8, ToCol),
             valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol)),
            Moves).

% Count pieces for evaluation (Unicode chess symbols)
count_pieces(Board, Player, Count) :-
    (Player = white -> 
        Pieces = ['♙', '♖', '♘', '♗', '♕', '♔'] 
    ; 
        Pieces = ['♟', '♜', '♞', '♝', '♛', '♚']
    ),
    count_pieces_list(Board, Pieces, Count).

count_pieces_list(_, [], 0).
count_pieces_list(Board, [Piece|Pieces], Count) :-
    count_piece(Board, Piece, PieceCount),
    count_pieces_list(Board, Pieces, RestCount),
    Count is PieceCount + RestCount.

count_piece(Board, Piece, Count) :-
    findall(1, find_piece(Board, Piece, _, _), List),
    length(List, Count).

% Basic position evaluation
evaluate_position(Board, Player, Score) :-
    count_pieces(Board, Player, PlayerPieces),
    (Player = white -> Opponent = black ; Opponent = white),
    count_pieces(Board, Opponent, OpponentPieces),
    Score is PlayerPieces - OpponentPieces.

% Display game state
display_game_state(GameState) :-
    GameState = game_state(Board, Player, MoveCount, GameOver),
    display_board(Board),
    write('Current Player: '), write(Player), nl,
    write('Move Count: '), write(MoveCount), nl,
    write('Game Over: '), write(GameOver), nl, nl.

% Test game logic
test_game_logic :-
    write('Testing Game Logic with Unicode'), nl,
    write('================================'), nl, nl,
    
    write('1. Initializing game...'), nl,
    init_game_state(GameState),
    display_game_state(GameState),
    
    write('2. Testing algebraic move...'), nl,
    (make_move_algebraic(GameState, "e2e4", NewGameState) ->
        write('OK - Algebraic move e2e4 successful'), nl,
        display_game_state(NewGameState)
    ; write('ERROR - Algebraic move e2e4 failed'), nl),
    
    write('3. Testing piece counting...'), nl,
    GameState = game_state(Board, _, _, _),
    count_pieces(Board, white, WhiteCount),
    count_pieces(Board, black, BlackCount),
    write('White pieces: '), write(WhiteCount), nl,
    write('Black pieces: '), write(BlackCount), nl,
    
    write('4. Testing position evaluation...'), nl,
    evaluate_position(Board, white, Score),
    write('Position score for white: '), write(Score), nl,
    
    write('OK - Game logic tests completed!'), nl.

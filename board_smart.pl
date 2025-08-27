% Smart Chess Board - Auto-detects terminal capabilities
% Author: Student IA1
% This version automatically chooses the best display method

% Unicode chess symbols using put_code
% White pieces
white_pawn(9817).    % White pawn (♙)
white_rook(9814).    % White rook (♖)
white_knight(9816).  % White knight (♘)
white_bishop(9815).  % White bishop (♗)
white_queen(9813).   % White queen (♕)
white_king(9812).    % White king (♔)

% Black pieces
black_pawn(9823).    % Black pawn (♟)
black_rook(9820).    % Black rook (♜)
black_knight(9822).  % Black knight (♞)
black_bishop(9821).  % Black bishop (♝)
black_queen(9819).   % Black queen (♛)
black_king(9818).    % Black king (♚)

% ASCII fallback symbols
white_pawn_ascii('P').
white_rook_ascii('R').
white_knight_ascii('N').
white_bishop_ascii('B').
white_queen_ascii('Q').
white_king_ascii('K').

black_pawn_ascii('p').
black_rook_ascii('r').
black_knight_ascii('n').
black_bishop_ascii('b').
black_queen_ascii('q').
black_king_ascii('k').

% Empty squares
empty_square(' ').

% Create empty board
create_empty_board(Board) :-
    create_empty_board(8, [], Board).

create_empty_board(0, Board, Board).

create_empty_board(Row, Acc, Board) :-
    Row > 0,
    create_row(Row, RowList),
    NextRow is Row - 1,
    create_empty_board(NextRow, [RowList|Acc], Board).

create_row(Row, RowList) :-
    create_row_cells(Row, 1, [], RowList).

create_row_cells(_, 9, Row, Row).

create_row_cells(Row, Col, Acc, RowList) :-
    Col =< 8,
    append(Acc, [' '], NewAcc),
    NextCol is Col + 1,
    create_row_cells(Row, NextCol, NewAcc, RowList).

% Place pieces using Unicode codes
place_pieces(Board, NewBoard) :-
    place_white_pieces(Board, Board1),
    place_black_pieces(Board1, NewBoard).

place_white_pieces(Board, NewBoard) :-
    place_piece_row(Board, 2, 9817, Board1),  % White pawns
    place_single_piece(Board1, 1, 1, 9814, Board2),  % White rooks
    place_single_piece(Board2, 1, 8, 9814, Board3),
    place_single_piece(Board3, 1, 2, 9816, Board4),  % White knights
    place_single_piece(Board4, 1, 7, 9816, Board5),
    place_single_piece(Board5, 1, 3, 9815, Board6),  % White bishops
    place_single_piece(Board6, 1, 6, 9815, Board7),
    place_single_piece(Board7, 1, 4, 9813, Board8),  % White queen
    place_single_piece(Board8, 1, 5, 9812, NewBoard). % White king

place_black_pieces(Board, NewBoard) :-
    place_piece_row(Board, 7, 9823, Board1),  % Black pawns
    place_single_piece(Board1, 8, 1, 9820, Board2),  % Black rooks
    place_single_piece(Board2, 8, 8, 9820, Board3),
    place_single_piece(Board3, 8, 2, 9822, Board4),  % Black knights
    place_single_piece(Board4, 8, 7, 9822, Board5),
    place_single_piece(Board5, 8, 3, 9821, Board6),  % Black bishops
    place_single_piece(Board6, 8, 6, 9821, Board7),
    place_single_piece(Board7, 8, 4, 9819, Board8),  % Black queen
    place_single_piece(Board8, 8, 5, 9818, NewBoard). % Black king

place_piece_row(Board, Row, Piece, NewBoard) :-
    place_piece_row(Board, Row, 1, Piece, NewBoard).

place_piece_row(Board, Row, 9, Piece, Board).

place_piece_row(Board, Row, Col, Piece, NewBoard) :-
    Col =< 8,
    place_single_piece(Board, Row, Col, Piece, Board1),
    NextCol is Col + 1,
    place_piece_row(Board1, Row, NextCol, Piece, NewBoard).

place_single_piece(Board, Row, Col, Piece, NewBoard) :-
    nth1(Row, Board, RowList),
    replace_element(RowList, Col, Piece, NewRowList),
    replace_element(Board, Row, NewRowList, NewBoard).

% Utilities
replace_element([_|T], 1, Element, [Element|T]).
replace_element([H|T], Index, Element, [H|NewT]) :-
    Index > 1,
    NextIndex is Index - 1,
    replace_element(T, NextIndex, Element, NewT).

get_piece(Board, Row, Col, Piece) :-
    nth1(Row, Board, RowList),
    nth1(Col, RowList, Piece).

% Smart piece display - tries Unicode first, falls back to ASCII
display_piece_smart(Piece) :-
    (integer(Piece) ->
        % Try Unicode first
        (catch(put_code(Piece), _, 
            % If Unicode fails, use ASCII fallback
            display_piece_ascii(Piece))
        )
    ;
        write(Piece)
    ).

% ASCII fallback display
display_piece_ascii(9817) :- write('P').  % White pawn
display_piece_ascii(9814) :- write('R').  % White rook
display_piece_ascii(9816) :- write('N').  % White knight
display_piece_ascii(9815) :- write('B').  % White bishop
display_piece_ascii(9813) :- write('Q').  % White queen
display_piece_ascii(9812) :- write('K').  % White king

display_piece_ascii(9823) :- write('p').  % Black pawn
display_piece_ascii(9820) :- write('r').  % Black rook
display_piece_ascii(9822) :- write('n').  % Black knight
display_piece_ascii(9821) :- write('b').  % Black bishop
display_piece_ascii(9819) :- write('q').  % Black queen
display_piece_ascii(9818) :- write('k').  % Black king

display_piece_ascii(' ') :- write(' ').   % Empty square

% Algebraic notation conversion
parse_algebraic_move(MoveString, FromRow, FromCol, ToRow, ToCol) :-
    string_chars(MoveString, [FromColChar, FromRowChar, ToColChar, ToRowChar]),
    char_to_col(FromColChar, FromCol),
    char_to_row(FromRowChar, FromRow),
    char_to_col(ToColChar, ToCol),
    char_to_row(ToRowChar, ToRow).

% Convert column letters to numbers (a=1, b=2, ..., h=8)
char_to_col('a', 1).
char_to_col('b', 2).
char_to_col('c', 3).
char_to_col('d', 4).
char_to_col('e', 5).
char_to_col('f', 6).
char_to_col('g', 7).
char_to_col('h', 8).

% Convert row numbers to board coordinates (1=bottom, 8=top)
char_to_row('1', 1).
char_to_row('2', 2).
char_to_row('3', 3).
char_to_row('4', 4).
char_to_row('5', 5).
char_to_row('6', 6).
char_to_row('7', 7).
char_to_row('8', 8).

% Convert coordinates back to algebraic notation
coordinates_to_algebraic(FromRow, FromCol, ToRow, ToCol, MoveString) :-
    col_to_char(FromCol, FromColChar),
    row_to_char(FromRow, FromRowChar),
    col_to_char(ToCol, ToColChar),
    row_to_char(ToRow, ToRowChar),
    string_chars(MoveString, [FromColChar, FromRowChar, ToColChar, ToRowChar]).

col_to_char(1, 'a').
col_to_char(2, 'b').
col_to_char(3, 'c').
col_to_char(4, 'd').
col_to_char(5, 'e').
col_to_char(6, 'f').
col_to_char(7, 'g').
col_to_char(8, 'h').

row_to_char(1, '1').
row_to_char(2, '2').
row_to_char(3, '3').
row_to_char(4, '4').
row_to_char(5, '5').
row_to_char(6, '6').
row_to_char(7, '7').
row_to_char(8, '8').

% Smart display - automatically chooses best method
display_board_smart(Board) :-
    nl,
    write('Smart Chess Board - Auto-detecting terminal capabilities...'), nl,
    display_board_rows_smart(Board, 8),
    write('  a b c d e f g h'), nl, nl.

display_board_rows_smart([], _).
display_board_rows_smart([Row|Rows], RowNum) :-
    write(RowNum), write('['),
    display_row_smart(Row),
    write(']'), nl,
    NextRowNum is RowNum - 1,
    display_board_rows_smart(Rows, NextRowNum).

display_row_smart([]).
display_row_smart([Piece|Pieces]) :-
    display_piece_smart(Piece),
    (Pieces = [] -> true ; write(','), display_row_smart(Pieces)).

% Initialize
initialize_board(Board) :-
    create_empty_board(EmptyBoard),
    place_pieces(EmptyBoard, Board).

% Test
test_board_smart :-
    write('Testing Smart Chess Board...'), nl,
    initialize_board(Board),
    display_board_smart(Board),
    write('Smart chess board test completed!'), nl.

% Test algebraic notation
test_algebraic :-
    write('Testing algebraic notation...'), nl,
    parse_algebraic_move("e2e4", FromRow, FromCol, ToRow, ToCol),
    write('e2e4 -> From: ('), write(FromRow), write(','), write(FromCol), 
    write('), To: ('), write(ToRow), write(','), write(ToCol), write(')'), nl,
    
    coordinates_to_algebraic(2, 5, 4, 5, MoveString),
    write('Coordinates (2,5) to (4,5) -> '), write(MoveString), nl,
    write('Algebraic notation test completed!'), nl.

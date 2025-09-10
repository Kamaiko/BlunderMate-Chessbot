% Test du bug get_piece avec conversion 9-Row

:- consult('src/board.pl').

test_get_piece_bug :-
    Board = [
        ['r', 'n', 'b', 'q', 'k', ' ', ' ', 'r'],  % Élément 1: 8e rangée d'échecs
        ['p', 'p', ' ', ' ', ' ', 'p', 'p', 'p'],  % Élément 2: 7e rangée d'échecs
        [' ', ' ', 'p', 'B', 'p', 'n', ' ', ' '],  % Élément 3: 6e rangée d'échecs
        [' ', ' ', ' ', 'p', ' ', ' ', ' ', ' '],  % Élément 4: 5e rangée d'échecs
        [' ', ' ', ' ', 'P', ' ', ' ', ' ', ' '],  % Élément 5: 4e rangée d'échecs
        [' ', ' ', 'N', 'B', 'P', ' ', ' ', ' '],  % Élément 6: 3e rangée d'échecs
        ['P', 'P', 'P', ' ', ' ', 'P', 'P', 'P'],  % Élément 7: 2e rangée d'échecs
        ['R', ' ', ' ', 'Q', 'K', ' ', 'N', 'R']   % Élément 8: 1ère rangée d'échecs
    ],
    
    writeln('=== TEST CONVERSION BUG ==='),
    
    % Test Row=1 (8e rangée d'échecs) avec conversion 9-1=8
    TestRow = 1, TestCol = 4,
    BoardRow is 9 - TestRow,
    format('Row=~w, 9-Row=~w (accès élément ~w)~n', [TestRow, BoardRow, BoardRow]),
    
    nth1(BoardRow, Board, RowList),
    format('Élément ~w de Board: ~w~n', [BoardRow, RowList]),
    
    nth1(TestCol, RowList, Piece),
    format('Pièce obtenue: ~w~n', [Piece]),
    
    % Ce qu'on devrait obtenir
    nth1(1, Board, ExpectedRow),
    nth1(TestCol, ExpectedRow, ExpectedPiece),
    format('Pièce attendue (élément 1): ~w~n', [ExpectedPiece]).

:- test_get_piece_bug.
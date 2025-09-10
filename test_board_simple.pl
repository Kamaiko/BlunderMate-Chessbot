% Test simple pour vérifier get_piece

:- consult('src/board.pl').

test_simple_board :-
    % Position utilisateur : [r,n,b,q,k, , ,r] = rangée 8 = index 1 en Prolog
    TestBoard = [['r', 'n', 'b', 'q', 'k', ' ', ' ', 'r']],
    
    get_piece(TestBoard, 1, 1, Piece1),
    get_piece(TestBoard, 1, 2, Piece2), 
    get_piece(TestBoard, 1, 3, Piece3),
    get_piece(TestBoard, 1, 4, Piece4),
    
    format('(1,1): ~w~n', [Piece1]),  % Devrait être 'r'
    format('(1,2): ~w~n', [Piece2]),  % Devrait être 'n' 
    format('(1,3): ~w~n', [Piece3]),  % Devrait être 'b'
    format('(1,4): ~w~n', [Piece4]).  % Devrait être 'q' !!!

:- test_simple_board.
% Debug piece ownership - Analyser get_piece_color

:- consult('src/board.pl').
:- consult('src/pieces.pl'). 
:- consult('src/game.pl').

debug_piece_ownership :-
    Board = [
        ['r', 'n', 'b', 'q', 'k', ' ', ' ', 'r'],  % 8e rangée = index 1 - DAME NOIRE 'q' en (1,4)
        ['p', 'p', ' ', ' ', ' ', 'p', 'p', 'p'],  % 7e rangée = index 2
        [' ', ' ', 'p', 'B', 'p', 'n', ' ', ' '],  % 6e rangée = index 3 - FOU BLANC 'B' en (3,4)
        [' ', ' ', ' ', 'p', ' ', ' ', ' ', ' '],  % 5e rangée = index 4
        [' ', ' ', ' ', 'P', ' ', ' ', ' ', ' '],  % 4e rangée = index 5
        [' ', ' ', 'N', 'B', 'P', ' ', ' ', ' '],  % 3e rangée = index 6
        ['P', 'P', 'P', ' ', ' ', 'P', 'P', 'P'],  % 2e rangée = index 7
        ['R', ' ', ' ', 'Q', 'K', ' ', 'N', 'R']   % 1e rangée = index 8
    ],
    
    FromRow = 1, FromCol = 4,
    
    writeln('=== DEBUG PIECE OWNERSHIP ==='),
    
    % 1. Vérifier quelle pièce est à cette position
    get_piece(Board, FromRow, FromCol, Piece),
    format('Pièce en (1,4): ~w~n', [Piece]),
    
    % 2. Tester get_piece_color
    (   get_piece_color(Piece, Color) ->
        format('Couleur détectée: ~w~n', [Color])
    ;   writeln('❌ get_piece_color ÉCHEC')
    ),
    
    % 3. Test direct validate_piece_ownership
    (   validate_piece_ownership(Board, FromRow, FromCol, black, PieceOut) ->
        format('✅ Validate ownership OK, pièce: ~w~n', [PieceOut])
    ;   writeln('❌ Validate ownership ÉCHEC')
    ),
    
    % 4. Tests de conventions de couleur
    writeln('=== TESTS COULEUR ==='),
    (   get_piece_color('q', black) ->
        writeln('✅ q = black')
    ;   writeln('❌ q ≠ black') 
    ),
    (   get_piece_color('Q', white) ->
        writeln('✅ Q = white')  
    ;   writeln('❌ Q ≠ white')
    ).

:- debug_piece_ownership.
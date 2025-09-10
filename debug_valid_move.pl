% Debug valid_move - Pourquoi Qd8xd6 est rejeté

:- consult('src/board.pl').
:- consult('src/pieces.pl'). 
:- consult('src/game.pl').

debug_valid_move :-
    % Position EXACTE
    Board = [
        ['r', 'n', 'b', 'q', 'k', ' ', ' ', 'r'],  % 8e rangée = index 1
        ['p', 'p', ' ', ' ', ' ', 'p', 'p', 'p'],  % 7e rangée = index 2
        [' ', ' ', 'p', 'B', 'p', 'n', ' ', ' '],  % 6e rangée = index 3 - Fou BLANC sur d6
        [' ', ' ', ' ', 'p', ' ', ' ', ' ', ' '],  % 5e rangée = index 4
        [' ', ' ', ' ', 'P', ' ', ' ', ' ', ' '],  % 4e rangée = index 5
        [' ', ' ', 'N', 'B', 'P', ' ', ' ', ' '],  % 3e rangée = index 6
        ['P', 'P', 'P', ' ', ' ', 'P', 'P', 'P'],  % 2e rangée = index 7
        ['R', ' ', ' ', 'Q', 'K', ' ', 'N', 'R']   % 1e rangée = index 8
    ],
    
    writeln('=== DEBUG VALID_MOVE pour Qd8xd6 ==='),
    FromRow = 1, FromCol = 4, ToRow = 3, ToCol = 4,
    
    % Test étape par étape
    writeln('1. Test validate_move_coordinates:'),
    (   validate_move_coordinates(FromRow, FromCol, ToRow, ToCol) ->
        writeln('   ✅ Coordonnées OK')
    ;   writeln('   ❌ Coordonnées INVALIDES')
    ),
    
    writeln('2. Test validate_piece_ownership:'),
    (   validate_piece_ownership(Board, FromRow, FromCol, black, Piece) ->
        format('   ✅ Pièce propriétaire OK: ~w~n', [Piece])
    ;   writeln('   ❌ Pièce ownership ÉCHEC')
    ),
    
    writeln('3. Test validate_destination_square:'),
    (   validate_destination_square(Board, ToRow, ToCol, black) ->
        writeln('   ✅ Case destination OK')
    ;   writeln('   ❌ Case destination INVALIDE')
    ),
    
    writeln('4. Test validate_piece_specific_rules:'),
    get_piece(Board, FromRow, FromCol, PieceTest),
    (   validate_piece_specific_rules(Board, FromRow, FromCol, ToRow, ToCol, PieceTest) ->
        writeln('   ✅ Règles pièce OK')
    ;   writeln('   ❌ Règles pièce ÉCHEC - PROBLÈME ICI!')
    ),
    
    writeln('5. Test move_leaves_king_in_check:'),
    GameState = game_state(Board, black, 12, active, [[], ['b']]),
    (   move_leaves_king_in_check(GameState, black, FromRow, FromCol, ToRow, ToCol) ->
        writeln('   ❌ Coup laisse roi en échec')
    ;   writeln('   ✅ Roi pas en échec')
    ).

:- debug_valid_move.
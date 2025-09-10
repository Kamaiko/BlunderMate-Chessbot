% Test recapture avec Row=8 pour confirmation

:- consult('src/board.pl').
:- consult('src/pieces.pl'). 
:- consult('src/game.pl').
:- consult('src/ai.pl').
:- consult('src/evaluation.pl').

test_row8_recapture :-
    Board = [
        ['r', 'n', 'b', 'q', 'k', ' ', ' ', 'r'],  % Élément 1 
        ['p', 'p', ' ', ' ', ' ', 'p', 'p', 'p'],  % Élément 2
        [' ', ' ', 'p', 'B', 'p', 'n', ' ', ' '],  % Élément 3
        [' ', ' ', ' ', 'p', ' ', ' ', ' ', ' '],  % Élément 4
        [' ', ' ', ' ', 'P', ' ', ' ', ' ', ' '],  % Élément 5
        [' ', ' ', 'N', 'B', 'P', ' ', ' ', ' '],  % Élément 6
        ['P', 'P', 'P', ' ', ' ', 'P', 'P', 'P'],  % Élément 7
        ['R', ' ', ' ', 'Q', 'K', ' ', 'N', 'R']   % Élément 8
    ],
    
    writeln('=== TEST RECAPTURE AVEC ROW=8 ==='),
    
    % Test: Dame noire en Row=8 Col=4 vers Fou blanc Row=6 Col=4
    % (Selon la conversion 9-Row, Row=8 devrait accéder à l'élément 1 avec 'q')
    FromRow = 8, FromCol = 4, ToRow = 6, ToCol = 4,
    
    get_piece(Board, FromRow, FromCol, PieceDame),
    format('Dame avec Row=8: ~w~n', [PieceDame]),
    
    get_piece(Board, ToRow, ToCol, PieceFou),
    format('Fou avec Row=6: ~w~n', [PieceFou]),
    
    % Test si cette recapture est validée
    GameState = game_state(Board, black, 12, active, [[], ['b']]),
    (   valid_move(Board, black, FromRow, FromCol, ToRow, ToCol) ->
        writeln('✅ Qd8xd6 avec Row=8 est LÉGAL!'),
        classify_single_move(GameState, black, [FromRow, FromCol, ToRow, ToCol], Priority),
        format('Priorité: ~w~n', [Priority])
    ;   writeln('❌ Encore illégal')
    ).

:- test_row8_recapture.
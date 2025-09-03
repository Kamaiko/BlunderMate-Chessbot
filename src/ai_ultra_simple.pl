% =============================================================================
% IA ULTRA SIMPLE - INSPIREE DIRECTEMENT DE LA REFERENCE
% =============================================================================
%
% Version extremement simplifiee pour corriger les problemes
% Pas de minimax complexe - juste selection intelligente
%
% =============================================================================

:- [pieces].
:- [board].
:- [game].

% =============================================================================
% INTERFACE PRINCIPALE - ULTRA SIMPLE
% =============================================================================

% choose_ai_move(+GameState, -BestMove)
% Version ultra simple - pas de minimax, juste heuristiques
choose_ai_move(GameState, BestMove) :-
    GameState = game_state(_, Player, _, _, _),
    
    % Generer tous les coups legaux
    generate_legal_moves_simple(GameState, Player, Moves),
    
    (   Moves = [] ->
        write('ERREUR: Aucun coup legal'), nl,
        BestMove = [1,1,1,1]
    ;   % Selection intelligente simple
        select_best_move_simple(GameState, Moves, BestMove)
    ).

% select_best_move_simple(+GameState, +Moves, -BestMove)
% Selection simple avec priorites basiques
select_best_move_simple(GameState, Moves, BestMove) :-
    GameState = game_state(Board, _, _, _, _),
    
    % 1. Chercher captures d'abord
    findall(Move, (
        member(Move, Moves),
        Move = [_, _, ToRow, ToCol],
        get_piece(Board, ToRow, ToCol, Target),
        Target \= '.'
    ), Captures),
    
    (   Captures \= [] ->
        % Prendre capture avec plus grosse piece
        select_best_capture(GameState, Captures, BestMove)
    ;   % 2. Coups de developpement
        findall(Move, (
            member(Move, Moves),
            is_development_move_simple(Move)
        ), DevMoves),
        (   DevMoves \= [] ->
            random_member(BestMove, DevMoves)
        ;   % 3. Coup aleatoire
            random_member(BestMove, Moves)
        )
    ).

% select_best_capture(+GameState, +Captures, -BestCapture)
% Selectionne la meilleure capture (plus grosse piece)
select_best_capture(GameState, Captures, BestCapture) :-
    GameState = game_state(Board, _, _, _, _),
    findall(Value-Move, (
        member(Move, Captures),
        Move = [_, _, ToRow, ToCol],
        get_piece(Board, ToRow, ToCol, Target),
        piece_value_simple_base(Target, Value)
    ), ValuedCaptures),
    keysort(ValuedCaptures, Sorted),
    reverse(Sorted, [_-BestCapture|_]).

% is_development_move_simple(+Move)
% Detecte coups de developpement basiques
is_development_move_simple([FromRow, FromCol, ToRow, ToCol]) :-
    (   % Cavalier se developpe
        FromRow = 1, member(FromCol, [2, 7]), member(ToRow, [3, 4])
    ;   FromRow = 8, member(FromCol, [2, 7]), member(ToRow, [6, 5])
    ;   % Fou se developpe  
        FromRow = 1, member(FromCol, [3, 6]), member(ToRow, [2, 3, 4])
    ;   FromRow = 8, member(FromCol, [3, 6]), member(ToRow, [7, 6, 5])
    ;   % Pion central
        member(ToRow, [4, 5]), member(ToCol, [4, 5])
    ).

% piece_value_simple_base(+Piece, -Value)
% Valeurs basiques pour selection captures
piece_value_simple_base('P', 100).
piece_value_simple_base('p', 100).
piece_value_simple_base('R', 450).
piece_value_simple_base('r', 450).
piece_value_simple_base('N', 320).
piece_value_simple_base('n', 320).
piece_value_simple_base('B', 330).
piece_value_simple_base('b', 330).
piece_value_simple_base('Q', 900).
piece_value_simple_base('q', 900).
piece_value_simple_base('K', 10000).
piece_value_simple_base('k', 10000).
piece_value_simple_base('.', 0).

% generate_legal_moves_simple(+GameState, +Player, -Moves)
% Generation simple de coups legaux
generate_legal_moves_simple(GameState, Player, Moves) :-
    GameState = game_state(Board, _, _, _, _),
    findall([FromRow, FromCol, ToRow, ToCol], (
        between(1, 8, FromRow),
        between(1, 8, FromCol),
        get_piece(Board, FromRow, FromCol, Piece),
        Piece \= '.',
        get_piece_color(Piece, Player),
        between(1, 8, ToRow),
        between(1, 8, ToCol),
        valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol),
        % Verification echec
        \+ move_leaves_king_in_check_simple(GameState, Player, FromRow, FromCol, ToRow, ToCol)
    ), Moves).

% move_leaves_king_in_check_simple(+GameState, +Player, +FromRow, +FromCol, +ToRow, +ToCol)
% Verification echec simplifiee
move_leaves_king_in_check_simple(GameState, Player, FromRow, FromCol, ToRow, ToCol) :-
    GameState = game_state(Board, _, _, _, _),
    get_piece(Board, FromRow, FromCol, MovingPiece),
    place_single_piece(Board, FromRow, FromCol, '.', TempBoard),
    place_single_piece(TempBoard, ToRow, ToCol, MovingPiece, NewBoard),
    SimulatedGameState = game_state(NewBoard, Player, 0, active, [[], []]),
    is_in_check(SimulatedGameState, Player).
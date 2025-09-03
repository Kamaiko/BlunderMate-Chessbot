% =============================================================================
% IA MINIMALE - VERSION DE DEBUG
% =============================================================================

:- [pieces].
:- [board].
:- [game].

% choose_ai_move(+GameState, -BestMove)
% Version amelioree avec logique captures et securite
choose_ai_move(GameState, BestMove) :-
    GameState = game_state(Board, Player, _, _, _),
    
    % Generation avec verification echec
    findall([FromRow, FromCol, ToRow, ToCol], (
        between(1, 8, FromRow),
        between(1, 8, FromCol),
        get_piece(Board, FromRow, FromCol, Piece),
        Piece \= '.',
        get_piece_color(Piece, Player),
        between(1, 8, ToRow),
        between(1, 8, ToCol),
        valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol),
        % Verification echec CRITIQUE
        \+ move_leaves_king_in_check_simple(GameState, Player, FromRow, FromCol, ToRow, ToCol)
    ), AllMoves),
    
    (   AllMoves = [] ->
        write('Aucun coup legal trouve'), nl,
        BestMove = [2,5,4,5]  % Fallback
    ;   % Selection intelligente avec captures prioritaires
        select_best_move_intelligent(GameState, AllMoves, BestMove)
    ).

% =============================================================================
% LOGIQUE DE SELECTION INTELLIGENTE
% =============================================================================

% select_best_move_intelligent(+GameState, +Moves, -BestMove)
% Selection intelligente avec priorites : captures > developpement > aleatoire
select_best_move_intelligent(GameState, Moves, BestMove) :-
    GameState = game_state(Board, _, _, _, _),
    
    % 1. Priorite captures
    findall(Move, (
        member(Move, Moves),
        Move = [_, _, ToRow, ToCol],
        get_piece(Board, ToRow, ToCol, Target),
        Target \= '.'
    ), Captures),
    
    (   Captures \= [] ->
        % Selectionner meilleure capture
        select_best_capture_simple(GameState, Captures, BestMove)
    ;   % 2. Coups de developpement
        findall(Move, (
            member(Move, Moves),
            is_development_move_basic(Move)
        ), DevMoves),
        (   DevMoves \= [] ->
            random_member(BestMove, DevMoves)
        ;   % 3. Coup vers centre
            findall(Move, (
                member(Move, Moves),
                Move = [_, _, ToRow, ToCol],
                member(ToRow, [4,5]), member(ToCol, [4,5])
            ), CenterMoves),
            (   CenterMoves \= [] ->
                random_member(BestMove, CenterMoves)
            ;   % 4. Coup aleatoire
                random_member(BestMove, Moves)
            )
        )
    ).

% select_best_capture_simple(+GameState, +Captures, -BestCapture)
% Selectionne capture avec piece la plus precieuse
select_best_capture_simple(GameState, Captures, BestCapture) :-
    GameState = game_state(Board, _, _, _, _),
    findall(Value-Move, (
        member(Move, Captures),
        Move = [_, _, ToRow, ToCol],
        get_piece(Board, ToRow, ToCol, Target),
        piece_value_basic(Target, Value)
    ), ValuedCaptures),
    (   ValuedCaptures = [] ->
        Captures = [BestCapture|_]  % Fallback
    ;   keysort(ValuedCaptures, Sorted),
        reverse(Sorted, [_-BestCapture|_])
    ).

% is_development_move_basic(+Move)
% Detecte coups de developpement de base
is_development_move_basic([FromRow, FromCol, ToRow, _]) :-
    (   % Cavalier se developpe
        FromRow = 1, member(FromCol, [2, 7]), member(ToRow, [3])
    ;   FromRow = 8, member(FromCol, [2, 7]), member(ToRow, [6])
    ;   % Fou se developpe
        FromRow = 1, member(FromCol, [3, 6]), member(ToRow, [2, 3])
    ;   FromRow = 8, member(FromCol, [3, 6]), member(ToRow, [7, 6])
    ).

% piece_value_basic(+Piece, -Value)
% Valeurs basiques des pieces pour captures
piece_value_basic('P', 100).
piece_value_basic('p', 100).
piece_value_basic('R', 500).
piece_value_basic('r', 500).
piece_value_basic('N', 300).
piece_value_basic('n', 300).
piece_value_basic('B', 300).
piece_value_basic('b', 300).
piece_value_basic('Q', 900).
piece_value_basic('q', 900).
piece_value_basic('K', 10000).
piece_value_basic('k', 10000).
piece_value_basic('.', 0).

% move_leaves_king_in_check_simple(+GameState, +Player, +FromRow, +FromCol, +ToRow, +ToCol)
% Verification echec simplifiee et robuste
move_leaves_king_in_check_simple(GameState, Player, FromRow, FromCol, ToRow, ToCol) :-
    GameState = game_state(Board, _, _, _, _),
    get_piece(Board, FromRow, FromCol, MovingPiece),
    place_single_piece(Board, FromRow, FromCol, '.', TempBoard),
    place_single_piece(TempBoard, ToRow, ToCol, MovingPiece, NewBoard),
    SimulatedGameState = game_state(NewBoard, Player, 0, active, [[], []]),
    is_in_check(SimulatedGameState, Player).
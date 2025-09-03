% =============================================================================
% IA PURE REFERENCE - STRICTEMENT SELON BOARD_EVAL.PL
% =============================================================================
%
% RETOUR à la référence EXACTE sans ajouts compliqués
% Focus sur le calcul correct des positions de pions
%
% =============================================================================

:- [pieces].
:- [board].
:- [game].

% =============================================================================
% CONSTANTES REFERENCE EXACTES - BOARD_EVAL.PL:5-12
% =============================================================================

compensate_ref(white, 15).
compensate_ref(black, -15).

% =============================================================================
% INTERFACE PRINCIPALE SIMPLE
% =============================================================================

% choose_ai_move(+GameState, -BestMove)
choose_ai_move(GameState, BestMove) :-
    GameState = game_state(_, Player, _, _, _),
    minimax_simple_ref(GameState, Player, 2, BestMove, _Value).

% =============================================================================
% MINIMAX SIMPLE SELON REFERENCE
% =============================================================================

% minimax_simple_ref(+GameState, +Player, +Depth, -BestMove, -BestValue)
minimax_simple_ref(GameState, Player, 0, [], Value) :-
    evaluate_pure_reference(GameState, Player, Value), !.

minimax_simple_ref(GameState, Player, Depth, BestMove, BestValue) :-
    Depth > 0,
    generate_moves_simple(GameState, Player, Moves),
    (   Moves = [] ->
        evaluate_pure_reference(GameState, Player, BestValue),
        BestMove = []
    ;   find_best_simple(GameState, Moves, Player, Depth, BestMove, BestValue)
    ).

% find_best_simple(+GameState, +Moves, +Player, +Depth, -BestMove, -BestValue)
find_best_simple(GameState, [Move], Player, Depth, Move, Value) :-
    eval_move_simple(GameState, Move, Player, Depth, Value), !.

find_best_simple(GameState, [FirstMove|RestMoves], Player, Depth, BestMove, BestValue) :-
    eval_move_simple(GameState, FirstMove, Player, Depth, FirstValue),
    compare_simple(GameState, RestMoves, Player, Depth, FirstValue, FirstMove, BestMove, BestValue).

% compare_simple(+GameState, +Moves, +Player, +Depth, +CurrentBest, +CurrentMove, -BestMove, -BestValue)
compare_simple(_, [], _, _, CurrentBest, CurrentMove, CurrentMove, CurrentBest) :- !.

compare_simple(GameState, [Move|RestMoves], Player, Depth, CurrentBest, CurrentMove, BestMove, BestValue) :-
    eval_move_simple(GameState, Move, Player, Depth, MoveValue),
    (   is_better_simple(Player, MoveValue, CurrentBest) ->
        NewBest = MoveValue, NewMove = Move
    ;   NewBest = CurrentBest, NewMove = CurrentMove
    ),
    compare_simple(GameState, RestMoves, Player, Depth, NewBest, NewMove, BestMove, BestValue).

% eval_move_simple(+GameState, +Move, +Player, +Depth, -Value)
eval_move_simple(GameState, [FromRow, FromCol, ToRow, ToCol], Player, Depth, Value) :-
    make_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState),
    opposite_player(Player, Opponent),
    NewDepth is Depth - 1,
    minimax_simple_ref(NewGameState, Opponent, NewDepth, _, OpponentValue),
    Value is -OpponentValue.

% is_better_simple(+Player, +NewValue, +OldValue)
is_better_simple(white, NewValue, OldValue) :- NewValue > OldValue.
is_better_simple(black, NewValue, OldValue) :- NewValue < OldValue.

% =============================================================================
% EVALUATION PURE REFERENCE - BOARD_EVAL.PL EXACT
% =============================================================================

% evaluate_pure_reference(+GameState, +Player, -Value)
% EXACTEMENT comme chess.pl:56-60 + board_eval.pl:14-24
evaluate_pure_reference(GameState, Player, Value) :-
    count_material_pure_ref(GameState, white, WhiteValue),
    count_material_pure_ref(GameState, black, BlackValue),
    compensate_ref(Player, Comp),
    Value is WhiteValue - BlackValue + Comp.

% count_material_pure_ref(+GameState, +Color, -Value)
% EXACTEMENT comme count_halfst dans board_eval.pl:14-24
count_material_pure_ref(GameState, Color, TotalValue) :-
    GameState = game_state(Board, _, _, _, _),
    
    % Compter EXACTEMENT comme référence
    count_pieces_type(Board, Color, pawn, V1),
    count_pieces_type(Board, Color, rook, V2),
    count_pieces_type(Board, Color, knight, V3),
    count_pieces_type(Board, Color, bishop, V4),
    count_pieces_type(Board, Color, queen, V5),
    
    % Double bonus EXACTEMENT comme board_eval.pl:21-24
    count_pieces_positions(Board, Color, rook, RookCount),
    count_pieces_positions(Board, Color, knight, KnightCount), 
    count_pieces_positions(Board, Color, bishop, BishopCount),
    
    double_bonus_pure(RookCount, D1),
    double_bonus_pure(KnightCount, D2),
    double_bonus_pure(BishopCount, D3),
    
    TotalValue is V1 + V2 + V3 + V4 + V5 + 30*(D1 + D2 + D3).

% double_bonus_pure(+Count, -Bonus)
% EXACTEMENT board_eval.pl:26-27
double_bonus_pure(2, 1) :- !.  % Exactement 2 pièces
double_bonus_pure(_, 0).

% count_pieces_type(+Board, +Color, +Type, -Value)
% EXACTEMENT comme pos_count dans board_eval.pl:29-33
count_pieces_type(Board, Color, Type, Value) :-
    findall(PieceValue, (
        between(1, 8, Row),
        between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        Piece \= '.',
        get_piece_color(Piece, Color),
        piece_is_type_pure(Piece, Type),
        pos_value_pure_ref(Type, Row, Col, Color, PieceValue)
    ), Values),
    sum_list(Values, Value).

% count_pieces_positions(+Board, +Color, +Type, -Count)
% Pour double_bonus - compte nombre de pièces
count_pieces_positions(Board, Color, Type, Count) :-
    findall(1, (
        between(1, 8, Row),
        between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        Piece \= '.',
        get_piece_color(Piece, Color),
        piece_is_type_pure(Piece, Type)
    ), Pieces),
    length(Pieces, Count).

% piece_is_type_pure(+Piece, +Type)
piece_is_type_pure('P', pawn). piece_is_type_pure('p', pawn).
piece_is_type_pure('R', rook). piece_is_type_pure('r', rook).
piece_is_type_pure('N', knight). piece_is_type_pure('n', knight).
piece_is_type_pure('B', bishop). piece_is_type_pure('b', bishop).
piece_is_type_pure('Q', queen). piece_is_type_pure('q', queen).
piece_is_type_pure('K', king). piece_is_type_pure('k', king).

% pos_value_pure_ref(+Type, +Row, +Col, +Color, -Value)
% EXACTEMENT board_eval.pl:35-70 avec CORRECTION calcul noirs
pos_value_pure_ref(Type, Row, Col, black, Value) :-
    % CORRECTION CRITIQUE: Calcul exact comme référence board_eval.pl:35-37
    Pos is Row * 10 + Col,
    RelPos is 99 - Pos,  % EXACTEMENT comme référence
    RelRow is RelPos // 10,
    RelCol is RelPos mod 10,
    pos_value_pure_ref(Type, RelRow, RelCol, white, Value), !.

% Pions blancs - EXACTEMENT board_eval.pl:43-47
pos_value_pure_ref(pawn, Row, Col, white, 127) :-
    Pos is Row * 10 + Col,
    member(Pos, [34,35]), !.  % d4, e4 
pos_value_pure_ref(pawn, Row, Col, white, 131) :-
    Pos is Row * 10 + Col,
    member(Pos, [44,45,54,55]), !.  % d5, e5, d6, e6
pos_value_pure_ref(pawn, _, _, white, 100) :- !.

% Roi - EXACTEMENT board_eval.pl:48-49
pos_value_pure_ref(king, Row, Col, white, 30) :-
    Pos is Row * 10 + Col,
    member(Pos, [11,12,13,17,18]), !.  % Roi sécurisé
pos_value_pure_ref(king, _, _, white, 0) :- !.

% Tour - EXACTEMENT board_eval.pl:50
pos_value_pure_ref(rook, _, _, white, 450) :- !.

% Cavalier - EXACTEMENT board_eval.pl:51-56
pos_value_pure_ref(knight, Row, Col, white, Value) :-
    row_value_pure(knight, Row, V1),
    line_value_pure(knight, Col, V2),
    Value is V1 + V2, !.

% Fou et Dame - EXACTEMENT board_eval.pl:39-42
pos_value_pure_ref(bishop, Row, _, white, Value) :-
    row_value_pure(bishop, Row, Value), !.
pos_value_pure_ref(queen, Row, _, white, Value) :-
    row_value_pure(queen, Row, Value), !.

% row_value_pure et line_value_pure - EXACTEMENT board_eval.pl:58-74
row_value_pure(knight, 2, 320) :- !.
row_value_pure(knight, 3, 321) :- !.
row_value_pure(knight, X, 348) :- member(X, [4,5]), !.
row_value_pure(knight, X, 376) :- member(X, [6,7]), !.
row_value_pure(knight, _, 290) :- !.

row_value_pure(bishop, 1, 300) :- !.
row_value_pure(bishop, X, 329) :- member(X, [2,3]), !.
row_value_pure(bishop, _, 330) :- !.

row_value_pure(queen, 1, 850) :- !.
row_value_pure(queen, _, 876) :- !.

line_value_pure(knight, X, 0) :- member(X, [1,8]), !.
line_value_pure(knight, _, 10) :- !.

% =============================================================================
% GENERATION COUPS SIMPLE
% =============================================================================

% generate_moves_simple(+GameState, +Player, -Moves)
generate_moves_simple(GameState, Player, Moves) :-
    GameState = game_state(Board, _, _, _, _),
    
    % Générer coups par priorité : développement d'abord, puis pions
    findall([FromRow, FromCol, ToRow, ToCol], (
        % D'abord cavaliers et fous (développement)
        between(1, 8, FromRow),
        between(1, 8, FromCol),
        get_piece(Board, FromRow, FromCol, Piece),
        Piece \= '.',
        get_piece_color(Piece, Player),
        member(Piece, ['N','n','B','b']),  % Cavaliers et fous d'abord
        between(1, 8, ToRow),
        between(1, 8, ToCol),
        valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol)
    ), DevelopmentMoves),
    
    % Puis coups de pions
    findall([FromRow, FromCol, ToRow, ToCol], (
        between(1, 8, FromRow),
        between(1, 8, FromCol),
        get_piece(Board, FromRow, FromCol, Piece),
        Piece \= '.',
        get_piece_color(Piece, Player),
        member(Piece, ['P','p']),  % Pions
        between(1, 8, ToRow),
        between(1, 8, ToCol),
        valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol)
    ), PawnMoves),
    
    % Puis autres pièces
    findall([FromRow, FromCol, ToRow, ToCol], (
        between(1, 8, FromRow),
        between(1, 8, FromCol),
        get_piece(Board, FromRow, FromCol, Piece),
        Piece \= '.',
        get_piece_color(Piece, Player),
        \+ member(Piece, ['P','p','N','n','B','b']),  % Autres
        between(1, 8, ToRow),
        between(1, 8, ToCol),
        valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol)
    ), OtherMoves),
    
    % Combiner par priorité
    append(DevelopmentMoves, PawnMoves, TempMoves),
    append(TempMoves, OtherMoves, AllMoves),
    
    % Prendre premiers 20 coups (augmenté pour inclure plus d'options)
    take_first_20_simple(AllMoves, Moves).

% take_first_20_simple(+List, -First20)
take_first_20_simple(List, First20) :-
    length(List, Len),
    (   Len =< 20 -> First20 = List
    ;   length(First20, 20), append(First20, _, List)
    ).
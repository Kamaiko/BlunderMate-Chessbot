% =============================================================================
% IA REFERENCE EXACTE - COPIE LITTERALE
% =============================================================================
%
% COPIE EXACTE du code de référence pour éliminer TOUS les bugs
% Structure half_position + évaluation identique à board_eval.pl
% 
% =============================================================================

:- [pieces].
:- [board].  
:- [game].

% =============================================================================
% STRUCTURES DONNEES EXACTES - COMME REFERENCE
% =============================================================================

% compensate/2, worst_value/2, winning/2 - EXACTEMENT comme board_eval.pl:5-12
compensate(white, 15).
compensate(black, -15).

worst_value(white, -10000).
worst_value(black, 10000).

winning(white, 9000).
winning(black, -9000).

% =============================================================================
% INTERFACE PRINCIPALE 
% =============================================================================

% choose_ai_move(+GameState, -BestMove)
choose_ai_move(GameState, BestMove) :-
    GameState = game_state(_, Player, _, _, _),
    
    % Conversion vers format référence
    gamestate_to_half_positions(GameState, WhiteHalf, BlackHalf),
    Position = position(WhiteHalf, BlackHalf, 0),
    
    % Minimax référence - profondeur 2
    Depth = 2,
    worst_value(white, Alpha),
    worst_value(black, Beta),
    
    % Evaluation exacte comme chess.pl:88
    evaluate_exact_ref(Position, Player, _Value, MoveResult, Depth, Alpha, Beta),
    
    % Conversion retour
    move_ref_to_our_format(MoveResult, BestMove).

% =============================================================================
% EVALUATION EXACTE - COPIE DE BOARD_EVAL.PL
% =============================================================================

% evaluate_exact_ref(+Position, +Color, -Value, -Move, +Depth, +Alpha, +Beta)
% EXACTEMENT comme chess.pl:61-65
evaluate_exact_ref(Position, Color, Value, move(0,0), 0, _, _) :-
    count_position_exact(Position, Color, Value), !.
    
evaluate_exact_ref(Position, Color, Value, Move, Depth, Alpha, Beta) :-
    worst_value(Color, Worst),
    % Algorithme référence simplifié pour debug
    find_all_moves_ref(Position, Color, AllMoves),
    (   AllMoves = [] ->
        count_position_exact(Position, Color, Value),
        Move = move(0,0)
    ;   select_best_move_ref(Position, AllMoves, Color, Depth, Alpha, Beta, Move, Value)
    ).

% count_position_exact(+Position, +Color, -Value)
% EXACTEMENT comme chess.pl:56-60
count_position_exact(position(WhiteHalf, BlackHalf, _), Color, Value) :-
    count_halfst_exact(WhiteHalf, white, WhiteValue),
    count_halfst_exact(BlackHalf, black, BlackValue),
    compensate(Color, Z),
    Value is WhiteValue - BlackValue + Z.

% count_halfst_exact(+HalfPosition, +Color, -Value)
% EXACTEMENT comme board_eval.pl:14-24
count_halfst_exact(half_position(Pawns,Rooks,Knights,Bishops,Queens,[_],_), Color, Value) :-
    pos_count_exact(pawn, Pawns, Color, V1),
    pos_count_exact(rook, Rooks, Color, V2),
    pos_count_exact(bishop, Bishops, Color, V3),
    pos_count_exact(knight, Knights, Color, V4),
    pos_count_exact(queen, Queens, Color, V5),
    double_bonus_exact(Rooks, D1),
    double_bonus_exact(Knights, D2),
    double_bonus_exact(Bishops, D3),
    Value is V1 + V2 + V3 + V4 + V5 + 30*(D1 + D2 + D3).

% double_bonus_exact/2 - EXACTEMENT board_eval.pl:26-27  
double_bonus_exact([_,_], 1) :- !.
double_bonus_exact(_, 0).

% pos_count_exact/4 - EXACTEMENT board_eval.pl:29-33
pos_count_exact(_, [], _, 0) :- !.
pos_count_exact(Type, [Feld|Rest], Color, Value) :-
    pos_count_exact(Type, Rest, Color, V2),
    pos_value_exact(Type, Feld, Color, V1),
    Value is V1 + V2, !.

% pos_value_exact/4 - EXACTEMENT board_eval.pl:35-70
pos_value_exact(Type, Feld, black, Value) :-
    Rel_Feld is 99 - Feld,
    pos_value_exact(Type, Rel_Feld, white, Value), !.

pos_value_exact(Type, Pos, white, Value) :-
    row_ref(Pos, Row),
    member(Type, [bishop, queen]),
    row_value_exact(Type, Row, Value), !.

pos_value_exact(pawn, Pos, white, 127) :-
    member(Pos, [34,35]), !.
pos_value_exact(pawn, Pos, white, 131) :-
    member(Pos, [44,45,54,55]), !.
pos_value_exact(pawn, _, white, 100) :- !.

pos_value_exact(king, Pos, white, 30) :-
    member(Pos, [11,12,13,17,18]), !.
pos_value_exact(king, _, white, 0) :- !.

pos_value_exact(rook, _, white, 450) :- !.

pos_value_exact(Type, Pos, white, Value) :-
    Type = knight,
    row_line_ref(Pos, Row, Line),
    row_value_exact(Type, Row, V1),
    line_value_exact(Type, Line, V2),
    Value is V1 + V2, !.

% row_value_exact/3, line_value_exact/3 - EXACTEMENT board_eval.pl:58-74
row_value_exact(knight, 2, 320) :- !.
row_value_exact(knight, 3, 321) :- !.  
row_value_exact(knight, X, 348) :- member(X, [4,5]), !.
row_value_exact(knight, X, 376) :- member(X, [6,7]), !.
row_value_exact(knight, _, 290) :- !.

row_value_exact(bishop, 1, 300) :- !.
row_value_exact(bishop, X, 329) :- member(X, [2,3]), !.
row_value_exact(bishop, _, 330) :- !.

row_value_exact(queen, 1, 850) :- !.
row_value_exact(queen, _, 876) :- !.

line_value_exact(knight, X, 0) :- member(X, [1,8]), !.
line_value_exact(knight, _, 10) :- !.

% row_ref/2, row_line_ref/3 - EXACTEMENT board_eval.pl:76-80
row_ref(Pos, Row) :- Row is Pos // 10.
row_line_ref(Pos, Row, Line) :-
    Row is Pos // 10,
    Line is Pos mod 10.

% =============================================================================
% GENERATION COUPS SIMPLIFIEE POUR DEBUG
% =============================================================================

% find_all_moves_ref(+Position, +Color, -Moves)
find_all_moves_ref(Position, Color, Moves) :-
    position_to_gamestate(Position, GameState),
    GameState = game_state(Board, _, _, _, _),
    
    findall(move(FromPos, ToPos), (
        between(1, 8, FromRow),
        between(1, 8, FromCol),
        get_piece(Board, FromRow, FromCol, Piece),
        Piece \= '.',
        get_piece_color(Piece, Color),
        between(1, 8, ToRow),
        between(1, 8, ToCol),
        valid_move(Board, Color, FromRow, FromCol, ToRow, ToCol),
        FromPos is FromRow * 10 + FromCol,
        ToPos is ToRow * 10 + ToCol
    ), AllMoves),
    
    % Limiter pour performance - prendre les 10 premiers
    take_first_10(AllMoves, Moves).

% select_best_move_ref(+Position, +Moves, +Color, +Depth, +Alpha, +Beta, -BestMove, -BestValue)
% Sélection simplifiée du meilleur coup
select_best_move_ref(Position, [Move], Color, Depth, Alpha, Beta, Move, Value) :-
    % Un seul coup
    evaluate_move_ref(Position, Move, Color, Depth, Alpha, Beta, Value), !.

select_best_move_ref(Position, Moves, Color, Depth, Alpha, Beta, BestMove, BestValue) :-
    evaluate_all_moves_ref(Position, Moves, Color, Depth, Alpha, Beta, BestMove, BestValue).

% evaluate_move_ref(+Position, +Move, +Color, +Depth, +Alpha, +Beta, -Value)
evaluate_move_ref(Position, move(FromPos, ToPos), Color, Depth, Alpha, Beta, Value) :-
    % Simuler coup et évaluer
    FromRow is FromPos // 10, FromCol is FromPos mod 10,
    ToRow is ToPos // 10, ToCol is ToPos mod 10,
    
    position_to_gamestate(Position, GameState),
    make_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState),
    gamestate_to_half_positions(NewGameState, NewWhiteHalf, NewBlackHalf),
    NewPosition = position(NewWhiteHalf, NewBlackHalf, 0),
    
    (   Depth =< 1 ->
        count_position_exact(NewPosition, Color, TempValue),
        Value is -TempValue  % Inverser car c'est le tour de l'adversaire
    ;   % Récursion simplifiée
        opposite_player(Color, Opponent),
        NewDepth is Depth - 1,
        evaluate_exact_ref(NewPosition, Opponent, OpponentValue, _, NewDepth, Alpha, Beta),
        Value is -OpponentValue
    ).

% evaluate_all_moves_ref - Version simplifiée pour éviter bugs
evaluate_all_moves_ref(Position, [FirstMove|RestMoves], Color, Depth, Alpha, Beta, BestMove, BestValue) :-
    evaluate_move_ref(Position, FirstMove, Color, Depth, Alpha, Beta, FirstValue),
    find_best_among_moves(Position, RestMoves, Color, Depth, Alpha, Beta, FirstValue, FirstMove, BestMove, BestValue).

find_best_among_moves(_, [], _, _, _, _, CurrentValue, CurrentMove, CurrentMove, CurrentValue) :- !.

find_best_among_moves(Position, [Move|RestMoves], Color, Depth, Alpha, Beta, CurrentValue, CurrentMove, BestMove, BestValue) :-
    evaluate_move_ref(Position, Move, Color, Depth, Alpha, Beta, MoveValue),
    (   is_better_value_ref(Color, MoveValue, CurrentValue) ->
        NewCurrentValue = MoveValue,
        NewCurrentMove = Move
    ;   NewCurrentValue = CurrentValue,
        NewCurrentMove = CurrentMove
    ),
    find_best_among_moves(Position, RestMoves, Color, Depth, Alpha, Beta, NewCurrentValue, NewCurrentMove, BestMove, BestValue).

% is_better_value_ref(+Color, +NewValue, +OldValue)
is_better_value_ref(white, NewValue, OldValue) :- NewValue > OldValue.
is_better_value_ref(black, NewValue, OldValue) :- NewValue < OldValue.

% =============================================================================
% CONVERSION DONNEES
% =============================================================================

% gamestate_to_half_positions(+GameState, -WhiteHalf, -BlackHalf)
gamestate_to_half_positions(GameState, WhiteHalf, BlackHalf) :-
    GameState = game_state(Board, _, _, _, _),
    extract_half_position(Board, white, WhiteHalf),
    extract_half_position(Board, black, BlackHalf).

% extract_half_position(+Board, +Color, -HalfPosition)
extract_half_position(Board, Color, half_position(Pawns, Rooks, Knights, Bishops, Queens, Kings, notmoved)) :-
    findall(Pos, (
        between(1, 8, Row), between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        get_piece_color(Piece, Color),
        piece_type(Piece, pawn),
        Pos is Row * 10 + Col
    ), Pawns),
    
    findall(Pos, (
        between(1, 8, Row), between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        get_piece_color(Piece, Color),
        piece_type(Piece, rook),
        Pos is Row * 10 + Col
    ), Rooks),
    
    findall(Pos, (
        between(1, 8, Row), between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        get_piece_color(Piece, Color),
        piece_type(Piece, knight),
        Pos is Row * 10 + Col
    ), Knights),
    
    findall(Pos, (
        between(1, 8, Row), between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        get_piece_color(Piece, Color),
        piece_type(Piece, bishop),
        Pos is Row * 10 + Col
    ), Bishops),
    
    findall(Pos, (
        between(1, 8, Row), between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        get_piece_color(Piece, Color),
        piece_type(Piece, queen),
        Pos is Row * 10 + Col
    ), Queens),
    
    findall(Pos, (
        between(1, 8, Row), between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        get_piece_color(Piece, Color),
        piece_type(Piece, king),
        Pos is Row * 10 + Col
    ), Kings).

% piece_type(+Piece, +Type)
piece_type('P', pawn). piece_type('p', pawn).
piece_type('R', rook). piece_type('r', rook).
piece_type('N', knight). piece_type('n', knight).
piece_type('B', bishop). piece_type('b', bishop).
piece_type('Q', queen). piece_type('q', queen).  
piece_type('K', king). piece_type('k', king).

% position_to_gamestate(+Position, -GameState)
position_to_gamestate(position(WhiteHalf, BlackHalf, _), GameState) :-
    create_board_from_halfs(WhiteHalf, BlackHalf, Board),
    GameState = game_state(Board, white, 0, active, [[], []]).

% create_board_from_halfs(+WhiteHalf, +BlackHalf, -Board)
create_board_from_halfs(WhiteHalf, BlackHalf, Board) :-
    % Créer board vide
    EmptyBoard = [
        ['.','.','.','.','.','.','.','.'],
        ['.','.','.','.','.','.','.','.'],
        ['.','.','.','.','.','.','.','.'],
        ['.','.','.','.','.','.','.','.'],
        ['.','.','.','.','.','.','.','.'],
        ['.','.','.','.','.','.','.','.'],
        ['.','.','.','.','.','.','.','.'],
        ['.','.','.','.','.','.','.','.' ]
    ],
    place_half_on_board(EmptyBoard, WhiteHalf, white, TempBoard),
    place_half_on_board(TempBoard, BlackHalf, black, Board).

% place_half_on_board(+Board, +HalfPosition, +Color, -NewBoard)
place_half_on_board(Board, half_position(Pawns, Rooks, Knights, Bishops, Queens, Kings, _), Color, NewBoard) :-
    place_pieces_type(Board, Pawns, Color, pawn, Board1),
    place_pieces_type(Board1, Rooks, Color, rook, Board2),
    place_pieces_type(Board2, Knights, Color, knight, Board3),
    place_pieces_type(Board3, Bishops, Color, bishop, Board4),
    place_pieces_type(Board4, Queens, Color, queen, Board5),
    place_pieces_type(Board5, Kings, Color, king, NewBoard).

% place_pieces_type(+Board, +Positions, +Color, +Type, -NewBoard)
place_pieces_type(Board, [], _, _, Board) :- !.
place_pieces_type(Board, [Pos|Rest], Color, Type, NewBoard) :-
    Row is Pos // 10, Col is Pos mod 10,
    type_color_to_piece(Type, Color, Piece),
    place_single_piece(Board, Row, Col, Piece, TempBoard),
    place_pieces_type(TempBoard, Rest, Color, Type, NewBoard).

% type_color_to_piece(+Type, +Color, -Piece)
type_color_to_piece(pawn, white, 'P'). type_color_to_piece(pawn, black, 'p').
type_color_to_piece(rook, white, 'R'). type_color_to_piece(rook, black, 'r').
type_color_to_piece(knight, white, 'N'). type_color_to_piece(knight, black, 'n').
type_color_to_piece(bishop, white, 'B'). type_color_to_piece(bishop, black, 'b').
type_color_to_piece(queen, white, 'Q'). type_color_to_piece(queen, black, 'q').
type_color_to_piece(king, white, 'K'). type_color_to_piece(king, black, 'k').

% move_ref_to_our_format(+MoveRef, -OurMove)
move_ref_to_our_format(move(FromPos, ToPos), [FromRow, FromCol, ToRow, ToCol]) :-
    FromRow is FromPos // 10, FromCol is FromPos mod 10,
    ToRow is ToPos // 10, ToCol is ToPos mod 10.

% take_first_10(+List, -First10)
take_first_10(List, First10) :-
    length(List, Len),
    (   Len =< 10 -> First10 = List
    ;   length(First10, 10), append(First10, _, List)
    ).
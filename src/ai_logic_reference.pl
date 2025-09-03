% =============================================================================
% IA LOGIQUE REFERENCE - EVALUATION EXACTE SANS CONVERSION
% =============================================================================
%
% APPROCHE FINALE:
% - Utilise notre structure GameState directement
% - Copie EXACTE de la logique d'évaluation référence  
% - Double bonus, valeurs positionnelles précises
% - Minimax simple mais évaluation référence exacte
%
% =============================================================================

:- [pieces].
:- [board].
:- [game].

% =============================================================================
% CONSTANTES REFERENCE EXACTES
% =============================================================================

% Exactement comme board_eval.pl:5-12
compensate_ref(white, 15).
compensate_ref(black, -15).

worst_value_ref(white, -10000).
worst_value_ref(black, 10000).

winning_ref(white, 9000).
winning_ref(black, -9000).

% =============================================================================
% INTERFACE PRINCIPALE
% =============================================================================

% choose_ai_move(+GameState, -BestMove)
choose_ai_move(GameState, BestMove) :-
    GameState = game_state(_, Player, _, _, _),
    
    % Minimax profondeur 2 avec évaluation référence exacte
    minimax_ref_logic(GameState, Player, 2, BestMove, _Value).

% =============================================================================
% MINIMAX AVEC LOGIQUE REFERENCE
% =============================================================================

% minimax_ref_logic(+GameState, +Player, +Depth, -BestMove, -BestValue)
minimax_ref_logic(GameState, Player, 0, [], Value) :-
    % Cas de base - évaluation référence exacte
    evaluate_reference_logic(GameState, Player, Value), !.

minimax_ref_logic(GameState, Player, Depth, BestMove, BestValue) :-
    Depth > 0,
    generate_moves_ref_logic(GameState, Player, Moves),
    
    (   Moves = [] ->
        evaluate_reference_logic(GameState, Player, BestValue),
        BestMove = []
    ;   find_best_move_ref_logic(GameState, Moves, Player, Depth, BestMove, BestValue)
    ).

% find_best_move_ref_logic(+GameState, +Moves, +Player, +Depth, -BestMove, -BestValue)
find_best_move_ref_logic(GameState, [Move], Player, Depth, Move, Value) :-
    % Un seul coup
    execute_and_evaluate_ref(GameState, Move, Player, Depth, Value), !.

find_best_move_ref_logic(GameState, [FirstMove|RestMoves], Player, Depth, BestMove, BestValue) :-
    execute_and_evaluate_ref(GameState, FirstMove, Player, Depth, FirstValue),
    compare_moves_ref_logic(GameState, RestMoves, Player, Depth, FirstValue, FirstMove, BestMove, BestValue).

% compare_moves_ref_logic(+GameState, +Moves, +Player, +Depth, +CurrentBest, +CurrentMove, -BestMove, -BestValue)
compare_moves_ref_logic(_, [], _, _, CurrentBest, CurrentMove, CurrentMove, CurrentBest) :- !.

compare_moves_ref_logic(GameState, [Move|RestMoves], Player, Depth, CurrentBest, CurrentMove, BestMove, BestValue) :-
    execute_and_evaluate_ref(GameState, Move, Player, Depth, MoveValue),
    (   is_better_ref_logic(Player, MoveValue, CurrentBest) ->
        NewBest = MoveValue, NewMove = Move
    ;   NewBest = CurrentBest, NewMove = CurrentMove
    ),
    compare_moves_ref_logic(GameState, RestMoves, Player, Depth, NewBest, NewMove, BestMove, BestValue).

% execute_and_evaluate_ref(+GameState, +Move, +Player, +Depth, -Value)
execute_and_evaluate_ref(GameState, [FromRow, FromCol, ToRow, ToCol], Player, Depth, Value) :-
    make_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState),
    opposite_player(Player, Opponent),
    NewDepth is Depth - 1,
    minimax_ref_logic(NewGameState, Opponent, NewDepth, _, OpponentValue),
    Value is -OpponentValue.

% is_better_ref_logic(+Player, +NewValue, +OldValue)
is_better_ref_logic(white, NewValue, OldValue) :- NewValue > OldValue.
is_better_ref_logic(black, NewValue, OldValue) :- NewValue < OldValue.

% =============================================================================
% EVALUATION REFERENCE EXACTE - LOGIQUE BOARD_EVAL.PL
% =============================================================================

% evaluate_reference_logic(+GameState, +Player, -Value)
% Copie EXACTE de la logique board_eval.pl mais sur notre GameState
evaluate_reference_logic(GameState, Player, Value) :-
    % Compter valeurs selon référence exacte
    count_material_ref_logic(GameState, white, WhiteValue),
    count_material_ref_logic(GameState, black, BlackValue),
    
    % Compensation selon référence
    compensate_ref(Player, Comp),
    
    Value is WhiteValue - BlackValue + Comp.

% count_material_ref_logic(+GameState, +Color, -Value)
% Équivalent de count_halfst dans board_eval.pl:14-24
count_material_ref_logic(GameState, Color, TotalValue) :-
    GameState = game_state(Board, _, _, _, _),
    
    % Compter chaque type de pièce séparément comme référence
    count_pieces_ref_logic(Board, Color, pawn, PawnValue),
    count_pieces_ref_logic(Board, Color, rook, RookValue), 
    count_pieces_ref_logic(Board, Color, knight, KnightValue),
    count_pieces_ref_logic(Board, Color, bishop, BishopValue),
    count_pieces_ref_logic(Board, Color, queen, QueenValue),
    
    % Double bonus EXACTEMENT comme référence board_eval.pl:21-24
    find_pieces_positions(Board, Color, rook, RookPositions),
    find_pieces_positions(Board, Color, knight, KnightPositions),
    find_pieces_positions(Board, Color, bishop, BishopPositions),
    
    double_bonus_ref_logic(RookPositions, D1),
    double_bonus_ref_logic(KnightPositions, D2),
    double_bonus_ref_logic(BishopPositions, D3),
    
    TotalValue is PawnValue + RookValue + KnightValue + BishopValue + QueenValue + 30*(D1 + D2 + D3).

% double_bonus_ref_logic(+Positions, -Bonus)
% EXACTEMENT comme board_eval.pl:26-27
double_bonus_ref_logic([_, _], 1) :- !.  % Exactement 2 pièces = bonus
double_bonus_ref_logic(_, 0).

% count_pieces_ref_logic(+Board, +Color, +Type, -Value)
% Équivalent de pos_count dans board_eval.pl:29-33
count_pieces_ref_logic(Board, Color, Type, Value) :-
    findall(PieceValue, (
        between(1, 8, Row),
        between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        Piece \= '.',
        get_piece_color(Piece, Color),
        piece_is_type(Piece, Type),
        pos_value_ref_logic(Type, Row, Col, Color, PieceValue)
    ), Values),
    sum_list(Values, Value).

% find_pieces_positions(+Board, +Color, +Type, -Positions)
% Pour calculer double_bonus
find_pieces_positions(Board, Color, Type, Positions) :-
    findall(Pos, (
        between(1, 8, Row),
        between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        Piece \= '.',
        get_piece_color(Piece, Color),
        piece_is_type(Piece, Type),
        Pos is Row * 10 + Col
    ), Positions).

% piece_is_type(+Piece, +Type)
piece_is_type('P', pawn). piece_is_type('p', pawn).
piece_is_type('R', rook). piece_is_type('r', rook).
piece_is_type('N', knight). piece_is_type('n', knight).
piece_is_type('B', bishop). piece_is_type('b', bishop).
piece_is_type('Q', queen). piece_is_type('q', queen).
piece_is_type('K', king). piece_is_type('k', king).

% pos_value_ref_logic(+Type, +Row, +Col, +Color, -Value)
% COPIE EXACTE de pos_value dans board_eval.pl:35-70
pos_value_ref_logic(Type, Row, Col, black, Value) :-
    % Flip pour les noirs - EXACTEMENT comme board_eval.pl:35-37
    RelRow is 9 - Row,
    pos_value_ref_logic(Type, RelRow, Col, white, Value), !.

% Pions - EXACTEMENT comme board_eval.pl:43-47
pos_value_ref_logic(pawn, Row, Col, white, 127) :-
    Pos is Row * 10 + Col,
    member(Pos, [34,35]), !.  % Cases centrales avancées
pos_value_ref_logic(pawn, Row, Col, white, 131) :-
    Pos is Row * 10 + Col,
    member(Pos, [44,45,54,55]), !.  % Cases centrales fortes
pos_value_ref_logic(pawn, _, _, white, 100) :- !.  % Pion normal

% Roi - EXACTEMENT comme board_eval.pl:48-49
pos_value_ref_logic(king, Row, Col, white, 30) :-
    Pos is Row * 10 + Col,
    member(Pos, [11,12,13,17,18]), !.  % Roi sécurisé
pos_value_ref_logic(king, _, _, white, 0) :- !.

% Tour - EXACTEMENT comme board_eval.pl:50
pos_value_ref_logic(rook, _, _, white, 450) :- !.

% Cavalier - EXACTEMENT comme board_eval.pl:51-56
pos_value_ref_logic(knight, Row, Col, white, Value) :-
    row_value_ref_logic(knight, Row, V1),
    line_value_ref_logic(knight, Col, V2),
    Value is V1 + V2, !.

% Fou et Dame - EXACTEMENT comme board_eval.pl:39-42  
pos_value_ref_logic(bishop, Row, _, white, Value) :-
    row_value_ref_logic(bishop, Row, Value), !.
pos_value_ref_logic(queen, Row, _, white, Value) :-
    row_value_ref_logic(queen, Row, Value), !.

% row_value_ref_logic et line_value_ref_logic - EXACTEMENT comme board_eval.pl:58-74
row_value_ref_logic(knight, 2, 320) :- !.
row_value_ref_logic(knight, 3, 321) :- !.
row_value_ref_logic(knight, X, 348) :- member(X, [4,5]), !.
row_value_ref_logic(knight, X, 376) :- member(X, [6,7]), !.
row_value_ref_logic(knight, _, 290) :- !.

row_value_ref_logic(bishop, 1, 300) :- !.
row_value_ref_logic(bishop, X, 329) :- member(X, [2,3]), !.
row_value_ref_logic(bishop, _, 330) :- !.

row_value_ref_logic(queen, 1, 850) :- !.
row_value_ref_logic(queen, _, 876) :- !.

line_value_ref_logic(knight, X, 0) :- member(X, [1,8]), !.
line_value_ref_logic(knight, _, 10) :- !.

% =============================================================================
% GENERATION COUPS OPTIMISEE
% =============================================================================

% generate_moves_ref_logic(+GameState, +Player, -Moves)
% Génération limitée pour performance 
generate_moves_ref_logic(GameState, Player, Moves) :-
    GameState = game_state(Board, _, _, _, _),
    
    findall([FromRow, FromCol, ToRow, ToCol], (
        between(1, 8, FromRow),
        between(1, 8, FromCol),
        get_piece(Board, FromRow, FromCol, Piece),
        Piece \= '.',
        get_piece_color(Piece, Color),
        Color = Player,  % Utiliser unification directe
        between(1, 8, ToRow),
        between(1, 8, ToCol),
        valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol)
    ), AllMoves),
    
    % Priorité aux coups centraux et développement - comme référence implicite
    prioritize_moves_ref_logic(AllMoves, PrioritizedMoves),
    
    % Limiter pour performance
    take_first_12(PrioritizedMoves, Moves).

% prioritize_moves_ref_logic(+Moves, -PrioritizedMoves)
% Tri basé sur la logique référence (développement > centre)
prioritize_moves_ref_logic(Moves, PrioritizedMoves) :-
    findall(Priority-Move, (
        member(Move, Moves),
        Move = [FromRow, FromCol, ToRow, ToCol],
        calculate_move_priority(FromRow, FromCol, ToRow, ToCol, Priority)
    ), PriorityMoves),
    keysort(PriorityMoves, Sorted),
    reverse(Sorted, ReverseSorted),  % Plus haute priorité en premier
    findall(Move, member(_-Move, ReverseSorted), PrioritizedMoves).

% calculate_move_priority(+FromRow, +FromCol, +ToRow, +ToCol, -Priority)
% Priorité selon logique référence (développement pièces mineures)
calculate_move_priority(FromRow, FromCol, ToRow, ToCol, Priority) :-
    (   % Développement cavalier - haute priorité
        FromRow = 1, member(FromCol, [2,7]), member(ToRow, [3]) ->
        Priority = 300
    ;   FromRow = 8, member(FromCol, [2,7]), member(ToRow, [6]) ->
        Priority = 300
    ;   % Coups vers centre - priorité moyenne
        member(ToRow, [4,5]), member(ToCol, [4,5]) ->
        Priority = 200
    ;   % Développement fou
        FromRow = 1, member(FromCol, [3,6]), member(ToRow, [2,3,4]) ->
        Priority = 250
    ;   FromRow = 8, member(FromCol, [3,6]), member(ToRow, [7,6,5]) ->
        Priority = 250
    ;   % Autres coups
        Priority = 100
    ).

% take_first_12(+List, -First12)
take_first_12(List, First12) :-
    length(List, Len),
    (   Len =< 12 -> First12 = List
    ;   length(First12, 12), append(First12, _, List)
    ).
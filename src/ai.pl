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
:- consult('./piece_values_sophisticated.pl').

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
    % CORRECTION: appeler minimax pour l'adversaire, puis negate
    minimax_simple_ref(NewGameState, Opponent, NewDepth, _, OpponentBestValue),
    % Dans minimax: la valeur retournee par l'adversaire est son meilleur score
    % Notre score est l'oppose de son meilleur score
    Value is -OpponentBestValue.

% is_better_simple(+Player, +NewValue, +OldValue)
% CORRECTION: avec evaluation symetrique, tous cherchent valeur PLUS ELEVEE
is_better_simple(white, NewValue, OldValue) :- NewValue > OldValue.
is_better_simple(black, NewValue, OldValue) :- NewValue > OldValue.

% =============================================================================
% EVALUATION PURE REFERENCE - BOARD_EVAL.PL EXACT
% =============================================================================

% evaluate_pure_reference(+GameState, +Player, -Value)
% CORRECTION CRITIQUE: Evaluation symmetrique pour minimax correct
evaluate_pure_reference(GameState, Player, Value) :-
    count_material_pure_ref(GameState, white, WhiteValue),
    count_material_pure_ref(GameState, black, BlackValue),
    MaterialDiff is WhiteValue - BlackValue,
    % Retourner valeur du point de vue du joueur actuel
    (   Player = white ->
        Value = MaterialDiff
    ;   Value is -MaterialDiff
    ).

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
% TABLES DE REFERENCE CHESS PROGRAMMING WIKI - Adaptées
pos_value_pure_ref(Type, Row, Col, Color, Value) :-
    pos_value_reference(Type, Row, Col, Color, Value), !.

% Fallback pour pieces non définies dans les tables
pos_value_pure_ref(Type, _, _, Color, Value) :-
    piece_reference_value(Type, BaseValue),
    (   Color = white -> Value = BaseValue
    ;   Value is -BaseValue
    ), !.

% =============================================================================
% GENERATION COUPS SIMPLE
% =============================================================================

% generate_moves_simple(+GameState, +Player, -Moves)
generate_moves_simple(GameState, Player, Moves) :-
    GameState = game_state(_, _, MoveCount, _, _),
    
    % Phase d'ouverture (premiers 15 coups) - priorités spéciales
    (   MoveCount =< 15 ->
        generate_opening_moves(GameState, Player, Moves)
    ;   generate_regular_moves(GameState, Player, Moves)
    ).

% generate_opening_moves(+GameState, +Player, -Moves)
% Génération optimisée pour l'ouverture selon principes classiques
generate_opening_moves(GameState, Player, Moves) :-
    GameState = game_state(Board, _, _, _, _),
    
    % 1. PIONS CENTRAUX PRIORITAIRES (e4, d4, e5, d5)
    findall([FromRow, FromCol, ToRow, ToCol], (
        between(1, 8, FromRow),
        between(1, 8, FromCol),
        get_piece(Board, FromRow, FromCol, Piece),
        Piece \= '.',
        get_piece_color(Piece, Player),
        member(Piece, ['P','p']),
        between(1, 8, ToRow),
        between(1, 8, ToCol),
        valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol),
        % FILTRE: Seulement pions centraux d4, e4, d5, e5
        member([FromCol, ToCol], [[4,4], [5,5]])  % d et e colonnes
    ), CentralPawnMoves),
    
    % 2. DÉVELOPPEMENT PIÈCES MINEURES (cavaliers puis fous)
    findall([FromRow, FromCol, ToRow, ToCol], (
        between(1, 8, FromRow),
        between(1, 8, FromCol),
        get_piece(Board, FromRow, FromCol, Piece),
        Piece \= '.',
        get_piece_color(Piece, Player),
        member(Piece, ['N','n','B','b']),  
        between(1, 8, ToRow),
        between(1, 8, ToCol),
        valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol),
        % FILTRE: Éviter développements sur bords
        ToCol >= 2, ToCol =< 7,  % Pas colonnes a/h
        ToRow >= 3, ToRow =< 6   % Positions centrales
    ), DevelopmentMoves),
    
    % 3. COUPS DE PIONS SECONDAIRES (seulement 2 cases si possible)
    findall([FromRow, FromCol, ToRow, ToCol], (
        between(1, 8, FromRow),
        between(1, 8, FromCol),
        get_piece(Board, FromRow, FromCol, Piece),
        Piece \= '.',
        get_piece_color(Piece, Player),
        member(Piece, ['P','p']),
        between(1, 8, ToRow),
        between(1, 8, ToCol),
        valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol),
        % FILTRE: Éviter pions faibles (f6, g6, h6 pour noirs)
        \+ member([FromCol, ToRow], [[6,6], [7,6], [8,6]]),  % Pas f6, g6, h6
        % Préférer coups 2 cases sur flancs
        (   abs(ToRow - FromRow) =:= 2  % Coup 2 cases
        ;   member(FromCol, [3,4,5,6])  % Ou pions centraux
        )
    ), SecondaryPawnMoves),
    
    % 4. AUTRES COUPS (tour, dame, roi)
    findall([FromRow, FromCol, ToRow, ToCol], (
        between(1, 8, FromRow),
        between(1, 8, FromCol),
        get_piece(Board, FromRow, FromCol, Piece),
        Piece \= '.',
        get_piece_color(Piece, Player),
        member(Piece, ['R','r','Q','q','K','k']),
        between(1, 8, ToRow),
        between(1, 8, ToCol),
        valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol)
    ), OtherMoves),
    
    % Priorité: Centraux > Développement > Pions secondaires > Autres
    append(CentralPawnMoves, DevelopmentMoves, Priority1),
    append(Priority1, SecondaryPawnMoves, Priority2), 
    append(Priority2, OtherMoves, AllMoves),
    
    take_first_20_simple(AllMoves, Moves).

% generate_regular_moves(+GameState, +Player, -Moves)  
% Génération standard pour milieu/fin de partie
generate_regular_moves(GameState, Player, Moves) :-
    GameState = game_state(Board, _, _, _, _),
    
    % Génération standard sans restrictions d'ouverture
    findall([FromRow, FromCol, ToRow, ToCol], (
        between(1, 8, FromRow),
        between(1, 8, FromCol),
        get_piece(Board, FromRow, FromCol, Piece),
        Piece \= '.',
        get_piece_color(Piece, Player),
        between(1, 8, ToRow),
        between(1, 8, ToCol),
        valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol)
    ), AllMoves),
    
    take_first_20_simple(AllMoves, Moves).

% take_first_20_simple(+List, -First20)
take_first_20_simple(List, First20) :-
    length(List, Len),
    (   Len =< 20 -> First20 = List
    ;   length(First20, 20), append(First20, _, List)
    ).
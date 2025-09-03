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

% =============================================================================
% COUPS D'OUVERTURE FIXES - CARO-KANN/SLAV DEFENSE
% =============================================================================

% use_fixed_opening(+MoveCount)
% Determine si on utilise les coups fixes (premiers 2 coups des noirs)
use_fixed_opening(1).  % Premier coup des noirs (c7-c6)
use_fixed_opening(3).  % Deuxieme coup des noirs (d7-d5)

% get_fixed_opening_move(+MoveCount, +Board, -Move)
% Retourne le coup d'ouverture fixe selon le nombre de coups
get_fixed_opening_move(1, Board, [7, 3, 6, 3]) :-
    % Premier coup: c7-c6 (rang 7 col 3 vers rang 6 col 3)
    get_piece(Board, 7, 3, 'p'),  % Verification qu'il y a bien un pion noir en c7
    get_piece(Board, 6, 3, ' ').  % Verification que c6 est libre (espace)

get_fixed_opening_move(3, Board, [7, 4, 5, 4]) :-
    % Deuxieme coup: d7-d5 (rang 7 col 4 vers rang 5 col 4)
    get_piece(Board, 7, 4, 'p'),  % Verification qu'il y a bien un pion noir en d7
    get_piece(Board, 6, 4, ' '),  % Verification que d6 est libre (espace)
    get_piece(Board, 5, 4, ' ').  % Verification que d5 est libre (espace)


% choose_ai_move(+GameState, -BestMove)
choose_ai_move(GameState, BestMove) :-
    GameState = game_state(Board, Player, MoveCount, _, _),
    
    % COUPS D'OUVERTURE FIXES pour les noirs (Caro-Kann/Slav)
    (   Player = black, use_fixed_opening(MoveCount) ->
        (   get_fixed_opening_move(MoveCount, Board, BestMove) ->
            true  % Coup fixe reussi
        ;   minimax_simple_ref(GameState, Player, 2, BestMove, _Value)  % Fallback
        )
    ;   % Utiliser minimax pour tous les autres cas
        minimax_simple_ref(GameState, Player, 2, BestMove, _Value)
    ).

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
    
    % Compter EXACTEMENT comme référence + ROI AJOUTÉ
    count_pieces_type(Board, Color, pawn, V1),
    count_pieces_type(Board, Color, rook, V2),
    count_pieces_type(Board, Color, knight, V3),
    count_pieces_type(Board, Color, bishop, V4),
    count_pieces_type(Board, Color, queen, V5),
    count_pieces_type(Board, Color, king, V6),  % ROI AJOUTÉ !
    
    % Double bonus EXACTEMENT comme board_eval.pl:21-24
    count_pieces_positions(Board, Color, rook, RookCount),
    count_pieces_positions(Board, Color, knight, KnightCount), 
    count_pieces_positions(Board, Color, bishop, BishopCount),
    
    double_bonus_pure(RookCount, D1),
    double_bonus_pure(KnightCount, D2),
    double_bonus_pure(BishopCount, D3),
    
    TotalValue is V1 + V2 + V3 + V4 + V5 + V6 + 30*(D1 + D2 + D3).

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
% TABLES DE REFERENCE AVEC BONUS DÉVELOPPEMENT OUVERTURE
pos_value_pure_ref(Type, Row, Col, Color, Value) :-
    pos_value_reference(Type, Row, Col, Color, BaseValue),
    % BONUS DÉVELOPPEMENT EN OUVERTURE
    (   (Type = knight; Type = bishop),
        is_development_square(Row, Col, Type, Color) ->
        % BONUS MASSIF pour développements naturels
        DevelopmentBonus = 100,
        Value is BaseValue + DevelopmentBonus
    ;   Value = BaseValue
    ), !.

% Fallback pour pieces non définies dans les tables
pos_value_pure_ref(Type, _, _, Color, Value) :-
    piece_reference_value(Type, BaseValue),
    (   Color = white -> Value = BaseValue
    ;   Value is -BaseValue
    ), !.

% is_development_square(+Row, +Col, +Type, +Color)
% Cases de développement naturel
is_development_square(6, 3, knight, black).  % Nc6
is_development_square(6, 6, knight, black).  % Nf6
is_development_square(3, 3, knight, white).  % Nc3
is_development_square(3, 6, knight, white).  % Nf3
is_development_square(6, 4, bishop, black).  % Bd6
is_development_square(6, 5, bishop, black).  % Be6
is_development_square(3, 4, bishop, white).  % Bd3
is_development_square(3, 5, bishop, white).  % Be3.

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
% Génération équilibrée pour l'ouverture - DÉVELOPPEMENT PRIORITAIRE
generate_opening_moves(GameState, Player, Moves) :-
    GameState = game_state(Board, _, _, _, _),
    
    % 1. DÉVELOPPEMENT PRIORITAIRE (cavaliers et fous AVANT tout)
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
        % DÉVELOPPEMENTS NATURELS: c6, d6, e6, f6 pour cavaliers et fous
        ToRow >= 3, ToRow =< 6,    % Rangs centraux
        ToCol >= 3, ToCol =< 6     % Colonnes centrales
    ), DevelopmentMoves),
    
    % Éliminer les doublons de développement
    remove_duplicates_simple(DevelopmentMoves, UniqueDevelopment),
    
    % 2. PIONS CENTRAUX (d4, e4, d5, e5)
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
        member(ToCol, [4,5]),  % Colonnes d et e
        member(ToRow, [4,5])   % Rangs 4 et 5
    ), CentralPawnMoves),
    
    % 3. PIONS SUPPORT ÉLARGIS (c6, d6, e6, f6 - plus restrictif sur flancs)
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
        % ÉLARGI: Colonnes centrales c,d,e,f (pas seulement c,f)
        member(ToCol, [3,4,5,6]),  % c, d, e, f
        member(ToRow, [6,5]),      % 6e rang (noirs) ou 5e rang si nécessaire  
        abs(ToRow - FromRow) =< 2  % 1 ou 2 cases maximum
    ), SupportPawnMoves),
    
    % 4. AUTRES COUPS
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
    
    % PRIORITÉ CORRECTE: DÉVELOPPEMENT EN PREMIER !
    take_first_8_simple(UniqueDevelopment, PriorityDevelopment),
    take_first_3_simple(CentralPawnMoves, LimitedCentral),
    take_first_4_simple(SupportPawnMoves, LimitedSupport),
    
    % ORDRE CORRECT: Développement d'abord, puis pions
    append(PriorityDevelopment, LimitedCentral, Priority1),
    append(Priority1, LimitedSupport, Priority2),
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

% take_first_N_simple(+List, +N, -FirstN)
take_first_20_simple(List, First20) :-
    take_first_n_simple(List, 20, First20).

take_first_10_simple(List, First10) :-
    take_first_n_simple(List, 10, First10).

take_first_8_simple(List, First8) :-
    take_first_n_simple(List, 8, First8).

take_first_5_simple(List, First5) :-
    take_first_n_simple(List, 5, First5).

take_first_4_simple(List, First4) :-
    take_first_n_simple(List, 4, First4).

take_first_3_simple(List, First3) :-
    take_first_n_simple(List, 3, First3).

take_first_n_simple(List, N, FirstN) :-
    length(List, Len),
    (   Len =< N -> FirstN = List
    ;   length(FirstN, N), append(FirstN, _, List)
    ).

% remove_duplicates_simple(+List, -UniqueList)
% Supprime les doublons d'une liste
remove_duplicates_simple([], []).
remove_duplicates_simple([H|T], [H|UniqueT]) :-
    \+ member(H, T),
    remove_duplicates_simple(T, UniqueT).
remove_duplicates_simple([H|T], UniqueT) :-
    member(H, T),
    remove_duplicates_simple(T, UniqueT).
% =============================================================================
% IA MINIMAX SIMPLE - SANS COMPLICATIONS
% =============================================================================
%
% Version ULTRA SIMPLIFIEE du minimax:
% - Profondeur 2 exacte
% - Alpha-beta basique sans variables problématiques
% - Evaluation référence
% - Performance acceptable
%
% =============================================================================

:- [pieces].
:- [board].
:- [game].

% =============================================================================
% INTERFACE PRINCIPALE
% =============================================================================

% choose_ai_move(+GameState, -BestMove)
choose_ai_move(GameState, BestMove) :-
    GameState = game_state(_, Player, _, _, _),
    
    % Minimax avec alpha-beta profondeur 2
    Alpha = -10000,
    Beta = 10000,
    minimax_alpha_beta(GameState, Player, 2, Alpha, Beta, BestMove, _Value).

% =============================================================================
% MINIMAX AVEC ALPHA-BETA PRUNING
% =============================================================================

% minimax_alpha_beta(+GameState, +Player, +Depth, +Alpha, +Beta, -BestMove, -BestValue)
% Minimax avec élagage alpha-beta
minimax_alpha_beta(GameState, Player, 0, _, _, [], Value) :-
    % Cas de base - évaluation
    evaluate_simple(GameState, Player, Value), !.

minimax_alpha_beta(GameState, Player, Depth, Alpha, Beta, BestMove, BestValue) :-
    Depth > 0,
    generate_moves_limited(GameState, Player, Moves),
    
    (   Moves = [] ->
        evaluate_simple(GameState, Player, BestValue),
        BestMove = []
    ;   % Recherche avec alpha-beta
        init_best_value_for_player(Player, InitValue),
        search_with_alpha_beta(GameState, Moves, Player, Depth, Alpha, Beta, InitValue, [], BestMove, BestValue)
    ).

% search_with_alpha_beta(+GameState, +Moves, +Player, +Depth, +Alpha, +Beta, +CurrentBest, +CurrentMove, -BestMove, -BestValue)
% Recherche avec élagage alpha-beta
search_with_alpha_beta(_, [], _, _, _, _, CurrentBest, CurrentMove, CurrentMove, CurrentBest) :- !.

search_with_alpha_beta(GameState, [Move|RestMoves], Player, Depth, Alpha, Beta, CurrentBest, CurrentMove, BestMove, BestValue) :-
    Move = [FromRow, FromCol, ToRow, ToCol],
    make_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState),
    
    opposite_player(Player, Opponent),
    NewDepth is Depth - 1,
    minimax_alpha_beta(NewGameState, Opponent, NewDepth, Alpha, Beta, _, OpponentValue),
    MoveValue is -OpponentValue,
    
    % Mise à jour du meilleur coup et alpha/beta
    (   is_better_for_player(Player, MoveValue, CurrentBest) ->
        NewCurrentBest = MoveValue,
        NewCurrentMove = Move,
        % Mise à jour alpha/beta selon joueur
        (   Player = white ->
            NewAlpha is max(Alpha, MoveValue),
            NewBeta = Beta
        ;   NewAlpha = Alpha,
            NewBeta is min(Beta, MoveValue)
        )
    ;   NewCurrentBest = CurrentBest,
        NewCurrentMove = CurrentMove,
        NewAlpha = Alpha,
        NewBeta = Beta
    ),
    
    % Test de coupure alpha-beta
    (   NewAlpha >= NewBeta ->
        % Coupure - pas besoin d'explorer les autres coups
        BestMove = NewCurrentMove,
        BestValue = NewCurrentBest
    ;   % Continuer la recherche
        search_with_alpha_beta(GameState, RestMoves, Player, Depth, NewAlpha, NewBeta, NewCurrentBest, NewCurrentMove, BestMove, BestValue)
    ).

% init_best_value_for_player(+Player, -Value)
% Valeur initiale worst-case pour le joueur
init_best_value_for_player(white, -10000).
init_best_value_for_player(black, 10000).

% =============================================================================
% MINIMAX SIMPLE SANS ALPHA-BETA (GARDE POUR REFERENCE)
% =============================================================================

% minimax_simple(+GameState, +Player, +Depth, -BestMove, -BestValue)
% Minimax sans alpha-beta pour tester d'abord
minimax_simple(GameState, Player, 0, [], Value) :-
    % Cas de base - évaluation
    evaluate_simple(GameState, Player, Value), !.

minimax_simple(GameState, Player, Depth, BestMove, BestValue) :-
    Depth > 0,
    generate_moves_limited(GameState, Player, Moves),
    
    (   Moves = [] ->
        evaluate_simple(GameState, Player, BestValue),
        BestMove = []
    ;   evaluate_all_moves_simple(GameState, Moves, Player, Depth, BestMove, BestValue)
    ).

% evaluate_all_moves_simple(+GameState, +Moves, +Player, +Depth, -BestMove, -BestValue)
% Évalue tous les coups sans alpha-beta
evaluate_all_moves_simple(GameState, [Move], Player, Depth, Move, Value) :-
    % Un seul coup - pas de comparaison
    execute_move_and_evaluate(GameState, Move, Player, Depth, Value), !.

evaluate_all_moves_simple(GameState, [FirstMove|RestMoves], Player, Depth, BestMove, BestValue) :-
    % Premier coup
    execute_move_and_evaluate(GameState, FirstMove, Player, Depth, FirstValue),
    
    % Évaluer les autres coups
    evaluate_remaining_moves(GameState, RestMoves, Player, Depth, FirstValue, FirstMove, BestMove, BestValue).

% evaluate_remaining_moves(+GameState, +Moves, +Player, +Depth, +CurrentBest, +CurrentMove, -BestMove, -BestValue)
evaluate_remaining_moves(_, [], _, _, CurrentBest, CurrentMove, CurrentMove, CurrentBest) :- !.

evaluate_remaining_moves(GameState, [Move|RestMoves], Player, Depth, CurrentBest, CurrentMove, BestMove, BestValue) :-
    execute_move_and_evaluate(GameState, Move, Player, Depth, MoveValue),
    
    % Comparer avec le meilleur actuel
    (   is_better_for_player(Player, MoveValue, CurrentBest) ->
        NewBest = MoveValue,
        NewMove = Move
    ;   NewBest = CurrentBest,
        NewMove = CurrentMove
    ),
    
    evaluate_remaining_moves(GameState, RestMoves, Player, Depth, NewBest, NewMove, BestMove, BestValue).

% execute_move_and_evaluate(+GameState, +Move, +Player, +Depth, -Value)
% Exécute un coup et évalue récursivement
execute_move_and_evaluate(GameState, [FromRow, FromCol, ToRow, ToCol], Player, Depth, Value) :-
    make_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState),
    opposite_player(Player, Opponent),
    NewDepth is Depth - 1,
    minimax_simple(NewGameState, Opponent, NewDepth, _, OpponentValue),
    Value is -OpponentValue.  % Inverser car c'est le tour de l'adversaire

% =============================================================================
% GÉNÉRATION DE COUPS LIMITÉE
% =============================================================================

% generate_moves_limited(+GameState, +Player, -Moves)
% Génère un nombre limité de coups pour performance avec randomisation
generate_moves_limited(GameState, Player, LimitedMoves) :-
    GameState = game_state(Board, _, _, _, _),
    
    % Génération rapide - max 15 coups pour performance
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
    
    % AJOUT: Randomisation pour variété
    random_permutation(AllMoves, ShuffledMoves),
    
    % Prendre les 15 premiers coups randomisés
    take_first_15(ShuffledMoves, LimitedMoves).

% take_first_15(+List, -First15)
take_first_15(List, First15) :-
    length(List, Len),
    (   Len =< 15 ->
        First15 = List
    ;   length(First15, 15),
        append(First15, _, List)
    ).

% =============================================================================
% ÉVALUATION SIMPLE BASÉE SUR RÉFÉRENCE
% =============================================================================

% evaluate_simple(+GameState, +Player, -Value)
% Évaluation basée sur board_eval.pl
evaluate_simple(GameState, Player, Value) :-
    count_material_ref(GameState, white, WhiteScore),
    count_material_ref(GameState, black, BlackScore),
    
    % Compensation selon joueur (référence: compensate/2)
    (   Player = white -> Comp = 15
    ;   Comp = -15
    ),
    
    Value is WhiteScore - BlackScore + Comp.

% count_material_ref(+GameState, +Color, -Score)
% Compte matériel selon valeurs référence
count_material_ref(GameState, Color, Score) :-
    GameState = game_state(Board, _, _, _, _),
    findall(PieceScore, (
        between(1, 8, Row),
        between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        Piece \= '.',
        get_piece_color(Piece, Color),
        piece_score_ref(Piece, Row, Col, PieceScore)
    ), Scores),
    sum_list(Scores, Score).

% piece_score_ref(+Piece, +Row, +Col, -Score)
% Scores EXACTS de la référence
piece_score_ref('P', Row, Col, Score) :-
    Pos is Row * 10 + Col,
    (   member(Pos, [34,35]) -> Score = 127
    ;   member(Pos, [44,45,54,55]) -> Score = 131
    ;   Score = 100
    ).
piece_score_ref('p', Row, Col, Score) :-
    FlippedRow is 9 - Row,
    piece_score_ref('P', FlippedRow, Col, TempScore),
    Score is -TempScore.

piece_score_ref('R', _, _, 450).
piece_score_ref('r', _, _, -450).

piece_score_ref('N', Row, Col, Score) :-
    (   Row = 2 -> V1 = 320
    ;   Row = 3 -> V1 = 321
    ;   member(Row, [4,5]) -> V1 = 348
    ;   member(Row, [6,7]) -> V1 = 376
    ;   V1 = 290
    ),
    (   member(Col, [1,8]) -> V2 = 0
    ;   V2 = 10
    ),
    Score is V1 + V2.
piece_score_ref('n', Row, Col, Score) :-
    FlippedRow is 9 - Row,
    piece_score_ref('N', FlippedRow, Col, TempScore),
    Score is -TempScore.

piece_score_ref('B', Row, _, Score) :-
    (   Row = 1 -> Score = 300
    ;   member(Row, [2,3]) -> Score = 329
    ;   Score = 330
    ).
piece_score_ref('b', Row, Col, Score) :-
    FlippedRow is 9 - Row,
    piece_score_ref('B', FlippedRow, Col, TempScore),
    Score is -TempScore.

piece_score_ref('Q', Row, _, Score) :-
    (   Row = 1 -> Score = 850
    ;   Score = 876
    ).
piece_score_ref('q', Row, Col, Score) :-
    FlippedRow is 9 - Row,
    piece_score_ref('Q', FlippedRow, Col, TempScore),
    Score is -TempScore.

piece_score_ref('K', Row, Col, Score) :-
    Pos is Row * 10 + Col,
    (   member(Pos, [11,12,13,17,18]) -> Score = 30
    ;   Score = 0
    ).
piece_score_ref('k', Row, Col, Score) :-
    FlippedRow is 9 - Row,
    piece_score_ref('K', FlippedRow, Col, TempScore),
    Score is -TempScore.

% =============================================================================
% UTILITAIRES
% =============================================================================

% is_better_for_player(+Player, +NewValue, +OldValue)
is_better_for_player(white, NewValue, OldValue) :- NewValue > OldValue.
is_better_for_player(black, NewValue, OldValue) :- NewValue < OldValue.
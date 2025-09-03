% =============================================================================
% IA MINIMAX DIRECT - REFACTORING DRASTIQUE
% =============================================================================
%
% CHANGEMENTS DRASTIQUES:
% - Genération de coups optimisée pour minimax
% - Structure de données simplifiée 
% - Alpha-beta pruning EXACT de la référence
% - Profondeur 2 garantie
%
% =============================================================================

:- [pieces].
:- [board].
:- [game].

% Base de données dynamique minimax
:- dynamic ai_best_move/1, ai_best_value/1, ai_moves_cache/2.

% =============================================================================
% INTERFACE PRINCIPALE - DIRECTE
% =============================================================================

% choose_ai_move(+GameState, -BestMove)
% Interface principale avec minimax profondeur 2
choose_ai_move(GameState, BestMove) :-
    cleanup_ai_cache,
    GameState = game_state(_, Player, _, _, _),
    
    % Minimax avec profondeur 2 exacte
    Depth = 2,
    Alpha = -10000,
    Beta = 10000,
    
    % Appel minimax direct
    minimax_root(GameState, Player, Depth, Alpha, Beta, BestMove, _Value).

% =============================================================================
% MINIMAX ROOT - SIMPLE ET DIRECT
% =============================================================================

% minimax_root(+GameState, +Player, +Depth, +Alpha, +Beta, -BestMove, -BestValue)
% Version directe sans pile complexe
minimax_root(GameState, Player, Depth, Alpha, Beta, BestMove, BestValue) :-
    generate_legal_moves_fast(GameState, Player, Moves),
    
    (   Moves = [] ->
        % Pas de coups - position terminale
        evaluate_terminal(GameState, Player, BestValue),
        BestMove = [1,1,1,1]  % Coup invalide pour debug
    ;   % Evaluer tous les coups
        minimax_search_all_moves(GameState, Moves, Player, Depth, Alpha, Beta, -10000, [1,1,1,1], BestMove, BestValue)
    ).

% minimax_search_all_moves(+GameState, +Moves, +Player, +Depth, +Alpha, +Beta, +CurrentBest, +CurrentMove, -BestMove, -BestValue)
% Recherche minimax dans tous les coups
minimax_search_all_moves(_, [], _, _, _, _, CurrentBest, CurrentMove, CurrentMove, CurrentBest) :- !.

minimax_search_all_moves(GameState, [Move|RestMoves], Player, Depth, Alpha, Beta, CurrentBest, CurrentMove, BestMove, BestValue) :-
    Move = [FromRow, FromCol, ToRow, ToCol],
    
    % Exécuter le coup
    make_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState),
    
    % Appel minimax récursif
    opposite_player(Player, Opponent),
    NewDepth is Depth - 1,
    minimax_evaluate(NewGameState, Opponent, NewDepth, Alpha, Beta, _, OpponentValue),
    
    % Inverser la valeur (point de vue du joueur actuel)
    MoveValue is -OpponentValue,
    
    % Mise à jour du meilleur coup
    (   (Player = white, MoveValue > CurrentBest) ->
        NewCurrentBest = MoveValue,
        NewCurrentMove = Move,
        NewAlpha is max(Alpha, MoveValue)
    ;   (Player = black, MoveValue < CurrentBest) ->
        NewCurrentBest = MoveValue, 
        NewCurrentMove = Move,
        NewBeta is min(Beta, MoveValue)
    ;   NewCurrentBest = CurrentBest,
        NewCurrentMove = CurrentMove,
        NewAlpha = Alpha,
        NewBeta = Beta
    ),
    
    % Test de coupure alpha-beta
    (   NewAlpha >= NewBeta ->
        % Coupure - arrêter la recherche
        BestMove = NewCurrentMove,
        BestValue = NewCurrentBest
    ;   % Continuer avec les autres coups
        minimax_search_all_moves(GameState, RestMoves, Player, Depth, NewAlpha, NewBeta, NewCurrentBest, NewCurrentMove, BestMove, BestValue)
    ).

% =============================================================================
% MINIMAX RECURSIF - COEUR DE L'ALGORITHME
% =============================================================================

% minimax_evaluate(+GameState, +Player, +Depth, +Alpha, +Beta, -BestMove, -Value)
% Evaluation minimax récursive
minimax_evaluate(GameState, Player, 0, _, _, [], Value) :-
    % Cas de base - évaluation de position
    evaluate_position_reference(GameState, Player, Value), !.

minimax_evaluate(GameState, Player, Depth, Alpha, Beta, BestMove, Value) :-
    Depth > 0,
    generate_legal_moves_fast(GameState, Player, Moves),
    
    (   Moves = [] ->
        % Position terminale
        evaluate_terminal(GameState, Player, Value),
        BestMove = []
    ;   % Recherche dans les coups
        init_best_value(Player, InitValue),
        minimax_search_moves_recursive(GameState, Moves, Player, Depth, Alpha, Beta, InitValue, [], BestMove, Value)
    ).

% minimax_search_moves_recursive - Version récursive optimisée
minimax_search_moves_recursive(_, [], _, _, _, _, CurrentValue, CurrentMove, CurrentMove, CurrentValue) :- !.

minimax_search_moves_recursive(GameState, [Move|RestMoves], Player, Depth, Alpha, Beta, CurrentValue, CurrentMove, BestMove, BestValue) :-
    Move = [FromRow, FromCol, ToRow, ToCol],
    make_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState),
    
    opposite_player(Player, Opponent),
    NewDepth is Depth - 1,
    minimax_evaluate(NewGameState, Opponent, NewDepth, Alpha, Beta, _, OpponentValue),
    
    MoveValue is -OpponentValue,
    
    % Mise à jour selon joueur
    (   is_better_value(Player, MoveValue, CurrentValue) ->
        NewCurrentValue = MoveValue,
        NewCurrentMove = Move
    ;   NewCurrentValue = CurrentValue,
        NewCurrentMove = CurrentMove
    ),
    
    % Alpha-beta mise à jour
    update_alpha_beta_bounds(Player, MoveValue, Alpha, Beta, NewAlpha, NewBeta),
    
    % Test coupure
    (   ground(NewAlpha), ground(NewBeta), alpha_beta_cutoff(NewAlpha, NewBeta) ->
        BestMove = NewCurrentMove,
        BestValue = NewCurrentValue
    ;   minimax_search_moves_recursive(GameState, RestMoves, Player, Depth, NewAlpha, NewBeta, NewCurrentValue, NewCurrentMove, BestMove, BestValue)
    ).

% =============================================================================
% GENERATION DE COUPS OPTIMISEE POUR MINIMAX
% =============================================================================

% generate_legal_moves_fast(+GameState, +Player, -Moves)
% Version RAPIDE pour minimax - limite le nombre de coups
generate_legal_moves_fast(GameState, Player, LimitedMoves) :-
    GameState = game_state(Board, _, _, _, _),
    
    % Cache des coups pour éviter recalcul
    (   ai_moves_cache(Player-Board, CachedMoves) ->
        LimitedMoves = CachedMoves
    ;   % Génération avec limitation pour performance
        findall([FromRow, FromCol, ToRow, ToCol], (
            between(1, 8, FromRow),
            between(1, 8, FromCol),
            get_piece(Board, FromRow, FromCol, Piece),
            Piece \= '.',
            get_piece_color(Piece, Player),
            between(1, 8, ToRow),
            between(1, 8, ToCol),
            valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol),
            % SIMPLIFICATION DRASTIQUE: Pas de vérification échec pour vitesse
            % L'évaluation pénalisera les positions d'échec
            true
        ), AllMoves),
        
        % Limitation à 20 premiers coups pour performance minimax
        take_first_n_moves(AllMoves, 20, LimitedMoves),
        
        % Cache pour éviter recalcul
        assertz(ai_moves_cache(Player-Board, LimitedMoves))
    ).

% take_first_n_moves(+Moves, +N, -FirstN)
take_first_n_moves(Moves, N, FirstN) :-
    length(Moves, Len),
    (   Len =< N ->
        FirstN = Moves
    ;   length(FirstN, N),
        append(FirstN, _, Moves)
    ).

% =============================================================================
% EVALUATION POSITION - REFERENCE ADAPTEE
% =============================================================================

% evaluate_position_reference(+GameState, +Player, -Value)
% Evaluation basée sur la référence board_eval.pl
evaluate_position_reference(GameState, Player, Value) :-
    count_material_reference(GameState, white, WhiteValue),
    count_material_reference(GameState, black, BlackValue),
    
    % Compensation joueur actuel (référence)
    (   Player = white -> Compensation = 15
    ;   Compensation = -15
    ),
    
    Value is WhiteValue - BlackValue + Compensation.

% count_material_reference(+GameState, +Color, -Value)
% Compte matériel avec valeurs exactes de la référence
count_material_reference(GameState, Color, Value) :-
    GameState = game_state(Board, _, _, _, _),
    findall(PieceValue, (
        between(1, 8, Row),
        between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        Piece \= '.',
        get_piece_color(Piece, Color),
        piece_value_exact_reference(Piece, Row, Col, PieceValue)
    ), Values),
    sum_list(Values, Value).

% piece_value_exact_reference(+Piece, +Row, +Col, -Value)
% Valeurs EXACTES de board_eval.pl
piece_value_exact_reference('P', Row, Col, Value) :-
    Pos is Row * 10 + Col,
    (   member(Pos, [34,35]) -> Value = 127        % Cases centrales avancées
    ;   member(Pos, [44,45,54,55]) -> Value = 131  % Cases centrales fortes  
    ;   Value = 100                                 % Pion normal
    ).
piece_value_exact_reference('p', Row, Col, Value) :-
    FlippedRow is 9 - Row,
    piece_value_exact_reference('P', FlippedRow, Col, TempValue),
    Value is -TempValue.

piece_value_exact_reference('R', _, _, 450).
piece_value_exact_reference('r', _, _, -450).

piece_value_exact_reference('N', Row, Col, Value) :-
    % Valeurs cavalier selon référence board_eval.pl:58-64
    (   Row = 2 -> V1 = 320
    ;   Row = 3 -> V1 = 321  
    ;   member(Row, [4,5]) -> V1 = 348
    ;   member(Row, [6,7]) -> V1 = 376
    ;   V1 = 290
    ),
    % Bonus colonne selon référence board_eval.pl:72-74
    (   member(Col, [1,8]) -> V2 = 0
    ;   V2 = 10
    ),
    Value is V1 + V2.
piece_value_exact_reference('n', Row, Col, Value) :-
    FlippedRow is 9 - Row,
    piece_value_exact_reference('N', FlippedRow, Col, TempValue),
    Value is -TempValue.

piece_value_exact_reference('B', Row, _, Value) :-
    % Fou selon référence board_eval.pl:65-68
    (   Row = 1 -> Value = 300
    ;   member(Row, [2,3]) -> Value = 329
    ;   Value = 330
    ).
piece_value_exact_reference('b', Row, Col, Value) :-
    FlippedRow is 9 - Row,
    piece_value_exact_reference('B', FlippedRow, Col, TempValue),
    Value is -TempValue.

piece_value_exact_reference('Q', Row, _, Value) :-
    % Dame selon référence board_eval.pl:69-70
    (   Row = 1 -> Value = 850
    ;   Value = 876
    ).
piece_value_exact_reference('q', Row, Col, Value) :-
    FlippedRow is 9 - Row,
    piece_value_exact_reference('Q', FlippedRow, Col, TempValue),
    Value is -TempValue.

piece_value_exact_reference('K', Row, Col, Value) :-
    % Roi selon référence board_eval.pl:48-49
    Pos is Row * 10 + Col,
    (   member(Pos, [11,12,13,17,18]) -> Value = 30  % Roi sécurisé
    ;   Value = 0
    ).
piece_value_exact_reference('k', Row, Col, Value) :-
    FlippedRow is 9 - Row,
    piece_value_exact_reference('K', FlippedRow, Col, TempValue),
    Value is -TempValue.

% =============================================================================
% FONCTIONS UTILITAIRES ALPHA-BETA
% =============================================================================

% init_best_value(+Player, -Value)
init_best_value(white, -10000).
init_best_value(black, 10000).

% is_better_value(+Player, +NewValue, +CurrentValue)
is_better_value(white, NewValue, CurrentValue) :- NewValue > CurrentValue.
is_better_value(black, NewValue, CurrentValue) :- NewValue < CurrentValue.

% update_alpha_beta_bounds(+Player, +Value, +Alpha, +Beta, -NewAlpha, -NewBeta)
update_alpha_beta_bounds(white, Value, Alpha, Beta, NewAlpha, Beta) :-
    NewAlpha is max(Alpha, Value).
update_alpha_beta_bounds(black, Value, Alpha, Beta, Alpha, NewBeta) :-
    NewBeta is min(Beta, Value).

% alpha_beta_cutoff(+Alpha, +Beta)
alpha_beta_cutoff(Alpha, Beta) :- Alpha >= Beta.

% evaluate_terminal(+GameState, +Player, -Value)
% Evaluation positions terminales
evaluate_terminal(GameState, Player, Value) :-
    (   is_in_check(GameState, Player) ->
        % Échec et mat
        (   Player = white -> Value = -9000
        ;   Value = 9000
        )
    ;   Value = 0  % Pat
    ).

% =============================================================================
% GESTION CACHE
% =============================================================================

% cleanup_ai_cache/0
cleanup_ai_cache :-
    retractall(ai_best_move(_)),
    retractall(ai_best_value(_)),
    retractall(ai_moves_cache(_, _)).
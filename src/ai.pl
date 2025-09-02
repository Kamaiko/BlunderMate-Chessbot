% =============================================================================
% INTELLIGENCE ARTIFICIELLE - MINIMAX AVEC ALPHA-BETA OPTIMISE
% =============================================================================
% 
% IA robuste avec algorithme minimax et alpha-beta pruning
% Inspire du code d'exemple fourni avec optimisations pour echecs
% - Minimax recursif avec alpha-beta pruning
% - Evaluation heuristique avancee
% - Performance optimisee (profondeur 2-3)
%
% Auteur : Patrick Patenaude
% Version : 3.0 (Minimax Optimise)
% =============================================================================

:- [pieces].
:- [board].
:- [game].

% Base de donnees dynamique pour alpha-beta
:- dynamic ai_stack/3, ai_top/1, ai_best_move/1, ai_best_value/1.

% =============================================================================
% INTERFACE PRINCIPALE IA
% =============================================================================

% choose_ai_move(+GameState, -BestMove)
% Point d'entree principal avec timeout de securite
choose_ai_move(GameState, BestMove) :-
    cleanup_ai_data,
    init_ai_stack,
    GameState = game_state(_, Player, _, _, _),
    ai_search_depth(Depth),
    worst_value(Player, Alpha),
    best_value(Player, Beta),
    
    % Timeout de securite - 3 secondes maximum
    (   call_with_time_limit(3, 
            minimax_root(GameState, Player, Depth, Alpha, Beta, BestMove, _)
        )
    ->  true
    ;   % Timeout ou echec - choix d'urgence
        write('TIMEOUT ou ERREUR - Selection coup d\'urgence'), nl,
        emergency_move_selection(GameState, BestMove)
    ),
    cleanup_ai_data.

% =============================================================================
% ALGORITHME MINIMAX AVEC ALPHA-BETA
% =============================================================================

% minimax_root(+GameState, +Player, +Depth, +Alpha, +Beta, -BestMove, -BestValue)
% Racine de l'algorithme minimax
minimax_root(GameState, Player, Depth, Alpha, Beta, BestMove, BestValue) :-
    worst_value(Player, InitValue),
    retractall(ai_best_move(_)),
    retractall(ai_best_value(_)),
    asserta(ai_best_move([])),
    asserta(ai_best_value(InitValue)),
    
    generate_legal_moves(GameState, Player, Moves),
    evaluate_all_moves(GameState, Moves, Player, Depth, Alpha, Beta),
    
    ai_best_move(BestMove),
    ai_best_value(BestValue).

% evaluate_all_moves(+GameState, +Moves, +Player, +Depth, +Alpha, +Beta)
% Evalue tous les coups avec alpha-beta pruning
evaluate_all_moves(_, [], _, _, _, _).
evaluate_all_moves(GameState, [Move|RestMoves], Player, Depth, Alpha, Beta) :-
    Move = [FromRow, FromCol, ToRow, ToCol],
    make_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState),
    
    opposite_player(Player, Opponent),
    NewDepth is Depth - 1,
    minimax_evaluate(NewGameState, Opponent, NewDepth, Alpha, Beta, _, Value),
    
    update_best_move(Move, Value, Player),
    
    % Test de coupure alpha-beta
    (   alpha_beta_cutoff(Value, Player, Alpha, Beta) ->
        true  % Coupure - arreter l'evaluation
    ;   update_alpha_beta(Player, Value, Alpha, Beta, NewAlpha, NewBeta),
        evaluate_all_moves(GameState, RestMoves, Player, Depth, NewAlpha, NewBeta)
    ).

% minimax_evaluate(+GameState, +Player, +Depth, +Alpha, +Beta, -BestMove, -Value)
% Coeur de l'algorithme minimax
minimax_evaluate(GameState, Player, 0, _, _, [], Value) :-
    % Cas de base - profondeur atteinte
    evaluate_position(GameState, Player, Value), !.

minimax_evaluate(GameState, Player, Depth, Alpha, Beta, BestMove, Value) :-
    Depth > 0,
    generate_legal_moves(GameState, Player, Moves),
    (   Moves = [] ->
        % Pas de coups legaux - position terminale
        evaluate_terminal_position(GameState, Player, Value),
        BestMove = []
    ;   % Recherche recursive
        worst_value(Player, InitValue),
        minimax_search_moves(GameState, Moves, Player, Depth, Alpha, Beta, InitValue, [], BestMove, Value)
    ).

% minimax_search_moves - Recherche recursive dans les coups
minimax_search_moves(_, [], _, _, _, _, CurrentValue, CurrentMove, CurrentMove, CurrentValue).

minimax_search_moves(GameState, [Move|RestMoves], Player, Depth, Alpha, Beta, CurrentValue, CurrentMove, BestMove, BestValue) :-
    Move = [FromRow, FromCol, ToRow, ToCol],
    make_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState),
    
    opposite_player(Player, Opponent),
    NewDepth is Depth - 1,
    minimax_evaluate(NewGameState, Opponent, NewDepth, Alpha, Beta, _, MoveValue),
    
    % Determine si c'est mieux selon le joueur
    (   is_better_move(Player, MoveValue, CurrentValue) ->
        NewCurrentValue = MoveValue,
        NewCurrentMove = Move
    ;   NewCurrentValue = CurrentValue,
        NewCurrentMove = CurrentMove
    ),
    
    % Test de coupure alpha-beta
    (   alpha_beta_cutoff(MoveValue, Player, Alpha, Beta) ->
        BestMove = Move, BestValue = MoveValue
    ;   update_alpha_beta(Player, MoveValue, Alpha, Beta, NewAlpha, NewBeta),
        minimax_search_moves(GameState, RestMoves, Player, Depth, NewAlpha, NewBeta, NewCurrentValue, NewCurrentMove, BestMove, BestValue)
    ).

% =============================================================================
% FONCTIONS UTILITAIRES ALPHA-BETA
% =============================================================================

% worst_value(+Player, -Value) - Valeur la plus mauvaise pour un joueur
worst_value(white, -9999).
worst_value(black, 9999).

% best_value(+Player, -Value) - Valeur la plus bonne pour un joueur
best_value(white, 9999).
best_value(black, -9999).

% is_better_move(+Player, +NewValue, +CurrentValue)
% Verifie si NewValue est meilleur que CurrentValue pour Player
is_better_move(white, NewValue, CurrentValue) :- NewValue > CurrentValue.
is_better_move(black, NewValue, CurrentValue) :- NewValue < CurrentValue.

% alpha_beta_cutoff(+Value, +Player, +Alpha, +Beta)
% Test de coupure alpha-beta
alpha_beta_cutoff(Value, white, _, Beta) :- Value >= Beta.
alpha_beta_cutoff(Value, black, Alpha, _) :- Value =< Alpha.

% update_alpha_beta(+Player, +Value, +Alpha, +Beta, -NewAlpha, -NewBeta)
% Met a jour les valeurs alpha et beta
update_alpha_beta(white, Value, Alpha, Beta, NewAlpha, Beta) :-
    NewAlpha is max(Alpha, Value).
update_alpha_beta(black, Value, Alpha, Beta, Alpha, NewBeta) :-
    NewBeta is min(Beta, Value).

% update_best_move(+Move, +Value, +Player)
% Met a jour le meilleur coup trouve
update_best_move(Move, Value, Player) :-
    ai_best_value(CurrentValue),
    (   is_better_move(Player, Value, CurrentValue) ->
        retract(ai_best_move(_)),
        retract(ai_best_value(_)),
        asserta(ai_best_move(Move)),
        asserta(ai_best_value(Value))
    ;   true
    ).

% get_current_best_value(+Player, -Value)
get_current_best_value(_, Value) :-
    ai_best_value(Value).

% =============================================================================
% EVALUATION DE POSITIONS
% =============================================================================

% evaluate_position(+GameState, +Player, -Value)
% Evaluation heuristique de position
evaluate_position(GameState, Player, Value) :-
    material_balance(GameState, Player, MaterialValue),
    positional_bonus(GameState, Player, PositionalValue),
    mobility_bonus(GameState, Player, MobilityValue),
    
    % Ponderation des facteurs
    Value is MaterialValue * 100 + PositionalValue * 10 + MobilityValue.

% material_balance(+GameState, +Player, -Value)
% Balance materielle
material_balance(GameState, Player, Value) :-
    GameState = game_state(Board, _, _, _, _),
    count_material(Board, Player, PlayerMaterial),
    opposite_player(Player, Opponent),
    count_material(Board, Opponent, OpponentMaterial),
    Value is PlayerMaterial - OpponentMaterial.

% count_material(+Board, +Player, -Total)
% Compte la valeur materielle d'un joueur
count_material(Board, Player, Total) :-
    findall(Value, (
        between(1, 8, Row),
        between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        Piece \= '.',
        get_piece_color(Piece, Player),
        piece_value(Piece, Value)
    ), Values),
    sum_list(Values, Total).

% positional_bonus(+GameState, +Player, -Value)
% Bonus positionnel (centre, avancement pions)
positional_bonus(GameState, Player, Value) :-
    GameState = game_state(Board, _, _, _, _),
    findall(Bonus, (
        between(1, 8, Row),
        between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        Piece \= '.',
        get_piece_color(Piece, Player),
        positional_piece_bonus(Piece, Player, Row, Col, Bonus)
    ), Bonuses),
    sum_list(Bonuses, Value).

% positional_piece_bonus(+Piece, +Player, +Row, +Col, -Bonus)
% Bonus positionnel pour une piece specifique
positional_piece_bonus('P', white, Row, Col, Bonus) :-
    center_bonus(Row, Col, CenterBonus),
    AdvancementBonus is Row * 2,
    Bonus is CenterBonus + AdvancementBonus.
positional_piece_bonus('p', black, Row, Col, Bonus) :-
    center_bonus(Row, Col, CenterBonus),
    AdvancementBonus is (9 - Row) * 2,
    Bonus is CenterBonus + AdvancementBonus.
positional_piece_bonus(_, _, Row, Col, Bonus) :-
    center_bonus(Row, Col, Bonus).

% center_bonus(+Row, +Col, -Bonus)
% Bonus pour proximite au centre
center_bonus(Row, Col, Bonus) :-
    CenterRow is abs(Row - 4.5),
    CenterCol is abs(Col - 4.5),
    Distance is CenterRow + CenterCol,
    Bonus is max(0, 4 - Distance).

% mobility_bonus(+GameState, +Player, -Value)
% Bonus de mobilite
mobility_bonus(GameState, Player, Value) :-
    generate_legal_moves(GameState, Player, PlayerMoves),
    length(PlayerMoves, PlayerMobility),
    opposite_player(Player, Opponent),
    generate_legal_moves(GameState, Opponent, OpponentMoves),
    length(OpponentMoves, OpponentMobility),
    Value is PlayerMobility - OpponentMobility.

% evaluate_terminal_position(+GameState, +Player, -Value)
% Evaluation position terminale (mat/pat)
evaluate_terminal_position(GameState, Player, Value) :-
    (   is_in_check(GameState, Player) ->
        worst_value(Player, Value)  % Echec et mat
    ;   Value = 0   % Pat
    ).

% =============================================================================
% GENERATION DE COUPS OPTIMISEE
% =============================================================================

% generate_all_moves(+GameState, -Moves) 
% Alias pour compatibilite avec tests
generate_all_moves(GameState, Moves) :-
    GameState = game_state(_, Player, _, _, _),
    generate_legal_moves(GameState, Player, Moves).

% minimax_search(+GameState, +Depth, -BestMove, -BestValue)
% Alias pour compatibilite avec tests
minimax_search(GameState, Depth, BestMove, BestValue) :-
    GameState = game_state(_, Player, _, _, _),
    worst_value(Player, Alpha),
    best_value(Player, Beta),
    minimax_root(GameState, Player, Depth, Alpha, Beta, BestMove, BestValue).

% material_value(+GameState, +Player, -Value)
% Alias pour compatibilite avec tests
material_value(GameState, Player, Value) :-
    material_balance(GameState, Player, Value).

% generate_legal_moves(+GameState, +Player, -Moves)
% Version optimisee - limite le nombre de coups
generate_legal_moves(GameState, Player, SortedMoves) :-
    GameState = game_state(Board, _, _, _, _),
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
    
    % Trie les coups par priorite et limite a 15 meilleurs
    prioritize_moves(GameState, AllMoves, PrioritizedMoves),
    take_best_moves(PrioritizedMoves, 15, SortedMoves).

% prioritize_moves(+GameState, +Moves, -PrioritizedMoves)
% Trie les coups par priorite
prioritize_moves(GameState, Moves, SortedMoves) :-
    GameState = game_state(Board, _, _, _, _),
    evaluate_move_priorities(Moves, Board, EvaluatedMoves),
    keysort(EvaluatedMoves, TempSorted),
    reverse(TempSorted, PrioritySorted),
    extract_moves_from_priority(PrioritySorted, SortedMoves).

% evaluate_move_priorities(+Moves, +Board, -EvaluatedMoves)
evaluate_move_priorities([], _, []).
evaluate_move_priorities([Move|RestMoves], Board, [Priority-Move|RestEvaluated]) :-
    Move = [_, _, ToRow, ToCol],
    get_piece(Board, ToRow, ToCol, TargetPiece),
    (   TargetPiece \= '.' ->
        piece_value(TargetPiece, CaptureValue),
        Priority is 1000 + CaptureValue
    ;   Priority = 100
    ),
    evaluate_move_priorities(RestMoves, Board, RestEvaluated).

% extract_moves_from_priority(+PriorityList, -Moves)
extract_moves_from_priority([], []).
extract_moves_from_priority([_-Move|Rest], [Move|Moves]) :-
    extract_moves_from_priority(Rest, Moves).

% take_best_moves(+Moves, +N, -BestMoves)
take_best_moves(Moves, N, BestMoves) :-
    length(Moves, Len),
    (   Len =< N ->
        BestMoves = Moves
    ;   take_first_n(Moves, N, BestMoves)
    ).

% take_first_n(+List, +N, -FirstN)
take_first_n(_, 0, []) :- !.
take_first_n([], _, []) :- !.
take_first_n([H|T], N, [H|Rest]) :-
    N > 0,
    N1 is N - 1,
    take_first_n(T, N1, Rest).

% =============================================================================
% GESTION DE LA PILE IA
% =============================================================================

% init_ai_stack/0 - Initialise la pile IA
init_ai_stack :-
    retractall(ai_top(_)),
    asserta(ai_top(0)).

% cleanup_ai_data/0 - Nettoie les donnees IA
cleanup_ai_data :-
    retractall(ai_stack(_, _, _)),
    retractall(ai_top(_)),
    retractall(ai_best_move(_)),
    retractall(ai_best_value(_)).

% emergency_move_selection(+GameState, -BestMove)
% Selection d'urgence en cas de timeout
emergency_move_selection(GameState, BestMove) :-
    generate_legal_moves(GameState, Player, Moves),
    GameState = game_state(_, Player, _, _, _),
    (   Moves = [] ->
        BestMove = []
    ;   Moves = [FirstMove|_],
        BestMove = FirstMove
    ).

% =============================================================================
% CONFIGURATION ET INTERFACE
% =============================================================================

% ai_search_depth(-Depth)
% Configure la profondeur de recherche
ai_search_depth(2).  % Profondeur 2 optimisee

% Interface IA integree dans le systeme principal (interface.pl)
% Utilisez start_ai_game/0 pour lancer une partie contre l'IA
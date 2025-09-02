% =============================================================================
% INTELLIGENCE ARTIFICIELLE - MODULE IA COMPLET
% =============================================================================
% 
% Ce module centralise TOUTE la logique d'intelligence artificielle :
% - Algorithme Minimax avec elagage Alpha-Beta
% - Evaluation de positions d'echecs
% - Selection optimale des coups
% - Interface IA pour integration au jeu
%
% Auteur : Patrick Patenaude
% Version : 1.0 (IA Phase 3)
%
% RESPONSABILITES :
% - Algorithme de recherche minimax
% - Fonction d'evaluation heuristique  
% - Optimisations performance (alpha-beta, tri coups)
% - Interface simple pour integration
% =============================================================================

:- [pieces].
:- [board].
:- [game].

% =============================================================================
% SECTION 1 : INTERFACE IA PRINCIPALE
% =============================================================================

% choose_ai_move(+GameState, -BestMove)
% Point d'entree principal : choisit le meilleur coup pour l'IA
% GameState : etat actuel du jeu
% BestMove : [FromRow, FromCol, ToRow, ToCol] meilleur coup trouve
choose_ai_move(GameState, BestMove) :-
    GameState = game_state(Board, Player, _, _, _),
    write('IA reflechit...'), nl,
    get_time(StartTime),
    ai_search_depth(Depth),  % Profondeur configurable
    minimax_search(GameState, Depth, BestMove, _),
    get_time(EndTime),
    Duration is EndTime - StartTime,
    format('Coup choisi en ~2f secondes~n', [Duration]).

% =============================================================================
% SECTION 2 : ALGORITHME MINIMAX AVEC ALPHA-BETA
% =============================================================================

% minimax_search(+GameState, +Depth, -BestMove, -BestValue)
% Lance la recherche minimax avec elagage alpha-beta
minimax_search(GameState, Depth, BestMove, BestValue) :-
    GameState = game_state(_, Player, _, _, _),
    generate_all_moves(GameState, Moves),
    (   Moves = [] ->
        BestMove = [], BestValue = -9999
    ;   sort_moves(GameState, Moves, SortedMoves),
        Alpha is -9999,
        Beta is 9999,
        search_moves(GameState, SortedMoves, Depth, Player, Alpha, Beta, BestMove, BestValue)
    ).

% search_moves(+GameState, +Moves, +Depth, +Player, +Alpha, +Beta, -BestMove, -BestValue)
% Evalue tous les coups possibles avec alpha-beta pruning
search_moves(_, [], _, _, Alpha, _, [], Alpha).

search_moves(GameState, [Move|Moves], Depth, Player, Alpha, Beta, BestMove, BestValue) :-
    Move = [FromRow, FromCol, ToRow, ToCol],
    make_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState),
    NewDepth is Depth - 1,
    opposite_player(Player, Opponent),
    minimax(NewGameState, NewDepth, Opponent, Alpha, Beta, _, Value),
    (   Value > Alpha ->
        NewAlpha = Value,
        (   Value >= Beta ->
            % Alpha-beta cutoff
            BestMove = Move, BestValue = Value
        ;   search_remaining_moves(GameState, Moves, Depth, Player, NewAlpha, Beta, Move, Value, BestMove, BestValue)
        )
    ;   search_moves(GameState, Moves, Depth, Player, Alpha, Beta, BestMove, BestValue)
    ).

% search_remaining_moves - Continue la recherche apres mise a jour alpha
search_remaining_moves(GameState, Moves, Depth, Player, Alpha, Beta, CurrentBest, CurrentValue, BestMove, BestValue) :-
    search_moves(GameState, Moves, Depth, Player, Alpha, Beta, RestBest, RestValue),
    (   RestValue > CurrentValue ->
        BestMove = RestBest, BestValue = RestValue
    ;   BestMove = CurrentBest, BestValue = CurrentValue
    ).

% minimax(+GameState, +Depth, +Player, +Alpha, +Beta, -BestMove, -Value)
% Coeur de l'algorithme minimax avec elagage alpha-beta
minimax(GameState, 0, Player, _, _, [], Value) :-
    % Cas de base : profondeur atteinte
    evaluate_position(GameState, Player, Value).

minimax(GameState, Depth, Player, Alpha, Beta, BestMove, Value) :-
    Depth > 0,
    generate_all_moves(GameState, Moves),
    (   Moves = [] ->
        % Aucun coup possible (mat ou pat)
        evaluate_terminal_position(GameState, Player, Value),
        BestMove = []
    ;   minimax_moves(GameState, Moves, Depth, Player, Alpha, Beta, BestMove, Value)
    ).

% minimax_moves - Evalue recursivement tous les coups
minimax_moves(GameState, Moves, Depth, Player, Alpha, Beta, BestMove, Value) :-
    opposite_player(Player, Opponent),
    NewDepth is Depth - 1,
    minimax_evaluate_moves(GameState, Moves, NewDepth, Opponent, Alpha, Beta, Player, [], -9999, BestMove, Value).

% minimax_evaluate_moves - Boucle principale d'evaluation
minimax_evaluate_moves(_, [], _, _, Alpha, _, _, CurrentBest, CurrentValue, CurrentBest, CurrentValue).

minimax_evaluate_moves(GameState, [Move|Moves], Depth, Opponent, Alpha, Beta, MaxPlayer, CurrentBest, CurrentValue, BestMove, BestValue) :-
    Move = [FromRow, FromCol, ToRow, ToCol],
    make_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState),
    minimax(NewGameState, Depth, Opponent, Alpha, Beta, _, MoveValue),
    
    % Ajustement selon joueur (max ou min)
    (   MaxPlayer = Opponent ->
        FinalValue is -MoveValue  % Minimisation pour l'adversaire
    ;   FinalValue = MoveValue    % Maximisation pour nous
    ),
    
    (   FinalValue > CurrentValue ->
        NewBest = Move,
        NewValue = FinalValue,
        NewAlpha is max(Alpha, FinalValue)
    ;   NewBest = CurrentBest,
        NewValue = CurrentValue,
        NewAlpha = Alpha
    ),
    
    % Test de coupure alpha-beta
    (   NewAlpha >= Beta ->
        BestMove = NewBest, BestValue = NewValue
    ;   minimax_evaluate_moves(GameState, Moves, Depth, Opponent, NewAlpha, Beta, MaxPlayer, NewBest, NewValue, BestMove, BestValue)
    ).

% =============================================================================
% SECTION 3 : EVALUATION DE POSITIONS
% =============================================================================

% evaluate_position(+GameState, +Player, -Value)
% Fonction d'evaluation heuristique principale
evaluate_position(GameState, Player, Value) :-
    material_value(GameState, Player, MaterialValue),
    mobility_value(GameState, Player, MobilityValue),
    king_safety_value(GameState, Player, KingSafetyValue),
    
    % Ponderation des facteurs
    Value is MaterialValue * 10 + MobilityValue * 2 + KingSafetyValue * 5.

% material_value(+GameState, +Player, -Value)
% Evaluation basee sur la valeur materielle des pieces
material_value(game_state(Board, _, _, _, _), Player, Value) :-
    count_material(Board, Player, PlayerMaterial),
    opposite_player(Player, Opponent),
    count_material(Board, Opponent, OpponentMaterial),
    Value is PlayerMaterial - OpponentMaterial.

% count_material(+Board, +Player, -Total)
% Compte la valeur totale des pieces d'un joueur
count_material(Board, Player, Total) :-
    findall(PieceValue, (
        member(Row, Board),
        member(Piece, Row),
        get_piece_color(Piece, Player),
        piece_value(Piece, PieceValue)
    ), Values),
    sum_list(Values, Total).

% piece_value/2 utilise la version de pieces.pl (importee automatiquement)

% mobility_value(+GameState, +Player, -Value)
% Evaluation basee sur la mobilite des pieces
mobility_value(GameState, Player, Value) :-
    count_legal_moves(GameState, Player, PlayerMoves),
    opposite_player(Player, Opponent),
    count_legal_moves(GameState, Opponent, OpponentMoves),
    Value is PlayerMoves - OpponentMoves.

% king_safety_value(+GameState, +Player, -Value)
% Evaluation de la securite du roi
king_safety_value(game_state(Board, _, _, _, _), Player, Value) :-
    find_king_position(Board, Player, KingRow, KingCol),
    count_king_defenders(Board, KingRow, KingCol, Player, Defenders),
    count_king_attackers(Board, KingRow, KingCol, Player, Attackers),
    Value is Defenders - Attackers * 2.  % Les attaques sont plus dangereuses

% evaluate_terminal_position(+GameState, +Player, -Value)
% Evaluation des positions terminales (mat/pat)
evaluate_terminal_position(GameState, Player, Value) :-
    (   is_in_check(GameState, Player) ->
        Value = -9999  % Mat
    ;   Value = 0      % Pat
    ).

% =============================================================================
% SECTION 4 : GENERATION ET TRI DES COUPS
% =============================================================================

% generate_all_moves(+GameState, -Moves)
% Genere coups legaux avec limitation intelligente pour performance
generate_all_moves(game_state(Board, Player, _, _, _), Moves) :-
    % Genere seulement les coups les plus prometteurs  
    findall(Priority-[FromRow, FromCol, ToRow, ToCol], (
        between(1, 8, FromRow),
        between(1, 8, FromCol),
        get_piece(Board, FromRow, FromCol, Piece),
        Piece \= '.',  % Skip empty squares immediately
        get_piece_color(Piece, Player),
        generate_piece_moves(Board, Player, FromRow, FromCol, ToRow, ToCol),
        move_priority(Board, Player, FromRow, FromCol, ToRow, ToCol, Priority)
    ), PrioritizedMoves),
    % Trie par priorite et prend les 20 meilleurs coups seulement
    keysort(PrioritizedMoves, SortedMoves),
    reverse(SortedMoves, BestFirst),
    take_first_n(BestFirst, 10, TopMoves),  % LIMITATION ULTRA-AGRESSIVE : 10 coups max
    extract_moves(TopMoves, Moves).  % Utilise extract_moves existant

% generate_piece_moves(+Board, +Player, +FromRow, +FromCol, -ToRow, -ToCol)
% Genere les coups possibles pour une piece specifique (optimise pour performance)
generate_piece_moves(Board, Player, FromRow, FromCol, ToRow, ToCol) :-
    between(1, 8, ToRow),
    between(1, 8, ToCol),
    \+ (ToRow = FromRow, ToCol = FromCol),  % Pas de mouvement sur place
    valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol).

% sort_moves(+GameState, +Moves, -SortedMoves)
% Trie les coups par priorite (captures, echecs, coups normaux)
sort_moves(GameState, Moves, SortedMoves) :-
    GameState = game_state(Board, Player, _, _, _),
    evaluate_moves(Moves, Board, Player, EvaluatedMoves),
    keysort(EvaluatedMoves, SortedPairs),
    reverse(SortedPairs, ReversedPairs),
    extract_moves(ReversedPairs, SortedMoves).

% evaluate_moves - Assigne une priorite a chaque coup
evaluate_moves([], _, _, []).
evaluate_moves([Move|Moves], Board, Player, [Priority-Move|EvaluatedMoves]) :-
    Move = [FromRow, FromCol, ToRow, ToCol],
    move_priority(Board, Player, FromRow, FromCol, ToRow, ToCol, Priority),
    evaluate_moves(Moves, Board, Player, EvaluatedMoves).

% move_priority - Calcule la priorite d'un coup
move_priority(Board, Player, FromRow, FromCol, ToRow, ToCol, Priority) :-
    get_piece(Board, ToRow, ToCol, TargetPiece),
    (   TargetPiece \= '.' ->
        piece_value(TargetPiece, CaptureValue),
        Priority is 1000 + CaptureValue  % Captures prioritaires
    ;   gives_check(Board, Player, FromRow, FromCol, ToRow, ToCol) ->
        Priority = 500                   % Echecs moyennement prioritaires  
    ;   Priority = 100                   % Coups normaux
    ).

% extract_moves - Extrait les coups de la liste Priority-Move
extract_moves([], []).
extract_moves([_-Move|Pairs], [Move|Moves]) :-
    extract_moves(Pairs, Moves).

% extract_moves_from_priority - Extrait coups de [Priority, FromRow, FromCol, ToRow, ToCol]  
extract_moves_from_priority([], []).
extract_moves_from_priority([[_, FromRow, FromCol, ToRow, ToCol]|Rest], [[FromRow, FromCol, ToRow, ToCol]|Moves]) :-
    extract_moves_from_priority(Rest, Moves).

% take_first_n(+List, +N, -FirstN)
% Prend les N premiers elements d'une liste
take_first_n(_, 0, []) :- !.
take_first_n([], _, []) :- !.
take_first_n([H|T], N, [H|Result]) :-
    N > 0,
    N1 is N - 1,
    take_first_n(T, N1, Result).

% =============================================================================
% SECTION 5 : UTILITAIRES IA
% =============================================================================

% count_legal_moves(+GameState, +Player, -Count)
% Compte le nombre de coups legaux pour un joueur
count_legal_moves(GameState, Player, Count) :-
    generate_all_moves(GameState, Moves),
    length(Moves, Count).

% find_king_position/3 utilise la version de board.pl (importee automatiquement)

% count_king_defenders(+Board, +KingRow, +KingCol, +Player, -Count)
% Compte les pieces qui defendentle roi
count_king_defenders(Board, KingRow, KingCol, Player, Count) :-
    findall(1, (
        between(1, 8, Row),
        between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        get_piece_color(Piece, Player),
        \+ (Row = KingRow, Col = KingCol),
        defends_square(Board, Row, Col, KingRow, KingCol)
    ), Defenders),
    length(Defenders, Count).

% count_king_attackers(+Board, +KingRow, +KingCol, +Player, -Count)  
% Compte les pieces adverses qui attaquent le roi
count_king_attackers(Board, KingRow, KingCol, Player, Count) :-
    opposite_player(Player, Opponent),
    findall(1, (
        between(1, 8, Row),
        between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        get_piece_color(Piece, Opponent),
        can_attack_square(Board, Row, Col, KingRow, KingCol)
    ), Attackers),
    length(Attackers, Count).

% defends_square(+Board, +FromRow, +FromCol, +ToRow, +ToCol)
% Verifie si une piece defend une case
defends_square(Board, FromRow, FromCol, ToRow, ToCol) :-
    get_piece(Board, FromRow, FromCol, Piece),
    get_piece_color(Piece, Player),
    valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol).

% can_attack_square(+Board, +FromRow, +FromCol, +ToRow, +ToCol)
% Verifie si une piece peut attaquer une case
can_attack_square(Board, FromRow, FromCol, ToRow, ToCol) :-
    get_piece(Board, FromRow, FromCol, Piece),
    get_piece_color(Piece, Player),
    valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol).

% gives_check(+Board, +Player, +FromRow, +FromCol, +ToRow, +ToCol)
% Verifie si un coup donne echec
gives_check(Board, Player, FromRow, FromCol, ToRow, ToCol) :-
    % Simulation temporaire du coup
    place_piece_optimized(Board, FromRow, FromCol, '.', TempBoard1),
    get_piece(Board, FromRow, FromCol, MovingPiece),
    place_piece_optimized(TempBoard1, ToRow, ToCol, MovingPiece, NewBoard),
    
    % Verification si le roi adverse est en echec
    opposite_player(Player, Opponent),
    find_king_position(NewBoard, Opponent, KingRow, KingCol),
    can_attack_square(NewBoard, ToRow, ToCol, KingRow, KingCol).

% opposite_player/2 utilise la version de pieces.pl (importee automatiquement)

% is_in_check/2 utilise la version de game.pl (importee automatiquement)

% =============================================================================
% SECTION 6 : INTERFACE POUR INTEGRATION
% =============================================================================

% ai_vs_human_mode/0
% Mode de jeu IA contre humain
ai_vs_human_mode :-
    write('=== MODE IA vs HUMAIN ==='), nl,
    write('L\'IA joue les noirs, vous jouez les blancs.'), nl, nl,
    init_game_state(GameState),
    ai_game_loop(GameState).

% =============================================================================
% SECTION : CONFIGURATION IA
% =============================================================================

% ai_search_depth(-Depth)
% Configure la profondeur de recherche selon le contexte
ai_search_depth(2) :-
    % Profondeur 2 pour version academique finale (optimisee)
    !.

% Pour debug rapide, utiliser:
% ai_search_depth(1) :- !.

% ai_game_loop(+GameState)
% Boucle principale du jeu avec IA
ai_game_loop(GameState) :-
    GameState = game_state(_, Player, _, Status, _),
    
    display_game_state(GameState),
    
    (   Status \= active ->
        format('Jeu termine : ~w~n', [Status])
    ;   Player = white ->
        % Tour du joueur humain
        write('Votre tour (blanc) - Entrez votre coup (ex: e2e4) : '),
        read_line_to_string(user_input, Input),
        process_human_move(GameState, Input, NewGameState),
        ai_game_loop(NewGameState)
    ;   % Tour de l'IA (noirs)
        write('Tour de l\'IA (noir)...'), nl,
        choose_ai_move(GameState, AIMove),
        (   AIMove = [] ->
            write('IA ne peut pas jouer - Fin de partie'), nl
        ;   AIMove = [FromRow, FromCol, ToRow, ToCol],
            format('IA joue : ~w~w~w~w~n', [FromRow, FromCol, ToRow, ToCol]),
            make_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState),
            ai_game_loop(NewGameState)
        )
    ).

% process_human_move(+GameState, +Input, -NewGameState)
% Traite le coup du joueur humain
process_human_move(GameState, Input, NewGameState) :-
    (   parse_algebraic_move(Input, FromRow, FromCol, ToRow, ToCol) ->
        (   make_move(GameState, FromRow, FromCol, ToRow, ToCol, TempGameState) ->
            NewGameState = TempGameState
        ;   write('Coup invalide! Reessayez.'), nl,
            NewGameState = GameState
        )
    ;   write('Format invalide! Utilisez le format e2e4'), nl,
        NewGameState = GameState
    ).
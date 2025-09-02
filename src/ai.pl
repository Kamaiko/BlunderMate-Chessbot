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

% Base de donnees dynamique pour alpha-beta et transposition table
:- dynamic ai_stack/3, ai_top/1, ai_best_move/1, ai_best_value/1.
:- dynamic position_cache/3.  % position_cache(PositionHash, Depth, Value)

% =============================================================================
% INTERFACE PRINCIPALE IA
% =============================================================================

% choose_ai_move(+GameState, -BestMove)
% Point d'entree principal AMELIORE avec randomisation anti-determinisme
choose_ai_move(GameState, BestMove) :-
    cleanup_ai_data,
    init_ai_stack,
    GameState = game_state(_, Player, MoveCount, _, _),
    
    % CORRECTION CRITIQUE: D'abord verifier livre d'ouvertures (plus de variete)
    (   MoveCount =< 12,
        opening_move(GameState, OpeningMove),
        OpeningMove = [FromRow, FromCol, ToRow, ToCol],
        GameState = game_state(Board, _, _, _, _),
        valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol) ->
        % Coup d'ouverture valide trouve - mais avec chance de l'ignorer pour variete
        random(RandomValue),
        (   RandomValue < 0.8 ->  % 80% chance d'utiliser l'ouverture
            BestMove = OpeningMove
        ;   % 20% chance d'utiliser minimax pour plus de variete
            choose_ai_move_minimax(GameState, BestMove)
        )
    ;   % Pas d'ouverture ou en milieu de partie - utiliser minimax
        choose_ai_move_minimax(GameState, BestMove)
    ).

% choose_ai_move_minimax(+GameState, -BestMove)
% Version minimax pure avec gestion d'erreurs
choose_ai_move_minimax(GameState, BestMove) :-
    GameState = game_state(_, Player, _, _, _),
    ai_search_depth(Depth),
    worst_value(Player, Alpha),
    best_value(Player, Beta),
    
    % Timeout de securite - 10 secondes pour profondeur 2
    (   catch(call_with_time_limit(10, 
                minimax_root(GameState, Player, Depth, Alpha, Beta, TempMove, _)
            ), Error, (
                write('EXCEPTION IA: '), write(Error), nl,
                fail
            ))
    ->  (   TempMove = [] ->
            % Minimax retourne coup vide - utiliser coup d'urgence
            write('Minimax retourne coup vide - Selection d\'urgence'), nl,
            emergency_move_selection(GameState, BestMove)
        ;   BestMove = TempMove
        )
    ;   % Timeout ou echec - choix d'urgence
        write('TIMEOUT ou ERREUR - Selection coup d\'urgence'), nl,
        emergency_move_selection(GameState, BestMove)
    ),
    
    % Validation finale du coup retourne
    (   BestMove = [FromRow, FromCol, ToRow, ToCol],
        integer(FromRow), integer(FromCol), integer(ToRow), integer(ToCol) ->
        true
    ;   write('ERREUR COUP INVALIDE - Dernier recours'), nl,
        generate_legal_moves(GameState, Player, [FirstMove|_]),
        BestMove = FirstMove
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
    minimax_evaluate(NewGameState, Opponent, NewDepth, Alpha, Beta, _, OpponentValue),
    
    % Inverser la valeur car elle vient du point de vue de l'adversaire
    Value is -OpponentValue,
    update_best_move(Move, Value, Player),
    
    % Test de coupure alpha-beta
    (   alpha_beta_cutoff(Value, Player, Alpha, Beta) ->
        true  % Coupure - arreter l'evaluation
    ;   update_alpha_beta(Player, Value, Alpha, Beta, NewAlpha, NewBeta),
        evaluate_all_moves(GameState, RestMoves, Player, Depth, NewAlpha, NewBeta)
    ).

% minimax_evaluate(+GameState, +Player, +Depth, +Alpha, +Beta, -BestMove, -Value)
% Coeur de l'algorithme minimax AVEC TRANSPOSITION TABLE
minimax_evaluate(GameState, Player, 0, _, _, [], Value) :-
    % Cas de base - profondeur atteinte avec cache
    game_state_hash(GameState, Hash),
    (   position_cache(Hash, 0, CachedValue) ->
        Value = CachedValue  % Cache hit
    ;   evaluate_position(GameState, Player, Value),
        assertz(position_cache(Hash, 0, Value))  % Store in cache
    ), !.

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
    minimax_evaluate(NewGameState, Opponent, NewDepth, Alpha, Beta, _, OpponentMoveValue),
    
    % Inverser la valeur car elle vient du point de vue de l'adversaire
    MoveValue is -OpponentMoveValue,
    
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
% PIECE-SQUARE TABLES - ARCHITECTURE STANDARD FREECODECAMP
% =============================================================================

% pawn_square_table(+Row, +Col, -Value)
% Table positionnelle pour pions (blancs - inverser pour noirs)
pawn_square_table(Row, Col, Value) :-
    PawnTable = [
        [ 0,  0,  0,  0,  0,  0,  0,  0],  % Row 1
        [50, 50, 50, 50, 50, 50, 50, 50],  % Row 2
        [10, 10, 20, 30, 30, 20, 10, 10],  % Row 3
        [ 5,  5, 10, 25, 25, 10,  5,  5],  % Row 4
        [ 0,  0,  0, 20, 20,  0,  0,  0],  % Row 5
        [ 5, -5,-10,  0,  0,-10, -5,  5],  % Row 6
        [ 5, 10, 10,-20,-20, 10, 10,  5],  % Row 7
        [ 0,  0,  0,  0,  0,  0,  0,  0]   % Row 8
    ],
    RowIndex is Row - 1,
    ColIndex is Col - 1,
    nth0(RowIndex, PawnTable, RowValues),
    nth0(ColIndex, RowValues, Value).

% knight_square_table(+Row, +Col, -Value)
% Table positionnelle pour cavaliers
knight_square_table(Row, Col, Value) :-
    KnightTable = [
        [-50,-40,-30,-30,-30,-30,-40,-50],
        [-40,-20,  0,  0,  0,  0,-20,-40],
        [-30,  0, 10, 15, 15, 10,  0,-30],
        [-30,  5, 15, 20, 20, 15,  5,-30],
        [-30,  0, 15, 20, 20, 15,  0,-30],
        [-30,  5, 10, 15, 15, 10,  5,-30],
        [-40,-20,  0,  5,  5,  0,-20,-40],
        [-50,-40,-30,-30,-30,-30,-40,-50]
    ],
    RowIndex is Row - 1,
    ColIndex is Col - 1,
    nth0(RowIndex, KnightTable, RowValues),
    nth0(ColIndex, RowValues, Value).

% bishop_square_table(+Row, +Col, -Value)
% Table positionnelle pour fous
bishop_square_table(Row, Col, Value) :-
    BishopTable = [
        [-20,-10,-10,-10,-10,-10,-10,-20],
        [-10,  0,  0,  0,  0,  0,  0,-10],
        [-10,  0,  5, 10, 10,  5,  0,-10],
        [-10,  5,  5, 10, 10,  5,  5,-10],
        [-10,  0, 10, 10, 10, 10,  0,-10],
        [-10, 10, 10, 10, 10, 10, 10,-10],
        [-10,  5,  0,  0,  0,  0,  5,-10],
        [-20,-10,-10,-10,-10,-10,-10,-20]
    ],
    RowIndex is Row - 1,
    ColIndex is Col - 1,
    nth0(RowIndex, BishopTable, RowValues),
    nth0(ColIndex, RowValues, Value).

% rook_square_table(+Row, +Col, -Value)
% Table positionnelle pour tours
rook_square_table(Row, Col, Value) :-
    RookTable = [
        [ 0,  0,  0,  0,  0,  0,  0,  0],
        [ 5, 10, 10, 10, 10, 10, 10,  5],
        [-5,  0,  0,  0,  0,  0,  0, -5],
        [-5,  0,  0,  0,  0,  0,  0, -5],
        [-5,  0,  0,  0,  0,  0,  0, -5],
        [-5,  0,  0,  0,  0,  0,  0, -5],
        [-5,  0,  0,  0,  0,  0,  0, -5],
        [ 0,  0,  0,  5,  5,  0,  0,  0]
    ],
    RowIndex is Row - 1,
    ColIndex is Col - 1,
    nth0(RowIndex, RookTable, RowValues),
    nth0(ColIndex, RowValues, Value).

% queen_square_table(+Row, +Col, -Value)
% Table positionnelle pour dames
queen_square_table(Row, Col, Value) :-
    QueenTable = [
        [-20,-10,-10, -5, -5,-10,-10,-20],
        [-10,  0,  0,  0,  0,  0,  0,-10],
        [-10,  0,  5,  5,  5,  5,  0,-10],
        [ -5,  0,  5,  5,  5,  5,  0, -5],
        [  0,  0,  5,  5,  5,  5,  0, -5],
        [-10,  5,  5,  5,  5,  5,  0,-10],
        [-10,  0,  5,  0,  0,  0,  0,-10],
        [-20,-10,-10, -5, -5,-10,-10,-20]
    ],
    RowIndex is Row - 1,
    ColIndex is Col - 1,
    nth0(RowIndex, QueenTable, RowValues),
    nth0(ColIndex, RowValues, Value).

% king_square_table(+Row, +Col, -Value)
% Table positionnelle pour roi (milieu de partie)
king_square_table(Row, Col, Value) :-
    KingTable = [
        [-30,-40,-40,-50,-50,-40,-40,-30],
        [-30,-40,-40,-50,-50,-40,-40,-30],
        [-30,-40,-40,-50,-50,-40,-40,-30],
        [-30,-40,-40,-50,-50,-40,-40,-30],
        [-20,-30,-30,-40,-40,-30,-30,-20],
        [-10,-20,-20,-20,-20,-20,-20,-10],
        [ 20, 20,  0,  0,  0,  0, 20, 20],
        [ 20, 30, 10,  0,  0, 10, 30, 20]
    ],
    RowIndex is Row - 1,
    ColIndex is Col - 1,
    nth0(RowIndex, KingTable, RowValues),
    nth0(ColIndex, RowValues, Value).

% get_piece_square_value(+Piece, +Row, +Col, -Value)
% Retourne la valeur positionnelle d'une piece selon son type
get_piece_square_value('P', Row, Col, Value) :-
    pawn_square_table(Row, Col, Value).
get_piece_square_value('p', Row, Col, Value) :-
    FlippedRow is 9 - Row,  % Inverser pour les noirs
    pawn_square_table(FlippedRow, Col, TempValue),
    Value is -TempValue.
get_piece_square_value('N', Row, Col, Value) :-
    knight_square_table(Row, Col, Value).
get_piece_square_value('n', Row, Col, Value) :-
    FlippedRow is 9 - Row,
    knight_square_table(FlippedRow, Col, TempValue),
    Value is -TempValue.
get_piece_square_value('B', Row, Col, Value) :-
    bishop_square_table(Row, Col, Value).
get_piece_square_value('b', Row, Col, Value) :-
    FlippedRow is 9 - Row,
    bishop_square_table(FlippedRow, Col, TempValue),
    Value is -TempValue.
get_piece_square_value('R', Row, Col, Value) :-
    rook_square_table(Row, Col, Value).
get_piece_square_value('r', Row, Col, Value) :-
    FlippedRow is 9 - Row,
    rook_square_table(FlippedRow, Col, TempValue),
    Value is -TempValue.
get_piece_square_value('Q', Row, Col, Value) :-
    queen_square_table(Row, Col, Value).
get_piece_square_value('q', Row, Col, Value) :-
    FlippedRow is 9 - Row,
    queen_square_table(FlippedRow, Col, TempValue),
    Value is -TempValue.
get_piece_square_value('K', Row, Col, Value) :-
    king_square_table(Row, Col, Value).
get_piece_square_value('k', Row, Col, Value) :-
    FlippedRow is 9 - Row,
    king_square_table(FlippedRow, Col, TempValue),
    Value is -TempValue.
get_piece_square_value('.', _, _, 0).  % Cases vides
get_piece_square_value(' ', _, _, 0).  % Espaces

% =============================================================================
% EVALUATION DE POSITIONS
% =============================================================================

% evaluate_position(+GameState, +Player, -Value)
% Evaluation heuristique AMELIOREE selon standards FreeCodeCamp
evaluate_position(GameState, Player, Value) :-
    % Composants d'evaluation standard
    material_evaluation(GameState, MaterialScore),
    positional_evaluation(GameState, PositionalScore),
    mobility_evaluation(GameState, Player, MobilityScore),
    king_safety_evaluation(GameState, Player, SafetyScore),
    
    % AMELIORATION: Ponderation optimisee selon phase de jeu
    GameState = game_state(_, _, MoveCount, _, _),
    (   MoveCount =< 20 ->  % Ouverture/milieu de jeu
        % Priorite mobilite et developpement
        Value is MaterialScore + PositionalScore + MobilityScore * 3 + SafetyScore * 2
    ;   % Fin de jeu
        % Priorite roi actif et material
        Value is MaterialScore * 1.2 + PositionalScore * 0.8 + MobilityScore * 2 + SafetyScore * 4
    ).

% material_evaluation(+GameState, -Value)
% Evaluation materielle avec valeurs standard
material_evaluation(GameState, Value) :-
    GameState = game_state(Board, _, _, _, _),
    findall(PieceValue, (
        between(1, 8, Row),
        between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        Piece \= '.',
        piece_value(Piece, PieceValue)
    ), Values),
    sum_list(Values, Value).

% positional_evaluation(+GameState, -Value)
% Evaluation positionnelle avec piece-square tables
positional_evaluation(GameState, Value) :-
    GameState = game_state(Board, _, _, _, _),
    findall(SquareValue, (
        between(1, 8, Row),
        between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        get_piece_square_value(Piece, Row, Col, SquareValue)
    ), Values),
    sum_list(Values, Value).

% mobility_evaluation(+GameState, +Player, -Value)
% Evaluation mobilite OPTIMISEE (evite double generation pour performance)
mobility_evaluation(GameState, Player, Value) :-
    GameState = game_state(Board, _, _, _, _),
    
    % OPTIMISATION: Compter directement les pieces actives plutot que tous les coups
    count_active_pieces(Board, Player, PlayerActivePieces),
    opposite_player(Player, Opponent),
    count_active_pieces(Board, Opponent, OpponentActivePieces),
    
    % Approximation rapide: pieces centrales et non bloquees = mobilite
    Value is (PlayerActivePieces - OpponentActivePieces) * 2.

% count_active_pieces(+Board, +Player, -Count)
% Compte les pieces actives (non bloquees) pour approximer la mobilite
count_active_pieces(Board, Player, Count) :-
    findall(1, (
        between(1, 8, Row),
        between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        Piece \= '.',
        get_piece_color(Piece, Player),
        % Verifier si piece dans une position active (centre ou pas trop en bordure)
        is_active_position(Row, Col, Piece)
    ), ActivePieces),
    length(ActivePieces, Count).

% is_active_position(+Row, +Col, +Piece)
% Determine si une piece est dans une position active
is_active_position(Row, Col, Piece) :-
    (   % Pieces centrales sont toujours actives
        member(Row, [3,4,5,6]), member(Col, [3,4,5,6])
    ;   % Pieces developpees (pas sur rangs de depart)
        Piece = 'N', Row \= 1  % Cavalier blanc developpe
    ;   Piece = 'n', Row \= 8  % Cavalier noir developpe
    ;   Piece = 'B', Row \= 1  % Fou blanc developpe
    ;   Piece = 'b', Row \= 8  % Fou noir developpe
    ;   % Rois, dames et tours sont toujours considerees actives
        member(Piece, ['K', 'k', 'Q', 'q', 'R', 'r'])
    ).

% king_safety_evaluation(+GameState, +Player, -Value)
% Evaluation securite du roi
king_safety_evaluation(GameState, Player, Value) :-
    GameState = game_state(Board, _, _, _, _),
    (   find_king_position(Board, Player, KingRow, KingCol) ->
        % Penaliser roi expose
        count_attackers_on_king(GameState, Player, KingRow, KingCol, AttackerCount),
        SafetyPenalty is AttackerCount * -10,
        
        % Bonus roi securise en coin
        (   (KingRow = 1, member(KingCol, [1,2,7,8])) -> 
            CornersBonus = 5 
        ; (KingRow = 8, member(KingCol, [1,2,7,8])) -> 
            CornersBonus = 5
        ;   CornersBonus = 0
        ),
        Value is SafetyPenalty + CornersBonus
    ;   Value = -50  % Roi introuvable = catastrophe
    ).

% count_attackers_on_king(+GameState, +Player, +KingRow, +KingCol, -Count)
% Compte les pieces adverses qui attaquent le roi
count_attackers_on_king(GameState, Player, KingRow, KingCol, Count) :-
    GameState = game_state(Board, _, _, _, _),
    opposite_player(Player, Opponent),
    findall(1, (
        between(1, 8, FromRow),
        between(1, 8, FromCol),
        get_piece(Board, FromRow, FromCol, Piece),
        Piece \= '.',
        get_piece_color(Piece, Opponent),
        valid_move(Board, Opponent, FromRow, FromCol, KingRow, KingCol)
    ), Attackers),
    length(Attackers, Count).

% Fonctions d'evaluation heritees supprimees - remplacees par architecture standard
% material_balance -> material_evaluation
% positional_bonus -> positional_evaluation avec piece-square tables
% mobility_bonus -> mobility_evaluation

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
% Alias pour compatibilite avec tests - retourne balance materielle pour un joueur
material_value(GameState, Player, Value) :-
    GameState = game_state(Board, _, _, _, _),
    findall(PieceValue, (
        between(1, 8, Row),
        between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        Piece \= '.',
        get_piece_color(Piece, Player),
        piece_value(Piece, PieceValue)
    ), Values),
    sum_list(Values, PlayerMaterial),
    
    % Calculer materiel adversaire
    opposite_player(Player, Opponent),
    findall(OppValue, (
        between(1, 8, Row),
        between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        Piece \= '.',
        get_piece_color(Piece, Opponent),
        piece_value(Piece, OppValue)
    ), OppValues),
    sum_list(OppValues, OpponentMaterial),
    
    Value is PlayerMaterial - OpponentMaterial.

% generate_legal_moves(+GameState, +Player, -Moves)
% Version CORRIGEE avec randomisation efficace pour eviter determinisme g8h6
generate_legal_moves(GameState, Player, SortedMoves) :-
    GameState = game_state(Board, _, _, _, _),
    
    % CORRECTION CRITIQUE: Generation avec verification echec obligatoire
    findall([FromRow, FromCol, ToRow, ToCol], (
        between(1, 8, FromRow),
        between(1, 8, FromCol), 
        get_piece(Board, FromRow, FromCol, Piece),
        Piece \= '.',
        get_piece_color(Piece, Player),
        between(1, 8, ToRow),
        between(1, 8, ToCol),
        valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol),
        % AJOUT CRITIQUE: Verifier que le coup ne laisse pas le roi en echec
        \+ move_leaves_king_in_check_ai(GameState, Player, FromRow, FromCol, ToRow, ToCol)
    ), AllLegalMoves),
    
    % NOUVELLE APPROCHE: Tri intelligent + randomisation forte
    prioritize_moves(GameState, AllLegalMoves, PrioritizedMoves),
    
    % RANDOMISATION RENFORCEE: Melanger 3 fois pour eliminer le determinisme
    random_permutation(PrioritizedMoves, Shuffled1),
    random_permutation(Shuffled1, Shuffled2), 
    random_permutation(Shuffled2, FullyRandomizedMoves),
    
    % Prendre les 20 meilleurs coups (augmente pour plus de variete)
    take_best_moves(FullyRandomizedMoves, 20, SortedMoves).

% prioritize_moves(+GameState, +Moves, -PrioritizedMoves)
% Tri intelligent MVV-LVA (Most Valuable Victim - Least Valuable Attacker)
prioritize_moves(GameState, Moves, SortedMoves) :-
    GameState = game_state(Board, _, _, _, _),
    evaluate_mvv_lva_priorities(Moves, Board, EvaluatedMoves),
    keysort(EvaluatedMoves, TempSorted),
    reverse(TempSorted, PrioritySorted),
    extract_moves_from_priority(PrioritySorted, SortedMoves).

% evaluate_mvv_lva_priorities(+Moves, +Board, -EvaluatedMoves)
% Evaluation MVV-LVA AMELIOREE : Captures > Promotions > Coups tactiques > Developpement > Normaux
evaluate_mvv_lva_priorities([], _, []).
evaluate_mvv_lva_priorities([Move|RestMoves], Board, [Priority-Move|RestEvaluated]) :-
    Move = [FromRow, FromCol, ToRow, ToCol],
    get_piece(Board, FromRow, FromCol, AttackerPiece),
    get_piece(Board, ToRow, ToCol, VictimPiece),
    
    (   VictimPiece \= '.' ->  % Capture
        % AMELIORATION: Valeurs absolues pour MVV-LVA correct
        abs_piece_value(VictimPiece, VictimValue),
        abs_piece_value(AttackerPiece, AttackerValue),
        % MVV-LVA: Grosse victime - petit attaquant = meilleur
        Priority is 10000 + VictimValue - AttackerValue
    ;   is_promotion_move_ai(FromRow, ToRow, AttackerPiece) ->
        Priority = 9000  % Promotions prioritaires
    ;   is_tactical_move(FromRow, FromCol, ToRow, ToCol, AttackerPiece) ->
        Priority = 1000  % NOUVEAU: Coups tactiques (forks, pins, etc.)
    ;   is_development_move(FromRow, FromCol, ToRow, ToCol, AttackerPiece) ->
        Priority = 500   % NOUVEAU: Coups de developpement
    ;   is_center_move(FromRow, FromCol, ToRow, ToCol) ->
        Priority = 200   % Coups vers centre
    ;   Priority = 100   % Coups normaux
    ),
    evaluate_mvv_lva_priorities(RestMoves, Board, RestEvaluated).

% abs_piece_value(+Piece, -AbsValue)
% Retourne la valeur absolue d'une piece (sans signe)
abs_piece_value(Piece, AbsValue) :-
    piece_value(Piece, Value),
    AbsValue is abs(Value).

% is_tactical_move(+FromRow, +FromCol, +ToRow, +ToCol, +Piece)
% Detecte les coups tactiques prioritaires
is_tactical_move(FromRow, FromCol, ToRow, ToCol, Piece) :-
    (   % Cavalier vers position de fork potentiel
        member(Piece, ['N', 'n']),
        is_knight_fork_position(ToRow, ToCol)
    ;   % Fou ou Dame vers diagonale puissante
        member(Piece, ['B', 'b', 'Q', 'q']),
        is_strong_diagonal(ToRow, ToCol)
    ;   % Tour vers colonne ouverte
        member(Piece, ['R', 'r']),
        is_open_file(ToCol)
    ).

% is_development_move(+FromRow, +FromCol, +ToRow, +ToCol, +Piece)  
% Detecte les coups de developpement
is_development_move(FromRow, FromCol, ToRow, ToCol, Piece) :-
    (   % Cavalier se developpe
        Piece = 'N', FromRow = 1, member(ToRow, [3, 4])
    ;   Piece = 'n', FromRow = 8, member(ToRow, [6, 5])  
    ;   % Fou se developpe
        Piece = 'B', FromRow = 1, member(ToRow, [2, 3, 4])
    ;   Piece = 'b', FromRow = 8, member(ToRow, [7, 6, 5])
    ).

% Heuristiques tactiques simplifiees
is_knight_fork_position(Row, Col) :-
    member(Row, [4, 5]), member(Col, [4, 5]).  % Centre-etendu

is_strong_diagonal(Row, Col) :-
    DiagSum is Row + Col,
    DiagDiff is Row - Col,
    member(DiagSum, [5, 6, 7, 8, 9]),  % Diagonales longues
    member(DiagDiff, [-3, -2, -1, 0, 1, 2, 3]).

is_open_file(Col) :-
    member(Col, [4, 5]).  % Colonnes centrales supposees ouvertes

% =============================================================================
% TRANSPOSITION TABLE - OPTIMISATION PERFORMANCE
% =============================================================================

% game_state_hash(+GameState, -Hash)
% Genere un hash simple de la position pour la transposition table
game_state_hash(GameState, Hash) :-
    GameState = game_state(Board, Player, MoveCount, _, _),
    % Hash simplifie : concatener representation board + joueur + coups
    board_to_string(Board, BoardString),
    atom_concat(BoardString, Player, TempHash1),
    atom_concat(TempHash1, MoveCount, Hash).

% board_to_string(+Board, -String)
% Convertit le plateau en string pour hachage
board_to_string([], '') :- !.
board_to_string([Row|RestRows], String) :-
    row_to_string(Row, RowString),
    board_to_string(RestRows, RestString),
    atom_concat(RowString, RestString, String).

% row_to_string(+Row, -String)
% Convertit une rangee en string
row_to_string([], '') :- !.
row_to_string([Piece|RestPieces], String) :-
    row_to_string(RestPieces, RestString),
    atom_concat(Piece, RestString, String).

% is_promotion_move_ai(+FromRow, +ToRow, +Piece)
% Detecte les mouvements de promotion pour IA
is_promotion_move_ai(7, 8, 'P').  % Pion blanc atteint 8e rangee
is_promotion_move_ai(2, 1, 'p').  % Pion noir atteint 1ere rangee

% is_center_move(+FromRow, +FromCol, +ToRow, +ToCol)
% Detecte les coups vers le centre (e4, e5, d4, d5)
is_center_move(_, _, ToRow, ToCol) :-
    member(ToRow, [4, 5]),
    member(ToCol, [4, 5]).

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

% randomize_equal_moves(+Moves, -RandomizedMoves)
% Randomise l'ordre des coups de priorite egale pour eviter determinisme
randomize_equal_moves([], []).
randomize_equal_moves([Move], [Move]) :- !.
randomize_equal_moves(Moves, RandomizedMoves) :-
    random_permutation(Moves, RandomizedMoves).

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
    retractall(ai_best_value(_)),
    retractall(position_cache(_, _, _)).  % Nettoyer le cache aussi

% emergency_move_selection(+GameState, -BestMove)
% Selection d'urgence AMELIOREE - priorite aux captures et coups forts
emergency_move_selection(GameState, BestMove) :-
    GameState = game_state(_, Player, _, _, _),
    generate_legal_moves(GameState, Player, Moves),
    (   Moves = [] ->
        write('ERREUR CRITIQUE: Aucun coup legal trouve!'), nl,
        BestMove = [1,1,1,1]  % Coup impossible pour debug
    ;   % AMELIORATION: Priorite aux captures d'abord, sinon aleatoire
        separate_captures_and_normal_moves(GameState, Moves, Captures, NormalMoves),
        (   Captures \= [] ->
            random_member(BestMove, Captures),
            write('COUP D\'URGENCE (capture): '), write(BestMove), nl
        ;   random_member(BestMove, NormalMoves),
            write('COUP D\'URGENCE (normal): '), write(BestMove), nl
        )
    ).

% move_leaves_king_in_check_ai(+GameState, +Player, +FromRow, +FromCol, +ToRow, +ToCol)
% Verifie si un coup laisse le roi du joueur en echec - version IA
move_leaves_king_in_check_ai(GameState, Player, FromRow, FromCol, ToRow, ToCol) :-
    GameState = game_state(Board, _, _, _, _),
    simulate_move_ai(Board, FromRow, FromCol, ToRow, ToCol, NewBoard),
    SimulatedGameState = game_state(NewBoard, Player, 0, active, [[], []]),
    is_in_check(SimulatedGameState, Player).

% simulate_move_ai(+Board, +FromRow, +FromCol, +ToRow, +ToCol, -NewBoard)
% Simule un coup sur l'echiquier pour verification echec
simulate_move_ai(Board, FromRow, FromCol, ToRow, ToCol, NewBoard) :-
    get_piece(Board, FromRow, FromCol, MovingPiece),
    place_single_piece(Board, FromRow, FromCol, ' ', TempBoard),
    place_single_piece(TempBoard, ToRow, ToCol, MovingPiece, NewBoard).

% separate_captures_and_normal_moves(+GameState, +Moves, -Captures, -NormalMoves)
% Separe les captures des coups normaux pour priorite
separate_captures_and_normal_moves(_, [], [], []).
separate_captures_and_normal_moves(GameState, [Move|RestMoves], Captures, NormalMoves) :-
    GameState = game_state(Board, _, _, _, _),
    Move = [_, _, ToRow, ToCol],
    get_piece(Board, ToRow, ToCol, TargetPiece),
    separate_captures_and_normal_moves(GameState, RestMoves, RestCaptures, RestNormal),
    (   TargetPiece \= '.' ->  % C'est une capture
        Captures = [Move|RestCaptures],
        NormalMoves = RestNormal
    ;   % Coup normal
        Captures = RestCaptures,
        NormalMoves = [Move|RestNormal]
    ).

% =============================================================================
% REPERTOIRE D'OUVERTURES INTEGRE
% =============================================================================

% opening_move(+GameState, -Move)
% Cherche un coup dans le répertoire d'ouvertures (6 premiers coups)
opening_move(GameState, Move) :-
    GameState = game_state(Board, Player, MoveCount, _, _),
    MoveCount =< 12,  % 6 coups par joueur
    opening_database(Board, Player, MoveCount, FromRow, FromCol, ToRow, ToCol),
    Move = [FromRow, FromCol, ToRow, ToCol].

% opening_database(+Board, +Player, +MoveCount, -FromRow, -FromCol, -ToRow, -ToCol)
% Base compacte d'ouvertures standard

% OUVERTURES BLANCHES
% 1. e4 - Roi Indien
opening_database(_, white, 1, 2, 5, 4, 5).  % e2e4
% 1. d4 - Ouverture dame  
opening_database(_, white, 1, 2, 4, 4, 4).  % d2d4
% 1. Nf3 - Ouverture Réti
opening_database(_, white, 1, 1, 7, 3, 6).  % Ng1f3
% 1. c4 - Ouverture Anglaise
opening_database(_, white, 1, 2, 3, 4, 3).  % c2c4

% REPONSES NOIRES à 1.e4
% 1...e5 (Défense classique)
opening_database(_, black, 2, 7, 5, 5, 5).  % e7e5 après 1.e4
% 1...c5 (Défense Sicilienne)
opening_database(_, black, 2, 7, 3, 5, 3).  % c7c5 après 1.e4
% 1...e6 (Défense Française)
opening_database(_, black, 2, 7, 5, 6, 5).  % e7e6 après 1.e4
% 1...c6 (Défense Caro-Kann)
opening_database(_, black, 2, 7, 3, 6, 3).  % c7c6 après 1.e4

% REPONSES NOIRES à 1.d4
% 1...d5 (Défense Dame)
opening_database(_, black, 2, 7, 4, 5, 4).  % d7d5 après 1.d4
% 1...Nf6 (Défense Indienne)
opening_database(_, black, 2, 8, 7, 6, 6).  % Ng8f6 après 1.d4

% DEVELOPPEMENTS BLANCS (coups 2-3)
% Après 1.e4 e5: 2.Nf3
opening_database(_, white, 3, 1, 7, 3, 6).  % Ng1f3
% Après 1.e4 e5 2.Nf3: 3.Bc4 (Ouverture Italienne)
opening_database(_, white, 5, 1, 6, 4, 3).  % Bf1c4
% Après 1.e4 e5 2.Nf3: 3.Bb5 (Ouverture Espagnole)
opening_database(_, white, 5, 1, 6, 4, 2).  % Bf1b5

% DEVELOPPEMENTS NOIRS (coups 2-3)
% Après 1.e4 e5 2.Nf3: 2...Nc6
opening_database(_, black, 4, 8, 2, 6, 3).  % Nb8c6
% Après 1.e4 e5 2.Nf3 Nc6: 3...Nf6
opening_database(_, black, 6, 8, 7, 6, 6).  % Ng8f6

% choose_ai_move_with_openings(+GameState, -BestMove)
% Version avec répertoire d'ouvertures intégré
choose_ai_move_with_openings(GameState, BestMove) :-
    % D'abord chercher dans le livre d'ouvertures
    (   opening_move(GameState, OpeningMove) ->
        % Vérifier que le coup d'ouverture est légal
        OpeningMove = [FromRow, FromCol, ToRow, ToCol],
        GameState = game_state(Board, Player, _, _, _),
        (   valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol) ->
            BestMove = OpeningMove
        ;   % Coup d'ouverture illégal - fallback minimax
            choose_ai_move(GameState, BestMove)
        )
    ;   % Pas d'ouverture trouvée - utiliser minimax
        choose_ai_move(GameState, BestMove)
    ).

% =============================================================================
% CONFIGURATION ET INTERFACE
% =============================================================================

% ai_search_depth(-Depth)
% Configure la profondeur de recherche selon performance OPTIMISEE
ai_search_depth(2).  % AMELIORATION: Profondeur 2 avec optimisations (objectif <1s)

% Interface IA integree dans le systeme principal (interface.pl)
% Utilisez start_ai_game/0 pour lancer une partie contre l'IA
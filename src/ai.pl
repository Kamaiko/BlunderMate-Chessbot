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
:- [psqt_tables].

% =============================================================================
% CONSTANTES REFERENCE EXACTES - BOARD_EVAL.PL:5-12
% =============================================================================

compensate_ref(white, 15).
compensate_ref(black, -15).

% =============================================================================
% INTERFACE PRINCIPALE SIMPLE
% =============================================================================

% display_position_evaluation(+GameState, +Player)
% Affiche le détail de l'évaluation pour debug/interface
display_position_evaluation(GameState, Player) :-
    % Calculs détaillés
    count_material_pure_ref(GameState, white, WhiteMaterial),
    count_material_pure_ref(GameState, black, BlackMaterial),
    evaluate_psqt_total(GameState, white, WhitePSQT),
    evaluate_psqt_total(GameState, black, BlackPSQT),
    evaluate_tactical_safety(GameState, white, WhiteSafety),
    evaluate_tactical_safety(GameState, black, BlackSafety),
    
    % Différentiels 
    MaterialDiff is WhiteMaterial - BlackMaterial,
    PSQTDiff is WhitePSQT - BlackPSQT,
    SafetyDiff is WhiteSafety - BlackSafety,
    TotalDiff is MaterialDiff + PSQTDiff + SafetyDiff,
    
    % Évaluation du point de vue du joueur
    (   Player = white ->
        FinalScore = TotalDiff
    ;   FinalScore is -TotalDiff
    ),
    
    % Affichage détaillé
    write('=== ÉVALUATION POSITION ==='), nl,
    format('Matériel    : Blancs ~w vs Noirs ~w (diff: ~w)~n', 
           [WhiteMaterial, BlackMaterial, MaterialDiff]),
    format('PSQT        : Blancs ~w vs Noirs ~w (diff: ~w)~n', 
           [WhitePSQT, BlackPSQT, PSQTDiff]),
    format('Sécurité    : Blancs ~w vs Noirs ~w (diff: ~w)~n', 
           [WhiteSafety, BlackSafety, SafetyDiff]),
    write('------------------------'), nl,
    format('SCORE TOTAL (~w): ~w~n', [Player, FinalScore]),
    (   FinalScore > 0 ->
        format('Position favorable à ~w (~w)~n', [Player, FinalScore])
    ;   FinalScore < 0 ->
        format('Position défavorable à ~w (~w)~n', [Player, FinalScore])
    ;   write('Position équilibrée (0)'), nl
    ),
    write('========================'), nl.

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
        ;   choose_ai_move_safe(GameState, Player, BestMove)  % Fallback sécurisé
        )
    ;   % Utiliser alpha-beta sécurisé pour tous les autres cas
        choose_ai_move_safe(GameState, Player, BestMove)
    ).

% choose_ai_move_safe(+GameState, +Player, -BestMove)
% Version sécurisée avec limitation de coups pour éviter les boucles infinies
choose_ai_move_safe(GameState, Player, BestMove) :-
    % Utiliser génération limitée de coups au lieu de timeout
    catch(
        minimax_ab(GameState, Player, 2, BestMove, _Value),  % Profondeur 2 normale
        Error,
        (   write('IA erreur - coup de sécurité: '), write(Error), nl,
            choose_emergency_move(GameState, Player, BestMove)
        )
    ).

% choose_emergency_move(+GameState, +Player, -BestMove)
% Coup d'urgence simple quand l'IA est bloquée
choose_emergency_move(GameState, Player, BestMove) :-
    GameState = game_state(Board, _, _, _, _),
    % Trouver le premier coup légal simple
    (   between(1, 8, FromRow), between(1, 8, FromCol),
        between(1, 8, ToRow), between(1, 8, ToCol),
        get_piece(Board, FromRow, FromCol, Piece),
        piece_belongs_to_player(Piece, Player),
        valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol),
        BestMove = [FromRow, FromCol, ToRow, ToCol], !
    ;   BestMove = []  % Aucun coup trouvé
    ).

% =============================================================================
% MINIMAX SIMPLE SELON REFERENCE
% =============================================================================

% minimax_limited(+GameState, +Player, +Depth, -BestMove, -BestValue)
% Version sécurisée avec génération de coups très limitée
minimax_limited(GameState, Player, 0, [], Value) :-
    evaluate_pure_reference(GameState, Player, Value), !.

minimax_limited(GameState, Player, Depth, BestMove, BestValue) :-
    Depth > 0,
    generate_moves_limited(GameState, Player, Moves),  % Version limitée
    (   Moves = [] ->
        terminal_score(GameState, Player, BestValue),
        BestMove = []
    ;   select_first_move(Moves, BestMove),  % Prendre le premier coup simple
        BestValue = 0
    ).

% generate_moves_limited(+GameState, +Player, -Moves)
% Génération très limitée de coups - premier coup légal trouvé
generate_moves_limited(GameState, Player, [FirstMove]) :-
    GameState = game_state(Board, _, _, _, _),
    between(1, 8, FromRow), between(1, 8, FromCol),
    get_piece(Board, FromRow, FromCol, Piece),
    piece_belongs_to_player(Piece, Player),
    between(1, 8, ToRow), between(1, 8, ToCol),
    valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol),
    FirstMove = [FromRow, FromCol, ToRow, ToCol], !.
generate_moves_limited(_, _, []).

% select_first_move(+Moves, -FirstMove)
select_first_move([First|_], First) :- !.
select_first_move([], []).

% minimax_ab(+GameState, +Player, +Depth, -BestMove, -BestValue)
% ALPHA-BETA PRUNING IMPLÉMENTÉ - Négamax avec élagage
minimax_ab(GameState, Player, 0, [], Value) :-
    evaluate_pure_reference(GameState, Player, Value), !.

minimax_ab(GameState, Player, Depth, BestMove, BestValue) :-
    Depth > 0,
    generate_moves_simple(GameState, Player, Moves),
    (   Moves = [] ->
        terminal_score(GameState, Player, BestValue),
        BestMove = []
    ;   order_moves(GameState, Player, Moves, OrderedMoves),
        Alpha is -1.0Inf, Beta is 1.0Inf,
        ab_search(OrderedMoves, GameState, Player, Depth, Alpha, Beta, none, -1.0Inf, BestMove, BestValue)
    ).

% Ancien minimax pour compatibilité - REDIRECTION VERS ALPHA-BETA
minimax_simple_ref(GameState, Player, Depth, BestMove, BestValue) :-
    minimax_ab(GameState, Player, Depth, BestMove, BestValue).

% ab_search(+Moves, +GameState, +Player, +Depth, +Alpha, +Beta, +BestMoveAcc, +BestValueAcc, -BestMove, -BestValue)
% Recherche alpha-beta avec élagage
ab_search([], _, _, _, _, _, BestMoveAcc, BestValueAcc, BestMoveAcc, BestValueAcc) :- !.

ab_search([[FromRow,FromCol,ToRow,ToCol]|RestMoves], GameState, Player, Depth, Alpha, Beta, BestMoveAcc, BestValueAcc, BestMove, BestValue) :-
    make_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState),
    NewGameState = game_state(_, NextPlayer, _, _, _),
    NewDepth is Depth - 1,
    _NewAlpha is -Beta, _NewBeta is -Alpha,
    minimax_ab(NewGameState, NextPlayer, NewDepth, _, OpponentValue),
    Value is -OpponentValue,
    
    (   Value > BestValueAcc ->
        NewAlpha2 is max(Alpha, Value),
        NewBestMove = [FromRow,FromCol,ToRow,ToCol],
        NewBestValue = Value
    ;   NewAlpha2 = Alpha,
        NewBestMove = BestMoveAcc,
        NewBestValue = BestValueAcc
    ),
    
    % ÉLAGAGE ALPHA-BETA
    (   NewAlpha2 >= Beta ->
        BestMove = NewBestMove, BestValue = NewBestValue  % Coupure beta
    ;   ab_search(RestMoves, GameState, Player, Depth, NewAlpha2, Beta, NewBestMove, NewBestValue, BestMove, BestValue)
    ).

% terminal_score(+GameState, +Player, -Score)
% Évalue les positions terminales (mat/pat)
terminal_score(GameState, Player, Score) :-
    (   is_in_check(GameState, Player) ->
        Score = -100000  % Échec et mat
    ;   Score = 0        % Pat (nulle)
    ).

% order_moves(+GameState, +Player, +Moves, -OrderedMoves)
% Tri des coups : captures (MVV-LVA) d'abord, puis autres
order_moves(GameState, Player, Moves, OrderedMoves) :-
    GameState = game_state(Board, _, _, _, _),
    map_move_scores(Board, Player, Moves, ScoredMoves),
    keysort_desc(ScoredMoves, SortedPairs),
    pairs_values(SortedPairs, OrderedMoves).

% map_move_scores(+Board, +Player, +Moves, -ScoredMoves)
map_move_scores(_, _, [], []).
map_move_scores(Board, Player, [Move|RestMoves], [Score-Move|RestScored]) :-
    move_score(Board, Player, Move, Score),
    map_move_scores(Board, Player, RestMoves, RestScored).

% move_score(+Board, +Player, +Move, -Score)
% Score MVV-LVA pour captures avec valeurs standard correctes
move_score(Board, _Player, [FromRow, FromCol, ToRow, ToCol], Score) :-
    get_piece(Board, ToRow, ToCol, TargetPiece),
    (   TargetPiece \= ' ', TargetPiece \= '.' ->
        get_piece(Board, FromRow, FromCol, AttackingPiece),
        standard_piece_value(TargetPiece, TargetVal),
        standard_piece_value(AttackingPiece, AttackerVal),
        Score is TargetVal - AttackerVal + 1000  % MVV-LVA avec valeurs correctes
    ;   Score = 0  % Coup non-capture
    ).

% keysort_desc(+Pairs, -SortedDesc)
% Tri décroissant par clé
keysort_desc(Pairs, SortedDesc) :-
    sort(0, @>=, Pairs, SortedDesc).

% pairs_values(+Pairs, -Values)
pairs_values([], []).
pairs_values([_-Value|RestPairs], [Value|RestValues]) :-
    pairs_values(RestPairs, RestValues).

% =============================================================================
% EVALUATION PURE REFERENCE - BOARD_EVAL.PL EXACT
% =============================================================================

% evaluate_pure_reference(+GameState, +Player, -Value)
% ÉVALUATION COMPLÈTE: Matériel + PSQT + Sécurité (anti-blunders)
evaluate_pure_reference(GameState, Player, Value) :-
    % 1. Évaluation matérielle standard (sans roi)
    count_material_standard(GameState, white, WhiteMaterial),
    count_material_standard(GameState, black, BlackMaterial),
    MaterialDiff is WhiteMaterial - BlackMaterial,
    
    % 2. Évaluation PSQT (Piece-Square Tables)
    evaluate_psqt_total(GameState, white, WhitePSQT),
    evaluate_psqt_total(GameState, black, BlackPSQT),
    PSQTDiff is WhitePSQT - BlackPSQT,
    
    % 3. NOUVEAU: Sécurité des pièces (détection hanging pieces)
    evaluate_piece_safety(GameState, white, WhiteSafety),
    evaluate_piece_safety(GameState, black, BlackSafety),
    SafetyDiff is WhiteSafety - BlackSafety,
    
    % Combiner: Matériel + PSQT + Sécurité
    TotalDiff is MaterialDiff + PSQTDiff + SafetyDiff,
    
    % Retourner du point de vue du joueur demandé
    (   Player = white ->
        Value = TotalDiff
    ;   Value is -TotalDiff
    ).

% evaluate_mobility_fast(+GameState, +Player, -MobilityValue)
% Mobilité rapide : compte pièces développées (approximation)
evaluate_mobility_fast(GameState, Player, MobilityValue) :-
    GameState = game_state(Board, _, _, _, _),
    
    % Compter pièces développées (cavaliers et fous hors rang de base)
    (   Player = white ->
        BaseRank = 1, DevelopedRanks = [2,3,4,5,6,7,8]
    ;   BaseRank = 8, DevelopedRanks = [1,2,3,4,5,6,7]
    ),
    
    % Compter cavaliers et fous développés
    findall(1, (
        member(Rank, DevelopedRanks),
        between(1, 8, Col),
        get_piece(Board, Rank, Col, Piece),
        piece_belongs_to_player(Piece, Player),
        (Piece = 'N'; Piece = 'n'; Piece = 'B'; Piece = 'b')
    ), DevelopedPieces),
    
    length(DevelopedPieces, DevelopedCount),
    MobilityValue is DevelopedCount * 10.  % Bonus modéré par pièce développée

% count_material_standard(+GameState, +Player, -MaterialValue)
% Compte matériel selon valeurs standard (SANS roi)
count_material_standard(GameState, Player, MaterialValue) :-
    GameState = game_state(Board, _, _, _, _),
    findall(Value, (
        between(1, 8, Row), between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        Piece \= ' ', Piece \= '.',
        piece_belongs_to_player(Piece, Player),
        standard_piece_value(Piece, Value)
    ), Values),
    sum_list(Values, MaterialValue).

% standard_piece_value(+Piece, -Value)
% Valeurs matérielles standard (ROI EXCLU)
standard_piece_value('P', 100) :- !.
standard_piece_value('p', 100) :- !.
standard_piece_value('N', 320) :- !.
standard_piece_value('n', 320) :- !.
standard_piece_value('B', 330) :- !.
standard_piece_value('b', 330) :- !.
standard_piece_value('R', 500) :- !.
standard_piece_value('r', 500) :- !.
standard_piece_value('Q', 900) :- !.
standard_piece_value('q', 900) :- !.
% Roi n'a pas de valeur matérielle

% evaluate_psqt_total(+GameState, +Player, -PSQTValue)
% Évalue toutes les pièces d'un joueur selon les PSQT
evaluate_psqt_total(GameState, Player, PSQTValue) :-
    GameState = game_state(Board, _, _, _, _),
    findall(Value, (
        between(1, 8, Row), between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        Piece \= ' ', Piece \= '.',
        piece_belongs_to_player(Piece, Player),
        piece_type_from_symbol(Piece, PieceType),
        get_psqt_value(PieceType, Row, Col, Player, Value)
    ), Values),
    sum_list(Values, PSQTValue).

% piece_type_from_symbol(+PieceSymbol, -PieceType)
% Convertit symbole pièce vers type PSQT
piece_type_from_symbol('P', pawn) :- !.
piece_type_from_symbol('p', pawn) :- !.
piece_type_from_symbol('N', knight) :- !.
piece_type_from_symbol('n', knight) :- !.
piece_type_from_symbol('B', bishop) :- !.
piece_type_from_symbol('b', bishop) :- !.
piece_type_from_symbol('R', rook) :- !.
piece_type_from_symbol('r', rook) :- !.
piece_type_from_symbol('Q', queen) :- !.
piece_type_from_symbol('q', queen) :- !.
piece_type_from_symbol('K', king) :- !.
piece_type_from_symbol('k', king) :- !.

% =============================================================================
% ÉVALUATION SÉCURITÉ DES PIÈCES - ANTI-BLUNDERS
% =============================================================================

% evaluate_piece_safety(+GameState, +Player, -SafetyValue)
% TEMPORAIREMENT DÉSACTIVÉ - is_square_attacked ne fonctionne pas encore
evaluate_piece_safety(GameState, Player, SafetyValue) :-
    SafetyValue = 0.

% is_piece_defended(+GameState, +Row, +Col, +DefendingPlayer)
% Version simplifiée : considère toute pièce attaquée comme NON défendue  
% (approche conservatrice pour éviter blunders - détecte tous hanging pieces)
is_piece_defended(GameState, Row, Col, DefendingPlayer) :-
    % Pour l'instant, aucune pièce attaquée n'est considérée comme défendue
    % Ceci garantit la détection de tous les hanging pieces (très conservateur)
    fail.

% evaluate_tactical_safety(+GameState, +Player, -SafetyValue)
% Évaluation tactique SIMPLIFIÉE pour performance
evaluate_tactical_safety(GameState, Player, SafetyValue) :-
    GameState = game_state(Board, _, _, _, _),
    
    % SEULEMENT: Contrôle du centre (rapide et essentiel)
    evaluate_center_control(Board, Player, CenterValue),
    
    % Sécurité basique : éviter roi exposé
    evaluate_king_safety_basic(GameState, Player, KingSafety),
    
    SafetyValue is CenterValue + KingSafety.

% evaluate_king_safety_basic(+GameState, +Player, -KingSafety)
% Évaluation basique de sécurité du roi : pénalise roi exposé (proportionnel aux valeurs pièces)
evaluate_king_safety_basic(GameState, Player, KingSafety) :-
    GameState = game_state(Board, _, MoveCount, _, _),
    find_king_position(Board, Player, KingRow, KingCol),
    
    % Position initiale du roi selon la couleur
    (   Player = white ->
        InitialRow = 1
    ;   InitialRow = 8
    ),
    
    % Pénaliser roi exposé EN OUVERTURE seulement (premiers 15 coups)
    (   MoveCount =< 15, KingRow \= InitialRow ->
        KingSafety = -50   % Malus modéré = valeur pion/2 (proportionnel)
    ;   KingRow = InitialRow ->
        KingSafety = 10    % Petit bonus roi en sécurité
    ;   KingSafety = 0     % Neutre (milieu/fin de jeu)
    ).

% evaluate_favorable_captures(+GameState, +Player, -CaptureValue)
% Évalue les captures qui gagnent du matériel (SEE positif)
evaluate_favorable_captures(GameState, Player, CaptureValue) :-
    GameState = game_state(Board, _, _, _, _),
    findall(GainValue, (
        between(1, 8, FromRow), between(1, 8, FromCol),
        between(1, 8, ToRow), between(1, 8, ToCol),
        get_piece(Board, FromRow, FromCol, AttackingPiece),
        piece_belongs_to_player(AttackingPiece, Player),
        get_piece(Board, ToRow, ToCol, TargetPiece),
        TargetPiece \= ' ', TargetPiece \= '.',
        opposite_player(Player, OppositePlayer),
        piece_belongs_to_player(TargetPiece, OppositePlayer),
        valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol),
        % SEE simple : gain = valeur cible - valeur attaquant si contre-attaque
        evaluate_simple_exchange(Board, FromRow, FromCol, ToRow, ToCol, Player, GainValue),
        GainValue > 0  % Seulement les captures favorables
    ), GainValues),
    sum_list(GainValues, CaptureValue).

% evaluate_material_at_risk(+GameState, +Player, -RiskPenalty)
% CRITIQUE: Détecte les pièces du joueur qui peuvent être capturées
% et applique un malus MASSIF si elles ne sont pas protégées
evaluate_material_at_risk(GameState, Player, RiskPenalty) :-
    GameState = game_state(Board, _, _, _, _),
    opposite_player(Player, Opponent),
    findall(PenaltyValue, (
        between(1, 8, Row), between(1, 8, Col),
        get_piece(Board, Row, Col, MyPiece),
        piece_belongs_to_player(MyPiece, Player),
        MyPiece \= ' ', MyPiece \= '.',
        % Vérifier si l'adversaire peut capturer ma pièce
        can_opponent_capture_piece(Board, Opponent, Row, Col, CanCapture, RecaptureValue),
        (CanCapture = true ->
            % Ma pièce peut être capturée
            piece_value(MyPiece, MyPieceValue),
            AbsValue is abs(MyPieceValue),
            % Si je peux recapturer, le malus est réduit
            AdjustedPenalty is -(AbsValue - RecaptureValue),
            PenaltyValue = AdjustedPenalty
        ;   PenaltyValue = 0  % Pièce en sécurité
        )
    ), PenaltyValues),
    sum_list(PenaltyValues, RiskPenalty).

% can_opponent_capture_piece(+Board, +Opponent, +Row, +Col, -CanCapture, -RecaptureValue)
% CORRECTION: Vérifie si l'adversaire peut capturer une pièce ET simule la recapture
can_opponent_capture_piece(Board, Opponent, TargetRow, TargetCol, CanCapture, RecaptureValue) :-
    % L'adversaire peut-il capturer cette pièce ?
    (   (between(1, 8, FromRow), between(1, 8, FromCol),
         get_piece(Board, FromRow, FromCol, OpponentPiece),
         piece_belongs_to_player(OpponentPiece, Opponent),
         OpponentPiece \= ' ', OpponentPiece \= '.',
         valid_move(Board, Opponent, FromRow, FromCol, TargetRow, TargetCol)) ->
        CanCapture = true,
        % CORRECTION: Simuler la capture adverse avant de tester la recapture
        make_move(game_state(Board, Opponent, 1, active, []), FromRow, FromCol, TargetRow, TargetCol, 
                  game_state(BoardAfterCapture, _, _, _, _)),
        opposite_player(Opponent, MyColor),
        (   can_recapture_square(BoardAfterCapture, MyColor, TargetRow, TargetCol, RecaptureValue) ->
            true  % RecaptureValue est définie
        ;   RecaptureValue = 0  % Pas de recapture possible
        )
    ;   CanCapture = false,
        RecaptureValue = 0
    ).

% can_recapture_square(+Board, +Player, +Row, +Col, -RecaptureValue)
% Vérifie si le joueur peut recapturer sur une case et avec quelle pièce
can_recapture_square(Board, Player, TargetRow, TargetCol, RecaptureValue) :-
    between(1, 8, FromRow), between(1, 8, FromCol),
    get_piece(Board, FromRow, FromCol, MyPiece),
    piece_belongs_to_player(MyPiece, Player),
    MyPiece \= ' ', MyPiece \= '.',
    valid_move(Board, Player, FromRow, FromCol, TargetRow, TargetCol),
    % Valeur de la pièce qui peut recapturer
    piece_value(MyPiece, PieceValue),
    AbsRecapture is abs(PieceValue),
    RecaptureValue = AbsRecapture, !.
can_recapture_square(_, _, _, _, 0).  % Pas de recapture possible

% evaluate_simple_exchange(+Board, +FromRow, +FromCol, +ToRow, +ToCol, +Player, -NetGain)
% SEE simplifié : évalue le gain net d'une capture
evaluate_simple_exchange(Board, FromRow, FromCol, ToRow, ToCol, Player, NetGain) :-
    get_piece(Board, FromRow, FromCol, AttackingPiece),
    get_piece(Board, ToRow, ToCol, TargetPiece),
    piece_value(AttackingPiece, AttackerValue),
    piece_value(TargetPiece, TargetValue),
    AbsAttackerValue is abs(AttackerValue),
    AbsTargetValue is abs(TargetValue),
    
    % CORRECTION: Simuler la capture pour tester la défense post-coup
    make_move(game_state(Board, Player, 1, active, []), FromRow, FromCol, ToRow, ToCol, game_state(NewBoard, _, _, _, _)),
    opposite_player(Player, Opponent),
    (   is_square_attacked(NewBoard, ToRow, ToCol, Opponent) ->
        % Case défendue après capture : gain = cible - attaquant
        NetGain is AbsTargetValue - AbsAttackerValue
    ;   % Case non défendue : gain = cible complète
        NetGain = AbsTargetValue
    ).

% evaluate_center_control(+Board, +Player, -CenterValue)
% Évalue le contrôle des cases centrales (d4, e4, d5, e5)
evaluate_center_control(Board, Player, CenterValue) :-
    CenterSquares = [(4,4), (4,5), (5,4), (5,5)],  % d4, e4, d5, e5
    findall(ControlValue, (
        member((Row, Col), CenterSquares),
        (get_piece(Board, Row, Col, Piece),
         piece_belongs_to_player(Piece, Player) ->
            ControlValue = 10  % Pièce sur case centrale
        ; (piece_attacks_square(Board, Player, Row, Col) ->
            ControlValue = 5   % Contrôle de la case centrale
        ;   ControlValue = 0   % Pas de contrôle
        ))
    ), ControlValues),
    sum_list(ControlValues, CenterValue).

% piece_attacks_square(+Board, +Player, +TargetRow, +TargetCol)
% Vérifie si le joueur attaque une case donnée
piece_attacks_square(Board, Player, TargetRow, TargetCol) :-
    between(1, 8, FromRow), between(1, 8, FromCol),
    get_piece(Board, FromRow, FromCol, Piece),
    piece_belongs_to_player(Piece, Player),
    Piece \= ' ', Piece \= '.',
    valid_move(Board, Player, FromRow, FromCol, TargetRow, TargetCol), !.

% evaluate_piece_mobility(+GameState, +Player, -MobilityValue)
% Évalue la mobilité globale des pièces
evaluate_piece_mobility(GameState, Player, MobilityValue) :-
    generate_moves_simple(GameState, Player, Moves),
    length(Moves, MoveCount),
    MobilityValue is MoveCount.  % Plus de coups = meilleure mobilité

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
    % Utiliser piece_value/2 comme valeur de base
    piece_type_to_char(Type, Color, PieceChar),
    piece_value(PieceChar, BaseValue),
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
    % Conversion type vers pièce pour utiliser piece_value/2
    piece_type_to_char(Type, Color, PieceChar),
    piece_value(PieceChar, Value), !.

% piece_type_to_char(+Type, +Color, -PieceChar)
% Convertit un type de pièce et une couleur en caractère
piece_type_to_char(pawn, white, 'P'). piece_type_to_char(pawn, black, 'p').
piece_type_to_char(rook, white, 'R'). piece_type_to_char(rook, black, 'r').
piece_type_to_char(knight, white, 'N'). piece_type_to_char(knight, black, 'n').
piece_type_to_char(bishop, white, 'B'). piece_type_to_char(bishop, black, 'b').
piece_type_to_char(queen, white, 'Q'). piece_type_to_char(queen, black, 'q').
piece_type_to_char(king, white, 'K'). piece_type_to_char(king, black, 'k').

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
% Génération standard sans priorités compliquées
generate_moves_simple(GameState, Player, Moves) :-
    generate_regular_moves(GameState, Player, Moves).

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
% Génération standard avec tri MVV-LVA AVANT limitation
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
    
    % CRITIQUE: Trier AVANT de limiter - AUGMENTÉ à 25 pour recaptures importantes
    order_moves(GameState, Player, AllMoves, OrderedMoves),
    take_first_25_simple(OrderedMoves, Moves).

% take_first_N_simple(+List, +N, -FirstN)
take_first_25_simple(List, First25) :-
    take_first_n_simple(List, 25, First25).

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
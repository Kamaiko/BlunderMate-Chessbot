% =============================================================================
% MODULE IA ECHECS - Negamax avec Elagage Alpha-Beta
% =============================================================================
%
% Implementation IA principale utilisant :
% - Algorithme negamax avec elagage alpha-beta (profondeur 2)
% - Tri MVV-LVA (Most Valuable Victim - Least Valuable Attacker)
% - Evaluation PSQT (Piece-Square Tables)
% - Evaluation materielle + positionnelle
%
% =============================================================================

:- [pieces].
:- [board].
:- [game].
:- [evaluation].

% =============================================================================
% CONSTANTES IA
% =============================================================================

% Configuration algorithme IA
negamax_depth(2).         % Profondeur recherche négamax + alpha-beta
ai_move_limit(25).        % Nombre maximum coups évalués (recaptures importantes)
ai_opening_moves(20).     % Limite coups en ouverture
ai_development_limit(8).  % Limite pièces développement prioritaire

% Limites sécurité et performance
ai_max_recursion(8).      % Protection récursion infinie (taille échiquier)

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
    evaluate_piece_safety(GameState, white, WhiteSafety),
    evaluate_piece_safety(GameState, black, BlackSafety),
    
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
        (negamax_depth(Depth), negamax_ab(GameState, Player, Depth, -1.0Inf, 1.0Inf, BestMove, _Value)),
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
% NÉGAMAX ALPHA-BETA - ALGORITHME IA PRINCIPAL
% =============================================================================

% negamax_ab(+GameState, +Player, +Depth, +Alpha, +Beta, -BestMove, -BestValue)
% NÉGAMAX AVEC ÉLAGAGE ALPHA-BETA - Algorithme de recherche principal

% negamax_ab_with_stats(+GameState, +Player, +Depth, +Alpha, +Beta, -BestMove, -BestValue, +NodesIn, -NodesOut)
% VERSION TEST - Même algorithme mais avec comptage des nœuds explorés pour validation élagage
negamax_ab_with_stats(GameState, Player, 0, _Alpha, _Beta, [], Value, NodesIn, NodesOut) :-
    NodesOut is NodesIn + 1,
    evaluate_pure_reference(GameState, Player, Value), !.

negamax_ab_with_stats(GameState, Player, Depth, Alpha, Beta, BestMove, BestValue, NodesIn, NodesOut) :-
    Depth > 0,
    NodesCount1 is NodesIn + 1,  % Compter ce nœud
    generate_moves_simple(GameState, Player, Moves),
    (   Moves = [] ->
        terminal_score(GameState, Player, BestValue),
        BestMove = [],
        NodesOut = NodesCount1
    ;   order_moves(GameState, Player, Moves, OrderedMoves),
        ab_search_with_stats(OrderedMoves, GameState, Player, Depth, Alpha, Beta, none, -1.0Inf, BestMove, BestValue, NodesCount1, NodesOut)
    ).
negamax_ab(GameState, Player, 0, _Alpha, _Beta, [], Value) :-
    evaluate_pure_reference(GameState, Player, Value), !.

negamax_ab(GameState, Player, Depth, Alpha, Beta, BestMove, BestValue) :-
    Depth > 0,
    generate_moves_simple(GameState, Player, Moves),
    (   Moves = [] ->
        terminal_score(GameState, Player, BestValue),
        BestMove = []
    ;   order_moves(GameState, Player, Moves, OrderedMoves),
        ab_search(OrderedMoves, GameState, Player, Depth, Alpha, Beta, none, -1.0Inf, BestMove, BestValue)
    ).

% SUPPRIMÉ: minimax_simple_ref - wrapper de compatibilité jamais utilisé

% ab_search(+Moves, +GameState, +Player, +Depth, +Alpha, +Beta, +BestMoveAcc, +BestValueAcc, -BestMove, -BestValue)  
% Recherche alpha-beta avec elagage - coeur de l'algorithme negamax
ab_search([], _, _, _, _, _, BestMoveAcc, BestValueAcc, BestMoveAcc, BestValueAcc) :- !.

ab_search([[FromRow,FromCol,ToRow,ToCol]|RestMoves], GameState, Player, Depth, Alpha, Beta, BestMoveAcc, BestValueAcc, BestMove, BestValue) :-
    make_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState),
    NewGameState = game_state(_, NextPlayer, _, _, _),
    NewDepth is Depth - 1,
    negamax_ab(NewGameState, NextPlayer, NewDepth, -Beta, -Alpha, _, OpponentValue),
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

% ab_search_with_stats(+Moves, +GameState, +Player, +Depth, +Alpha, +Beta, +BestMoveAcc, +BestValueAcc, -BestMove, -BestValue, +NodesIn, -NodesOut)  
% VERSION TEST - ab_search avec comptage nœuds et traces de coupures alpha-beta
ab_search_with_stats([], _, _, _, _, _, BestMoveAcc, BestValueAcc, BestMoveAcc, BestValueAcc, NodesIn, NodesIn) :- !.

ab_search_with_stats([[FromRow,FromCol,ToRow,ToCol]|RestMoves], GameState, Player, Depth, Alpha, Beta, BestMoveAcc, BestValueAcc, BestMove, BestValue, NodesIn, NodesOut) :-
    make_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState),
    NewGameState = game_state(_, NextPlayer, _, _, _),
    NewDepth is Depth - 1,
    negamax_ab_with_stats(NewGameState, NextPlayer, NewDepth, -Beta, -Alpha, _, OpponentValue, NodesIn, NodesCount1),
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
        BestMove = NewBestMove, BestValue = NewBestValue, NodesOut = NodesCount1  % Coupure beta - pas de récursion
    ;   ab_search_with_stats(RestMoves, GameState, Player, Depth, NewAlpha2, Beta, NewBestMove, NewBestValue, BestMove, BestValue, NodesCount1, NodesOut)
    ).

% terminal_score(+GameState, +Player, -Score)
% Évalue les positions terminales (mat/pat)
terminal_score(GameState, Player, Score) :-
    (   is_in_check(GameState, Player) ->
        Score = -100000  % Échec et mat
    ;   Score = 0        % Pat (nulle)
    ).

% order_moves(+GameState, +Player, +Moves, -OrderedMoves)
% Tri des coups : captures (MVV-LVA basique) d'abord, puis autres - INCOMPLET
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

% make_move_simulation(+Board, +FromRow, +FromCol, +ToRow, +ToCol, -NewBoard)
% Simule un coup sans modifier GameState complet (optimisé pour move_score)
% Utilisé pour détection défense après capture
make_move_simulation(Board, FromRow, FromCol, ToRow, ToCol, NewBoard) :-
    ground(Board), ground(FromRow), ground(FromCol), ground(ToRow), ground(ToCol),
    valid_chess_position(FromRow, FromCol),
    valid_chess_position(ToRow, ToCol),
    get_piece(Board, FromRow, FromCol, Piece),
    \+ is_empty_square(Piece),
    % Simulation: vider case départ et placer pièce à l'arrivée
    place_piece_optimized(Board, FromRow, FromCol, ' ', TempBoard),
    place_piece_optimized(TempBoard, ToRow, ToCol, Piece, NewBoard).

% move_score_with_defense(+Board, +Player, +Move, -Score)
% Score MVV-LVA avec détection défense - NOUVEAU SYSTÈME COMPLET
move_score_with_defense(Board, Player, [FromRow, FromCol, ToRow, ToCol], Score) :-
    get_piece(Board, ToRow, ToCol, TargetPiece),
    (   \+ is_empty_square(TargetPiece) ->
        % CAPTURE: Calculer score base MVV-LVA
        get_piece(Board, FromRow, FromCol, AttackingPiece),
        piece_value(TargetPiece, TargetVal),
        piece_value(AttackingPiece, AttackerVal),
        AbsTargetVal is abs(TargetVal),
        AbsAttackerVal is abs(AttackerVal),
        BaseScore is AbsTargetVal - AbsAttackerVal + 1000,
        
        % DÉTECTION DÉFENSE après simulation coup
        (   make_move_simulation(Board, FromRow, FromCol, ToRow, ToCol, NewBoard),
            opposite_player(Player, Opponent),
            is_square_attacked(NewBoard, ToRow, ToCol, Opponent) ->
            % Attaquée par adversaire: pénalité sévère pour pièces de haute valeur
            (   AbsAttackerVal >= 900 ->  % Dame
                PenaltyScore is BaseScore - (AbsAttackerVal * 1.5)  % Pénalité 1.5x
            ;   AbsAttackerVal >= 500 ->  % Tour
                PenaltyScore is BaseScore - (AbsAttackerVal * 1.2)  % Pénalité 1.2x  
            ;   % Pièces mineures: pénalité standard
                PenaltyScore is BaseScore - AbsAttackerVal
            ),
            Score = PenaltyScore
        ;   % Sûre: score base inchangé
            Score = BaseScore
        )
    ;   % NON-CAPTURE: évaluer qualité du coup
        evaluate_non_capture_move(Board, Player, [FromRow, FromCol, ToRow, ToCol], Score)
    ).

% move_score(+Board, +Player, +Move, -Score)  
% Score MVV-LVA AMÉLIORÉ avec détection défense + promotions + échecs
move_score(Board, Player, Move, FinalScore) :-
    % Score base MVV-LVA avec détection défense
    move_score_with_defense(Board, Player, Move, BaseScore),
    
    % Bonus promotions (Phase 3)
    detect_promotion_bonus(Move, Board, Player, PromotionBonus),
    
    % Bonus échecs (Phase 3) 
    detect_check_bonus(Board, Player, Move, CheckBonus),
    
    % Score final combiné
    FinalScore is BaseScore + PromotionBonus + CheckBonus.

% detect_promotion_bonus(+Move, +Board, +Player, -Bonus)
% Détecte si le coup est une promotion et attribue bonus élevé
detect_promotion_bonus([FromRow, FromCol, ToRow, _ToCol], Board, Player, Bonus) :-
    get_piece(Board, FromRow, FromCol, Piece),
    (   % Pion blanc atteignant 8e rangée
        (Piece = 'P', Player = white, ToRow = 8) ->
        Bonus = 90
    ;   % Pion noir atteignant 1ère rangée  
        (Piece = 'p', Player = black, ToRow = 1) ->
        Bonus = 90
    ;   % Pas une promotion
        Bonus = 0
    ).

% detect_check_bonus(+Board, +Player, +Move, -Bonus)
% Détecte si le coup donne échec et attribue bonus modéré
detect_check_bonus(Board, Player, [FromRow, FromCol, ToRow, ToCol], Bonus) :-
    (   % Simuler le coup et tester si opponent en échec
        make_move_simulation(Board, FromRow, FromCol, ToRow, ToCol, NewBoard),
        opposite_player(Player, Opponent),
        is_in_check(game_state(NewBoard, Opponent, 0, ongoing, []), Opponent) ->
        Bonus = 50  % Score échec forçant
    ;   % Pas un échec
        Bonus = 0
    ).

% keysort_desc(+Pairs, -SortedDesc)
% Tri décroissant par clé
keysort_desc(Pairs, SortedDesc) :-
    sort(0, @>=, Pairs, SortedDesc).

% pairs_values(+Pairs, -Values)
pairs_values([], []).
pairs_values([_-Value|RestPairs], [Value|RestValues]) :-
    pairs_values(RestPairs, RestValues).

% evaluate_non_capture_move(+Board, +Player, +Move, -Score)
% Évalue la qualité des coups non-capture pour priorités développement intelligentes
evaluate_non_capture_move(Board, Player, [FromRow, FromCol, ToRow, ToCol], Score) :-
    get_piece(Board, FromRow, FromCol, Piece),
    get_piece_type(Piece, PieceType),
    
    % Score base: amélioration PSQT
    psqt_improvement_score(PieceType, FromRow, FromCol, ToRow, ToCol, Player, PSQTScore),
    
    % Bonus développement: cavaliers/fous sortant de rang de base
    development_bonus(PieceType, FromRow, FromCol, Player, DevBonus),
    
    % Malus coups roi précoces en ouverture  
    early_king_move_penalty(PieceType, Player, KingPenalty),
    
    % Score final
    Score is PSQTScore + DevBonus + KingPenalty.

% psqt_improvement_score(+PieceType, +FromRow, +FromCol, +ToRow, +ToCol, +Player, -Score)
% Calcule l'amélioration PSQT du coup
psqt_improvement_score(PieceType, FromRow, FromCol, ToRow, ToCol, Player, Score) :-
    % Utiliser Player directement comme couleur
    evaluation:get_psqt_value(PieceType, FromRow, FromCol, Player, FromValue),
    evaluation:get_psqt_value(PieceType, ToRow, ToCol, Player, ToValue),
    RawScore is ToValue - FromValue,
    % Réduire impact (PSQT déjà dans évaluation globale)
    Score is RawScore // 4.

% development_bonus(+PieceType, +FromRow, +FromCol, +Player, -Bonus)
% Bonus pour développement cavaliers/fous
development_bonus(cavalier, FromRow, _FromCol, white, 15) :- FromRow = 1, !.
development_bonus(cavalier, FromRow, _FromCol, black, 15) :- FromRow = 8, !.
development_bonus(fou, FromRow, _FromCol, white, 12) :- FromRow = 1, !. 
development_bonus(fou, FromRow, _FromCol, black, 12) :- FromRow = 8, !.
development_bonus(_, _, _, _, 0).

% early_king_move_penalty(+PieceType, +Player, -Penalty)  
% Malus fort pour coups roi précoces
early_king_move_penalty(roi, _, -25) :- !.  % Décourager fortement coups roi
early_king_move_penalty(_, _, 0).



% GENERATION COUPS SIMPLE
% =============================================================================

% generate_moves_simple(+GameState, +Player, -Moves)
% Génération adaptative selon phase de jeu avec captures prioritaires
generate_moves_simple(GameState, Player, Moves) :-
    % SOLUTION SIMPLE: Structure de priorisation pour TOUTE la partie
    % Tri MVV-LVA garanti, développement intelligent, restrictions adaptatives
    generate_structured_moves(GameState, Player, Moves).

% generate_structured_moves(+GameState, +Player, -Moves)
% Structure de priorisation intelligente pour toute la partie
% Tri MVV-LVA garanti + recaptures Dame prioritaires
generate_structured_moves(GameState, Player, Moves) :-
    GameState = game_state(Board, _, MoveCount, _, _),
    
    % 0. CAPTURES PRIORITAIRES - TOUTES PIECES (y compris Dame)
    findall([FromRow, FromCol, ToRow, ToCol], (
        between(1, 8, FromRow),
        between(1, 8, FromCol),
        get_piece(Board, FromRow, FromCol, Piece),
        \+ is_empty_square(Piece),
        get_piece_color(Piece, Player),
        between(1, 8, ToRow),
        between(1, 8, ToCol),
        valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol),
        % CAPTURE: case destination non vide
        get_piece(Board, ToRow, ToCol, TargetPiece),
        \+ is_empty_square(TargetPiece)
    ), AllCaptureMoves),
    
    % 1. DÉVELOPPEMENT PRIORITAIRE (cavaliers et fous AVANT tout, NON-captures seulement)  
    findall([FromRow, FromCol, ToRow, ToCol], (
        between(1, 8, FromRow),
        between(1, 8, FromCol),
        get_piece(Board, FromRow, FromCol, Piece),
        \+ is_empty_square(Piece),
        get_piece_color(Piece, Player),
        member(Piece, ['N','n','B','b']),
        between(1, 8, ToRow),
        between(1, 8, ToCol),
        valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol),
        % ÉVITER DOUBLONS: seulement non-captures
        get_piece(Board, ToRow, ToCol, TargetPiece),
        is_empty_square(TargetPiece),
        % DÉVELOPPEMENTS NATURELS: restrictions adaptatives selon phase
        (   MoveCount =< 15 ->
            (ToRow >= 3, ToRow =< 6, ToCol >= 3, ToCol =< 6)  % Ouverture: centraux seulement
        ;   true  % Milieu/fin: aucune restriction géographique
        )
    ), DevelopmentMoves),
    
    % Éliminer les doublons de développement
    remove_duplicates_simple(DevelopmentMoves, UniqueDevelopment),
    
    % 2. PIONS CENTRAUX (d4, e4, d5, e5, NON-captures seulement)
    findall([FromRow, FromCol, ToRow, ToCol], (
        between(1, 8, FromRow),
        between(1, 8, FromCol),
        get_piece(Board, FromRow, FromCol, Piece),
        \+ is_empty_square(Piece),
        get_piece_color(Piece, Player),
        member(Piece, ['P','p']),
        between(1, 8, ToRow),
        between(1, 8, ToCol),
        valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol),
        % ÉVITER DOUBLONS: seulement non-captures
        get_piece(Board, ToRow, ToCol, TargetPiece),
        is_empty_square(TargetPiece),
        member(ToCol, [4,5]),  % Colonnes d et e
        member(ToRow, [4,5])   % Rangs 4 et 5
    ), CentralPawnMoves),
    
    % 3. PIONS SUPPORT ÉLARGIS (c6, d6, e6, f6, NON-captures seulement)
    findall([FromRow, FromCol, ToRow, ToCol], (
        between(1, 8, FromRow),
        between(1, 8, FromCol),
        get_piece(Board, FromRow, FromCol, Piece),
        \+ is_empty_square(Piece),
        get_piece_color(Piece, Player),
        member(Piece, ['P','p']),
        between(1, 8, ToRow),
        between(1, 8, ToCol),
        valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol),
        % ÉVITER DOUBLONS: seulement non-captures
        get_piece(Board, ToRow, ToCol, TargetPiece),
        is_empty_square(TargetPiece),
        % ÉLARGI: Colonnes centrales c,d,e,f (pas seulement c,f)
        member(ToCol, [3,4,5,6]),  % c, d, e, f
        member(ToRow, [6,5]),      % 6e rang (noirs) ou 5e rang si nécessaire  
        abs(ToRow - FromRow) =< 2  % 1 ou 2 cases maximum
    ), SupportPawnMoves),
    
    % 4. AUTRES COUPS NON-CAPTURE - SIMPLIFIÉ: toutes pièces non-développement
    findall([FromRow, FromCol, ToRow, ToCol], (
        between(1, 8, FromRow),
        between(1, 8, FromCol),
        get_piece(Board, FromRow, FromCol, Piece),
        \+ is_empty_square(Piece),
        get_piece_color(Piece, Player),
        % TRÈS SIMPLE: Dame après développement minimal (coup 6+)
        (   MoveCount =< 6 ->
            member(Piece, ['R','r','K','k'])      % Ouverture pure: pas de Dame  
        ;   member(Piece, ['R','r','K','k','Q','q'])  % Dame active
        ),
        between(1, 8, ToRow),
        between(1, 8, ToCol),
        valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol),
        % EXCLURE LES CAPTURES (déjà traitées)
        get_piece(Board, ToRow, ToCol, TargetPiece),
        is_empty_square(TargetPiece)
    ), OtherMoves),
    
    % PRIORITÉ CAPTURES: Tri MVV-LVA immédiat sur TOUTES les captures
    order_moves(GameState, Player, AllCaptureMoves, OrderedCaptures),
    
    % Limite adaptative des captures (recommandation 3.5)
    (   MoveCount =< 10 -> CaptureLimit = 8
    ;   MoveCount =< 20 -> CaptureLimit = 12  
    ;   CaptureLimit = 15
    ),
    take_first_n_simple(OrderedCaptures, CaptureLimit, LimitedCaptures),
    
    % Limitation développement et pions
    ai_development_limit(DevLimit), take_first_n_simple(UniqueDevelopment, DevLimit, PriorityDevelopment),
    take_first_n_simple(CentralPawnMoves, 3, LimitedCentral),
    take_first_n_simple(SupportPawnMoves, 4, LimitedSupport),
    
    % ORDRE OPTIMAL: Captures triées → Développement → Pions → Autres non-capture
    append(LimitedCaptures, PriorityDevelopment, Priority1),
    append(Priority1, LimitedCentral, Priority2),
    append(Priority2, LimitedSupport, Priority3),
    append(Priority3, OtherMoves, AllMoves),
    
    % Pas de tri supplémentaire nécessaire - déjà optimisé
    ai_opening_moves(Limit), take_first_n_simple(AllMoves, Limit, Moves).

% SUPPRIMÉ: generate_regular_moves - remplacée par generate_structured_moves 
% pour toute la partie (tri MVV-LVA garanti + restrictions adaptatives)

% SUPPRIMÉ: take_first_N_simple wrappers - consolidés vers take_first_n_simple/3

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
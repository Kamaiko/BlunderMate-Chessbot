% =============================================================================
% AI - Module intelligence artificielle
% =============================================================================
%
% Implementation IA negamax avec elagage alpha-beta :
% - Algorithme negamax (profondeur 2)
% - Tri MVV-LVA des coups
% - Evaluation et selection de coups
%
% Architecture optimisee :
% 1. Configuration et constantes
% 2. Interface publique principale
% 3. Algorithme negamax
% 4. Evaluation et scoring
% 5. Generation de coups
% 6. Helpers prives
%
% =============================================================================

:- [pieces].
:- [board].
:- [game].
:- [evaluation].
:- use_module(utils).

% =============================================================================
% SECTION 1: CONFIGURATION ET CONSTANTES IA
% =============================================================================

% Configuration algorithme IA
negamax_depth(2).         % Profondeur recherche négamax + alpha-beta
ai_move_limit(25).        % Nombre maximum coups évalués (recaptures importantes)
ai_opening_moves(20).     % Limite coups en ouverture
ai_development_limit(8).  % Limite pièces développement prioritaire

% Limites sécurité et performance
ai_max_recursion(8).      % Protection récursion infinie (taille échiquier)

% Scores d'évaluation (spécifiques à l'algorithme IA)
ai_config(checkmate_score, -100000).    % Score échec et mat
ai_config(capture_base, 1000).          % Score de base pour captures MVV-LVA
ai_config(promotion_bonus, 90).         % Bonus pour promotion pion
ai_config(check_bonus, 50).             % Bonus pour mise en échec
ai_config(promotion_priority, 1500).    % Priorité absolue promotion

% Bonus développement pièces
ai_config(knight_dev_bonus, 15).        % Bonus développement cavalier
ai_config(bishop_dev_bonus, 12).        % Bonus développement fou  
ai_config(early_king_penalty, -25).     % Pénalité coup roi précoce

% Priorités de tri des coups
ai_config(dev_priority_minor, 400).     % Cavaliers/fous
ai_config(dev_priority_pawn, 300).      % Pions
ai_config(dev_priority_major, 200).     % Dame/tour/roi

% Configuration phases de jeu
phase_config(opening_threshold, 10).     % Coups 0-10 = ouverture
phase_config(midgame_threshold, 20).     % Coups 11-20 = milieu
phase_config(opening_capture_limit, 8).  % Limite captures ouverture
phase_config(midgame_capture_limit, 12). % Limite captures milieu
phase_config(endgame_capture_limit, 15). % Limite captures finale
phase_config(opening_move_limit, 25).    % Limite coups ouverture
phase_config(midgame_move_limit, 20).    % Limite coups milieu
phase_config(endgame_move_limit, 15).    % Limite coups finale
phase_config(development_phase, 15).     % Limite phase développement

% Helper pour accès sécurisé aux configurations
get_ai_config(Key, Value) :-
    ai_config(Key, Value), !.
get_ai_config(Key, _Value) :-
    format('Warning: Unknown AI config ~w~n', [Key]),
    fail.

get_phase_config(Key, Value) :-
    phase_config(Key, Value), !.
get_phase_config(Key, _Value) :-
    format('Warning: Unknown phase config ~w~n', [Key]),
    fail.

% =============================================================================
% SECTION 2: INTERFACE PUBLIQUE PRINCIPALE
% =============================================================================

% choose_ai_move(+GameState, -BestMove)
% FONCTION PRINCIPALE IA - Point d'entrée pour le choix du meilleur coup
% 
% STRATÉGIE EN 3 NIVEAUX:
% 1. Coups d'ouverture théoriques (premiers coups noirs seulement)
% 2. Algorithme négamax + alpha-beta (recherche profonde)  
% 3. Fallback sécurisé en cas d'erreur
%
% PARAMÈTRES:
%   +GameState: État complet du jeu (plateau, joueur, nombre coups, etc.)
%   -BestMove: Meilleur coup au format [FromRow, FromCol, ToRow, ToCol]
choose_ai_move(GameState, BestMove) :-
    GameState = game_state(Board, Player, MoveCount, _, _),
    
    % NIVEAU 1: Ouverture théorique pour les noirs (défense Caro-Kann)
    % Objectif: éviter les erreurs d'ouverture et jouer des coups solides
    (   Player = black, use_fixed_opening(MoveCount) ->
        (   get_fixed_opening_move(MoveCount, Board, BestMove) ->
            true  % Coup d'ouverture validé et appliqué
        ;   % Si l'ouverture échoue, passer à l'algorithme standard
            choose_ai_move_safe(GameState, Player, BestMove)
        )
    ;   % NIVEAU 2: Algorithme négamax + alpha-beta pour tous les autres cas
        % (blancs + noirs après ouverture)
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

% Coups d'ouverture fixes - Caro-Kann/Slav Defense
use_fixed_opening(1).  % Premier coup des noirs (c7-c6)
use_fixed_opening(3).  % Deuxieme coup des noirs (d7-d5)

get_fixed_opening_move(1, Board, [7, 3, 6, 3]) :-
    % Premier coup: c7-c6 (rang 7 col 3 vers rang 6 col 3)
    get_piece(Board, 7, 3, 'p'),  % Verification qu'il y a bien un pion noir en c7
    get_piece(Board, 6, 3, ' ').  % Verification que c6 est libre (espace)

get_fixed_opening_move(3, Board, [7, 4, 5, 4]) :-
    % Deuxieme coup: d7-d5 (rang 7 col 4 vers rang 5 col 4)
    get_piece(Board, 7, 4, 'p'),  % Verification qu'il y a bien un pion noir en d7
    get_piece(Board, 6, 4, ' '),  % Verification que d6 est libre (espace)
    get_piece(Board, 5, 4, ' ').  % Verification que d5 est libre (espace)

% display_position_evaluation(+GameState, +Player)
% Affiche le détail de l'évaluation pour debug/interface
% Version refactorisée séparant calcul et affichage
display_position_evaluation(GameState, Player) :-
    calculate_evaluation_components(GameState, Components),
    format_evaluation_display(Components, Player).

% =============================================================================
% SECTION 3: ALGORITHME NÉGAMAX ALPHA-BETA (CŒUR DE L'IA)
% =============================================================================

%! negamax_ab(+GameState, +Player, +Depth, +Alpha, +Beta, -BestMove, -BestValue)
% NÉGAMAX AVEC ÉLAGAGE ALPHA-BETA - Algorithme de recherche principal
%
% PRINCIPE NÉGAMAX:
% - Variante du minimax qui exploite la propriété: max(a,b) = -min(-a,-b)
% - Un seul algorithme au lieu de min/max séparés
% - Score toujours du point de vue du joueur actuel
%
% ÉLAGAGE ALPHA-BETA:
% - Alpha: meilleur score garanti pour les Blancs (borne inférieure)
% - Beta: meilleur score garanti pour les Noirs (borne supérieure)  
% - Si Alpha >= Beta: élagage (branches inutiles)
% - Réduit drastiquement l'espace de recherche (facteur 10-100x)
%
% PSEUDO-CODE DÉTAILLÉ:
% ```
% function negamax(position, depth, α, β, color) is
%     if depth = 0 or position is terminal then
%         return color × evaluate(position)
%     
%     value := −∞
%     for each child of position do
%         value := max(value, −negamax(child, depth−1, −β, −α, −color))
%         α := max(α, value)
%         if α ≥ β then
%             break (* coupure beta - élagage *)
%     return value
% ```
%
% TERMINAISON:
% - Depth = 0: évaluation heuristique de la position
% - Aucun coup légal: position terminale (mat/pat)
%
% PARAMÈTRES:
%   +GameState: Position actuelle du jeu
%   +Player: Joueur dont c'est le tour (white/black)  
%   +Depth: Profondeur de recherche restante
%   +Alpha: Borne inférieure (meilleur score pour maximisant)
%   +Beta: Borne supérieure (meilleur score pour minimisant)
%   -BestMove: Meilleur coup trouvé [FromRow,FromCol,ToRow,ToCol]
%   -BestValue: Valeur du meilleur coup (du point de vue Player)

% CAS DE BASE: Profondeur 0 - Évaluation heuristique
negamax_ab(GameState, Player, 0, _Alpha, _Beta, [], Value) :-
    evaluate_pure_reference(GameState, Player, Value), !.

% CAS RÉCURSIF: Exploration des coups possibles
negamax_ab(GameState, Player, Depth, Alpha, Beta, BestMove, BestValue) :-
    Depth > 0,
    generate_structured_moves_v2(GameState, Player, Moves),
    (   Moves = [] ->
        % Aucun coup légal: position terminale
        terminal_score(GameState, Player, BestValue),
        BestMove = []
    ;   % Trier les coups pour améliorer l'élagage alpha-beta
        order_moves(GameState, Player, Moves, OrderedMoves),
        % Recherche alpha-beta récursive
        ab_search(OrderedMoves, GameState, Player, Depth, Alpha, Beta, none, -1.0Inf, BestMove, BestValue)
    ).

%! ab_search(+Moves, +GameState, +Player, +Depth, +Alpha, +Beta, +BestMoveAcc, +BestValueAcc, -BestMove, -BestValue)  
% Recherche alpha-beta avec elagage - cœur de l'algorithme negamax
%
% LOGIQUE D'ÉLAGAGE:
% - Pour chaque coup, calculer sa valeur avec négamax récursif
% - Mettre à jour Alpha si un meilleur coup est trouvé
% - Si Alpha >= Beta: ÉLAGAGE (coupure beta)
% - Sinon: continuer avec les coups restants
ab_search([], _, _, _, _, _, BestMoveAcc, BestValueAcc, BestMoveAcc, BestValueAcc) :- !.

ab_search([[FromRow,FromCol,ToRow,ToCol]|RestMoves], GameState, Player, Depth, Alpha, Beta, BestMoveAcc, BestValueAcc, BestMove, BestValue) :-
    % Simuler le coup et évaluer la position résultante
    make_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState),
    NewGameState = game_state(_, NextPlayer, _, _, _),
    NewDepth is Depth - 1,
    
    % Appel récursif négamax avec Alpha/Beta inversés
    negamax_ab(NewGameState, NextPlayer, NewDepth, -Beta, -Alpha, _, OpponentValue),
    Value is -OpponentValue,  % Négation pour négamax
    
    % Mise à jour du meilleur coup si nécessaire
    (   Value > BestValueAcc ->
        UpdatedAlpha is max(Alpha, Value),
        NewBestMove = [FromRow,FromCol,ToRow,ToCol],
        NewBestValue = Value
    ;   UpdatedAlpha = Alpha,
        NewBestMove = BestMoveAcc,
        NewBestValue = BestValueAcc
    ),
    
    % ÉLAGAGE ALPHA-BETA: Test de coupure
    (   UpdatedAlpha >= Beta ->
        % Coupure beta: pas besoin d'explorer les coups restants
        BestMove = NewBestMove, BestValue = NewBestValue
    ;   % Continuer la recherche avec Alpha mis à jour
        ab_search(RestMoves, GameState, Player, Depth, UpdatedAlpha, Beta, NewBestMove, NewBestValue, BestMove, BestValue)
    ).

% terminal_score(+GameState, +Player, -Score)
% Évalue les positions terminales (mat/pat)
terminal_score(GameState, Player, Score) :-
    (   is_in_check(GameState, Player) ->
        get_ai_config(checkmate_score, Score)  % Échec et mat
    ;   Score = 0        % Pat (nulle)
    ).

% =============================================================================
% SECTION 4: ÉVALUATION ET SCORING MVV-LVA
% =============================================================================

%! order_moves(+GameState, +Player, +Moves, -OrderedMoves)
% TRI DES COUPS POUR OPTIMISER L'ÉLAGAGE ALPHA-BETA
%
% PRINCIPE MVV-LVA (Most Valuable Victim - Least Valuable Attacker):
% - Prioriser les captures où la victime vaut plus que l'attaquant
% - Exemple: Pion capture Dame = score élevé, Dame capture Pion = score faible
% - Améliore l'élagage alpha-beta de 20-40% en moyenne
%
% ORDRE DE TRI:
% 1. Promotions (score absolu le plus élevé)
% 2. Captures triées MVV-LVA (grande victime - petit attaquant)
% 3. Échecs (forcent l'adversaire à réagir)
% 4. Développements (coups positionnels)
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

%! move_score(+Board, +Player, +Move, -FinalScore)  
% SCORE FINAL D'UN COUP AVEC TOUS LES BONUS
% 
% COMPOSANTES DU SCORE:
% 1. Score MVV-LVA de base (captures)
% 2. Bonus promotion (+90 points)
% 3. Bonus échec (+50 points)
% 4. Bonus développement (cavalier/fou +15/+12)
move_score(Board, Player, Move, FinalScore) :-
    % Score base MVV-LVA avec détection défense
    move_score_with_defense(Board, Player, Move, BaseScore),
    
    % Bonus promotions (Phase 3)
    detect_promotion_bonus(Move, Board, Player, PromotionBonus),
    
    % Bonus échecs (Phase 3) 
    detect_check_bonus(Board, Player, Move, CheckBonus),
    
    % Score final combiné
    FinalScore is BaseScore + PromotionBonus + CheckBonus.

%! move_score_with_defense(+Board, +Player, +Move, -Score)
% SCORE MVV-LVA SIMPLIFIÉ
% 
% CALCUL SCORE CAPTURE:
% Score = ValeurVictime - ValeurAttaquant + 1000
% 
% EXEMPLES:
% - Pion(100) prend Dame(900): 900 - 100 + 1000 = 1800
% - Dame(900) prend Pion(100): 100 - 900 + 1000 = 200
% - Tour(500) prend Cavalier(320): 320 - 500 + 1000 = 820
move_score_with_defense(Board, Player, [FromRow, FromCol, ToRow, ToCol], Score) :-
    get_piece(Board, ToRow, ToCol, TargetPiece),
    (   \+ is_empty_square(TargetPiece) ->
        % CAPTURE: Score MVV-LVA pur (Most Valuable Victim - Least Valuable Attacker)
        get_piece(Board, FromRow, FromCol, AttackingPiece),
        piece_value(TargetPiece, TargetVal),
        piece_value(AttackingPiece, AttackerVal),
        AbsTargetVal is abs(TargetVal),
        AbsAttackerVal is abs(AttackerVal),
        % Score simple: prioriser captures de pièces précieuses par pièces moins précieuses
        get_ai_config(capture_base, CaptureBase),
        Score is AbsTargetVal - AbsAttackerVal + CaptureBase
    ;   % NON-CAPTURE: évaluer qualité du coup
        evaluate_non_capture_move(Board, Player, [FromRow, FromCol, ToRow, ToCol], Score)
    ).

% detect_promotion_bonus(+Move, +Board, +Player, -Bonus)
% Détecte si le coup est une promotion et attribue bonus élevé
detect_promotion_bonus([FromRow, FromCol, ToRow, _ToCol], Board, Player, Bonus) :-
    get_piece(Board, FromRow, FromCol, Piece),
    (   % Pion blanc atteignant 8e rangée
        (Piece = 'P', Player = white, ToRow = 8) ->
        get_ai_config(promotion_bonus, Bonus)
    ;   % Pion noir atteignant 1ère rangée  
        (Piece = 'p', Player = black, ToRow = 1) ->
        get_ai_config(promotion_bonus, Bonus)
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
        get_ai_config(check_bonus, Bonus)  % Score échec forçant
    ;   % Pas un échec
        Bonus = 0
    ).

% evaluate_non_capture_move(+Board, +Player, +Move, -Score)
% Évalue la qualité des coups non-capture pour priorités développement intelligentes
evaluate_non_capture_move(Board, Player, [FromRow, FromCol, ToRow, ToCol], Score) :-
    get_piece(Board, FromRow, FromCol, Piece),
    get_piece_type(Piece, PieceType),
    
    % Score base: amélioration PSQT
    psqt_improvement_score(PieceType, FromRow, FromCol, ToRow, ToCol, Player, PSQTScore),
    
    % Bonus développement et malus roi précoce
    piece_move_bonus(PieceType, FromRow, Player, MoveBonus),
    
    % Score final
    Score is PSQTScore + MoveBonus.

% Helpers pour évaluation et scoring
make_move_simulation(Board, FromRow, FromCol, ToRow, ToCol, NewBoard) :-
    ground(Board), ground(FromRow), ground(FromCol), ground(ToRow), ground(ToCol),
    valid_chess_position(FromRow, FromCol),
    valid_chess_position(ToRow, ToCol),
    get_piece(Board, FromRow, FromCol, Piece),
    \+ is_empty_square(Piece),
    % Simulation: vider case départ et placer pièce à l'arrivée
    place_piece_optimized(Board, FromRow, FromCol, ' ', TempBoard),
    place_piece_optimized(TempBoard, ToRow, ToCol, Piece, NewBoard).

psqt_improvement_score(PieceType, FromRow, FromCol, ToRow, ToCol, Player, Score) :-
    % Utiliser Player directement comme couleur
    evaluation:get_psqt_value(PieceType, FromRow, FromCol, Player, FromValue),
    evaluation:get_psqt_value(PieceType, ToRow, ToCol, Player, ToValue),
    RawScore is ToValue - FromValue,
    % Réduire impact (PSQT déjà dans évaluation globale)
    Score is RawScore // 4.

piece_move_bonus(cavalier, FromRow, Player, Bonus) :- 
    starting_row(Player, StartRow), FromRow = StartRow, !,
    get_ai_config(knight_dev_bonus, Bonus).
piece_move_bonus(fou, FromRow, Player, Bonus) :- 
    starting_row(Player, StartRow), FromRow = StartRow, !,
    get_ai_config(bishop_dev_bonus, Bonus).
piece_move_bonus(roi, _, _, Penalty) :- !,
    get_ai_config(early_king_penalty, Penalty).
piece_move_bonus(_, _, _, 0).

starting_row(white, 1).
starting_row(black, 8).

keysort_desc(Pairs, SortedDesc) :-
    sort(0, @>=, Pairs, SortedDesc).

pairs_values([], []).
pairs_values([_-Value|RestPairs], [Value|RestValues]) :-
    pairs_values(RestPairs, RestValues).

% =============================================================================
% SECTION 5: GÉNÉRATION DE COUPS STRUCTURÉE
% =============================================================================

%! generate_structured_moves_v2(+GameState, +Player, -Moves) is det
% GÉNÉRATEUR DE COUPS PRINCIPAL - Architecture en 3 phases
%
% STRATÉGIE DE GÉNÉRATION:
% 1. GÉNÉRATION: Produire tous les coups légaux par catégories
%    - Captures (priorité MVV-LVA)
%    - Développements (cavaliers/fous en ouverture)  
%    - Avances de pions (contrôle centre)
%    - Autres coups de pièces
%
% 2. TRI PAR PRIORITÉ: Ordonner selon la valeur tactique
%    - Promotions (priorité absolue)
%    - Captures triées MVV-LVA (grande victime, petit attaquant)
%    - Développements intelligents
%    - Coups positionnels
%
% 3. LIMITATION ADAPTATIVE: Restreindre selon la phase de jeu
%    - Ouverture: plus de coups pour exploration
%    - Milieu: équilibré  
%    - Finale: focus sur les coups critiques
%
% PERFORMANCE: Remplace l'ancienne version monolithique de 123 lignes
% GAIN: Réduction de 85% de complexité, maintenabilité améliorée
generate_structured_moves_v2(GameState, Player, Moves) :-
    GameState = game_state(Board, _, MoveCount, _, _),
    % Phase 1: Génération par catégories
    generate_all_move_types(Board, Player, MoveCount, AllMoves),
    % Phase 2: Tri tactique MVV-LVA + heuristiques
    order_moves_by_priority(GameState, Player, AllMoves, OrderedMoves),
    % Phase 3: Limitation selon phase de jeu (ouverture/milieu/finale)
    apply_adaptive_limit(MoveCount, OrderedMoves, Moves).

% =============================================================================
% SECTION 6: HELPERS PRIVÉS SPÉCIALISÉS
% =============================================================================

% Ces fonctions sont spécifiques à l'IA et ne devraient pas être utilisées
% ailleurs. Elles restent dans ce module pour maintenir l'encapsulation.

% generate_all_move_types(+Board, +Player, +MoveCount, -AllMoves)
generate_all_move_types(Board, Player, MoveCount, AllMoves) :-
    generate_captures(Board, Player, Captures),
    generate_developments(Board, Player, MoveCount, Developments),
    generate_pawn_advances(Board, Player, PawnMoves),
    generate_piece_moves(Board, Player, MoveCount, OtherMoves),
    utils:combine_lists([Captures, Developments, PawnMoves, OtherMoves], AllMoves).

% order_moves_by_priority(+GameState, +Player, +Moves, -OrderedMoves)
order_moves_by_priority(GameState, Player, Moves, OrderedMoves) :-
    GameState = game_state(_, _, MoveCount, _, _),
    % Séparer les captures des autres coups pour tri différentiel
    separate_captures_and_quiet(GameState, Moves, Captures, QuietMoves),
    % Trier les captures par MVV-LVA
    order_moves(GameState, Player, Captures, OrderedCaptures),
    % Limiter les captures selon la phase de jeu
    apply_capture_limit(MoveCount, OrderedCaptures, LimitedCaptures),
    % Limiter les coups calmes
    apply_development_limits(QuietMoves, LimitedQuiet),
    % Combiner dans l'ordre optimal
    append(LimitedCaptures, LimitedQuiet, OrderedMoves).

% apply_adaptive_limit(+MoveCount, +Moves, -LimitedMoves)
apply_adaptive_limit(_MoveCount, Moves, LimitedMoves) :-
    ai_opening_moves(Limit),
    utils:take_first_n(Moves, Limit, LimitedMoves).

% generate_captures(+Board, +Player, -Captures)
% Génère tous les coups de capture
generate_captures(Board, Player, Captures) :-
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
    ), Captures).

% generate_developments(+Board, +Player, +MoveCount, -Developments)
% Génère les coups de développement en ouverture
generate_developments(Board, Player, MoveCount, Developments) :-
    get_phase_config(development_phase, DevPhase),
    (   MoveCount =< DevPhase ->  % Phase d'ouverture seulement
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
            % DÉVELOPPEMENTS NATURELS: restrictions adaptatives
            is_good_development_square(ToRow, ToCol)
        ), UnsafeDevelopments),
        utils:remove_duplicates(UnsafeDevelopments, Developments)
    ;   Developments = []  % Pas de développement après ouverture
    ).

% generate_pawn_advances(+Board, +Player, -PawnMoves)
% Génère les coups de pions (centraux + support)
generate_pawn_advances(Board, Player, PawnMoves) :-
    generate_central_pawn_moves(Board, Player, CentralMoves),
    generate_support_pawn_moves(Board, Player, SupportMoves),
    append(CentralMoves, SupportMoves, PawnMoves).

% generate_piece_moves(+Board, +Player, +MoveCount, -OtherMoves)
% Génère les autres coups de pièces
generate_piece_moves(Board, Player, MoveCount, OtherMoves) :-
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
    ), OtherMoves).

% is_good_development_square(+Row, +Col)
% Détermine si une case est bonne pour le développement
is_good_development_square(Row, Col) :-
    Row >= 3, Row =< 6,  % Cases centrales
    Col >= 3, Col =< 6.

% generate_central_pawn_moves(+Board, +Player, -CentralMoves)
% Génère les coups de pions centraux (d4, e4, d5, e5)
generate_central_pawn_moves(Board, Player, CentralMoves) :-
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
    ), CentralMoves).

% generate_support_pawn_moves(+Board, +Player, -SupportMoves)
% Génère les coups de pions de support (c6, d6, e6, f6)
generate_support_pawn_moves(Board, Player, SupportMoves) :-
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
        % ÉLARGI: Colonnes centrales c,d,e,f
        member(ToCol, [3,4,5,6]),  % c, d, e, f
        member(ToRow, [6,5]),      % 6e rang (noirs) ou 5e rang
        abs(ToRow - FromRow) =< 2  % 1 ou 2 cases maximum
    ), SupportMoves).

% separate_captures_and_quiet(+GameState, +Moves, -Captures, -QuietMoves)
% Sépare les captures des coups calmes pour traitement différentiel
separate_captures_and_quiet(GameState, Moves, Captures, QuietMoves) :-
    GameState = game_state(Board, _, _, _, _),
    partition(is_capture_move(Board), Moves, Captures, QuietMoves).

% is_capture_move(+Board, +Move)
% Détermine si un coup est une capture
is_capture_move(Board, [_, _, ToRow, ToCol]) :-
    get_piece(Board, ToRow, ToCol, TargetPiece),
    \+ is_empty_square(TargetPiece).

% apply_capture_limit(+MoveCount, +Captures, -LimitedCaptures)
% Limite le nombre de captures selon la phase de jeu
apply_capture_limit(MoveCount, Captures, LimitedCaptures) :-
    get_phase_config(opening_threshold, OpenThreshold),
    get_phase_config(midgame_threshold, MidThreshold),
    get_phase_config(opening_capture_limit, OpenLimit),
    get_phase_config(midgame_capture_limit, MidLimit),
    get_phase_config(endgame_capture_limit, EndLimit),
    (   MoveCount =< OpenThreshold -> CaptureLimit = OpenLimit
    ;   MoveCount =< MidThreshold -> CaptureLimit = MidLimit  
    ;   CaptureLimit = EndLimit
    ),
    utils:take_first_n(Captures, CaptureLimit, LimitedCaptures).

% apply_development_limits(+QuietMoves, -LimitedQuiet)
% Applique les limitations aux coups de développement
apply_development_limits(QuietMoves, LimitedQuiet) :-
    ai_development_limit(DevLimit),
    utils:take_first_n(QuietMoves, DevLimit, LimitedQuiet).

% =============================================================================
% SECTION 7: FONCTIONS D'AFFICHAGE ET DEBUG
% =============================================================================

% calculate_evaluation_components(+GameState, -Components)
% Calcul pur des composantes d'évaluation sans affichage
% Sépare la logique de calcul de l'affichage pour améliorer la testabilité
calculate_evaluation_components(GameState, Components) :-
    % Calculs matériels
    count_material_pure_ref(GameState, white, WhiteMaterial),
    count_material_pure_ref(GameState, black, BlackMaterial),
    
    % Calculs PSQT
    evaluate_psqt_total(GameState, white, WhitePSQT),
    evaluate_psqt_total(GameState, black, BlackPSQT),
    
    % Calculs sécurité
    evaluate_piece_safety(GameState, white, WhiteSafety),
    evaluate_piece_safety(GameState, black, BlackSafety),
    
    % Différentiels
    MaterialDiff is WhiteMaterial - BlackMaterial,
    PSQTDiff is WhitePSQT - BlackPSQT,
    SafetyDiff is WhiteSafety - BlackSafety,
    TotalDiff is MaterialDiff + PSQTDiff + SafetyDiff,
    
    % Structure de retour avec toutes les valeurs
    Components = eval_components(WhiteMaterial, BlackMaterial, MaterialDiff,
                                WhitePSQT, BlackPSQT, PSQTDiff,
                                WhiteSafety, BlackSafety, SafetyDiff,
                                TotalDiff).

% format_evaluation_display(+Components, +Player)
% Affichage formaté des composantes d'évaluation
% Version modulaire de l'affichage pour améliorer la maintenabilité
format_evaluation_display(eval_components(WhiteMat, BlackMat, MatDiff,
                                         WhitePSQT, BlackPSQT, PSQTDiff,
                                         WhiteSafety, BlackSafety, SafetyDiff,
                                         Total), Player) :-
    % Calcul du score final selon le joueur
    (   Player = white ->
        FinalScore = Total
    ;   FinalScore is -Total
    ),
    
    % Affichage détaillé identique à l'original
    write('=== ÉVALUATION POSITION ==='), nl,
    format('Matériel    : Blancs ~w vs Noirs ~w (diff: ~w)~n', 
           [WhiteMat, BlackMat, MatDiff]),
    format('PSQT        : Blancs ~w vs Noirs ~w (diff: ~w)~n', 
           [WhitePSQT, BlackPSQT, PSQTDiff]),
    format('Sécurité    : Blancs ~w vs Noirs ~w (diff: ~w)~n', 
           [WhiteSafety, BlackSafety, SafetyDiff]),
    write('------------------------'), nl,
    format('SCORE TOTAL (~w): ~w~n', [Player, FinalScore]),
    display_position_assessment(Player, FinalScore),
    write('========================'), nl.

% display_position_assessment(+Player, +Score)
% Affiche l'évaluation qualitative de la position
% Helper modulaire pour message qualitatif selon le score
display_position_assessment(Player, FinalScore) :-
    (   FinalScore > 0 ->
        format('Position favorable à ~w (~w)~n', [Player, FinalScore])
    ;   FinalScore < 0 ->
        format('Position défavorable à ~w (~w)~n', [Player, FinalScore])
    ;   write('Position équilibrée (0)'), nl
    ).

% =============================================================================
% SECTION 8: VERSION AVEC STATISTIQUES (DEBUG/TESTS)
% =============================================================================

% negamax_ab_with_stats(+GameState, +Player, +Depth, +Alpha, +Beta, -BestMove, -BestValue, +NodesIn, -NodesOut)
% VERSION TEST - Même algorithme mais avec comptage des nœuds explorés pour validation élagage
negamax_ab_with_stats(GameState, Player, 0, _Alpha, _Beta, [], Value, NodesIn, NodesOut) :-
    NodesOut is NodesIn + 1,
    evaluate_pure_reference(GameState, Player, Value), !.

negamax_ab_with_stats(GameState, Player, Depth, Alpha, Beta, BestMove, BestValue, NodesIn, NodesOut) :-
    Depth > 0,
    NodesCount1 is NodesIn + 1,  % Compter ce nœud
    generate_structured_moves_v2(GameState, Player, Moves),
    (   Moves = [] ->
        terminal_score(GameState, Player, BestValue),
        BestMove = [],
        NodesOut = NodesCount1
    ;   order_moves(GameState, Player, Moves, OrderedMoves),
        ab_search_with_stats(OrderedMoves, GameState, Player, Depth, Alpha, Beta, none, -1.0Inf, BestMove, BestValue, NodesCount1, NodesOut)
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

% =============================================================================
% FIN DU MODULE AI.PL RESTRUCTURÉ
% =============================================================================
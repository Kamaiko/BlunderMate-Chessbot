% =============================================================================
% MODULE ÉVALUATION - Centralisé et Optimisé  
% =============================================================================
%
% RESPONSABILITÉ : Toutes les fonctions d'évaluation de positions
% - PSQT (Piece-Square Tables) ChessProgramming.org
% - Évaluation matérielle standard
% - Sécurité des pièces (anti-blunders)
% - Mobilité et contrôle du centre
% - Évaluation tactique complète
%
% USAGE : Importé par ai.pl, interface.pl et tests
% =============================================================================

% Dependencies nécessaires
:- [pieces].
:- [board].

% =============================================================================
% PIECE-SQUARE TABLES - Adaptées système coordonnées Prolog Chess
% =============================================================================
%
% SYSTÈME COORDONNÉES:
% - Row 1 = Rang 1 échecs (base blancs)  
% - Row 8 = Rang 8 échecs (base noirs)
% - Col 1-8 = Colonnes a-h
%
% TABLES ChessProgramming.org ADAPTÉES:
% - Tables inversées verticalement pour correspondre à notre système
% - Valeurs s'AJOUTENT aux valeurs matérielles de base
% - Blancs: utilisent tables directement
% - Noirs: miroir vertical automatique dans get_psqt_value/4
%
% =============================================================================

% =============================================================================
% PION PSQT - Encourage avance et contrôle centre
% =============================================================================
pawn_psqt([
    [  0,  0,  0,  0,  0,  0,  0,  0], % Row 8 (promotion noirs)
    [  5, 10, 10,-20,-20, 10, 10,  5], % Row 7 (base pions noirs)
    [  5, -5,-10,  0,  0,-10, -5,  5], % Row 6
    [  0,  0,  0, 20, 20,  0,  0,  0], % Row 5 (avant-postes)
    [  5,  5, 10, 25, 25, 10,  5,  5], % Row 4 (centre fort)
    [ 10, 10, 20, 30, 30, 20, 10, 10], % Row 3
    [ 50, 50, 50, 50, 50, 50, 50, 50], % Row 2 (base pions blancs)
    [  0,  0,  0,  0,  0,  0,  0,  0]  % Row 1 (promotion blancs)
]).

% =============================================================================
% CAVALIER PSQT - Évite les bords, favorise centre
% =============================================================================
knight_psqt([
    [-50,-40,-30,-30,-30,-30,-40,-50], % Row 8
    [-40,-20,  0,  0,  0,  0,-20,-40], % Row 7
    [-30,  0, 10, 15, 15, 10,  0,-30], % Row 6
    [-30,  5, 15, 20, 20, 15,  5,-30], % Row 5 (cases centrales fortes)
    [-30,  0, 15, 20, 20, 15,  0,-30], % Row 4
    [-30,  5, 10, 15, 15, 10,  5,-30], % Row 3
    [-40,-20,  0,  5,  5,  0,-20,-40], % Row 2
    [-50,-40,-30,-30,-30,-30,-40,-50]  % Row 1
]).

% =============================================================================
% FOU PSQT - Diagonales longues, évite blocage
% =============================================================================
bishop_psqt([
    [-20,-10,-10,-10,-10,-10,-10,-20], % Row 8
    [-10,  0,  0,  0,  0,  0,  0,-10], % Row 7
    [-10,  0,  5, 10, 10,  5,  0,-10], % Row 6
    [-10,  5,  5, 10, 10,  5,  5,-10], % Row 5
    [-10,  0, 10, 10, 10, 10,  0,-10], % Row 4 (diagonales centrales)
    [-10, 10, 10, 10, 10, 10, 10,-10], % Row 3
    [-10,  5,  0,  0,  0,  0,  5,-10], % Row 2
    [-20,-10,-10,-10,-10,-10,-10,-20]  % Row 1
]).

% =============================================================================
% TOUR PSQT - Colonnes ouvertes, 7e rang
% =============================================================================
rook_psqt([
    [  0,  0,  0,  0,  0,  0,  0,  0], % Row 8
    [  5, 10, 10, 10, 10, 10, 10,  5], % Row 7 (7e rang fort)
    [ -5,  0,  0,  0,  0,  0,  0, -5], % Row 6
    [ -5,  0,  0,  0,  0,  0,  0, -5], % Row 5
    [ -5,  0,  0,  0,  0,  0,  0, -5], % Row 4
    [ -5,  0,  0,  0,  0,  0,  0, -5], % Row 3
    [ -5,  0,  0,  0,  0,  0,  0, -5], % Row 2 (évite mouvement prématuré)
    [  0,  0,  0,  5,  5,  0,  0,  0]  % Row 1 (colonnes centrales légèrement mieux)
]).

% =============================================================================
% DAME PSQT - Évite développement précoce
% =============================================================================
queen_psqt([
    [-20,-10,-10, -5, -5,-10,-10,-20], % Row 8
    [-10,  0,  0,  0,  0,  0,  0,-10], % Row 7
    [-10,  0,  5,  5,  5,  5,  0,-10], % Row 6
    [ -5,  0,  5,  5,  5,  5,  0, -5], % Row 5
    [  0,  0,  5,  5,  5,  5,  0, -5], % Row 4
    [-10,  5,  5,  5,  5,  5,  0,-10], % Row 3
    [-10,  0,  5,  0,  0,  0,  0,-10], % Row 2 (décourage développement précoce)
    [-20,-10,-10, -5, -5,-10,-10,-20]  % Row 1
]).

% =============================================================================
% ROI PSQT - Sécurité d'abord, puis activité
% =============================================================================
king_psqt([
    [-30,-40,-40,-50,-50,-40,-40,-30], % Row 8
    [-30,-40,-40,-50,-50,-40,-40,-30], % Row 7
    [-30,-40,-40,-50,-50,-40,-40,-30], % Row 6
    [-30,-40,-40,-50,-50,-40,-40,-30], % Row 5
    [-20,-30,-30,-40,-40,-30,-30,-20], % Row 4
    [-10,-20,-20,-20,-20,-20,-20,-10], % Row 3
    [ 20, 20,  0,  0,  0,  0, 20, 20], % Row 2 (encourage roque)
    [ 20, 30, 10,  0,  0, 10, 30, 20]  % Row 1 (positions roquées sécurisées)
]).

% =============================================================================
% INTERFACE PSQT - ACCÈS AUX TABLES
% =============================================================================

% get_psqt_value(+PieceType, +Row, +Col, +Color, -Value)
% Interface principale pour récupérer valeurs PSQT
get_psqt_value(pawn, Row, Col, white, Value) :-
    pawn_psqt(Table),
    get_table_value(Table, Row, Col, Value).
get_psqt_value(pawn, Row, Col, black, Value) :-
    pawn_psqt(Table),
    MirrorRow is 9 - Row,  % Miroir vertical pour noirs
    get_table_value(Table, MirrorRow, Col, Value).

get_psqt_value(knight, Row, Col, white, Value) :-
    knight_psqt(Table),
    get_table_value(Table, Row, Col, Value).
get_psqt_value(knight, Row, Col, black, Value) :-
    knight_psqt(Table),
    MirrorRow is 9 - Row,
    get_table_value(Table, MirrorRow, Col, Value).

get_psqt_value(bishop, Row, Col, white, Value) :-
    bishop_psqt(Table),
    get_table_value(Table, Row, Col, Value).
get_psqt_value(bishop, Row, Col, black, Value) :-
    bishop_psqt(Table),
    MirrorRow is 9 - Row,
    get_table_value(Table, MirrorRow, Col, Value).

get_psqt_value(rook, Row, Col, white, Value) :-
    rook_psqt(Table),
    get_table_value(Table, Row, Col, Value).
get_psqt_value(rook, Row, Col, black, Value) :-
    rook_psqt(Table),
    MirrorRow is 9 - Row,
    get_table_value(Table, MirrorRow, Col, Value).

get_psqt_value(queen, Row, Col, white, Value) :-
    queen_psqt(Table),
    get_table_value(Table, Row, Col, Value).
get_psqt_value(queen, Row, Col, black, Value) :-
    queen_psqt(Table),
    MirrorRow is 9 - Row,
    get_table_value(Table, MirrorRow, Col, Value).

get_psqt_value(king, Row, Col, white, Value) :-
    king_psqt(Table),
    get_table_value(Table, Row, Col, Value).
get_psqt_value(king, Row, Col, black, Value) :-
    king_psqt(Table),
    MirrorRow is 9 - Row,
    get_table_value(Table, MirrorRow, Col, Value).

% Pièce non reconnue ou hors limites -> 0
get_psqt_value(_, _, _, _, 0).

% =============================================================================
% UTILITAIRES TABLES
% =============================================================================

% get_table_value(+Table, +Row, +Col, -Value)
% Récupère valeur dans une table 8x8 avec validation des limites
get_table_value(Table, Row, Col, Value) :-
    between(1, 8, Row), between(1, 8, Col),
    nth1(Row, Table, RowList),
    nth1(Col, RowList, Value).

get_table_value(_, _, _, 0).  % Valeur par défaut si hors limites

% =============================================================================
% ÉVALUATION POSITION PRINCIPALE - INTERFACE PUBLIQUE
% =============================================================================

% evaluate_position(+GameState, +Player, -Value) 
% NOUVELLE INTERFACE PRINCIPALE (ex evaluate_pure_reference)
% ÉVALUATION COMPLÈTE: Matériel + PSQT + Sécurité (anti-blunders)
evaluate_position(GameState, Player, Value) :-
    % 1. Évaluation matérielle standard (sans roi)
    count_material_standard(GameState, white, WhiteMaterial),
    count_material_standard(GameState, black, BlackMaterial),
    MaterialDiff is WhiteMaterial - BlackMaterial,
    
    % 2. Évaluation PSQT (Piece-Square Tables)
    evaluate_psqt_total(GameState, white, WhitePSQT),
    evaluate_psqt_total(GameState, black, BlackPSQT),
    PSQTDiff is WhitePSQT - BlackPSQT,
    
    % 3. Sécurité des pièces (détection hanging pieces)
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

% =============================================================================
% ÉVALUATION MATÉRIELLE
% =============================================================================

% count_material_standard(+GameState, +Player, -MaterialValue)
% Compte matériel selon valeurs standard (SANS roi)
count_material_standard(GameState, Player, MaterialValue) :-
    GameState = game_state(Board, _, _, _, _),
    findall(Value, (
        between(1, 8, Row), between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        \+ is_empty_square(Piece),
        piece_belongs_to_player(Piece, Player),
        piece_value(Piece, SignedValue),
        Value is abs(SignedValue)
    ), Values),
    sum_list(Values, MaterialValue).

% =============================================================================
% ÉVALUATION PSQT
% =============================================================================

% evaluate_psqt_total(+GameState, +Player, -PSQTValue)
% Évalue toutes les pièces d'un joueur selon les PSQT
evaluate_psqt_total(GameState, Player, PSQTValue) :-
    GameState = game_state(Board, _, _, _, _),
    findall(Value, (
        between(1, 8, Row), between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        \+ is_empty_square(Piece),
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
% REACTIVE : Detection simple des pieces non defendues (hanging pieces)
% Version optimisee pour detecter uniquement les pieces de valeur elevee exposees
evaluate_piece_safety(GameState, Player, SafetyValue) :-
    GameState = game_state(Board, _, _, _, _),
    
    % Detecter pieces precieuses (Dame, Tours, Fous, Cavaliers) non defendues
    findall(PieceValue, (
        between(1, 8, Row),
        between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        piece_belongs_to_player(Piece, Player),
        valuable_piece(Piece, PieceValue), % Seulement pieces >300 points
        opposite_player(Player, Opponent),
        is_square_attacked(Board, Row, Col, Opponent), % Piece attaquee
        \+ is_piece_defended(GameState, Row, Col, Player) % Non defendue
    ), HangingValues),
    
    sum_list(HangingValues, TotalLoss),
    SafetyValue is -TotalLoss. % Negatif = mauvais pour le joueur

% valuable_piece(+Piece, -Value) - Seulement pieces precieuses
valuable_piece('Q', 900). valuable_piece('q', 900).
valuable_piece('R', 500). valuable_piece('r', 500).  
valuable_piece('B', 330). valuable_piece('b', 330).
valuable_piece('N', 320). valuable_piece('n', 320).

% is_piece_defended(+GameState, +Row, +Col, +DefendingPlayer)
% CORRECTION CRITIQUE: Implémente vraie détection défense via is_square_attacked
% Remplace approche conservatrice fail systématique par vérification fonctionnelle
is_piece_defended(GameState, Row, Col, DefendingPlayer) :-
    ground(GameState), ground(Row), ground(Col), ground(DefendingPlayer),
    valid_chess_position(Row, Col),
    GameState = game_state(Board, _, _, _, _),
    is_square_attacked(Board, Row, Col, DefendingPlayer).

% =============================================================================
% ÉVALUATION MOBILITÉ
% =============================================================================

% evaluate_piece_development(+GameState, +Player, -DevelopmentValue)
% Évalue le développement des pièces : compte cavaliers/fous hors rang de base
evaluate_piece_development(GameState, Player, DevelopmentValue) :-
    GameState = game_state(Board, _, _, _, _),
    
    % Compter pièces développées (cavaliers et fous hors rang de base)
    (   Player = white ->
        _BaseRank = 1, DevelopedRanks = [2,3,4,5,6,7,8]
    ;   _BaseRank = 8, DevelopedRanks = [1,2,3,4,5,6,7]
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
    DevelopmentValue is DevelopedCount * 10.  % Bonus modéré par pièce développée

% =============================================================================
% ÉVALUATIONS TACTIQUES AVANCÉES
% =============================================================================

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

% evaluate_king_safety_basic(+GameState, +Player, -KingSafety)
% Évaluation basique de sécurité du roi : pénalise roi exposé
evaluate_king_safety_basic(GameState, Player, KingSafety) :-
    GameState = game_state(Board, _, MoveCount, _, _),
    find_king_position(Board, Player, KingRow, _KingCol),
    
    % Position initiale du roi selon la couleur
    (   Player = white ->
        InitialRow = 1
    ;   InitialRow = 8
    ),
    
    % Pénaliser roi exposé EN OUVERTURE seulement (premiers 15 coups)
    (   MoveCount =< 15, KingRow \= InitialRow ->
        KingSafety = -50   % Malus modéré = valeur pion/2
    ;   KingRow = InitialRow ->
        KingSafety = 10    % Petit bonus roi en sécurité
    ;   KingSafety = 0     % Neutre (milieu/fin de jeu)
    ).

% =============================================================================
% FONCTIONS DE SUPPORT ÉVALUATION
% =============================================================================

% evaluate_move_count(+GameState, +Player, -MoveCountValue)
% Évalue la mobilité par comptage des coups légaux possibles
evaluate_move_count(GameState, Player, MoveCountValue) :-
    generate_moves_simple(GameState, Player, Moves),
    length(Moves, MoveCount),
    MoveCountValue is MoveCount.  % Plus de coups = meilleure mobilité

% =============================================================================
% COMPATIBILITY LAYER - RÉFÉRENCES ANCIENNES
% =============================================================================

% evaluate_pure_reference/3 - ALIAS pour compatibility
evaluate_pure_reference(GameState, Player, Value) :-
    evaluate_position(GameState, Player, Value).

% count_material_pure_ref/3 - ALIAS pour compatibility  
count_material_pure_ref(GameState, Player, Value) :-
    count_material_standard(GameState, Player, Value).
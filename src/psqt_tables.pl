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
% CAVALIER PSQT - Centre privilégié, bords pénalisés
% =============================================================================
knight_psqt([
    [-50,-40,-30,-30,-30,-30,-40,-50], % Row 8 (base noirs)
    [-40,-20,  0,  5,  5,  0,-20,-40], % Row 7  
    [-30,  5, 10, 15, 15, 10,  5,-30], % Row 6
    [-30,  0, 15, 20, 20, 15,  0,-30], % Row 5 (centre excellent)
    [-30,  5, 15, 20, 20, 15,  5,-30], % Row 4 (centre excellent) 
    [-30,  0, 10, 15, 15, 10,  0,-30], % Row 3
    [-40,-20,  0,  0,  0,  0,-20,-40], % Row 2
    [-50,-40,-30,-30,-30,-30,-40,-50]  % Row 1 (base blancs)
]).

% =============================================================================
% FOU PSQT - Diagonales longues favorisées
% =============================================================================
bishop_psqt([
    [-20,-10,-10,-10,-10,-10,-10,-20], % Row 8
    [-10,  5,  0,  0,  0,  0,  5,-10], % Row 7
    [-10, 10, 10, 10, 10, 10, 10,-10], % Row 6
    [-10,  0, 10, 10, 10, 10,  0,-10], % Row 5
    [-10,  5,  5, 10, 10,  5,  5,-10], % Row 4
    [-10,  0,  5, 10, 10,  5,  0,-10], % Row 3
    [-10,  0,  0,  0,  0,  0,  0,-10], % Row 2
    [-20,-10,-10,-10,-10,-10,-10,-20]  % Row 1
]).

% =============================================================================
% TOUR PSQT - 7ème rangée et colonnes ouvertes
% =============================================================================
rook_psqt([
    [  0,  0,  0,  5,  5,  0,  0,  0], % Row 8 (base noirs)
    [ -5,  0,  0,  0,  0,  0,  0, -5], % Row 7 (7ème rangée noirs)
    [ -5,  0,  0,  0,  0,  0,  0, -5], % Row 6
    [ -5,  0,  0,  0,  0,  0,  0, -5], % Row 5
    [ -5,  0,  0,  0,  0,  0,  0, -5], % Row 4
    [ -5,  0,  0,  0,  0,  0,  0, -5], % Row 3
    [  5, 10, 10, 10, 10, 10, 10,  5], % Row 2 (7ème rangée blancs)
    [  0,  0,  0,  0,  0,  0,  0,  0]  % Row 1 (base blancs)
]).

% =============================================================================
% DAME PSQT - ChessProgramming.org EXACTE (inversée pour notre système)
% =============================================================================
queen_psqt([
    [-20,-10,-10, -5, -5,-10,-10,-20], % Row 8 (base noirs)
    [-10,  0,  5,  0,  0,  0,  0,-10], % Row 7 
    [-10,  5,  5,  5,  5,  5,  0,-10], % Row 6 
    [  0,  0,  5,  5,  5,  5,  0, -5], % Row 5 
    [ -5,  0,  5,  5,  5,  5,  0, -5], % Row 4 
    [-10,  0,  5,  5,  5,  5,  0,-10], % Row 3 
    [-10,  0,  0,  0,  0,  0,  0,-10], % Row 2
    [-20,-10,-10, -5, -5,-10,-10,-20]  % Row 1 (base blancs)
]).

% =============================================================================
% ROI PSQT - Sécurité milieu de partie
% =============================================================================
king_psqt([
    [ 20, 30, 10,  0,  0, 10, 30, 20], % Row 8 (base noirs sécurisée)
    [ 20, 20,  0,  0,  0,  0, 20, 20], % Row 7 (roque noirs)
    [-10,-20,-20,-20,-20,-20,-20,-10], % Row 6 (exposé)
    [-20,-30,-30,-40,-40,-30,-30,-20], % Row 5 (très exposé)
    [-30,-40,-40,-50,-50,-40,-40,-30], % Row 4 (centre dangereux)
    [-30,-40,-40,-50,-50,-40,-40,-30], % Row 3 (centre dangereux)
    [-30,-40,-40,-50,-50,-40,-40,-30], % Row 2 (avant roque blancs)
    [ 20, 30, 10,  0,  0, 10, 30, 20]  % Row 1 (base blancs sécurisée)
]).

% =============================================================================
% INTERFACE PRINCIPALE PSQT
% =============================================================================

% get_psqt_value(+PieceType, +Row, +Col, +Color, -Value)
% Récupère valeur PSQT pour position donnée
% Color: white = tables directes, black = miroir vertical
get_psqt_value(pawn, Row, Col, white, Value) :-
    pawn_psqt(Table),
    get_table_value(Table, Row, Col, Value).

get_psqt_value(pawn, Row, Col, black, Value) :-
    pawn_psqt(Table),
    MirroredRow is 9 - Row,  % Miroir vertical pour noirs
    get_table_value(Table, MirroredRow, Col, Value).

get_psqt_value(knight, Row, Col, white, Value) :-
    knight_psqt(Table),
    get_table_value(Table, Row, Col, Value).

get_psqt_value(knight, Row, Col, black, Value) :-
    knight_psqt(Table),
    MirroredRow is 9 - Row,
    get_table_value(Table, MirroredRow, Col, Value).

get_psqt_value(bishop, Row, Col, white, Value) :-
    bishop_psqt(Table),
    get_table_value(Table, Row, Col, Value).

get_psqt_value(bishop, Row, Col, black, Value) :-
    bishop_psqt(Table),
    MirroredRow is 9 - Row,
    get_table_value(Table, MirroredRow, Col, Value).

get_psqt_value(rook, Row, Col, white, Value) :-
    rook_psqt(Table),
    get_table_value(Table, Row, Col, Value).

get_psqt_value(rook, Row, Col, black, Value) :-
    rook_psqt(Table),
    MirroredRow is 9 - Row,
    get_table_value(Table, MirroredRow, Col, Value).

get_psqt_value(queen, Row, Col, white, Value) :-
    queen_psqt(Table),
    get_table_value(Table, Row, Col, Value).

get_psqt_value(queen, Row, Col, black, Value) :-
    queen_psqt(Table),
    MirroredRow is 9 - Row,
    get_table_value(Table, MirroredRow, Col, Value).

get_psqt_value(king, Row, Col, white, Value) :-
    king_psqt(Table),
    get_table_value(Table, Row, Col, Value).

get_psqt_value(king, Row, Col, black, Value) :-
    king_psqt(Table),
    MirroredRow is 9 - Row,
    get_table_value(Table, MirroredRow, Col, Value).

% Pièces non définies = 0
get_psqt_value(_, _, _, _, 0).

% =============================================================================
% UTILITAIRE ACCÈS TABLE
% =============================================================================

% get_table_value(+Table, +Row, +Col, -Value)
% Accès sécurisé aux valeurs PSQT
get_table_value(Table, Row, Col, Value) :-
    between(1, 8, Row),
    between(1, 8, Col),
    nth1(Row, Table, RowList),
    nth1(Col, RowList, Value), !.

get_table_value(_, _, _, 0).  % Valeur par défaut si hors limites
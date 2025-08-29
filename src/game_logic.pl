% =============================================================================
% CHESS GAME LOGIC - LOGIQUE DU JEU D'ECHECS
% =============================================================================
%
% Ce fichier contient la logique métier pure du jeu d'échecs.
% Version nettoyée - Production Ready
%
% Auteur : Patrick Patenaude
% Version : 5.0 (Version nettoyée et fonctionnelle)
% Dépendances : board_smart.pl
%
% FONCTIONNALITÉS IMPLÉMENTÉES :
% ✅ Gestion de l'état du jeu
% ✅ Validation des mouvements pour toutes les pièces
% ✅ Exécution des mouvements
% ✅ Notation algébrique
% ✅ Identification des pièces
% =============================================================================

:- [board_smart].

% =============================================================================
% SECTION 1 : GESTION DE L'ÉTAT DU JEU
% =============================================================================

% Structure de l'état du jeu
% game_state(Board, CurrentPlayer, MoveCount, GameStatus)
% - Board: État de l'échiquier 8x8
% - CurrentPlayer: 'white' ou 'black'  
% - MoveCount: Nombre de coups joués
% - GameStatus: 'active', 'checkmate', 'stalemate', 'draw'

% Initialiser un nouvel état de jeu
init_game_state(GameState) :-
    initialize_board(Board),
    GameState = game_state(Board, white, 0, active).

% Obtenir le joueur actuel
current_player(game_state(_, Player, _, _), Player).

% Changer de joueur
switch_player(white, black).
switch_player(black, white).

% =============================================================================
% SECTION 2 : EXÉCUTION DES MOUVEMENTS
% =============================================================================

% Effectuer un mouvement en notation algébrique (ex: "e2e4")
make_move_algebraic(GameState, MoveString, NewGameState) :-
    parse_algebraic_move(MoveString, FromRow, FromCol, ToRow, ToCol),
    make_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState).

% Effectuer un mouvement avec coordonnées
make_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState) :-
    GameState = game_state(Board, Player, MoveCount, active),
    valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol),
    execute_move(Board, FromRow, FromCol, ToRow, ToCol, NewBoard),
    switch_player(Player, NewPlayer),
    NewMoveCount is MoveCount + 1,
    
    % Pour l'instant, le jeu continue toujours (pas de détection d'échec et mat)
    NewGameState = game_state(NewBoard, NewPlayer, NewMoveCount, active).

% Exécuter un mouvement sur l'échiquier
execute_move(Board, FromRow, FromCol, ToRow, ToCol, NewBoard) :-
    get_piece(Board, FromRow, FromCol, Piece),
    place_single_piece(Board, FromRow, FromCol, ' ', Board1),
    place_single_piece(Board1, ToRow, ToCol, Piece, NewBoard).

% =============================================================================
% SECTION 3 : VALIDATION DES MOUVEMENTS
% =============================================================================

% Validation principale des mouvements
valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol) :-
    % Vérifier les limites de l'échiquier
    FromRow >= 1, FromRow =< 8,
    FromCol >= 1, FromCol =< 8,
    ToRow >= 1, ToRow =< 8,
    ToCol >= 1, ToCol =< 8,
    
    % Vérifier qu'il y a une pièce à la position de départ
    get_piece(Board, FromRow, FromCol, Piece),
    (Player = white -> is_white_piece(Piece) ; is_black_piece(Piece)),
    
    % Vérifier que la destination est différente
    (FromRow \= ToRow ; FromCol \= ToCol),
    
    % Vérifier que la destination est libre ou contient une pièce ennemie
    get_piece(Board, ToRow, ToCol, DestPiece),
    (DestPiece = ' ' ; 
     (Player = white -> is_black_piece(DestPiece) ; is_white_piece(DestPiece))),
    
    % Vérifier les règles spécifiques à la pièce
    can_piece_move_basic(Board, FromRow, FromCol, ToRow, ToCol, Piece).

% =============================================================================
% SECTION 4 : RÈGLES DE MOUVEMENT PAR PIÈCE
% =============================================================================

% Dispatcher principal pour les règles de mouvement
can_piece_move_basic(Board, FromRow, FromCol, ToRow, ToCol, Piece) :-
    (
        % Pion blanc
        Piece = 'P' ->
            can_white_pawn_move(Board, FromRow, FromCol, ToRow, ToCol)
        ;
        % Pion noir  
        Piece = 'p' ->
            can_black_pawn_move(Board, FromRow, FromCol, ToRow, ToCol)
        ;
        % Tour (blanche ou noire)
        (Piece = 'R' ; Piece = 'r') ->
            can_rook_move(Board, FromRow, FromCol, ToRow, ToCol)
        ;
        % Cavalier (blanc ou noir)
        (Piece = 'N' ; Piece = 'n') ->
            can_knight_move(FromRow, FromCol, ToRow, ToCol)
        ;
        % Fou (blanc ou noir)
        (Piece = 'B' ; Piece = 'b') ->
            can_bishop_move(Board, FromRow, FromCol, ToRow, ToCol)
        ;
        % Dame (blanche ou noire)
        (Piece = 'Q' ; Piece = 'q') ->
            can_queen_move(Board, FromRow, FromCol, ToRow, ToCol)
        ;
        % Roi (blanc ou noir)
        (Piece = 'K' ; Piece = 'k') ->
            can_king_move(FromRow, FromCol, ToRow, ToCol)
        ;
        % Pièce inconnue
        false
    ).

% --- RÈGLES DES PIONS ---

% Pion blanc - se déplace vers les rangées supérieures
can_white_pawn_move(Board, FromRow, FromCol, ToRow, ToCol) :-
    (
        % Mouvement simple d'une case
        (ToRow is FromRow + 1, FromCol = ToCol, 
         get_piece(Board, ToRow, ToCol, ' '))
    ;
        % Mouvement initial de deux cases depuis la rangée 2
        (FromRow = 2, ToRow = 4, FromCol = ToCol, 
         get_piece(Board, 3, ToCol, ' '), 
         get_piece(Board, 4, ToCol, ' '))
    ;
        % Capture en diagonale
        (ToRow is FromRow + 1, abs(ToCol - FromCol) =:= 1,
         get_piece(Board, ToRow, ToCol, TargetPiece),
         is_black_piece(TargetPiece))
    ).

% Pion noir - se déplace vers les rangées inférieures
can_black_pawn_move(Board, FromRow, FromCol, ToRow, ToCol) :-
    (
        % Mouvement simple d'une case
        (ToRow is FromRow - 1, FromCol = ToCol, 
         get_piece(Board, ToRow, ToCol, ' '))
    ;
        % Mouvement initial de deux cases depuis la rangée 7
        (FromRow = 7, ToRow = 5, FromCol = ToCol, 
         get_piece(Board, 6, ToCol, ' '), 
         get_piece(Board, 5, ToCol, ' '))
    ;
        % Capture en diagonale
        (ToRow is FromRow - 1, abs(ToCol - FromCol) =:= 1,
         get_piece(Board, ToRow, ToCol, TargetPiece),
         is_white_piece(TargetPiece))
    ).

% --- RÈGLES DES AUTRES PIÈCES ---

% Tour - mouvement horizontal ou vertical
can_rook_move(Board, FromRow, FromCol, ToRow, ToCol) :-
    (FromRow = ToRow ; FromCol = ToCol),
    is_path_clear(Board, FromRow, FromCol, ToRow, ToCol).

% Cavalier - mouvement en forme de L
can_knight_move(FromRow, FromCol, ToRow, ToCol) :-
    RowDiff is abs(FromRow - ToRow),
    ColDiff is abs(FromCol - ToCol),
    ((RowDiff = 2, ColDiff = 1) ; (RowDiff = 1, ColDiff = 2)).

% Fou - mouvement diagonal
can_bishop_move(Board, FromRow, FromCol, ToRow, ToCol) :-
    RowDiff is abs(FromRow - ToRow),
    ColDiff is abs(FromCol - ToCol),
    RowDiff = ColDiff,
    is_path_clear(Board, FromRow, FromCol, ToRow, ToCol).

% Dame - combinaison tour et fou
can_queen_move(Board, FromRow, FromCol, ToRow, ToCol) :-
    (can_rook_move(Board, FromRow, FromCol, ToRow, ToCol) ;
     can_bishop_move(Board, FromRow, FromCol, ToRow, ToCol)).

% Roi - mouvement d'une case dans toutes les directions
can_king_move(FromRow, FromCol, ToRow, ToCol) :-
    RowDiff is abs(FromRow - ToRow),
    ColDiff is abs(FromCol - ToCol),
    RowDiff =< 1, ColDiff =< 1,
    (RowDiff \= 0 ; ColDiff \= 0).

% =============================================================================
% SECTION 5 : UTILITAIRES
% =============================================================================

% Vérification que le chemin est libre entre deux positions
is_path_clear(Board, FromRow, FromCol, ToRow, ToCol) :-
    % Calculer la direction du mouvement
    RowDir is sign(ToRow - FromRow),
    ColDir is sign(ToCol - FromCol),
    
    % Vérifier chaque case intermédiaire
    NextRow is FromRow + RowDir,
    NextCol is FromCol + ColDir,
    check_path_recursive(Board, NextRow, NextCol, ToRow, ToCol, RowDir, ColDir).

% Vérification récursive du chemin
check_path_recursive(_, Row, Col, Row, Col, _, _) :-
    % Arrivé à la destination - chemin libre
    !.

check_path_recursive(Board, Row, Col, ToRow, ToCol, RowDir, ColDir) :-
    % Vérifier si la case courante est libre
    get_piece(Board, Row, Col, Piece),
    Piece = ' ',  % Case vide
    
    % Continuer vers la case suivante
    NextRow is Row + RowDir,
    NextCol is Col + ColDir,
    check_path_recursive(Board, NextRow, NextCol, ToRow, ToCol, RowDir, ColDir).

% Vérifier si une pièce appartient aux blancs
is_white_piece(Piece) :-
    member(Piece, ['P', 'R', 'N', 'B', 'Q', 'K']).

% Vérifier si une pièce appartient aux noirs
is_black_piece(Piece) :-
    member(Piece, ['p', 'r', 'n', 'b', 'q', 'k']).

% Afficher l'état du jeu
display_game_state(GameState) :-
    GameState = game_state(Board, Player, MoveCount, Status),
    display_board(Board),
    write('Current Player: '), write(Player), nl,
    write('Move Count: '), write(MoveCount), nl,
    write('Game Status: '), write(Status), nl, nl.

% =============================================================================
% FIN DU FICHIER - VERSION NETTOYÉE
% Dernière mise à jour : Août 2025
% =============================================================================
% =============================================================================
% CHESS PIECES - LOGIQUE COMPLETE DES PIECES D'ECHECS
% =============================================================================
% 
% Ce module centralise TOUTE la logique liee aux pieces d'echecs :
% - Identification et validation des pieces
% - Regles de mouvement pour chaque type de piece  
% - Utilitaires pour les couleurs et les joueurs
%
% Auteur : Patrick Patenaude
% Version : 5.1 (Consolidation intuitive)
%
% RESPONSABILITES :
% - Definition et identification des pieces
% - Regles de mouvement specifiques (pion, tour, cavalier, etc.)
% - Validation des mouvements selon les regles d'echecs
% - Gestion des couleurs et appartenance aux joueurs
% =============================================================================

% =============================================================================
% SECTION 1 : DEFINITIONS DES PIECES D'ECHECS
% =============================================================================

% --- PIECES BLANCHES (majuscules) ---
piece_definition('P', pion, white).      % Pion blanc
piece_definition('R', tour, white).      % Tour blanche  
piece_definition('N', cavalier, white).  % Cavalier blanc
piece_definition('B', fou, white).       % Fou blanc
piece_definition('Q', dame, white).      % Dame blanche
piece_definition('K', roi, white).       % Roi blanc

% --- PIECES NOIRES (minuscules) ---
piece_definition('p', pion, black).      % Pion noir
piece_definition('r', tour, black).      % Tour noire
piece_definition('n', cavalier, black).  % Cavalier noir
piece_definition('b', fou, black).       % Fou noir
piece_definition('q', dame, black).      % Dame noire
piece_definition('k', roi, black).       % Roi noir

% --- CASE VIDE ---
piece_definition(' ', vide, none).       % Case vide

% =============================================================================
% SECTION 2 : IDENTIFICATION DES PIECES
% =============================================================================

% is_piece(+Piece, ?Type, ?Color)
% Predicate unifie pour identifier une piece, son type et sa couleur.
is_piece(Piece, Type, Color) :-
    piece_definition(Piece, Type, Color).

% is_white_piece(+Piece)
% Verifie si une piece est blanche.
is_white_piece(Piece) :-
    piece_definition(Piece, _, white).

% is_black_piece(+Piece)
% Verifie si une piece est noire.
is_black_piece(Piece) :-
    piece_definition(Piece, _, black).

% is_empty_square(+Piece)
% Verifie si une case est vide.
is_empty_square(Piece) :-
    piece_definition(Piece, vide, none).

% piece_belongs_to_player(+Piece, +Player)
% Verifie si une piece appartient a un joueur specifique.
piece_belongs_to_player(Piece, white) :-
    is_white_piece(Piece).
piece_belongs_to_player(Piece, black) :-
    is_black_piece(Piece).

% get_piece_type(+Piece, -Type)
% Extrait le type d'une piece.
get_piece_type(Piece, Type) :-
    piece_definition(Piece, Type, _).

% get_piece_color(+Piece, -Color)
% Extrait la couleur d'une piece.
get_piece_color(Piece, Color) :-
    piece_definition(Piece, _, Color).

% =============================================================================
% SECTION 3 : REGLES DE MOUVEMENT PRINCIPALES
% =============================================================================

% can_piece_move(+Board, +FromRow, +FromCol, +ToRow, +ToCol, +Piece)
% Dispatcher principal pour valider le mouvement d'une piece selon ses regles.
can_piece_move(Board, FromRow, FromCol, ToRow, ToCol, Piece) :-
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
        % Piece inconnue
        false
    ).

% =============================================================================
% SECTION 4 : REGLES SPECIFIQUES DES PIONS
% =============================================================================

% Pion blanc - se deplace vers les rangees superieures
can_white_pawn_move(Board, FromRow, FromCol, ToRow, ToCol) :-
    (
        % Mouvement simple d'une case
        (ToRow is FromRow + 1, FromCol = ToCol, 
         get_piece(Board, ToRow, ToCol, ' '))
    ;
        % Mouvement initial de deux cases depuis la rangee 2
        (FromRow = 2, ToRow = 4, FromCol = ToCol, 
         get_piece(Board, 3, ToCol, ' '), 
         get_piece(Board, 4, ToCol, ' '))
    ;
        % Capture en diagonale
        (ToRow is FromRow + 1, abs(ToCol - FromCol) =:= 1,
         get_piece(Board, ToRow, ToCol, TargetPiece),
         is_black_piece(TargetPiece))
    ).

% Pion noir - se deplace vers les rangees inferieures
can_black_pawn_move(Board, FromRow, FromCol, ToRow, ToCol) :-
    (
        % Mouvement simple d'une case
        (ToRow is FromRow - 1, FromCol = ToCol, 
         get_piece(Board, ToRow, ToCol, ' '))
    ;
        % Mouvement initial de deux cases depuis la rangee 7
        (FromRow = 7, ToRow = 5, FromCol = ToCol, 
         get_piece(Board, 6, ToCol, ' '), 
         get_piece(Board, 5, ToCol, ' '))
    ;
        % Capture en diagonale
        (ToRow is FromRow - 1, abs(ToCol - FromCol) =:= 1,
         get_piece(Board, ToRow, ToCol, TargetPiece),
         is_white_piece(TargetPiece))
    ).

% =============================================================================
% SECTION 5 : REGLES SPECIFIQUES DES AUTRES PIECES
% =============================================================================

% Tour - mouvement horizontal ou vertical avec chemin libre
can_rook_move(Board, FromRow, FromCol, ToRow, ToCol) :-
    (FromRow = ToRow ; FromCol = ToCol),
    is_path_clear(Board, FromRow, FromCol, ToRow, ToCol).

% Cavalier - mouvement en forme de L (pas de verification de chemin)
can_knight_move(FromRow, FromCol, ToRow, ToCol) :-
    RowDiff is abs(FromRow - ToRow),
    ColDiff is abs(FromCol - ToCol),
    ((RowDiff = 2, ColDiff = 1) ; (RowDiff = 1, ColDiff = 2)).

% Fou - mouvement diagonal avec chemin libre
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
% SECTION 6 : VERIFICATION DES CHEMINS
% =============================================================================

% is_path_clear(+Board, +FromRow, +FromCol, +ToRow, +ToCol)
% Verifie que le chemin entre deux positions est libre (pieces glissantes).
is_path_clear(Board, FromRow, FromCol, ToRow, ToCol) :-
    % Validation des parametres
    nonvar(Board), nonvar(FromRow), nonvar(FromCol), nonvar(ToRow), nonvar(ToCol),
    FromRow >= 1, FromRow =< 8, FromCol >= 1, FromCol =< 8,
    ToRow >= 1, ToRow =< 8, ToCol >= 1, ToCol =< 8,
    
    % Calculer la direction du mouvement
    RowDir is sign(ToRow - FromRow),
    ColDir is sign(ToCol - FromCol),
    
    % Verifier chaque case intermediaire avec limite de profondeur
    NextRow is FromRow + RowDir,
    NextCol is FromCol + ColDir,
    check_path_recursive(Board, NextRow, NextCol, ToRow, ToCol, RowDir, ColDir, 0).

% check_path_recursive(+Board, +Row, +Col, +ToRow, +ToCol, +RowDir, +ColDir, +Depth)
% Verification recursive du chemin avec protection contre boucles infinies.
check_path_recursive(_, Row, Col, Row, Col, _, _, _) :-
    % Arrive a la destination - chemin libre
    !.

check_path_recursive(Board, Row, Col, ToRow, ToCol, RowDir, ColDir, Depth) :-
    % Protection contre boucle infinie
    Depth < 8,
    
    % Verifier si la case courante est libre
    get_piece(Board, Row, Col, Piece),
    Piece = ' ',  % Case vide
    
    % Continuer vers la case suivante
    NextRow is Row + RowDir,
    NextCol is Col + ColDir,
    NextDepth is Depth + 1,
    check_path_recursive(Board, NextRow, NextCol, ToRow, ToCol, RowDir, ColDir, NextDepth).

% =============================================================================
% SECTION 7 : UTILITAIRES POUR LES COULEURS ET JOUEURS
% =============================================================================

% switch_player(+CurrentPlayer, -NewPlayer)
% Change le joueur actuel (blanc ↔ noir).
switch_player(white, black).
switch_player(black, white).

% opposite_player(+Player, -OppositePlayer)
% Alias pour switch_player.
opposite_player(Player, OppositePlayer) :-
    switch_player(Player, OppositePlayer).

% translate_player(+Player, -PlayerFR)
% Traduit le nom du joueur en francais.
translate_player(white, 'blanc').
translate_player(black, 'noir').

% =============================================================================
% SECTION 8 : LISTES ET ENSEMBLES DE PIECES
% =============================================================================

% all_white_pieces(-Pieces)
% Retourne la liste de toutes les pieces blanches.
all_white_pieces(['P', 'R', 'N', 'B', 'Q', 'K']).

% all_black_pieces(-Pieces)
% Retourne la liste de toutes les pieces noires.
all_black_pieces(['p', 'r', 'n', 'b', 'q', 'k']).

% all_pieces(-Pieces)
% Retourne la liste de toutes les pieces valides.
all_pieces(AllPieces) :-
    all_white_pieces(White),
    all_black_pieces(Black),
    append(White, Black, AllPieces).

% is_valid_piece(+Piece)
% Verifie si un caractere represente une piece d'echecs valide.
is_valid_piece(Piece) :-
    piece_definition(Piece, _, _),
    Piece \= ' '.  % Exclure les cases vides

% =============================================================================
% SECTION 9 : VALEURS DES PIECES (POUR FUTURE IA)
% =============================================================================

% piece_value(+Piece, -Value)
% Retourne la valeur traditionnelle d'une piece aux echecs.
piece_value(Piece, 1) :- get_piece_type(Piece, pion).
piece_value(Piece, 3) :- get_piece_type(Piece, cavalier).
piece_value(Piece, 3) :- get_piece_type(Piece, fou).
piece_value(Piece, 5) :- get_piece_type(Piece, tour).
piece_value(Piece, 9) :- get_piece_type(Piece, dame).
piece_value(Piece, 1000) :- get_piece_type(Piece, roi).  % Valeur symbolique
piece_value(' ', 0).  % Case vide

% opposite_color(+Color, -OppositeColor)
% Retourne la couleur opposee.
opposite_color(white, black).
opposite_color(black, white).

% =============================================================================
% SECTION 10 : VALIDATION ET TESTS
% =============================================================================

% validate_piece_consistency
% Teste la coherence de toutes les definitions de pieces.
validate_piece_consistency :-
    % Tester que toutes les pieces blanches sont bien identifiees
    all_white_pieces(Whites),
    forall(member(P, Whites), is_white_piece(P)),
    % Tester que toutes les pieces noires sont bien identifiees  
    all_black_pieces(Blacks),
    forall(member(P, Blacks), is_black_piece(P)),
    % Tester qu'aucune piece n'est a la fois blanche et noire
    \+ (is_white_piece(P), is_black_piece(P)).

% =============================================================================
% FIN DU FICHIER PIECES.PL
% Derniere mise a jour : Aout 2025
% =============================================================================
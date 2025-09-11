% =============================================================================
% GAME - Module de logique de jeu
% =============================================================================
%
% Gestion centrale du jeu d'echecs :
% - Validation et execution des coups
% - Detection echec/mat/pat
% - Gestion d'etat et erreurs
%
% =============================================================================

:- [pieces].
:- [board].
:- [evaluation].

% =============================================================================
% SECTION 1 : STRUCTURE DE L'ETAT DU JEU
% =============================================================================

% Structure de l'etat du jeu : game_state(Board, CurrentPlayer, MoveCount, GameStatus, CapturedPieces)
% - Board: Etat de l'echiquier 8x8
% - CurrentPlayer: 'white' ou 'black'  
% - MoveCount: Nombre de coups joues
% - GameStatus: 'active', 'checkmate', 'stalemate', 'draw'
% - CapturedPieces: Liste des pieces capturees [WhiteCaptured, BlackCaptured]

% init_game_state(-GameState)
% Initialise un nouvel etat de jeu avec l'echiquier standard.
init_game_state(GameState) :-
    initialize_board(Board),
    GameState = game_state(Board, white, 0, active, [[], []]).

% current_player(+GameState, -Player)
% Extrait le joueur actuel depuis l'etat du jeu.
current_player(game_state(_, Player, _, _, _), Player).

% =============================================================================
% SECTION 2 : VALIDATION COMPLETE DES MOUVEMENTS
% =============================================================================

% valid_move(+Board, +Player, +FromRow, +FromCol, +ToRow, +ToCol)
% Valide un mouvement selon toutes les regles d'echecs standard.
valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol) :-
    validate_move_coordinates(FromRow, FromCol, ToRow, ToCol),
    validate_piece_ownership(Board, FromRow, FromCol, Player, Piece),
    validate_destination_square(Board, ToRow, ToCol, Player),
    validate_piece_specific_rules(Board, FromRow, FromCol, ToRow, ToCol, Piece),
    % CRITIQUE: Vérifier que le mouvement ne laisse pas le roi en échec
    validate_king_safety_after_move(Board, Player, FromRow, FromCol, ToRow, ToCol).

% Validation helpers pour reduire la complexite
validate_move_coordinates(FromRow, FromCol, ToRow, ToCol) :-
    valid_chess_move(FromRow, FromCol, ToRow, ToCol).

validate_piece_ownership(Board, FromRow, FromCol, Player, Piece) :-
    get_piece(Board, FromRow, FromCol, Piece),
    piece_belongs_to_player(Piece, Player).

validate_destination_square(Board, ToRow, ToCol, Player) :-
    get_piece(Board, ToRow, ToCol, DestPiece),
    (is_empty_square(DestPiece) ; is_enemy_piece(DestPiece, Player)).

is_enemy_piece(Piece, Player) :-
    opposite_player(Player, OppositePlayer),
    piece_belongs_to_player(Piece, OppositePlayer).

validate_piece_specific_rules(Board, FromRow, FromCol, ToRow, ToCol, Piece) :-
    can_piece_move(Board, FromRow, FromCol, ToRow, ToCol, Piece).

% =============================================================================
% SECTION 3 : EXECUTION DES MOUVEMENTS
% =============================================================================

% make_move_algebraic(+GameState, +MoveString, -NewGameState)
% Effectue un mouvement en notation algebrique (ex: "e2e4").
make_move_algebraic(GameState, MoveString, NewGameState) :-
    parse_algebraic_move(MoveString, FromRow, FromCol, ToRow, ToCol),
    make_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState).

% make_move(+GameState, +FromRow, +FromCol, +ToRow, +ToCol, -NewGameState)
% Effectue un mouvement avec coordonnees numeriques.
make_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState) :-
    GameState = game_state(Board, Player, MoveCount, active, CapturedPieces),
    valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol),
    execute_move(Board, from(FromRow, FromCol), to(ToRow, ToCol), NewBoard, CapturedPieces, NewCapturedPieces),
    advance_game_state(Player, MoveCount, NewBoard, NewCapturedPieces, NewGameState).

% advance_game_state(+CurrentPlayer, +MoveCount, +NewBoard, +NewCapturedPieces, -NewGameState)
% Met a jour l'etat du jeu apres un mouvement avec detection echec/mat.
advance_game_state(Player, MoveCount, NewBoard, NewCapturedPieces, NewGameState) :-
    switch_player(Player, NewPlayer),
    NewMoveCount is MoveCount + 1,
    TempGameState = game_state(NewBoard, NewPlayer, NewMoveCount, active, NewCapturedPieces),
    determine_game_status(TempGameState, NewGameState).

% determine_game_status(+GameState, -FinalGameState)
% Determine le statut final du jeu (actif, mat, pat).
determine_game_status(GameState, FinalGameState) :-
    GameState = game_state(Board, Player, MoveCount, _, CapturedPieces),
    (   is_checkmate(GameState, Player) ->
        FinalGameStatus = checkmate
    ;   is_stalemate(GameState, Player) ->
        FinalGameStatus = stalemate
    ;   FinalGameStatus = active
    ),
    FinalGameState = game_state(Board, Player, MoveCount, FinalGameStatus, CapturedPieces).

% execute_move(+Board, +FromPos, +ToPos, -NewBoard, +CapturedPieces, -NewCapturedPieces)
% Execute un mouvement avec positions groupees pour plus de clarte.
execute_move(Board, from(FromRow, FromCol), to(ToRow, ToCol), NewBoard, CapturedPieces, NewCapturedPieces) :-
    get_piece(Board, FromRow, FromCol, MovingPiece),
    get_piece(Board, ToRow, ToCol, TargetPiece),
    
    % Mettre a jour les pieces capturees si capture
    handle_capture(TargetPiece, CapturedPieces, NewCapturedPieces),
    
    % Executer le mouvement atomique
    perform_move(Board, MovingPiece, from(FromRow, FromCol), to(ToRow, ToCol), NewBoard).

% handle_capture(+TargetPiece, +CapturedPieces, -NewCapturedPieces)
% Gere la capture d'une piece si elle existe.
handle_capture(' ', CapturedPieces, CapturedPieces) :- !.
handle_capture(TargetPiece, CapturedPieces, NewCapturedPieces) :-
    update_captured_pieces(TargetPiece, CapturedPieces, NewCapturedPieces).

% perform_move(+Board, +Piece, +FromPos, +ToPos, -NewBoard)
% Execute physiquement le mouvement sur l'echiquier.
perform_move(Board, Piece, from(FromRow, FromCol), to(ToRow, ToCol), NewBoard) :-
    place_single_piece(Board, FromRow, FromCol, ' ', TempBoard),
    determine_final_piece(Piece, FromRow, ToRow, FinalPiece),
    place_single_piece(TempBoard, ToRow, ToCol, FinalPiece, NewBoard).

% determine_final_piece(+OriginalPiece, +FromRow, +ToRow, -FinalPiece)
% Determine la piece finale apres mouvement (gere la promotion des pions).
% SECURITE: Validation complete des parametres pour eviter corruption.
determine_final_piece(Piece, FromRow, ToRow, FinalPiece) :-
    % Validation des parametres - SECURITE CRITIQUE
    ground(Piece), ground(FromRow), ground(ToRow),
    integer(FromRow), integer(ToRow),
    valid_chess_position(FromRow, 1),  % FromRow dans les limites de l'echiquier
    valid_chess_position(ToRow, 1),    % ToRow dans les limites de l'echiquier
    % Logique de promotion validee
    (is_pawn_promotion(Piece, FromRow, ToRow) ->
        get_piece_color(Piece, Color),
        get_promotion_piece(Color, FinalPiece)
    ;   FinalPiece = Piece
    ).

% is_pawn_promotion(+Piece, +FromRow, +ToRow)
% Verifie si le mouvement d'un pion constitue une promotion.
is_pawn_promotion(Piece, FromRow, ToRow) :-
    get_piece_type(Piece, pion),
    get_piece_color(Piece, Color),
    is_promotion_move(Color, FromRow, ToRow).

% update_captured_pieces(+Piece, +CapturedPieces, -NewCapturedPieces)
% Met a jour la liste des pieces capturees.
update_captured_pieces(Piece, [WhiteCaptured, BlackCaptured], [NewWhiteCaptured, NewBlackCaptured]) :-
    (is_white_piece(Piece) ->
        append(WhiteCaptured, [Piece], NewWhiteCaptured),
        NewBlackCaptured = BlackCaptured
    ;   append(BlackCaptured, [Piece], NewBlackCaptured),
        NewWhiteCaptured = WhiteCaptured
    ).

% =============================================================================
% SECTION 4 : AFFICHAGE DE L'ETAT DU JEU
% =============================================================================

% display_game_state(+GameState)
% Affiche l'etat complet du jeu avec évaluation.
display_game_state(GameState) :-
    GameState = game_state(Board, Player, MoveCount, _, CapturedPieces),
    display_board(Board),
    display_captured_pieces(CapturedPieces),
    write('Joueur actuel: '), 
    translate_player(Player, PlayerFR), 
    write(PlayerFR), nl,
    write('Nombre de coups: '), write(MoveCount), nl,
    
    % NOUVEAU: Affichage évaluation position
    display_position_score(GameState, Player), nl.

% display_position_score(+GameState, +_Player)
% Affiche un score simple de la position TOUJOURS du point de vue des blancs
display_position_score(GameState, _Player) :-
    % TOUJOURS évaluer du point de vue des blancs pour cohérence
    (   catch(evaluate_pure_reference(GameState, white, Score), _, fail) ->
        format('[EVAL] Position: ~w (+blanc/-noir)~n', [Score])
    ;   % Fallback: évaluation matérielle simple
        GameState = game_state(Board, _, _, _, _),
        count_material_simple(Board, white, WhiteMaterial),
        count_material_simple(Board, black, BlackMaterial),
        Score is WhiteMaterial - BlackMaterial,
        format('[EVAL] Position: ~w (+blanc/-noir)~n', [Score])
    ).

% count_material_simple(+Board, +Player, -MaterialValue)
% Compte le matériel d'un joueur (fallback simple)
count_material_simple(Board, Player, MaterialValue) :-
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
% VALIDATION SÉCURITÉ ROI (FIX BUG ÉCHEC IGNORÉ)
% =============================================================================

% validate_king_safety_after_move(+Board, +Player, +FromRow, +FromCol, +ToRow, +ToCol)
% Vérifie qu'après le mouvement, le roi du joueur n'est pas en échec
validate_king_safety_after_move(Board, Player, FromRow, FromCol, ToRow, ToCol) :-
    % Simuler le mouvement
    get_piece(Board, FromRow, FromCol, MovingPiece),
    place_single_piece(Board, FromRow, FromCol, ' ', TempBoard1),
    place_single_piece(TempBoard1, ToRow, ToCol, MovingPiece, NewBoard),
    
    % Vérifier que le roi n'est pas en échec après le mouvement
    \+ is_king_in_check(NewBoard, Player).

% is_king_in_check(+Board, +Player)
% Vérifie si le roi du joueur est en échec
is_king_in_check(Board, Player) :-
    find_king_position(Board, Player, KingRow, KingCol),
    opposite_player(Player, Opponent),
    can_player_attack_square(Board, Opponent, KingRow, KingCol).

% NOTE: find_king_position/4 est definie dans board.pl (version robuste avec validation)
% Redefinition supprimee pour eviter conflit

% can_player_attack_square(+Board, +Player, +TargetRow, +TargetCol)
% Vérifie si le joueur peut attaquer une case donnée
can_player_attack_square(Board, Player, TargetRow, TargetCol) :-
    between(1, 8, FromRow),
    between(1, 8, FromCol),
    get_piece(Board, FromRow, FromCol, Piece),
    \+ is_empty_square(Piece),
    piece_belongs_to_player(Piece, Player),
    % Test si cette pièce peut attaquer la case cible (sans vérifier sécurité roi)
    validate_move_coordinates(FromRow, FromCol, TargetRow, TargetCol),
    validate_destination_square_attack(Board, TargetRow, TargetCol, Player),
    validate_piece_specific_rules(Board, FromRow, FromCol, TargetRow, TargetCol, Piece).

% validate_destination_square_attack(+Board, +ToRow, +ToCol, +Player)
% Version spéciale pour attaque (peut capturer pièces adverses)
validate_destination_square_attack(Board, ToRow, ToCol, Player) :-
    get_piece(Board, ToRow, ToCol, TargetPiece),
    (   is_empty_square(TargetPiece)  % Case vide OK
    ;   \+ piece_belongs_to_player(TargetPiece, Player)  % Pièce adverse OK
    ).

% display_captured_pieces(+CapturedPieces)
% Affiche les pieces capturees de maniere lisible.
display_captured_pieces([WhiteCaptured, BlackCaptured]) :-
    (WhiteCaptured \= [] ; BlackCaptured \= []) ->
        (write('Pieces capturees: '),
         (WhiteCaptured \= [] -> 
            (write('Blanches: '), write(WhiteCaptured), write(' '))
         ; true),
         (BlackCaptured \= [] -> 
            (write('Noires: '), write(BlackCaptured))
         ; true),
         nl)
    ; true.

% =============================================================================
% SECTION 5 : GESTION D'ERREUR ET VALIDATION
% =============================================================================

% Messages d'erreur/info uniformises - reduit la duplication
chess_message(Type, Context, Message) :-
    chess_message_prefix(Type, Prefix),
    format('~w ~w : ~w~n', [Prefix, Context, Message]).

chess_message_prefix(error, 'ERREUR').
chess_message_prefix(warning, 'ATTENTION').
chess_message_prefix(info, 'INFO').

% Compatibilite avec l'API existante  
chess_error(_Type, Context, Message) :-
    chess_message(error, Context, Message).
chess_warning(Context, Message) :-
    chess_message(warning, Context, Message).
chess_info(Context, Message) :-
    chess_message(info, Context, Message).

% validate_move_input(+Input, -FromRow, -FromCol, -ToRow, -ToCol)
% Valide et parse une entree de mouvement - version simplifiee
validate_move_input(Input, FromRow, FromCol, ToRow, ToCol) :-
    nonvar(Input),
    string_length(Input, 4),
    parse_algebraic_move(Input, FromRow, FromCol, ToRow, ToCol), !.
validate_move_input(Input, _, _, _, _) :-
    report_input_validation_error(Input),
    fail.

% report_input_validation_error - Gestion d'erreur centralisee
report_input_validation_error(Input) :-
    (var(Input) ->
        chess_error(entree, saisie_mouvement, 'Aucune entree fournie')
    ; (string_length(Input, Len), Len \= 4) ->
        chess_error(syntaxe, saisie_mouvement, 'Le mouvement doit contenir exactement 4 caracteres')
    ;
        chess_error(validation, mouvement, 'Coordonnees invalides dans le mouvement')
    ).

% =============================================================================
% SECTION 6 : UTILITAIRES DE JEU
% =============================================================================

% safe_read_input(-Input)
% Lecture securisee de l'entree utilisateur - version simplifiee
safe_read_input(Input) :-
    repeat,
    prompt_user,
    (read_input_safely(Input) -> ! ; fail).

prompt_user :-
    write('> '),
    flush_output.

read_input_safely(Input) :-
    catch(
        (read_line_to_string(user_input, String),
         normalize_input_string(String, Input)),
        Error,
        handle_input_error(Error)
    ).

handle_input_error(_) :-
    chess_error(entree, lecture, 'Erreur de lecture - veuillez reessayer'),
    fail.

% normalize_input_string(+RawString, -CleanInput)
% Nettoie et normalise une chaine d'entree.
normalize_input_string(String, Input) :-
    (String = end_of_file ->
        Input = quit
    ;   normalize_space(string(CleanString), String),
        remove_trailing_dot(CleanString, FinalString),
        atom_string(Input, FinalString)).

% remove_trailing_dot(+String, -StringWithoutDot)
% Supprime le point final d'une chaine s'il existe.
remove_trailing_dot(String, Output) :-
    (string_concat(Output, ".", String) ->
        true
    ;   Output = String).

% =============================================================================
% SECTION 7 : VALIDATION D'ETAT DE JEU
% =============================================================================

% valid_game_state(+GameState)
% Valide qu'un etat de jeu est coherent.
valid_game_state(game_state(Board, Player, MoveCount, Status, CapturedPieces)) :-
    % Valider le plateau
    is_list(Board),
    board_size(Size), length(Board, Size),
    
    % Valider le joueur
    member(Player, [white, black]),
    
    % Valider le compteur de mouvements
    integer(MoveCount),
    MoveCount >= 0,
    
    % Valider le statut
    member(Status, [active, checkmate, stalemate, draw]),
    
    % Valider les pieces capturees
    is_list(CapturedPieces),
    length(CapturedPieces, 2).

% =============================================================================
% SECTION 8 : CONSTANTES DE JEU
% =============================================================================

% Constantes moved to utils.pl to avoid conflicts
% Les constantes sont maintenant centralisées dans utils.pl

% game_mode(+Mode, +Description)
% Definit les modes de jeu disponibles.
game_mode(human_vs_human, 'Humain vs Humain').
game_mode(human_vs_bot, 'Humain vs Bot (Bientot disponible)').
game_mode(bot_vs_bot, 'Bot vs Bot (Futur)').

% =============================================================================
% SECTION 9 : UTILITAIRES DE DEBUG
% =============================================================================

% Mode debug dynamique
:- dynamic(debug_enabled/0).

% enable_debug
% Active le mode debug.
enable_debug :-
    retractall(debug_enabled),
    assertz(debug_enabled),
    chess_info(debug, 'Mode debug active').

% disable_debug  
% Desactive le mode debug.
disable_debug :-
    retractall(debug_enabled),
    chess_info(debug, 'Mode debug desactive').

% debug_write(+Message)
% Affiche un message seulement si le mode debug est actif.
debug_write(Message) :-
    (debug_enabled ->
        (write('[DEBUG] '), write(Message), nl)
    ;   true).

% debug_game_state(+GameState)
% Affiche l'etat interne du jeu pour debugging.
debug_game_state(GameState) :-
    debug_enabled,
    !,
    write('=== ETAT INTERNE DU JEU (DEBUG) ==='), nl,
    GameState = game_state(_Board, Player, MoveCount, Status, Captured),
    write('Structure: game_state(Board, Player, MoveCount, Status, Captured)'), nl,
    write('Joueur actuel: '), write(Player), nl,
    write('Nombre de coups: '), write(MoveCount), nl,
    write('Statut: '), write(Status), nl,
    write('Pieces capturees: '), write(Captured), nl, nl.
debug_game_state(_).  % Ne rien faire si debug desactive

% =============================================================================
% SECTION 10 : DETECTION ECHEC ET MAT
% =============================================================================

% is_in_check(+GameState, +Player)
% Verifie si le roi du joueur specifie est en echec.
% Utilise une approche optimisee avec verification directionnelle.
is_in_check(GameState, Player) :-
    GameState = game_state(Board, _, _, _, _),
    ground(GameState), ground(Player),
    member(Player, [white, black]),
    find_king_position(Board, Player, KingRow, KingCol),
    is_square_attacked(Board, KingRow, KingCol, Player).

% is_square_attacked(+Board, +Row, +Col, +DefendingPlayer)
% Verifie si une case est attaquee par le joueur adverse.
% Optimise pour detection rapide avec arret des la premiere attaque.
is_square_attacked(Board, Row, Col, DefendingPlayer) :-
    ground(Board), ground(Row), ground(Col), ground(DefendingPlayer),
    valid_chess_position(Row, Col),
    opposite_player(DefendingPlayer, AttackingPlayer),
    square_attacked_by_any_piece(Board, Row, Col, AttackingPlayer).

% square_attacked_by_any_piece(+Board, +Row, +Col, +AttackingPlayer)
% Verifie si une piece adverse peut attaquer la case donnee.
% Utilise une recherche optimisee par type de piece.
square_attacked_by_any_piece(Board, Row, Col, AttackingPlayer) :-
    (   square_attacked_by_sliding_pieces(Board, Row, Col, AttackingPlayer) ;
        square_attacked_by_knight(Board, Row, Col, AttackingPlayer) ;
        square_attacked_by_pawn(Board, Row, Col, AttackingPlayer) ;
        square_attacked_by_king(Board, Row, Col, AttackingPlayer)
    ).

% square_attacked_by_sliding_pieces(+Board, +Row, +Col, +AttackingPlayer)
% Verifie les attaques des pieces glissantes (tour, fou, dame).
square_attacked_by_sliding_pieces(Board, Row, Col, AttackingPlayer) :-
    sliding_direction_pattern(RowDir, ColDir, PieceTypes),
    check_sliding_attack(Board, Row, Col, AttackingPlayer, RowDir, ColDir, PieceTypes).

% sliding_direction_pattern - Patterns de mouvement optimises
sliding_direction_pattern(RowDir, ColDir, PieceTypes) :-
    movement_direction(horizontal_vertical, RowDir, ColDir),
    PieceTypes = [tour, dame].
sliding_direction_pattern(RowDir, ColDir, PieceTypes) :-
    movement_direction(diagonal, RowDir, ColDir),
    PieceTypes = [fou, dame].

% movement_direction - Directions de base
movement_direction(horizontal_vertical,  0,  1).  % Horizontal droite
movement_direction(horizontal_vertical,  0, -1).  % Horizontal gauche
movement_direction(horizontal_vertical,  1,  0).  % Vertical haut
movement_direction(horizontal_vertical, -1,  0).  % Vertical bas
movement_direction(diagonal,  1,  1).             % Diagonal haut-droite
movement_direction(diagonal,  1, -1).             % Diagonal haut-gauche
movement_direction(diagonal, -1,  1).             % Diagonal bas-droite
movement_direction(diagonal, -1, -1).             % Diagonal bas-gauche

% check_sliding_attack(+Board, +Row, +Col, +AttackingPlayer, +RowDir, +ColDir, +PieceTypes)
% Verifie une attaque glissante dans une direction donnee.
check_sliding_attack(Board, Row, Col, AttackingPlayer, RowDir, ColDir, PieceTypes) :-
    NextRow is Row + RowDir,
    NextCol is Col + ColDir,
    check_sliding_attack_recursive(Board, NextRow, NextCol, AttackingPlayer, RowDir, ColDir, PieceTypes, 1).

% check_sliding_attack_recursive - Avec protection contre recursion infinie
check_sliding_attack_recursive(Board, Row, Col, AttackingPlayer, RowDir, ColDir, PieceTypes, Depth) :-
    Depth =< 8,  % Limite de securite pour echiquier 8x8
    valid_chess_position(Row, Col),
    get_piece(Board, Row, Col, Piece),
    (   Piece = ' ' ->
        % Case vide - continuer la recherche
        NextRow is Row + RowDir,
        NextCol is Col + ColDir,
        NextDepth is Depth + 1,
        check_sliding_attack_recursive(Board, NextRow, NextCol, AttackingPlayer, RowDir, ColDir, PieceTypes, NextDepth)
    ;   piece_belongs_to_player(Piece, AttackingPlayer) ->
        % Piece adverse trouvee - verifier si elle peut attaquer
        get_piece_type(Piece, PieceType),
        member(PieceType, PieceTypes)
    ;   % Piece alliee rencontree - attaque bloquee  
        fail
    ).

% square_attacked_by_knight(+Board, +Row, +Col, +AttackingPlayer)
% Verifie les attaques de cavalier (mouvement en L).
square_attacked_by_knight(Board, Row, Col, AttackingPlayer) :-
    knight_attack_offset(RowOffset, ColOffset),
    KnightRow is Row + RowOffset,
    KnightCol is Col + ColOffset,
    valid_chess_position(KnightRow, KnightCol),
    get_piece(Board, KnightRow, KnightCol, Piece),
    piece_belongs_to_player(Piece, AttackingPlayer),
    get_piece_type(Piece, cavalier).

% knight_attack_offset(-RowOffset, -ColOffset)
% Toutes les positions d'attaque possible d'un cavalier.
knight_attack_offset(-2, -1).  knight_attack_offset(-2,  1).
knight_attack_offset(-1, -2).  knight_attack_offset(-1,  2).
knight_attack_offset( 1, -2).  knight_attack_offset( 1,  2).
knight_attack_offset( 2, -1).  knight_attack_offset( 2,  1).

% square_attacked_by_pawn(+Board, +Row, +Col, +AttackingPlayer)
% Verifie les attaques de pion (diagonales uniquement).
square_attacked_by_pawn(Board, Row, Col, AttackingPlayer) :-
    pawn_attack_direction(AttackingPlayer, RowOffset),
    % FIX CRITIQUE: Tester TOUTES les diagonales explicitement (-1 ET +1)
    (   ColOffset = -1 ; ColOffset = 1),  % Test diagonale gauche ET droite
    PawnRow is Row + RowOffset,
    PawnCol is Col + ColOffset,
    valid_chess_position(PawnRow, PawnCol),
    get_piece(Board, PawnRow, PawnCol, Piece),
    piece_belongs_to_player(Piece, AttackingPlayer),
    get_piece_type(Piece, pion).

% pawn_attack_direction(+Player, -RowOffset)  
% Direction INVERSE pour trouver le pion qui attaque une case donnee.
% Si pions blancs attaquent vers le haut (+1), pour trouver l'attaquant on va vers le bas (-1).
pawn_attack_direction(white, -1).  % Pour trouver pion blanc attaquant, aller vers le bas
pawn_attack_direction(black,  1).  % Pour trouver pion noir attaquant, aller vers le haut


% square_attacked_by_king(+Board, +Row, +Col, +AttackingPlayer)
% Verifie les attaques du roi (une case dans toutes les directions).
square_attacked_by_king(Board, Row, Col, AttackingPlayer) :-
    king_attack_offset(RowOffset, ColOffset),
    KingRow is Row + RowOffset,
    KingCol is Col + ColOffset,
    valid_chess_position(KingRow, KingCol),
    get_piece(Board, KingRow, KingCol, Piece),
    piece_belongs_to_player(Piece, AttackingPlayer),
    get_piece_type(Piece, roi).

% king_attack_offset(-RowOffset, -ColOffset)
% Toutes les directions d'attaque du roi.
king_attack_offset(-1, -1).  king_attack_offset(-1,  0).  king_attack_offset(-1,  1).
king_attack_offset( 0, -1).                              king_attack_offset( 0,  1).
king_attack_offset( 1, -1).  king_attack_offset( 1,  0).  king_attack_offset( 1,  1).

% =============================================================================
% DETECTION DE MAT ET PAT
% =============================================================================

% is_checkmate(+GameState, +Player)
% Verifie si le joueur specifie est mat.
% Mat = en echec ET aucun coup legal disponible.
is_checkmate(GameState, Player) :-
    is_in_check(GameState, Player),
    \+ has_legal_moves(GameState, Player).

% is_stalemate(+GameState, +Player)
% Verifie si le joueur specifie est pat.
% Pat = NOT en echec ET aucun coup legal disponible.
is_stalemate(GameState, Player) :-
    \+ is_in_check(GameState, Player),
    \+ has_legal_moves(GameState, Player).

% has_legal_moves(+GameState, +Player)
% Verifie s'il existe au moins un coup legal pour le joueur.
% Utilise une approche optimisee avec coupe des la premiere solution.
has_legal_moves(GameState, Player) :-
    generate_legal_move(GameState, Player, _, _, _, _), !.

% generate_legal_move(+GameState, +Player, -FromRow, -FromCol, -ToRow, -ToCol)
% Genere un coup legal (ne laissant pas le roi en echec).
% Utilise backtracking pour explorer toutes les possibilites.
generate_legal_move(GameState, Player, FromRow, FromCol, ToRow, ToCol) :-
    GameState = game_state(Board, _, _, _, _),
    between(1, 8, FromRow),
    between(1, 8, FromCol),
    get_piece(Board, FromRow, FromCol, Piece),
    piece_belongs_to_player(Piece, Player),
    between(1, 8, ToRow),
    between(1, 8, ToCol),
    valid_move_basic(Board, Player, FromRow, FromCol, ToRow, ToCol),
    \+ move_leaves_king_in_check(GameState, Player, FromRow, FromCol, ToRow, ToCol).

% valid_move_basic(+Board, +Player, +FromRow, +FromCol, +ToRow, +ToCol)
% Version basique de valid_move sans verification d'echec.
% Utilise pour eviter recursion infinie lors de la verification d'echec.
valid_move_basic(Board, Player, FromRow, FromCol, ToRow, ToCol) :-
    % Verifier les coordonnees avec validation unifiee
    valid_chess_move(FromRow, FromCol, ToRow, ToCol),
    
    % Verifier qu'il y a une piece a la position de depart
    get_piece(Board, FromRow, FromCol, Piece),
    piece_belongs_to_player(Piece, Player),
    
    % Verifier que la destination est libre ou contient une piece ennemie
    get_piece(Board, ToRow, ToCol, DestPiece),
    (DestPiece = ' ' ; 
     (opposite_player(Player, OppositePlayer), piece_belongs_to_player(DestPiece, OppositePlayer))),
    
    % Verifier les regles specifiques a la piece
    can_piece_move(Board, FromRow, FromCol, ToRow, ToCol, Piece).

% move_leaves_king_in_check(+GameState, +Player, +FromRow, +FromCol, +ToRow, +ToCol)
% Verifie si un coup laisse le roi du joueur en echec.
% Simule le coup et teste la position resultante.
move_leaves_king_in_check(GameState, Player, FromRow, FromCol, ToRow, ToCol) :-
    GameState = game_state(Board, _, _, _, _),
    simulate_move(Board, FromRow, FromCol, ToRow, ToCol, NewBoard),
    SimulatedGameState = game_state(NewBoard, Player, 0, active, [[], []]),
    is_in_check(SimulatedGameState, Player).

% simulate_move(+Board, +FromRow, +FromCol, +ToRow, +ToCol, -NewBoard)
% Simule un coup sur l'echiquier sans modifier l'etat de jeu.
% Optimise pour les verifications rapides d'echec.
simulate_move(Board, FromRow, FromCol, ToRow, ToCol, NewBoard) :-
    get_piece(Board, FromRow, FromCol, MovingPiece),
    place_single_piece(Board, FromRow, FromCol, ' ', TempBoard),
    place_single_piece(TempBoard, ToRow, ToCol, MovingPiece, NewBoard).

% =============================================================================
% SECTION 11 : VALIDATION SYSTEME
% =============================================================================

% validate_system_consistency
% Test global de la coherence du systeme de jeu.
validate_system_consistency :-
    % Tester la coherence des pieces
    validate_piece_consistency,
    
    % Tester la coherence des coordonnees
    validate_all_positions,
    
    % Tester les conversions algebriques
    test_coordinate_conversion,
    
    chess_info(systeme, 'Validation de coherence reussie').


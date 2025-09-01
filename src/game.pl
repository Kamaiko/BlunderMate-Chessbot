% =============================================================================
% CHESS GAME - LOGIQUE DE JEU ET GESTION D'ETAT
% =============================================================================
% 
% Ce module centralise TOUTE la logique de jeu :
% - Gestion de l'etat du jeu (initialisation, mise a jour)
% - Validation et execution des mouvements
% - Logique des regles d'echecs
% - Gestion des captures et de l'alternance des joueurs
%
% Auteur : Patrick Patenaude
% Version : 5.1 (Consolidation intuitive)
%
% RESPONSABILITES :
% - Structure et manipulation de l'etat du jeu
% - Validation complete des mouvements
% - Execution des mouvements avec mise a jour d'etat
% - Gestion des pieces capturees
% - Affichage de l'etat du jeu
% =============================================================================

:- [pieces].
:- [board].

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
    % Verifier les coordonnees avec validation unifiee
    valid_chess_move(FromRow, FromCol, ToRow, ToCol),
    
    % Verifier qu'il y a une piece a la position de depart
    get_piece(Board, FromRow, FromCol, Piece),
    piece_belongs_to_player(Piece, Player),
    
    % Verifier que la destination est libre ou contient une piece ennemie
    get_piece(Board, ToRow, ToCol, DestPiece),
    (is_empty_square(DestPiece) ; 
     (opposite_player(Player, OppositePlayer), piece_belongs_to_player(DestPiece, OppositePlayer))),
    
    % Verifier les regles specifiques a la piece
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
    place_single_piece(TempBoard, ToRow, ToCol, Piece, NewBoard).

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
% Affiche l'etat complet du jeu.
display_game_state(GameState) :-
    GameState = game_state(Board, Player, MoveCount, _, CapturedPieces),
    display_board(Board),
    display_captured_pieces(CapturedPieces),
    write('Joueur actuel: '), 
    translate_player(Player, PlayerFR), 
    write(PlayerFR), nl,
    write('Nombre de coups: '), write(MoveCount), nl, nl.

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

% chess_error(+Type, +Context, +Message)
% Affiche un message d'erreur standardise.
chess_error(Type, Context, Message) :-
    write('ERREUR '), write(Type), write(' dans '), write(Context), 
    write(' : '), write(Message), nl.

% chess_warning(+Context, +Message)
% Affiche un avertissement.
chess_warning(Context, Message) :-
    write('ATTENTION '), write(Context), write(' : '), write(Message), nl.

% chess_info(+Context, +Message)
% Affiche un message informatif.
chess_info(Context, Message) :-
    write('INFO '), write(Context), write(' : '), write(Message), nl.

% validate_move_input(+Input, -FromRow, -FromCol, -ToRow, -ToCol)
% Valide et parse une entree de mouvement avec gestion d'erreur.
validate_move_input(Input, FromRow, FromCol, ToRow, ToCol) :-
    validate_input_exists(Input),
    validate_input_length(Input),
    validate_input_coordinates(Input, FromRow, FromCol, ToRow, ToCol).

% validate_input_exists(+Input)
% Verifie que l'entree n'est pas vide.
validate_input_exists(Input) :-
    (nonvar(Input) ->
        true
    ;   chess_error(entree, saisie_mouvement, 'Aucune entree fournie'),
        fail
    ).

% validate_input_length(+Input)
% Verifie que l'entree a la bonne longueur.
validate_input_length(Input) :-
    (string_length(Input, 4) ->
        true
    ;   chess_error(syntaxe, saisie_mouvement, 'Le mouvement doit contenir exactement 4 caracteres'),
        fail
    ).

% validate_input_coordinates(+Input, -FromRow, -FromCol, -ToRow, -ToCol)
% Valide que les coordonnees sont correctes.
validate_input_coordinates(Input, FromRow, FromCol, ToRow, ToCol) :-
    (parse_algebraic_move(Input, FromRow, FromCol, ToRow, ToCol) ->
        true
    ;   chess_error(validation, mouvement, 'Coordonnees invalides dans le mouvement'),
        fail
    ).

% =============================================================================
% SECTION 6 : UTILITAIRES DE JEU
% =============================================================================

% safe_read_input(-Input)
% Lecture securisee de l'entree utilisateur.
safe_read_input(Input) :-
    repeat,
    write('> '),
    flush_output,
    catch(
        (read_line_to_string(user_input, String),
         normalize_input_string(String, Input)),
        _Error,
        (chess_error(entree, lecture, 'Erreur de lecture - veuillez reessayer'),
         fail)
    ),
    !.

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
    length(Board, 8),
    
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

% chess_constant(+Name, -Value)
% Definit les constantes importantes du jeu.
chess_constant(board_size, 8).
chess_constant(max_moves, 200).  % Limite de securite
chess_constant(min_row, 1).
chess_constant(max_row, 8).
chess_constant(min_col, 1).
chess_constant(max_col, 8).

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
    ), !.

% square_attacked_by_sliding_pieces(+Board, +Row, +Col, +AttackingPlayer)
% Verifie les attaques des pieces glissantes (tour, fou, dame).
square_attacked_by_sliding_pieces(Board, Row, Col, AttackingPlayer) :-
    sliding_direction(RowDir, ColDir, PieceTypes),
    check_sliding_attack(Board, Row, Col, AttackingPlayer, RowDir, ColDir, PieceTypes).

% sliding_direction(-RowDir, -ColDir, -PieceTypes)
% Definit les directions de glissement et les pieces correspondantes.
sliding_direction( 0,  1, [tour, dame]).    % Horizontal droite
sliding_direction( 0, -1, [tour, dame]).    % Horizontal gauche  
sliding_direction( 1,  0, [tour, dame]).    % Vertical haut
sliding_direction(-1,  0, [tour, dame]).    % Vertical bas
sliding_direction( 1,  1, [fou, dame]).     % Diagonal haut-droite
sliding_direction( 1, -1, [fou, dame]).     % Diagonal haut-gauche
sliding_direction(-1,  1, [fou, dame]).     % Diagonal bas-droite
sliding_direction(-1, -1, [fou, dame]).     % Diagonal bas-gauche

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
    ).
    % Si piece alliee rencontree, l'attaque est bloquee (echec implicite)

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
    pawn_attack_column_offset(ColOffset),
    PawnRow is Row + RowOffset,
    PawnCol is Col + ColOffset,
    valid_chess_position(PawnRow, PawnCol),
    get_piece(Board, PawnRow, PawnCol, Piece),
    piece_belongs_to_player(Piece, AttackingPlayer),
    get_piece_type(Piece, pion).

% pawn_attack_direction(+Player, -RowOffset)  
% Direction INVERSE pour trouver le pion qui attaque une case donnee.
% Si pions blancs attaquent vers le haut (+1), pour trouver l'attaquant on va vers le bas (-1).
pawn_attack_direction(white,  1).  % Pour trouver pion blanc attaquant, aller vers le bas
pawn_attack_direction(black, -1).  % Pour trouver pion noir attaquant, aller vers le haut

% pawn_attack_column_offset(-ColOffset)
% Colonnes d'attaque diagonale des pions.
pawn_attack_column_offset(-1).  % Diagonale gauche
pawn_attack_column_offset( 1).  % Diagonale droite

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

% =============================================================================
% FIN DU FICHIER GAME.PL
% Derniere mise a jour : Aout 2025
% =============================================================================
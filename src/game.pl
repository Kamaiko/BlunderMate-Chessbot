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
    execute_move(Board, FromRow, FromCol, ToRow, ToCol, NewBoard, CapturedPieces, NewCapturedPieces),
    switch_player(Player, NewPlayer),
    NewMoveCount is MoveCount + 1,
    
    % Pour l'instant, le jeu continue toujours (pas de detection d'echec et mat)
    NewGameState = game_state(NewBoard, NewPlayer, NewMoveCount, active, NewCapturedPieces).

% execute_move(+Board, +FromRow, +FromCol, +ToRow, +ToCol, -NewBoard, +CapturedPieces, -NewCapturedPieces)
% Execute un mouvement sur l'echiquier avec gestion des captures.
execute_move(Board, FromRow, FromCol, ToRow, ToCol, NewBoard, CapturedPieces, NewCapturedPieces) :-
    get_piece(Board, FromRow, FromCol, Piece),
    get_piece(Board, ToRow, ToCol, TargetPiece),
    
    % Mettre a jour les pieces capturees si capture
    (TargetPiece \= ' ' ->
        update_captured_pieces(TargetPiece, CapturedPieces, NewCapturedPieces)
    ;   NewCapturedPieces = CapturedPieces
    ),
    
    % Executer le mouvement
    place_single_piece(Board, FromRow, FromCol, ' ', Board1),
    place_single_piece(Board1, ToRow, ToCol, Piece, NewBoard).

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
    (nonvar(Input) ->
        (string_length(Input, 4) ->
            (parse_algebraic_move(Input, FromRow, FromCol, ToRow, ToCol) ->
                true
            ;   chess_error(validation, mouvement, 'Coordonnees invalides dans le mouvement'),
                fail)
        ;   chess_error(syntaxe, saisie_mouvement, 'Le mouvement doit contenir exactement 4 caracteres'),
            fail)
    ;   chess_error(entree, saisie_mouvement, 'Aucune entree fournie'),
        fail).

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
% SECTION 10 : VALIDATION SYSTEME
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
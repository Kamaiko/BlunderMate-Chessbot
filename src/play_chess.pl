% =============================================================================
% CHESS GAME LAUNCHER - POINT D'ENTRÉE PRINCIPAL
% =============================================================================
% 
% Ce fichier gère uniquement l'interface utilisateur et la navigation.
% Version nettoyée sans tests intégrés.
%
% Auteur : Patrick Patenaude
% Version : 5.0 (Version nettoyée)
% Dépendances : game_logic.pl, board_smart.pl
% =============================================================================

:- [game_logic].
:- [board_smart].

% =============================================================================
% SECTION 1 : MENU PRINCIPAL
% =============================================================================

% Point d'entrée principal
start :-
    main_menu.

% Affichage et gestion du menu principal
main_menu :-
    write('======================'), nl,
    write('  PROLOG CHESS GAME'), nl,
    write('======================'), nl, nl,
    write('Choose an option:'), nl,
    write('1 - Start Human vs Human game'), nl,
    write('2 - Start Human vs Bot game (Coming soon)'), nl,
    write('3 - Quick system test'), nl,
    write('4 - Show help'), nl,
    write('5 - Exit'), nl, nl,
    write('Enter your choice (1-5): '),
    get_single_char(CharCode),
    char_code(Choice, CharCode),
    nl,
    process_choice(Choice).

% Traitement des choix du menu
process_choice('1') :-
    start_human_game.

process_choice('2') :-
    write('Human vs Bot mode is not yet implemented.'), nl,
    write('Coming in future version!'), nl, nl,
    write('Press any key to continue...'), nl,
    get_single_char(_),
    main_menu.

process_choice('3') :-
    write('Running system tests...'), nl,
    write('Please run: consult(''tests/test_all'').'), nl,
    write('Then: run_all_tests.'), nl, nl,
    write('Press any key to continue...'), nl,
    get_single_char(_),
    main_menu.

process_choice('4') :-
    show_help,
    main_menu.

process_choice('5') :-
    write('Goodbye!'), nl.

process_choice(_) :-
    write('Invalid choice. Please enter 1, 2, 3, 4, or 5.'), nl,
    main_menu.

% =============================================================================
% SECTION 2 : JEU HUMAIN VS HUMAIN
% =============================================================================

% Démarrer une partie humain vs humain
start_human_game :-
    write('=== HUMAN VS HUMAN CHESS GAME ==='), nl,
    display_legend,
    init_game_state(GameState),
    display_game_state(GameState),
    write('Enter moves in algebraic notation (e.g., e2e4.)'), nl,
    write('Type "quit." to exit, "help." for commands'), nl, nl,
    game_loop(GameState).

% Afficher la légende des pièces
display_legend :-
    nl,
    write('=== PIECE LEGEND ==='), nl,
    write('White pieces (uppercase): P=Pawn, R=Rook, N=Knight, B=Bishop, Q=Queen, K=King'), nl,
    write('Black pieces (lowercase): p=pawn, r=rook, n=knight, b=bishop, q=queen, k=king'), nl,
    write('Move format: e2e4. (from e2 to e4, don''t forget the dot!)'), nl, nl.

% Boucle principale du jeu
game_loop(GameState) :-
    GameState = game_state(_, Player, _, Status),
    (Status = active ->
        (write('Player '), write(Player), write('> '),
         read_player_input(Input),
         process_game_input(Input, GameState, NewGameState),
         game_loop(NewGameState))
    ;   write('Game Over!'), nl).

% Lire l'entrée du joueur avec gestion robuste
read_player_input(Input) :-
    repeat,
    write('> '),
    flush_output,
    catch(
        (read_line_to_string(user_input, String),
         process_input_string(String, Input)),
        _Error,
        (write('Invalid input. Please try again.'), nl, fail)
    ),
    !.

% Traiter la chaîne d'entrée
process_input_string(String, Input) :-
    (String = end_of_file ->
        Input = quit
    ;   % Nettoyer la chaîne (enlever espaces et point final)
        normalize_space(string(CleanString), String),
        remove_trailing_dot(CleanString, FinalString),
        atom_string(Input, FinalString)
    ).

% Enlever le point final s'il existe
remove_trailing_dot(String, Output) :-
    (string_concat(Output, ".", String) ->
        true
    ;   Output = String
    ).

% Traitement des commandes de jeu
process_game_input(quit, _, _) :-
    write('Thanks for playing!'), nl, !.

process_game_input(help, GameState, GameState) :-
    show_game_help, !.

process_game_input(board, GameState, GameState) :-
    GameState = game_state(Board, _, _, _),
    display_board(Board), !.

process_game_input(Input, GameState, NewGameState) :-
    atom(Input),
    atom_string(Input, InputStr),
    (parse_move_input(InputStr, FromRow, FromCol, ToRow, ToCol) ->
        attempt_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState)
    ;   write('Invalid move format. Use algebraic notation like e2e4'), nl,
        NewGameState = GameState).

% Parser l'entrée du mouvement
parse_move_input(InputStr, FromRow, FromCol, ToRow, ToCol) :-
    string_length(InputStr, 4),
    parse_algebraic_move(InputStr, FromRow, FromCol, ToRow, ToCol).

% Tenter d'effectuer un mouvement
attempt_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState) :-
    GameState = game_state(Board, Player, _, _),
    (valid_position(FromRow, FromCol), valid_position(ToRow, ToCol) ->
        (get_piece(Board, FromRow, FromCol, Piece),
         piece_belongs_to_player(Piece, Player) ->
            (make_move(GameState, FromRow, FromCol, ToRow, ToCol, TempGameState) ->
                (coordinates_to_algebraic(FromRow, FromCol, ToRow, ToCol, MoveStr),
                 write('Move played: '), write(MoveStr), nl, nl,
                 display_game_state(TempGameState),
                 NewGameState = TempGameState)
            ;   write('Illegal move. Try again.'), nl,
                NewGameState = GameState)
        ;   write('No piece of your color at that position.'), nl,
            NewGameState = GameState)
    ;   write('Invalid coordinates.'), nl,
        NewGameState = GameState).

% =============================================================================
% SECTION 3 : UTILITAIRES ET AIDE
% =============================================================================

% Vérifier si les coordonnées sont valides
valid_position(Row, Col) :-
    Row >= 1, Row =< 8, Col >= 1, Col =< 8.

% Vérifier si une pièce appartient au joueur
piece_belongs_to_player(Piece, white) :-
    is_white_piece(Piece).
piece_belongs_to_player(Piece, black) :-
    is_black_piece(Piece).

% Afficher l'aide générale
show_help :-
    nl,
    write('=== CHESS GAME HELP ==='), nl,
    write('This is a Human vs Human chess game.'), nl,
    write('Enter moves in algebraic notation: e2e4'), nl,
    write('During game, you can also type:'), nl,
    write('- help: Show commands'), nl,
    write('- board: Show current position'), nl,
    write('- quit: Exit game'), nl,
    write('Don''t forget the dot (.) after each command!'), nl, nl.

% Afficher l'aide de jeu
show_game_help :-
    write('GAME COMMANDS:'), nl,
    write('- Move: Enter in format "e2e4." (from e2 to e4)'), nl,
    write('- quit.: Exit the game'), nl,
    write('- help.: Show this help'), nl,
    write('- board.: Show current board'), nl,
    write('Remember: Always end commands with a dot (.)'), nl, nl.

% =============================================================================
% SECTION 4 : TEST RAPIDE
% =============================================================================

% Test rapide du système
quick_test :-
    write('=== QUICK SYSTEM TEST ==='), nl,
    init_game_state(GameState),
    display_game_state(GameState),
    
    % Test quelques mouvements
    write('Testing move e2e4...'), nl,
    make_move(GameState, 2, 5, 4, 5, GameState2),
    display_game_state(GameState2),
    
    write('Testing move e7e5...'), nl,
    make_move(GameState2, 7, 5, 5, 5, GameState3),
    display_game_state(GameState3),
    
    write('System test completed!'), nl.

% =============================================================================
% FIN DU FICHIER - VERSION NETTOYÉE
% Dernière mise à jour : Août 2025
% =============================================================================
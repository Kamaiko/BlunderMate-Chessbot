% =============================================================================
% CHESS INTERFACE - INTERFACE UTILISATEUR ET MESSAGES FRANCAIS
% =============================================================================
% 
% Ce module centralise TOUTE l'interface utilisateur :
% - Menu principal et navigation
% - Boucle de jeu et interaction joueur
% - Messages en francais centralises
% - Gestion des commandes et de l'aide
%
% Auteur : Patrick Patenaude
% Version : 5.1 (Consolidation intuitive)
%
% RESPONSABILITES :
% - Menu principal et choix utilisateur
% - Interface de jeu humain vs humain
% - Messages francais et aide
% - Gestion des entrees et validation
% - Integration avec les tests externes
% =============================================================================

:- [pieces].
:- [board].
:- [game].

% =============================================================================
% SECTION 1 : MESSAGES FRANCAIS CENTRALISES
% =============================================================================

% Messages du menu principal
message(welcome_title, 'JEU D\'ECHECS PROLOG').
message(welcome_separator, '======================').
message(menu_prompt, 'Choisissez une option:').
message(menu_option_1, '1 - Commencer une partie Humain vs Humain').
message(menu_option_2, '2 - Commencer une partie Humain vs Bot (Bientot disponible)').
message(menu_option_3, '3 - Executer les tests rapides (externes)').
message(menu_option_4, '4 - Executer la suite complete de tests (externe)').
message(menu_option_5, '5 - Afficher l\'aide').
message(menu_option_6, '6 - Quitter').
message(menu_choice_prompt, 'Entrez votre choix (1-6): ').
message(invalid_choice, 'Choix invalide. Veuillez entrer 1, 2, 3, 4, 5, ou 6.').
message(goodbye, 'Au revoir!').
message(thanks_playing, 'Merci d\'avoir joue aux Echecs Prolog!').
message(press_key_continue, 'Appuyez sur une touche pour continuer...').
message(press_key_menu, 'Appuyez sur une touche pour retourner au menu principal...').

% Messages de jeu
message(game_title_human_vs_human, '=== PARTIE D\'ECHECS HUMAIN VS HUMAIN ===').
message(legend_title, '=== LEGENDE DES PIECES ===').
message(white_pieces_legend, 'Pieces blanches (majuscules): P=Pion, R=Tour, N=Cavalier, B=Fou, Q=Dame, K=Roi').
message(black_pieces_legend, 'Pieces noires (minuscules): p=pion, r=tour, n=cavalier, b=fou, q=dame, k=roi').
message(move_format_help, 'Format des mouvements: e2e4. (de e2 vers e4, n\'oubliez pas le point!)').
message(move_instructions, 'Entrez les mouvements en notation algebrique (ex: e2e4)').
message(game_commands, 'Commandes: quit (menu), exit (quitter), help (aide)').
message(current_player, 'Joueur actuel: ').
message(move_count, 'Nombre de coups: ').
message(game_finished, 'Partie terminee!').
message(move_played, 'Mouvement joue: ').
message(illegal_move, 'Mouvement illegal!').
message(invalid_coordinates, 'Coordonnees invalides!').
message(no_piece_at_position, 'Aucune piece de votre couleur a cette position!').

% Messages d'aide
message(help_title, '=== AIDE JEU D\'ECHECS ===').
message(help_description, 'Ceci est un jeu d\'echecs Humain vs Humain.').
message(help_move_format, 'Entrez les mouvements en notation algebrique: e2e4').
message(help_commands_intro, 'Pendant le jeu, vous pouvez aussi taper:').
message(help_command_help, '- help: Afficher les commandes').
message(help_command_board, '- board: Afficher la position actuelle').
message(help_command_quit, '- quit: Retour au menu principal').
message(help_command_exit, '- exit: Quitter le programme completement').
message(help_dot_reminder, 'N\'oubliez pas le point (.) apres chaque commande!').

% Messages d'aide pendant le jeu
message(game_help_title, '=== AIDE PENDANT LE JEU ===').
message(game_help_commands, 'Commandes disponibles :').
message(game_help_moves, '- Mouvements : e2e4 (notation algebrique)').
message(game_help_move_example, '  * De e2 vers e4 - exemple: pion avance de 2 cases').
message(game_help_format_required, '  * Format obligatoire: 4 caracteres exactement').
message(game_help_help, '- help : Afficher cette aide').
message(game_help_board, '- board : Afficher l\'echiquier actuel').
message(game_help_quit, '- quit : Retour au menu principal').
message(game_help_exit, '- exit : Quitter le programme completement').
message(game_help_pieces_title, 'PIECES (notation ASCII):').
message(game_help_white_pieces, 'Blanches: P=Pion R=Tour N=Cavalier B=Fou Q=Dame K=Roi').
message(game_help_black_pieces, 'Noires:   p=pion r=tour n=cavalier b=fou q=dame k=roi').

% Messages de test
message(running_quick_tests, 'Execution des tests rapides externes...').
message(loading_quick_tests, 'Chargement de tests/quick_tests.pl...').
message(tests_loaded_success, 'Tests charges avec succes. Execution de quick_test...').
message(running_full_tests, 'Execution de la suite complete de tests externe...').
message(loading_full_tests, 'Chargement de tests/chess_tests.pl...').
message(full_tests_loaded_success, 'Tests charges avec succes. Execution de run_all_tests...').
message(error_loading_quick_tests, 'Erreur: Impossible de charger tests/quick_tests.pl').
message(error_loading_full_tests, 'Erreur: Impossible de charger tests/chess_tests.pl').
message(ensure_file_exists, 'Veuillez vous assurer que le fichier existe et est accessible.').
message(bot_not_implemented, 'Le mode Humain vs Bot n\'est pas encore implemente.').
message(available_future_version, 'Disponible dans une version future!').

% =============================================================================
% SECTION 2 : UTILITAIRES D'AFFICHAGE DES MESSAGES
% =============================================================================

% display_message(+MessageKey)
% Affiche un message par sa cle.
display_message(Key) :-
    (message(Key, Text) ->
        write(Text)
    ;   write('Message non trouve: '), write(Key)
    ).

% display_message_ln(+MessageKey)
% Affiche un message suivi d'un retour a la ligne.
display_message_ln(Key) :-
    display_message(Key),
    nl.

% get_message(+MessageKey, -Text)
% Recupere le texte d'un message.
get_message(Key, Text) :-
    message(Key, Text).

% =============================================================================
% SECTION 3 : MENU PRINCIPAL ET NAVIGATION
% =============================================================================

% start
% Point d'entree principal du programme.
start :- main_menu.

% main_menu
% Affiche et gere le menu principal.
main_menu :-
    display_message_ln(welcome_separator),
    display_message_ln(welcome_title),
    display_message_ln(welcome_separator), nl,
    display_message_ln(menu_prompt),
    display_message_ln(menu_option_1),
    display_message_ln(menu_option_2),
    display_message_ln(menu_option_3),
    display_message_ln(menu_option_4),
    display_message_ln(menu_option_5),
    display_message_ln(menu_option_6), nl,
    display_message(menu_choice_prompt),
    get_single_char(CharCode),
    char_code(Choice, CharCode),
    nl,
    process_choice(Choice).

% process_choice(+Choice)
% Traitement des choix du menu principal.
process_choice('1') :-
    start_human_game.

process_choice('2') :-
    display_message_ln(bot_not_implemented),
    display_message_ln(available_future_version), nl,
    display_message_ln(press_key_continue),
    get_single_char(_),
    main_menu.

process_choice('3') :-
    display_message_ln(running_quick_tests),
    display_message_ln(loading_quick_tests),
    (consult('tests/quick_tests') ->
        display_message_ln(tests_loaded_success), nl,
        quick_test
    ;   display_message_ln(error_loading_quick_tests),
        display_message_ln(ensure_file_exists)), nl,
    display_message_ln(press_key_continue),
    get_single_char(_),
    main_menu.

process_choice('4') :-
    display_message_ln(running_full_tests),
    display_message_ln(loading_full_tests),
    (consult('tests/chess_tests') ->
        display_message_ln(full_tests_loaded_success), nl,
        run_all_tests
    ;   display_message_ln(error_loading_full_tests),
        display_message_ln(ensure_file_exists)), nl,
    display_message_ln(press_key_continue),
    get_single_char(_),
    main_menu.

process_choice('5') :-
    show_help,
    display_message_ln(press_key_continue),
    get_single_char(_),
    main_menu.

process_choice('6') :-
    display_message_ln(goodbye),
    display_message_ln(thanks_playing),
    halt.

process_choice(_) :-
    display_message_ln(invalid_choice),
    main_menu.

% =============================================================================
% SECTION 4 : JEU HUMAIN VS HUMAIN
% =============================================================================

% start_human_game
% Demarre une partie humain vs humain.
start_human_game :-
    display_message_ln(game_title_human_vs_human),
    display_legend,
    init_game_state(GameState),
    display_game_state(GameState),
    display_message_ln(move_instructions),
    display_message_ln(game_commands), nl,
    game_loop(GameState).

% display_legend
% Affiche la legende des pieces.
display_legend :-
    nl,
    display_message_ln(legend_title),
    display_message_ln(white_pieces_legend),
    display_message_ln(black_pieces_legend),
    display_message_ln(move_format_help), nl.

% game_loop(+GameState)
% Boucle principale du jeu.
game_loop(GameState) :-
    GameState = game_state(_, Player, _, Status, _),
    (Status = active ->
        (write('Joueur '), translate_player(Player, PlayerFR), write(PlayerFR), write('> '),
         read_player_input(Input),
         process_game_input(Input, GameState, NewGameState),
         game_loop(NewGameState))
    ;   display_message_ln(game_finished)).

% read_player_input(-Input)
% Lit l'entree du joueur avec gestion robuste.
read_player_input(Input) :-
    repeat,
    catch(
        (read_line_to_string(user_input, String),
         normalize_input_string(String, Input)),
        _Error,
        (chess_error(entree, saisie_joueur, 'Entree invalide - veuillez reessayer'), fail)
    ),
    !.

% =============================================================================
% SECTION 5 : TRAITEMENT DES COMMANDES DE JEU
% =============================================================================

% process_game_input(+Input, +GameState, -NewGameState)
% Traitement des commandes pendant le jeu.
process_game_input(quit, _, _) :-
    display_message_ln(thanks_playing),
    display_message_ln(press_key_menu),
    get_single_char(_),
    main_menu, !.

process_game_input(exit, _, _) :-
    display_message_ln(thanks_playing),
    display_message_ln(goodbye),
    halt.

process_game_input(help, GameState, GameState) :-
    show_game_help, !.

process_game_input(board, GameState, GameState) :-
    GameState = game_state(Board, _, _, _, _),
    display_board(Board), !.

process_game_input(Input, GameState, NewGameState) :-
    atom(Input),
    atom_string(Input, InputStr),
    % Verifier les commandes speciales
    (InputStr = "exit" ->
        (display_message_ln(thanks_playing),
         display_message_ln(goodbye),
         halt)
    ; InputStr = "quit" ->
        (display_message_ln(thanks_playing),
         display_message_ln(press_key_menu),
         get_single_char(_),
         main_menu, !)
    ; InputStr = "help" ->
        (show_game_help, NewGameState = GameState)
    ; % Sinon, c'est un mouvement
        (parse_move_input(InputStr, FromRow, FromCol, ToRow, ToCol) ->
            attempt_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState)
        ;   write('Format de mouvement invalide!'), nl,
            write('  Attendu: 4 caracteres comme "e2e4" (de e2 vers e4)'), nl,
            write('  Ou: exit, quit, help'), nl,
            write('  Votre entree: '), write(InputStr), nl,
            NewGameState = GameState)
    ).

% parse_move_input(+InputStr, -FromRow, -FromCol, -ToRow, -ToCol)
% Parse l'entree de mouvement.
parse_move_input(InputStr, FromRow, FromCol, ToRow, ToCol) :-
    string_length(InputStr, 4),
    parse_algebraic_move(InputStr, FromRow, FromCol, ToRow, ToCol).

% attempt_move(+GameState, +FromRow, +FromCol, +ToRow, +ToCol, -NewGameState)
% Tente d'executer un mouvement.
attempt_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState) :-
    GameState = game_state(Board, Player, _, _, _),
    (valid_chess_position(FromRow, FromCol), valid_chess_position(ToRow, ToCol) ->
        (get_piece(Board, FromRow, FromCol, Piece),
         piece_belongs_to_player(Piece, Player) ->
            (make_move(GameState, FromRow, FromCol, ToRow, ToCol, TempGameState) ->
                (coordinates_to_algebraic(FromRow, FromCol, ToRow, ToCol, MoveStr),
                 display_message(move_played), write(MoveStr), nl, nl,
                 display_game_state(TempGameState),
                 display_message_ln(move_instructions),
                 display_message_ln(game_commands), nl,
                 NewGameState = TempGameState)
            ;   display_message_ln(illegal_move),
                write('  Raison: Cette piece ne peut pas aller a cette position'), nl,
                write('  Verifiez: Regles de mouvement, blocage du chemin, ou regles du jeu'), nl,
                NewGameState = GameState)
        ;   display_message_ln(no_piece_at_position),
            write('  Verifiez: Assurez-vous d\'avoir une piece '), translate_player(Player, PlayerFR), write(PlayerFR), write(' a la position de depart'), nl,
            NewGameState = GameState)
    ;   display_message_ln(invalid_coordinates),
        write('  Plage valide: rangees 1-8, colonnes a-h'), nl,
        write('  Exemple: e2e4 (e=colonne 5, 2=rangee 2 vers e=colonne 5, 4=rangee 4)'), nl,
        NewGameState = GameState).

% =============================================================================
% SECTION 6 : AFFICHAGE DE L'AIDE
% =============================================================================

% show_help
% Affiche l'aide generale.
show_help :-
    nl,
    display_message_ln(help_title),
    display_message_ln(help_description),
    display_message_ln(help_move_format),
    display_message_ln(help_commands_intro),
    display_message_ln(help_command_help),
    display_message_ln(help_command_board),
    display_message_ln(help_command_quit),
    display_message_ln(help_command_exit),
    display_message_ln(help_dot_reminder), nl.

% show_game_help
% Affiche l'aide pendant le jeu.
show_game_help :-
    nl,
    display_message_ln(game_help_title),
    display_message_ln(game_help_commands),
    display_message_ln(game_help_moves),
    display_message_ln(game_help_move_example),
    display_message_ln(game_help_format_required),
    display_message_ln(game_help_help),
    display_message_ln(game_help_board),
    display_message_ln(game_help_quit),
    display_message_ln(game_help_exit),
    display_message_ln(help_dot_reminder),
    write(''), nl,
    display_message_ln(game_help_pieces_title),
    display_message_ln(game_help_white_pieces),
    display_message_ln(game_help_black_pieces), nl.

% =============================================================================
% FIN DU FICHIER INTERFACE.PL
% Derniere mise a jour : Aout 2025
% =============================================================================
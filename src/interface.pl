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

% Messages du menu - PARTIELLEMENT NETTOYES
message(invalid_choice, 'Choix invalide. Veuillez entrer 1, 2, 3, 4, 5, ou 6.').
message(goodbye, 'Au revoir!').
message(thanks_playing, 'Merci d\'avoir joue aux Echecs Prolog!').

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

% Messages d'aide - NETTOYES (fonctions modernes utilisees maintenant)

% Messages de test
message(running_quick_tests, 'Execution des tests rapides externes...').
message(loading_quick_tests, 'Chargement de tests/smoke_tests.pl...').
message(tests_loaded_success, 'Tests charges avec succes. Execution de quick_test...').
message(running_full_tests, 'Execution de la suite complete de tests externe...').
message(loading_full_tests, 'Chargement de tests/regression_tests.pl...').
message(full_tests_loaded_success, 'Tests charges avec succes. Execution de run_all_tests...').
message(error_loading_quick_tests, 'Erreur: Impossible de charger tests/smoke_tests.pl').
message(error_loading_full_tests, 'Erreur: Impossible de charger tests/regression_tests.pl').
message(ensure_file_exists, 'Veuillez vous assurer que le fichier existe et est accessible.').
message(bot_not_implemented, 'Le mode Humain vs Bot n\'est pas encore implemente.').
message(available_future_version, 'Disponible dans une version future!').

% =============================================================================
% SECTION 2 : UTILITAIRES D'AFFICHAGE ET INTERFACE
% =============================================================================

% clear_screen
% Nettoie l'ecran de maniere compatible multi-plateformes.
clear_screen :-
    (current_prolog_flag(windows, true) ->
        shell('cls', 0)
    ;   shell('clear', 0)
    ).

% draw_line(+Length, +Character)
% Dessine une ligne horizontale de longueur donnee.
draw_line(Length, Char) :-
    draw_line_aux(Length, Char).

draw_line_aux(0, _) :- !.
draw_line_aux(N, Char) :-
    N > 0,
    write(Char),
    N1 is N - 1,
    draw_line_aux(N1, Char).

% center_text(+Text, +Width)
% Centre un texte dans une largeur donnee.
center_text(Text, Width) :-
    atom_length(Text, TextLen),
    (TextLen >= Width ->
        write(Text)
    ;   Padding is (Width - TextLen) // 2,
        draw_spaces(Padding),
        write(Text),
        RemainingSpaces is Width - TextLen - Padding,
        draw_spaces(RemainingSpaces)
    ).

% draw_spaces(+Count)
% Dessine un nombre donne d'espaces.
draw_spaces(0) :- !.
draw_spaces(N) :-
    N > 0,
    write(' '),
    N1 is N - 1,
    draw_spaces(N1).

% display_title_box(+Title)
% Affiche un titre dans une boite ASCII de 50 caracteres de large.
display_title_box(Title) :-
    nl, nl,
    write('    '), draw_line(50, '='), nl,
    write('    |'), draw_spaces(48), write('|'), nl,
    write('    |'),
    center_text(Title, 48),
    write('|'), nl,
    write('    |'), draw_spaces(48), write('|'), nl,
    write('    '), draw_line(50, '='), nl, nl.

% =============================================================================
% SECTION 3 : UTILITAIRES D'AFFICHAGE DES MESSAGES
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
% Affiche et gere le menu principal moderne.
main_menu :-
    display_modern_menu,
    read_menu_choice(Choice),
    process_choice(Choice).

% display_modern_menu
% Affiche le menu principal avec design ASCII moderne.
display_modern_menu :-
    nl, nl,
    % Bordure superieure
    write('    '), draw_line(50, '='), nl,
    write('    |'), draw_spaces(48), write('|'), nl,
    
    % Titre centre
    write('    |'),
    center_text('JEU D\'ECHECS PROLOG', 48),
    write('|'), nl,
    
    write('    |'), draw_spaces(48), write('|'), nl,
    write('    '), draw_line(50, '='), nl,
    
    % Options du menu
    nl,
    write('    1. Nouvelle partie Humain vs Humain'), nl,
    write('    2. Mode IA (bientot disponible)'), nl,
    nl,
    write('    3. Tests rapides'), nl,
    write('    4. Tests complets'), nl,
    nl,
    write('    5. Aide'), nl,
    write('    6. Quitter'), nl,
    nl,
    
    % Ligne de separation
    write('    '), draw_line(35, '-'), nl,
    write('    Entrez votre choix (1-6): ').

% read_menu_choice(-Choice)
% Lit le choix du menu de maniere robuste.
read_menu_choice(Choice) :-
    get_single_char(CharCode),
    char_code(Choice, CharCode),
    nl, nl.

% pause_and_return_menu
% Pause elegant et retour au menu principal.
pause_and_return_menu :-
    nl,
    write('    '), draw_line(35, '-'), nl,
    write('    Appuyez sur une touche pour continuer...'),
    get_single_char(_),
    main_menu.

% process_choice(+Choice)
% Traitement des choix du menu principal.
process_choice('1') :-
    start_human_game.

process_choice('2') :-
    display_title_box('MODE INTELLIGENCE ARTIFICIELLE'),
    display_message_ln(bot_not_implemented),
    display_message_ln(available_future_version),
    pause_and_return_menu.

process_choice('3') :-
    display_title_box('TESTS RAPIDES'),
    display_message_ln(loading_quick_tests),
    (consult('tests/smoke_tests') ->
        quick_test
    ;   display_message_ln(error_loading_quick_tests),
        display_message_ln(ensure_file_exists)),
    pause_and_return_menu.

process_choice('4') :-
    display_title_box('TESTS COMPLETS'),
    display_message_ln(loading_full_tests),
    (consult('tests/regression_tests') ->
        run_all_tests
    ;   display_message_ln(error_loading_full_tests),
        display_message_ln(ensure_file_exists)),
    pause_and_return_menu.

process_choice('5') :-
    show_help,
    pause_and_return_menu.

process_choice('6') :-
    display_message_ln(goodbye),
    display_message_ln(thanks_playing),
    halt.

process_choice(_) :-
    nl,
    write('    '), draw_line(35, '-'), nl,
    write('    CHOIX INVALIDE'), nl,
    write('    '), draw_line(35, '-'), nl,
    display_message_ln(invalid_choice),
    pause_and_return_menu.

% =============================================================================
% SECTION 4 : JEU HUMAIN VS HUMAIN
% =============================================================================

% start_human_game
% Demarre une partie humain vs humain.
start_human_game :-
    display_title_box('NOUVELLE PARTIE'),
    display_legend,
    init_game_state(GameState),
    display_game_state(GameState),
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
        (write('Joueur '), translate_player(Player, PlayerFR), write(PlayerFR), write(' (tapez "aide")> '),
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
% Traitement des commandes pendant le jeu (francais et anglais supportes).
process_game_input(Input, _, _) :-
    member(Input, [quit, quitter, menu]),
    display_message_ln(thanks_playing),
    main_menu, !.

process_game_input(Input, _, _) :-
    member(Input, [exit, sortir, quitter_jeu]),
    display_message_ln(thanks_playing),
    display_message_ln(goodbye),
    halt.

process_game_input(Input, GameState, GameState) :-
    member(Input, [help, aide]),
    show_game_help, !.


% Clauses separees pour chaque type de commande - plus lisible
process_game_input(Input, GameState, NewGameState) :-
    atom(Input),
    atom_string(Input, InputStr),
    process_command_string(InputStr, GameState, NewGameState).

% process_command_string(+InputStr, +GameState, -NewGameState)
% Traite les commandes sous forme de chaines (francais et anglais).
process_command_string(InputStr, _, _) :-
    member(InputStr, ["exit", "sortir", "quitter_jeu"]),
    display_message_ln(thanks_playing),
    display_message_ln(goodbye),
    halt.

process_command_string(InputStr, _, _) :-
    member(InputStr, ["quit", "quitter", "menu"]),
    display_message_ln(thanks_playing),
    main_menu, !.

process_command_string(InputStr, GameState, GameState) :-
    member(InputStr, ["help", "aide"]),
    show_game_help.

process_command_string(InputStr, GameState, NewGameState) :-
    % Tentative de mouvement
    (parse_move_input(InputStr, FromRow, FromCol, ToRow, ToCol) ->
        attempt_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState)
    ;   display_invalid_input_error(InputStr),
        NewGameState = GameState
    ).

% display_invalid_input_error(+InputStr)
% Affiche une erreur de format d'entree.
display_invalid_input_error(InputStr) :-
    write('Format de mouvement invalide!'), nl,
    write('  Attendu: 4 caracteres comme "e2e4" (de e2 vers e4)'), nl,
    write('  Ou: exit, quit, help'), nl,
    write('  Votre entree: '), write(InputStr), nl.

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
% Affiche l'aide generale avec design moderne.
show_help :-
    nl, nl,
    % En-tete aide
    write('    '), draw_line(50, '='), nl,
    write('    |'), draw_spaces(48), write('|'), nl,
    write('    |'),
    center_text('AIDE - JEU D\'ECHECS PROLOG', 48),
    write('|'), nl,
    write('    |'), draw_spaces(48), write('|'), nl,
    write('    '), draw_line(50, '='), nl,
    nl,
    
    % Sections d'aide
    write('    PRINCIPE DU JEU'), nl,
    write('    '), draw_line(35, '-'), nl,
    write('    Jeu d\'echecs classique Humain vs Humain'), nl,
    nl,
    
    write('    FORMAT DES COUPS'), nl,
    write('    '), draw_line(35, '-'), nl,
    write('    Notation algebrique: e2e4 (de e2 vers e4)'), nl,
    write('    Colonnes: a-h  |  Rangees: 1-8'), nl,
    nl,
    
    write('    COMMANDES PENDANT LE JEU'), nl,
    write('    '), draw_line(35, '-'), nl,
    write('    aide        : Afficher cette aide'), nl,
    write('    quitter     : Retour au menu principal'), nl,
    write('    sortir      : Quitter le programme'), nl,
    nl,
    
    write('    PIECES (notation ASCII)'), nl,
    write('    '), draw_line(35, '-'), nl,
    write('    Blanches: P=Pion R=Tour N=Cavalier'), nl,
    write('              B=Fou  Q=Dame K=Roi'), nl,
    write('    Noires:   p=pion r=tour n=cavalier'), nl,
    write('              b=fou  q=dame k=roi'), nl,
    nl.

% show_game_help
% Affiche l'aide pendant le jeu avec design moderne.
show_game_help :-
    nl,
    write('    '), draw_line(40, '='), nl,
    write('    AIDE RAPIDE'), nl,
    write('    '), draw_line(40, '='), nl,
    
    write('    COUPS: e2e4 (de e2 vers e4)'), nl,
    write('    COMMANDES: aide, quitter, sortir'), nl,
    write('    PIECES: P/p=Pion R/r=Tour N/n=Cavalier'), nl,
    write('            B/b=Fou Q/q=Dame K/k=Roi'), nl,
    
    write('    '), draw_line(40, '='), nl, nl.

% =============================================================================
% FIN DU FICHIER INTERFACE.PL
% Derniere mise a jour : Aout 2025
% =============================================================================
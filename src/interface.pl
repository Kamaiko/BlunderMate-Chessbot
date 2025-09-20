% =============================================================================
% INTERFACE - Module interface utilisateur
% =============================================================================
%
% Interface francaise et boucle de jeu :
% - Menu principal et commandes
% - Boucle de jeu unifiee
% - Gestion tours humain/IA
%
% =============================================================================

:- [pieces].
:- [board].
:- [game].
:- [ai].

% =============================================================================
% SECTION 1 : ETAT DE JEU UNIFIE AVEC TYPES DE JOUEURS
% =============================================================================

% Structure d'etat de jeu unifie
% unified_game_state(Board, CurrentPlayer, MoveCount, GameStatus, CapturedPieces, PlayerTypes)
% PlayerTypes = player_types(WhitePlayerType, BlackPlayerType)  
% PlayerType = human | ai

% init_unified_game_state(+WhiteType, +BlackType, -UnifiedGameState)
% Initialise un etat de jeu unifie avec types de joueurs
init_unified_game_state(WhiteType, BlackType, UnifiedGameState) :-
    init_game_state(GameState),
    GameState = game_state(Board, CurrentPlayer, MoveCount, GameStatus, CapturedPieces),
    PlayerTypes = player_types(WhiteType, BlackType),
    UnifiedGameState = unified_game_state(Board, CurrentPlayer, MoveCount, GameStatus, CapturedPieces, PlayerTypes).

% extract_game_state(+UnifiedGameState, -GameState)
% Extrait l'etat de jeu standard depuis l'etat unifie
extract_game_state(unified_game_state(Board, CurrentPlayer, MoveCount, GameStatus, CapturedPieces, _), 
                   game_state(Board, CurrentPlayer, MoveCount, GameStatus, CapturedPieces)).

% update_unified_game_state(+UnifiedGameState, +NewGameState, -NewUnifiedGameState)
% Met a jour l'etat unifie avec un nouvel etat de jeu standard
update_unified_game_state(unified_game_state(_, _, _, _, _, PlayerTypes), 
                          game_state(Board, CurrentPlayer, MoveCount, GameStatus, CapturedPieces),
                          unified_game_state(Board, CurrentPlayer, MoveCount, GameStatus, CapturedPieces, PlayerTypes)).

% get_player_type(+UnifiedGameState, +Player, -PlayerType)
% Determine le type d'un joueur (human ou ai)
get_player_type(unified_game_state(_, _, _, _, _, player_types(WhiteType, _)), white, WhiteType).
get_player_type(unified_game_state(_, _, _, _, _, player_types(_, BlackType)), black, BlackType).

% =============================================================================
% SECTION 2 : MESSAGES FRANCAIS CENTRALISES
% =============================================================================

% Messages du menu
message(invalid_choice, 'Choix invalide. Veuillez entrer 1, 2, 3, 4, 5, ou 6.').
message(goodbye, 'Au revoir!').

% Fin du fichier interface.pl

% Messages de jeu
message(game_title_human_vs_human, '=== PARTIE D\'ECHECS HUMAIN VS HUMAIN ===').
message(legend_title, '=== LEGENDE DES PIECES ===').
message(white_pieces_legend, 'Pieces blanches (majuscules): P=Pion, R=Tour, N=Cavalier, B=Fou, Q=Dame, K=Roi').
message(black_pieces_legend, 'Pieces noires (minuscules): p=pion, r=tour, n=cavalier, b=fou, q=dame, k=roi').
message(move_format_help, 'Format des mouvements: e2e4. (de e2 vers e4, n\'oubliez pas le point!)').
message(move_instructions, 'Entrez les mouvements en notation algebrique (ex: e2e4)').
message(game_commands, 'Commandes: menu (retour menu), quitter, aide (help)').
message(current_player, 'Joueur actuel: ').
message(move_count, 'Nombre de coups: ').
message(game_finished, 'Partie terminee!').
message(illegal_move, 'Mouvement illegal!').
message(invalid_coordinates, 'Coordonnees invalides!').
message(no_piece_at_position, 'Aucune piece de votre couleur a cette position!').

% Messages d'echec et mat
message(in_check, 'ECHEC! Votre roi est en danger.').
message(checkmate_white_wins, 'ECHEC ET MAT! Les blancs gagnent!').
message(checkmate_black_wins, 'ECHEC ET MAT! Les noirs gagnent!').
message(stalemate_draw, 'MATCH NUL par pat.').

% Messages de test
message(running_tests, 'Execution de la suite de tests...').
message(loading_tests, 'Chargement de tests/tests.pl...').
message(tests_loaded_success, 'Tests charges avec succes. Execution de run_all_tests...').
message(error_loading_tests, 'Erreur: Impossible de charger tests/tests.pl').
message(ensure_file_exists, 'Veuillez vous assurer que le fichier existe et est accessible.').

% =============================================================================
% SECTION 3 : UTILITAIRES D'AFFICHAGE ET INTERFACE
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
    Length >= 0,
    forall(between(1, Length, _), write(Char)).

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
draw_spaces(Count) :-
    Count >= 0,
    forall(between(1, Count, _), write(' ')).

% display_title_box(+Title)
% Affiche un titre dans une boite Unicode professionnelle.
display_title_box(Title) :-
    nl, nl,
    write('    ╔════════════════════════════════════════════════════════════════╗'), nl,
    write('    ║                                                                ║'), nl,
    write('    ║'),
    center_text(Title, 64),
    write('║'), nl,
    write('    ║                                                                ║'), nl,
    write('    ║    ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━    ║'), nl,
    write('    ║                                                                ║'), nl,
    write('    ╚════════════════════════════════════════════════════════════════╝'), nl, nl.

% =============================================================================
% SECTION 4 : UTILITAIRES D'AFFICHAGE DES MESSAGES
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
% SECTION 5 : PAGE D'ACCUEIL BLUNDERBOT
% =============================================================================

%! display_welcome_screen is det.
%  Affiche la page d'accueil avec ASCII art Blunderbot
%  Design élégant avec pièces d'échecs Unicode si supporté
display_welcome_screen :-
    % Ne pas effacer l'écran pour éviter les problèmes shell
    nl, nl,
    write('    ╔════════════════════════════════════════════════════════════════╗'), nl,
    write('    ║                                                                ║'), nl,
    write('    ║  ██████╗ ██╗     ██╗   ██╗███╗   ██╗██████╗ ███████╗██████╗    ║'), nl,
    write('    ║  ██╔══██╗██║     ██║   ██║████╗  ██║██╔══██╗██╔════╝██╔══██╗   ║'), nl,
    write('    ║  ██████╔╝██║     ██║   ██║██╔██╗ ██║██║  ██║█████╗  ██████╔╝   ║'), nl,
    write('    ║  ██╔══██╗██║     ██║   ██║██║╚██╗██║██║  ██║██╔══╝  ██╔══██╗   ║'), nl,
    write('    ║  ██████╔╝███████╗╚██████╔╝██║ ╚████║██████╔╝███████╗██║  ██║   ║'), nl,
    write('    ║  ╚═════╝ ╚══════╝ ╚═════╝ ╚═╝  ╚═══╝╚═════╝ ╚══════╝╚═╝  ╚═╝   ║'), nl,
    write('    ║                                                                ║'), nl,
    write('    ║             ███╗   ███╗ █████╗ ████████╗███████╗               ║'), nl,
    write('    ║             ████╗ ████║██╔══██╗╚══██╔══╝██╔════╝               ║'), nl,
    write('    ║             ██╔████╔██║███████║   ██║   █████╗                 ║'), nl,
    write('    ║             ██║╚██╔╝██║██╔══██║   ██║   ██╔══╝                 ║'), nl,
    write('    ║             ██║ ╚═╝ ██║██║  ██║   ██║   ███████╗               ║'), nl,
    write('    ║             ╚═╝     ╚═╝╚═╝  ╚═╝   ╚═╝   ╚══════╝               ║'), nl,
    write('    ║                                                                ║'), nl,
    write('    ║                           Chessbot.IA                          ║'), nl,
    write('    ║                                                                ║'), nl,
    write('    ║                           Version 6.0                          ║'), nl,
    write('    ║                                                                ║'), nl,
    write('    ║                 Developpe par Patrick Patenaude                ║'), nl,
    write('    ║                    19 septembre 2025 - v6.0                    ║'), nl,
    write('    ║                                                                ║'), nl,
    write('    ╚════════════════════════════════════════════════════════════════╝'), nl,
    nl.

%! wait_for_keypress is det.
%  Attend qu'une touche soit pressée pour continuer
%  Version robuste qui accepte n'importe quelle touche
wait_for_keypress :-
    flush_output,
    write('               Appuyez sur une touche pour continuer...'),
    % Pause pour permettre la lecture, mais ne consomme pas l'input
    nl.


% =============================================================================
% SECTION 6 : MENU PRINCIPAL ET NAVIGATION
% =============================================================================

% start
% Point d'entree principal du programme.
% Affiche d'abord la page d'accueil, puis le menu principal
start :-
    display_welcome_screen,
    wait_for_keypress,
    main_menu.

% main_menu
% Affiche et gere le menu principal moderne.
main_menu :-
    display_modern_menu,
    read_menu_choice(Choice),
    process_choice(Choice).

% display_modern_menu
% Affiche le menu principal avec design professionnel matching l'ecran d'accueil.
display_modern_menu :-
    nl, nl,
    % Boite principale avec hauteur identique a l'ecran d'accueil (24 lignes)
    write('    ╔════════════════════════════════════════════════════════════════╗'), nl,
    write('    ║                                                                ║'), nl,
    write('    ║                          MENU PRINCIPAL                        ║'), nl,
    write('    ║                                                                ║'), nl,
    write('    ║        ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━       ║'), nl,
    write('    ║                       ┌──────────────────┐                     ║'), nl,
    write('    ║                    8  │ ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖  │                     ║'), nl,
    write('    ║                    7  │ ♙ ♙ ♙ ♙ ♙ ♙ ♙ ♙  │                     ║'), nl,
    write('    ║                    6  │ · · · · · · · ·  │                     ║'), nl,
    write('    ║                    5  │ · · · · · · · ·  │                     ║'), nl,
    write('    ║                    4  │ · · · · · · · ·  │                     ║'), nl,
    write('    ║                    3  │ · · · · · · · ·  │                     ║'), nl,
    write('    ║                    2  │ ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟  │                     ║'), nl,
    write('    ║                    1  │ ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜  │                     ║'), nl,
    write('    ║                       └──────────────────┘                     ║'), nl,
    write('    ║                         a b c d e f g h                        ║'), nl,
    write('    ║                                                                ║'), nl,
    write('    ║     ┌─────────────────────────────────────────────────────┐    ║'), nl,
    write('    ║     │  [1] Humain vs Humain     [4] Aide                  │    ║'), nl,
    write('    ║     │  [2] IA vs Humain         [5] A Propos              │    ║'), nl,
    write('    ║     │  [3] Suite de Tests       [6] Quitter               │    ║'), nl,
    write('    ║     └─────────────────────────────────────────────────────┘    ║'), nl,
    write('    ║                                                                ║'), nl,
    write('    ║                  Chessbot.IA - Menu Interactif                 ║'), nl,
    write('    ║                                                                ║'), nl,
    write('    ╚════════════════════════════════════════════════════════════════╝'), nl,
    nl,
    % Prompt d'entree centre en dehors de la boite
    write('Entrez votre choix [1-6] : ').

% read_menu_choice(-Choice)
% Lit le choix du menu de maniere robuste.
read_menu_choice(Choice) :-
    flush_output,
    catch(
        (read_line_to_codes(user_input, Codes),
         (Codes = [CharCode|_] ->
             char_code(Choice, CharCode)
         ;   Choice = '6'  % Défaut: quitter si ligne vide
         )
        ),
        _,
        Choice = '6'  % Défaut: quitter si erreur
    ),
    nl.

% pause_and_return_menu
% Pause elegant et retour au menu principal.
pause_and_return_menu :-
    nl,
    write('    Appuyez sur une touche pour continuer...'),
    catch(get_single_char(_), _, true),
    main_menu.

% process_choice(+Choice)
% Traitement des choix du menu principal.
% Accepte à la fois atoms et strings pour robustesse
process_choice('1') :- start_human_game.
process_choice(1) :- start_human_game.

process_choice('2') :- start_ai_game.
process_choice(2) :- start_ai_game.

process_choice('3') :-
    display_title_box('TESTS'),
    display_message_ln(loading_tests),
    (consult('tests/tests') ->
        (catch(run_all_tests, Error,
            (write('[TESTS ERROR: '), write(Error), write(']'), nl)
         ) -> true ; true)
    ;   display_message_ln(error_loading_tests),
        display_message_ln(ensure_file_exists)),
    pause_and_return_menu.
process_choice(3) :-
    display_title_box('TESTS'),
    display_message_ln(loading_tests),
    (consult('tests/tests') ->
        (catch(run_all_tests, Error,
            (write('[TESTS ERROR: '), write(Error), write(']'), nl)
         ) -> true ; true)
    ;   display_message_ln(error_loading_tests),
        display_message_ln(ensure_file_exists)),
    pause_and_return_menu.

process_choice('4') :- show_help, pause_and_return_menu.
process_choice(4) :- show_help, pause_and_return_menu.

process_choice('5') :- show_about, pause_and_return_menu.
process_choice(5) :- show_about, pause_and_return_menu.

process_choice('6') :- display_message_ln(goodbye), nl, halt.
process_choice(6) :- display_message_ln(goodbye), nl, halt.

process_choice(_) :-
    nl,
    write('CHOIX INVALIDE'), nl,
    pause_and_return_menu.

% =============================================================================
% SECTION 6 : MOTEUR DE JEU UNIFIE
% =============================================================================

% display_game_state_if_needed(+UnifiedGameState)
% Gere l'affichage conditionnel du plateau selon le mode de jeu
display_game_state_if_needed(UnifiedGameState) :-
    extract_game_state(UnifiedGameState, GameState),
    GameState = game_state(_, Player, MoveCount, _, _),
    (   should_display_board(UnifiedGameState, Player, MoveCount) ->
        % Utiliser la nouvelle interface seamless
        determine_game_mode(UnifiedGameState, GameMode),
        determine_last_move(GameState, LastMove),
        display_game_interface(GameState, GameMode, LastMove)
    ;   true  % Pas d'affichage nécessaire
    ).

% should_display_board(+UnifiedGameState, +Player, +MoveCount)
% Logique de décision d'affichage du plateau - SEMPRE AFFICHER LA NOUVELLE INTERFACE
should_display_board(UnifiedGameState, Player, MoveCount) :-
    get_player_type(UnifiedGameState, Player, PlayerType),
    opposite_player(Player, OtherPlayer),
    get_player_type(UnifiedGameState, OtherPlayer, OtherPlayerType),
    (   (PlayerType = human, OtherPlayerType = human) ->
        true  % Humain vs Humain : toujours afficher
    ;   (PlayerType = human ; OtherPlayerType = human) ->
        true  % IA vs Humain : toujours afficher quand un humain joue
    ;   fail  % Autres cas (IA vs IA)
    ).

% check_and_display_warnings(+UnifiedGameState)
% Vérifie et affiche les avertissements (échec, etc.)
check_and_display_warnings(UnifiedGameState) :-
    extract_game_state(UnifiedGameState, GameState),
    GameState = game_state(_, Player, _, _, _),
    (   is_in_check(GameState, Player) ->
        display_message_ln(in_check)
    ;   true
    ).

% process_game_turn(+UnifiedGameState)
% Traitement du tour selon le statut du jeu
process_game_turn(UnifiedGameState) :-
    extract_game_state(UnifiedGameState, GameState),
    GameState = game_state(_, Player, _, Status, _),
    (   Status = active ->
        handle_active_game(UnifiedGameState, Player)
    ;   Status = checkmate ->
        handle_checkmate(Player)
    ;   Status = stalemate ->
        handle_stalemate()
    ;   handle_other_game_end(Status)
    ).

% handle_active_game(+UnifiedGameState, +Player)
% Gère un tour de jeu actif
handle_active_game(UnifiedGameState, Player) :-
    get_player_type(UnifiedGameState, Player, PlayerType),
    handle_player_turn(UnifiedGameState, Player, PlayerType, NewUnifiedGameState),
    unified_game_loop(NewUnifiedGameState).

% handle_checkmate(+Player)
% Gère la fin de partie par échec et mat
handle_checkmate(Player) :-
    opposite_player(Player, Winner),
    announce_checkmate_winner(Winner).

% handle_stalemate
% Gère la fin de partie par pat
handle_stalemate :-
    handle_game_end_with_message(stalemate_draw).

% handle_other_game_end(+Status)
% Gère les autres fins de partie
handle_other_game_end(_Status) :-
    handle_game_end_with_message(game_finished).

% handle_game_end_with_message(+Message)
% Helper pour fins de partie avec message et retour menu
handle_game_end_with_message(Message) :-
    display_message_ln(Message),
    pause_and_return_menu.

% unified_game_loop(+UnifiedGameState)
% Boucle de jeu principale gerant tous les types de joueurs (version refactorisée)
unified_game_loop(UnifiedGameState) :-
    display_game_state_if_needed(UnifiedGameState),
    check_and_display_warnings(UnifiedGameState),
    process_game_turn(UnifiedGameState).

% handle_player_turn(+UnifiedGameState, +Player, +PlayerType, -NewUnifiedGameState)
% Gere le tour d'un joueur selon son type (humain ou IA)
handle_player_turn(UnifiedGameState, _Player, human, NewUnifiedGameState) :-
    % Tour d'un joueur humain - utilise l'interface unifiee
    write('Entrez votre coup ou \'aide\' : '),
    read_player_input(Input),
    extract_game_state(UnifiedGameState, GameState),
    process_game_input(Input, GameState, NewGameState),
    update_unified_game_state(UnifiedGameState, NewGameState, NewUnifiedGameState).

handle_player_turn(UnifiedGameState, Player, ai, NewUnifiedGameState) :-
    % Tour de l'IA - genere un coup automatiquement
    nl, nl,
    write('IA reflechit ('), translate_player(Player, PlayerFR), write(PlayerFR), write(', negamax alpha-beta)...'), nl,
    extract_game_state(UnifiedGameState, GameState),
    get_time(StartTime),
    choose_ai_move(GameState, AIMove),
    get_time(EndTime),
    Duration is EndTime - StartTime,
    
    (   AIMove = [] ->
        write('IA ne peut pas jouer - Fin de partie'), nl,
        NewUnifiedGameState = UnifiedGameState
    ;   AIMove = [FromRow, FromCol, ToRow, ToCol],
        coordinates_to_algebraic(FromRow, FromCol, ToRow, ToCol, MoveStr),
        make_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState),
        update_unified_game_state(UnifiedGameState, NewGameState, NewUnifiedGameState),
        % Afficher le board après le coup de l'IA en mode IA vs Humain
        opposite_player(Player, OpponentPlayer),
        get_player_type(UnifiedGameState, OpponentPlayer, OpponentType),
        (   OpponentType = human ->
            format('IA joue : ~w (~2f sec)~n', [MoveStr, Duration])
            % L'affichage sera géré par display_game_state_if_needed dans unified_game_loop
        ;   true
        )
    ).

% game_loop(+GameState) 
% Boucle de jeu pour compatibilite retrograde.
game_loop(GameState) :-
    init_unified_game_state(human, human, UnifiedGameState),
    update_unified_game_state(UnifiedGameState, GameState, UpdatedUnifiedGameState),
    unified_game_loop(UpdatedUnifiedGameState).

% =============================================================================
% SECTION 7 : MODES DE JEU - HUMAIN VS HUMAIN
% =============================================================================

% start_human_game/0
% Demarre une partie humain vs humain.
start_human_game :-
    init_unified_game_state(human, human, UnifiedGameState),
    unified_game_loop(UnifiedGameState).

% announce_checkmate_winner(+Winner)
% Annonce le gagnant d'une partie par mat.
announce_checkmate_winner(white) :-
    nl, nl,
    display_title_box('ECHEC ET MAT'),
    display_message_ln(checkmate_white_wins),
    nl,
    write('    Felicitations aux blancs!'), nl,
    pause_and_return_menu.
    
announce_checkmate_winner(black) :-
    nl, nl,
    display_title_box('ECHEC ET MAT'),
    display_message_ln(checkmate_black_wins),
    nl,
    write('    Felicitations aux noirs!'), nl,
    pause_and_return_menu.

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
% SECTION 8 : MODES DE JEU - IA VS HUMAIN
% =============================================================================

% start_ai_game/0
% Lance une partie IA vs Humain.
start_ai_game :-
    init_unified_game_state(human, ai, UnifiedGameState),
    unified_game_loop(UnifiedGameState).

% =============================================================================
% SECTION 9 : TRAITEMENT DES COMMANDES DE JEU
% =============================================================================

% process_game_input(+Input, +GameState, -NewGameState)
% Traitement des commandes pendant le jeu - version simplifiee
process_game_input(Input, GameState, NewGameState) :-
    normalize_input(Input, NormalizedInput),
    dispatch_game_command(NormalizedInput, GameState, NewGameState).

% normalize_input - Normalise les entrees (atom/string)
normalize_input(Input, Input) :- string(Input), !.
normalize_input(Input, InputStr) :- atom(Input), atom_string(Input, InputStr).
normalize_input(Input, Input).  % Autres types inchanges

% dispatch_game_command - Dispatch unifie des commandes
dispatch_game_command(Input, _, _) :-
    is_exit_command(Input),
    handle_exit_command(Input).
dispatch_game_command(Input, GameState, GameState) :-
    is_help_command(Input),
    show_game_help(GameState), !.
dispatch_game_command(Input, GameState, NewGameState) :-
    process_move_command(Input, GameState, NewGameState).

% Utilitaires de classification des commandes
is_exit_command(Input) :- member(Input, ["quitter", "menu", "quitter_jeu", quitter, menu, quitter_jeu]).
is_help_command(Input) :- member(Input, ["aide", aide]).

% handle_exit_command - Gestion unifiee des sorties
handle_exit_command(Input) :-
    member(Input, ["menu", menu]),
    display_message_ln(goodbye),
    main_menu, !.
handle_exit_command(Input) :-
    member(Input, ["quitter", "quitter_jeu", quitter, quitter_jeu]),
    display_message_ln(goodbye),
    halt, !.
handle_exit_command(_) :-
    display_message_ln(goodbye),
    halt.

% process_move_command - Traitement des mouvements
process_move_command(Input, GameState, NewGameState) :-
    (parse_move_input(Input, FromRow, FromCol, ToRow, ToCol) ->
        attempt_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState)
    ;   display_invalid_input_error(Input),
        NewGameState = GameState
    ).

% display_invalid_input_error(+InputStr)
% Affiche une erreur de format d'entree.
display_invalid_input_error(InputStr) :-
    write('Format de mouvement invalide!'), nl,
    write('  Attendu: 4 caracteres comme "e2e4" (de e2 vers e4)'), nl,
    write('  Ou: quitter, aide'), nl,
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
                NewGameState = TempGameState
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
% SECTION 10 : AFFICHAGE DE L'AIDE
% =============================================================================

% show_help
% Affiche l'aide generale depuis le menu principal (sans pause).
show_help :-
    show_unified_help_no_pause.

% show_game_help(+GameState)
% Affiche l'aide pendant le jeu puis retourne au plateau.
show_game_help(GameState) :-
    show_unified_help(GameState).

% show_unified_help_no_pause
% Affiche l'aide sans pause (pour menu principal).
show_unified_help_no_pause :-
    display_help_content.

% show_unified_help(+GameStateOrNone)
% Fonction d'aide unifiee pour jeu avec pause et affichage plateau.
show_unified_help(GameStateOrNone) :-
    display_help_content,

    % Pause interactive
    write('    Appuyez sur une touche pour continuer...'),
    catch(get_single_char(_), _, true),
    nl,

    % L'affichage du board sera géré par la nouvelle interface
    true.

% display_help_content
% Affiche le contenu de l'aide (partie commune).
display_help_content :-
    nl, nl,
    % Fenetre Aide unifiee avec dimensions identiques aux autres fenetres
    write('    ╔════════════════════════════════════════════════════════════════╗'), nl,
    write('    ║                                                                ║'), nl,
    write('    ║                              AIDE                              ║'), nl,
    write('    ║                                                                ║'), nl,
    write('    ║    ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━    ║'), nl,
    write('    ║                                                                ║'), nl,
    write('    ║              ╭──────  Guide d\'utilisation  ──────╮             ║'), nl,
    write('    ║    ╔══════════════════════════════════════════════════════╗    ║'), nl,
    write('    ║    ║                                                      ║    ║'), nl,
    write('    ║    ║  ▸ FORMAT DES COUPS                                  ║    ║'), nl,
    write('    ║    ║    Notation algebrique: e2e4 (de e2 vers e4)         ║    ║'), nl,
    write('    ║    ║    Colonnes: a-h  |  Rangees: 1-8                    ║    ║'), nl,
    write('    ║    ║                                                      ║    ║'), nl,
    write('    ║    ║  ▸ COMMANDES PENDANT LE JEU                          ║    ║'), nl,
    write('    ║    ║    aide    : Afficher cette aide                     ║    ║'), nl,
    write('    ║    ║    menu    : Retour au menu principal                ║    ║'), nl,
    write('    ║    ║    quitter : Quitter le programme                    ║    ║'), nl,
    write('    ║    ║                                                      ║    ║'), nl,
    write('    ║    ║  ▸ PIECES: P/p=Pion  R/r=Tour  N/n=Cavalier          ║    ║'), nl,
    write('    ║    ║            B/b=Fou   Q/q=Dame  K/k=Roi               ║    ║'), nl,
    write('    ║    ║                                                      ║    ║'), nl,
    write('    ║    ║  ▸ PIECES & SYMBOLES                                 ║    ║'), nl,
    write('    ║    ║    Blancs: ♜ ♞ ♝ ♛ ♚ ♟   (minuscules p/r/n/b/q/k)  ║    ║'), nl,
    write('    ║    ║    Noirs:  ♖ ♘ ♗ ♕ ♔ ♙   (MAJUSCULES P/R/N/B/Q/K)  ║    ║'), nl,
    write('    ║    ║                                                      ║    ║'), nl,
    write('    ║    ║  ▸ RACCOURCIS: [Tab] historique  [↑↓] navigation    ║    ║'), nl,
    write('    ║    ║                                                      ║    ║'), nl,
    write('    ║    ╚══════════════════════════════════════════════════════╝    ║'), nl,
    write('    ║                                                                ║'), nl,
    write('    ╚════════════════════════════════════════════════════════════════╝'), nl,
    nl.

% show_about
% Affiche les informations "A Propos" du projet avec design professionnel.
show_about :-
    nl, nl,
    % Fenetre A Propos avec dimensions identiques aux autres fenetres
    write('    ╔════════════════════════════════════════════════════════════════╗'), nl,
    write('    ║                                                                ║'), nl,
    write('    ║                            A PROPOS                            ║'), nl,
    write('    ║                                                                ║'), nl,
    write('    ║    ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━    ║'), nl,
    write('    ║                                                                ║'), nl,
    write('    ║             ╭─────  Chessbot ecrit en Prolog  ────╮            ║'), nl,
    write('    ║               IA utilisant Negamax avec Alpha-Beta             ║'), nl,
    write('    ║                                                                ║'), nl,
    write('    ║    ╔══════════════════════════════════════════════════════╗    ║'), nl,
    write('    ║    ║                                                      ║    ║'), nl,
    write('    ║    ║  ▸ Developpe par Patrick Patenaude                   ║    ║'), nl,
    write('    ║    ║  ▸ VERSION: 6.0 Unicode - Septembre 2025             ║    ║'), nl,
    write('    ║    ║  ▸ COURS: IFT-2003 Intelligence Artificielle         ║    ║'), nl,
    write('    ║    ║  ▸ INSTITUTION: Universite Laval                     ║    ║'), nl,
    write('    ║    ║                                                      ║    ║'), nl,
    write('    ║    ║  ──────────────────────────────────────────────────  ║    ║'), nl,
    write('    ║    ║                                                      ║    ║'), nl,
    write('    ║    ║  Performance IA:                                     ║    ║'), nl,
    write('    ║    ║   • Profondeur 2 avec elagage alpha-beta            ║    ║'), nl,
    write('    ║    ║   • Temps de reponse < 0.1s par coup                ║    ║'), nl,
    write('    ║    ║   • Evaluation PSQT + materiel + defense             ║    ║'), nl,
    write('    ║    ║                                                      ║    ║'), nl,
    write('    ║    ╚══════════════════════════════════════════════════════╝    ║'), nl,
    write('    ║                                                                ║'), nl,
    write('    ╚════════════════════════════════════════════════════════════════╝'), nl,
    nl.

%! display_game_interface(+GameState, +GameMode, +LastMove) is det.
%  Affiche l'interface de jeu avec design seamless du menu principal
%  Boite identique au menu principal avec informations de jeu
display_game_interface(GameState, GameMode, LastMove) :-
    GameState = game_state(Board, Player, MoveCount, _, CapturedPieces),
    nl, nl,
    % Boite principale avec dimensions identiques au menu principal (24 lignes)
    write('    ╔════════════════════════════════════════════════════════════════╗'), nl,
    write('    ║                                                                ║'), nl,
    % Centrer le titre dynamiquement
    atom_length(GameMode, TitleLen),
    PaddingTotal is 64 - TitleLen,  % 64 = largeur interne
    PaddingLeft is PaddingTotal // 2,
    PaddingRight is PaddingTotal - PaddingLeft,
    format('    ║~*c~w~*c║~n', [PaddingLeft, 32, GameMode, PaddingRight, 32]),
    write('    ║        ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━       ║'), nl,
    write('    ║                                                                ║'), nl,

    % Board actuel (meme position que menu principal)
    display_board_in_box(Board),

    write('    ║                         a b c d e f g h                        ║'), nl,
    write('    ║                                                                ║'), nl,

    % Boite d'informations (remplace le menu 1-6)
    write('    ║     ┌─────────────────────────────────────────────────────┐    ║'), nl,
    format_game_info_box(Player, MoveCount, LastMove, GameState, CapturedPieces),
    write('    ║     └─────────────────────────────────────────────────────┘    ║'), nl,
    write('    ║                                                                ║'), nl,
    write('    ║                                                                ║'), nl,
    write('    ╚════════════════════════════════════════════════════════════════╝'), nl,
    nl.

%! draw_game_box_header(+GameMode) is det.
%  Dessine l'en-tete de la boite de jeu avec titre centre
draw_game_box_header(GameMode) :-
    write('    ╔════════════════════════════════════════════════════════════════╗'), nl,
    write('    ║                                                                ║'), nl,
    draw_centered_title(GameMode),
    write('    ║        ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━        ║'), nl,
    write('    ║                                                                ║'), nl.

%! draw_centered_title(+Title) is det.
%  Centre un titre dans une ligne de 64 caracteres
draw_centered_title(Title) :-
    atom_length(Title, TitleLen),
    PaddingTotal is 64 - TitleLen,
    PaddingLeft is PaddingTotal // 2,
    PaddingRight is PaddingTotal - PaddingLeft,
    format('    ║~*c~w~*c║~n', [PaddingLeft, 32, Title, PaddingRight, 32]).

%! draw_board_coordinates is det.
%  Affiche les coordonnees sous le plateau
draw_board_coordinates :-
    write('    ║                         a b c d e f g h                        ║'), nl,
    write('    ║                                                                ║'), nl.

%! draw_info_section(+Player, +MoveCount, +LastMove, +GameState, +CapturedPieces) is det.
%  Dessine la section d'informations de jeu
draw_info_section(Player, MoveCount, LastMove, GameState, CapturedPieces) :-
    write('    ║     ┌─────────────────────────────────────────────────────┐    ║'), nl,
    format_game_info_box(Player, MoveCount, LastMove, GameState, CapturedPieces),
    write('    ║     └─────────────────────────────────────────────────────┘    ║'), nl.

%! draw_game_box_footer is det.
%  Dessine le pied de la boite de jeu
draw_game_box_footer :-
    write('    ║                                                                ║'), nl,
    write('    ║                                                                ║'), nl,
    write('    ╚════════════════════════════════════════════════════════════════╝'), nl.

%! display_board_in_box(+Board) is det.
%  Affiche le board aux memes coordonnees que le menu principal
display_board_in_box(Board) :-
    write('    ║                       ┌──────────────────┐                     ║'), nl,
    display_board_rows_in_box(Board, 8),
    write('    ║                       └──────────────────┘                     ║'), nl.

%! display_board_rows_in_box(+Board, +RowNum) is det.
%  Affiche les rangees du board dans la boite (accès direct liste)
display_board_rows_in_box([], _) :- !.
display_board_rows_in_box([CurrentRow|RestRows], RowNum) :-
    write('    ║                    '), write(RowNum), write('  │ '),
    display_row_pieces_in_box(CurrentRow),
    write(' │                     ║'), nl,
    NextRowNum is RowNum - 1,
    display_board_rows_in_box(RestRows, NextRowNum).

%! display_row_pieces_in_box(+RowPieces) is det.
%  Affiche les pieces d'une rangee avec espacement
display_row_pieces_in_box([]).
display_row_pieces_in_box([Piece|RestPieces]) :-
    (   Piece = ' ' ->
        write('· ')
    ;   display_piece_unicode(Piece), write(' ')
    ),
    display_row_pieces_in_box(RestPieces).

%! format_game_info_box(+Player, +MoveCount, +LastMove, +GameState, +CapturedPieces) is det.
%  Formate la boite d'informations avec alignement parfait (58 chars internes)
format_game_info_box(Player, MoveCount, LastMove, GameState, CapturedPieces) :-
    % Ligne 1: Joueur actuel | Tour
    translate_player(Player, PlayerFR),
    format_info_line(['Joueur actuel: ', PlayerFR], ['Tour: ', MoveCount]),

    % Ligne 2: Score | Dernier coup
    get_position_score(GameState, Score),
    format_info_line(['Score: ', Score], ['Dernier coup: ', LastMove]),

    % Ligne 3: Pieces capturees (ligne complete)
    format_captures_line_clean(CapturedPieces).

%! format_info_line(+LeftParts, +RightParts) is det.
%  Formate une ligne d'info avec 2 colonnes alignees (29 chars chacune)
format_info_line(LeftParts, RightParts) :-
    % Construire le texte de gauche et droite
    build_text_from_parts(LeftParts, LeftText),
    build_text_from_parts(RightParts, RightText),

    % Aligner chaque colonne sur 29 caracteres
    format_column(LeftText, 29, LeftFormatted),
    format_column(RightText, 29, RightFormatted),

    % Afficher la ligne complete
    format('    ║     │  ~w│~w│    ║~n', [LeftFormatted, RightFormatted]).

%! format_captures_line_clean(+CapturedPieces) is det.
%  Affiche les pieces capturees sur une ligne complete centree
format_captures_line_clean(CapturedPieces) :-
    (   CapturedPieces = [] ->
        CapturesText = 'Pieces capturees: Aucune'
    ;   separate_captures(CapturedPieces, WhiteCaptures, BlackCaptures),
        build_captures_text(WhiteCaptures, BlackCaptures, CapturesText)
    ),
    format_column(CapturesText, 58, CapturesFormatted),
    format('    ║     │  ~w│    ║~n', [CapturesFormatted]).

%! build_text_from_parts(+Parts, -Text) is det.
%  Construit un texte a partir d'une liste de parties
build_text_from_parts(Parts, Text) :-
    maplist(to_atom, Parts, Atoms),
    atomic_list_concat(Atoms, '', Text).

%! to_atom(+Term, -Atom) is det.
%  Convertit n'importe quel terme en atom
to_atom(Term, Atom) :-
    (   atom(Term) -> Atom = Term
    ;   number(Term) -> atom_number(Atom, Term)
    ;   term_to_atom(Term, Atom)
    ).

%! format_column(+Text, +Width, -FormattedText) is det.
%  Formate un texte pour occuper exactement Width caracteres (avec padding droite)
format_column(Text, Width, FormattedText) :-
    atom_length(Text, Len),
    (   Len >= Width ->
        sub_atom(Text, 0, Width, _, FormattedText)  % Tronquer si trop long
    ;   Padding is Width - Len,
        format(atom(FormattedText), '~w~*c', [Text, Padding, 32])  % Pad avec espaces
    ).

%! build_captures_text(+WhiteCaptures, +BlackCaptures, -Text) is det.
%  Construit le texte des captures
build_captures_text(WhiteCaptures, BlackCaptures, Text) :-
    convert_pieces_to_text(WhiteCaptures, WhiteText),
    convert_pieces_to_text(BlackCaptures, BlackText),
    format(atom(Text), 'Captures: Blancs ~w | Noirs ~w', [WhiteText, BlackText]).

%! separate_captures(+AllCaptures, -WhiteCaptures, -BlackCaptures) is det.
%  Separe les pieces capturees par couleur (simple et robuste)
separate_captures([], [], []).
separate_captures([Piece|Rest], [Piece|WhiteRest], BlackRest) :-
    % Piece blanche (majuscule) - verification securisee
    atom(Piece),
    atom_codes(Piece, [Code]),
    Code >= 65, Code =< 90, !,
    separate_captures(Rest, WhiteRest, BlackRest).
separate_captures([Piece|Rest], WhiteRest, [Piece|BlackRest]) :-
    % Piece noire (minuscule) ou autre
    separate_captures(Rest, WhiteRest, BlackRest).

%! format_captures_display(+WhiteCaptures, +BlackCaptures) is det.
%  Affiche les captures dans la boite
format_captures_display(WhiteCaptures, BlackCaptures) :-
    convert_pieces_to_text(WhiteCaptures, WhiteText),
    convert_pieces_to_text(BlackCaptures, BlackText),
    format('    ║     │  Captures: Blancs ~w | Noirs ~w               │    ║~n',
           [WhiteText, BlackText]).

%! convert_pieces_to_text(+Pieces, -Text) is det.
%  Convertit une liste de pieces en texte simple (robuste)
convert_pieces_to_text([], '').
convert_pieces_to_text(Pieces, Text) :-
    % Version plus robuste qui gere les listes vides et les atoms
    (   Pieces = [] ->
        Text = ''
    ;   catch(atomic_list_concat(Pieces, '', Text), _, Text = '')
    ).

%! get_position_score(+GameState, -Score) is det.
%  Obtient le score de position (unifie la logique)
get_position_score(GameState, Score) :-
    (   catch(evaluate_pure_reference(GameState, white, Score), _, fail) ->
        true
    ;   Score = 0
    ).

%! get_piece_unicode(+Piece, -Unicode) is det.
%  Mapping piece -> Unicode (necessaire pour l'affichage)
get_piece_unicode('P', '♟'). get_piece_unicode('p', '♙').
get_piece_unicode('R', '♜'). get_piece_unicode('r', '♖').
get_piece_unicode('N', '♞'). get_piece_unicode('n', '♘').
get_piece_unicode('B', '♝'). get_piece_unicode('b', '♗').
get_piece_unicode('Q', '♛'). get_piece_unicode('q', '♕').
get_piece_unicode('K', '♚'). get_piece_unicode('k', '♔').

%! display_piece_unicode(+Piece) is det.
%  Affiche une piece en unicode (utilise le mapping unifie)
display_piece_unicode(Piece) :-
    (   get_piece_unicode(Piece, Unicode) ->
        write(Unicode)
    ;   write('?')  % Piece inconnue
    ).

%! determine_game_mode(+UnifiedGameState, -GameMode) is det.
%  Determine le mode de jeu pour affichage
determine_game_mode(UnifiedGameState, GameMode) :-
    get_player_type(UnifiedGameState, white, WhiteType),
    get_player_type(UnifiedGameState, black, BlackType),
    (   WhiteType = human, BlackType = human ->
        GameMode = 'HUMAIN VS HUMAIN'
    ;   WhiteType = human, BlackType = ai ->
        GameMode = 'IA VS HUMAIN'
    ;   WhiteType = ai, BlackType = human ->
        GameMode = 'IA VS HUMAIN'
    ;   GameMode = 'MODE INCONNU'
    ).

%! determine_last_move(+GameState, -LastMove) is det.
%  Determine le dernier coup joue pour affichage
determine_last_move(GameState, LastMove) :-
    GameState = game_state(_, _, MoveCount, _, _),
    (   MoveCount =< 0 ->
        LastMove = ''  % Rien au premier coup
    ;   % Pour l'instant, afficher un placeholder pour les coups suivants
        % TODO: Implementer tracking du dernier coup
        LastMove = '-'
    ).


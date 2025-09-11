% =============================================================================
% CHESS INTERFACE - INTERFACE UTILISATEUR
% =============================================================================
% 
% Module interface utilisateur :
% - Menu principal avec design uniforme
% - Traitement commandes unifie et extensible
% - Messages francais centralises
% - Utilitaires d'affichage optimises
%
% Auteur : Patrick Patenaude
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
% SECTION 5 : MENU PRINCIPAL ET NAVIGATION
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
    nl,
    
    % Sections organisees du menu
    write('--- MODES DE JEU ---'), nl,
    write('1. Humain vs Humain'), nl,
    write('2. Mode IA vs Humain'), nl,
    nl,
    write('--- OUTILS ---'), nl,
    write('3. Tests'), nl,
    nl,
    write('--- INFORMATIONS ---'), nl,
    write('4. Aide'), nl,
    write('5. A Propos'), nl,
    nl,
    write('6. Quitter'), nl,
    nl,
    
    % Ligne de separation
    write('    '), draw_line(35, '-'), nl,
    write('Entrez votre choix (1-6): ').

% read_menu_choice(-Choice)
% Lit le choix du menu de maniere robuste.
read_menu_choice(Choice) :-
    catch(get_single_char(CharCode), _, CharCode = -1),
    (CharCode >= 0 ->
        char_code(Choice, CharCode)
    ;   Choice = '6'  % Défaut: quitter si EOF ou erreur
    ),
    nl, nl.

% pause_and_return_menu
% Pause elegant et retour au menu principal.
pause_and_return_menu :-
    nl,
    write('    '), draw_line(35, '-'), nl,
    write('    Appuyez sur une touche pour continuer...'),
    catch(get_single_char(_), _, true),
    main_menu.

% process_choice(+Choice)
% Traitement des choix du menu principal.
process_choice('1') :-
    start_human_game.

process_choice('2') :-
    start_ai_game.

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

process_choice('4') :-
    show_help,
    pause_and_return_menu.

process_choice('5') :-
    show_about,
    pause_and_return_menu.

process_choice('6') :-
    display_message_ln(goodbye),
    halt.

process_choice(_) :-
    nl,
    write('CHOIX INVALIDE'), nl,
    pause_and_return_menu.

% =============================================================================
% SECTION 6 : MOTEUR DE JEU UNIFIE
% =============================================================================

% unified_game_loop(+UnifiedGameState)
% Boucle de jeu principale gerant tous les types de joueurs.
unified_game_loop(UnifiedGameState) :-
    extract_game_state(UnifiedGameState, GameState),
    GameState = game_state(_, Player, _, Status, _),
    
    % Afficher l'etat du jeu en début de tour
    get_player_type(UnifiedGameState, Player, PlayerType),
    opposite_player(Player, OtherPlayer),
    get_player_type(UnifiedGameState, OtherPlayer, OtherPlayerType),
    
    % Mode Humain vs Humain : toujours afficher
    % Mode IA vs Humain : afficher seulement au tout premier coup (MoveCount = 0)
    GameState = game_state(_, _, MoveCount, _, _),
    (   (PlayerType = human, OtherPlayerType = human) ->
        display_game_state(GameState)  % Humain vs Humain
    ;   (PlayerType = human, MoveCount = 0) ->  
        display_game_state(GameState)  % Premier coup de la partie seulement
    ;   true  % Autres cas : pas d'affichage (sera fait après coup IA)
    ),
    
    (   Status = active ->
        % Verifier si le joueur est en echec
        (is_in_check(GameState, Player) ->
            display_message_ln(in_check)
        ;   true),
        
        % Traitement selon le type de joueur
        get_player_type(UnifiedGameState, Player, PlayerType),
        handle_player_turn(UnifiedGameState, Player, PlayerType, NewUnifiedGameState),
        unified_game_loop(NewUnifiedGameState)
        
    ;   Status = checkmate ->
        % Annoncer le gagnant (celui qui a donne mat)
        opposite_player(Player, Winner),
        announce_checkmate_winner(Winner)
    ;   Status = stalemate ->
        display_message_ln(stalemate_draw),
        pause_and_return_menu
    ;   % Autre statut
        display_message_ln(game_finished),
        pause_and_return_menu
    ).

% handle_player_turn(+UnifiedGameState, +Player, +PlayerType, -NewUnifiedGameState)
% Gere le tour d'un joueur selon son type (humain ou IA)
handle_player_turn(UnifiedGameState, _Player, human, NewUnifiedGameState) :-
    % Tour d'un joueur humain - utilise l'interface unifiee
    write('    '), draw_line(35, '-'), nl,
    write('Entrez votre coup (ex: e2e4) ou \'aide\': '),
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
            format('IA joue : ~w (~2f sec)~n', [MoveStr, Duration]),
            nl, display_game_state(NewGameState)
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
    display_title_box('NOUVELLE PARTIE'),
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
    display_title_box('MODE IA vs HUMAIN'),
    write('Vous jouez les Blancs, l\'IA joue les Noirs.'), nl, nl,
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
    write('    FORMAT DES COUPS'), nl,
    write('    '), draw_line(35, '-'), nl,
    write('    Notation algebrique: e2e4 (de e2 vers e4)'), nl,
    write('    Colonnes: a-h  |  Rangees: 1-8'), nl,
    nl,
    
    write('    COMMANDES PENDANT LE JEU'), nl,
    write('    '), draw_line(35, '-'), nl,
    write('    aide        : Afficher cette aide'), nl,
    write('    menu        : Retour au menu principal'), nl,
    write('    quitter     : Quitter le programme'), nl,
    nl.

% show_game_help(+GameState)
% Affiche l'aide pendant le jeu avec design moderne et pause interactive, puis le board.
show_game_help(GameState) :-
    nl,
    write('    '), draw_line(40, '='), nl,
    write('    AIDE RAPIDE'), nl,
    write('    '), draw_line(40, '='), nl,
    write('    COUPS: e2e4 (de e2 vers e4)'), nl,
    write('    COMMANDES: menu, quitter'), nl,
    write('    PIECES: P/p=Pion R/r=Tour N/n=Cavalier'), nl,
    write('            B/b=Fou Q/q=Dame K/k=Roi'), nl,
    write('    '), draw_line(40, '='), nl,
    nl,
    
    % Pause interactive
    write('    Appuyez sur une touche pour continuer...'),
    catch(get_single_char(_), _, true),
    nl,
    
    % Afficher le board state apres l'aide
    display_game_state(GameState).

% show_about
% Affiche les informations "A Propos" du projet.
show_about :-
    nl, nl,
    % En-tete A Propos
    draw_line(50, '='), nl,
    write('|'), draw_spaces(48), write('|'), nl,
    write('|'),
    center_text('A PROPOS', 48),
    write('|'), nl,
    write('|'), draw_spaces(48), write('|'), nl,
    draw_line(50, '='), nl,
    nl,
    
    % Informations du projet
    write('JEU D\'ECHECS PROLOG'), nl,
    write('Projet universitaire IFT-2003'), nl,
    write('Universite Laval'), nl,
    nl,
    write('Auteur: Patrick Patenaude'), nl,
    write('Date: Octobre 2025'), nl,
    nl,
    write('CARACTERISTIQUES TECHNIQUES:'), nl,
    write('- Moteur d\'echecs complet en Prolog'), nl,
    write('- IA Negamax + Alpha-Beta (profondeur 2)'), nl,
    write('- Architecture modulaire 6 couches'), nl,
    write('- Interface francaise professionnelle'), nl,
    write('- Suite de tests automatises'), nl,
    nl.


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
% 
% RESPONSABILITÉS :
% - Menu principal et navigation
% - Boucle de jeu Humain vs Humain
% - Traitement des entrées utilisateur
% - Affichage de l'état du jeu
% - Interface avec les modules de test externes
% =============================================================================

:- [game_logic].
:- [board_smart].

% =============================================================================
% SECTION 1 : MENU PRINCIPAL ET NAVIGATION
% =============================================================================

% Point d'entrée principal
start :- main_menu.

% Affichage et gestion du menu principal
main_menu :-
    write('======================'), nl,
    write('   JEU D''ECHECS PROLOG'), nl,
    write('======================'), nl, nl,
    write('Choisissez une option:'), nl,
    write('1 - Commencer une partie Humain vs Humain'), nl,
    write('2 - Commencer une partie Humain vs Bot (Bientot disponible)'), nl,
    write('3 - Executer les tests rapides (externes)'), nl,
    write('4 - Executer la suite complete de tests (externe)'), nl,
    write('5 - Afficher l''aide'), nl,
    write('6 - Quitter'), nl, nl,
    write('Entrez votre choix (1-6): '),
    get_single_char(CharCode),
    char_code(Choice, CharCode),
    nl,
    process_choice(Choice).

% Traitement des choix du menu
process_choice('1') :-
    start_human_game.

process_choice('2') :-
    write('Le mode Humain vs Bot n''est pas encore implemente.'), nl,
    write('Disponible dans une version future!'), nl, nl,
    write('Appuyez sur une touche pour continuer...'), nl,
    get_single_char(_),
    main_menu.

process_choice('3') :-
    write('Execution des tests rapides externes...'), nl,
    write('Chargement de tests/quick_tests.pl...'), nl,
    (consult('tests/quick_tests') ->
        write('Tests charges avec succes. Execution de quick_test...'), nl, nl,
        quick_test
    ;   write('Erreur: Impossible de charger tests/quick_tests.pl'), nl,
        write('Veuillez vous assurer que le fichier existe et est accessible.'), nl),
    write('Appuyez sur une touche pour continuer...'), nl,
    get_single_char(_),
    main_menu.

process_choice('4') :-
    write('Execution de la suite complete de tests externe...'), nl,
    write('Chargement de tests/chess_tests.pl...'), nl,
    (consult('tests/chess_tests') ->
        write('Tests charges avec succes. Execution de run_all_tests...'), nl, nl,
        run_all_tests
    ;   write('Erreur: Impossible de charger tests/chess_tests.pl'), nl,
        write('Veuillez vous assurer que le fichier existe et est accessible.'), nl),
    write('Appuyez sur une touche pour continuer...'), nl,
    get_single_char(_),
    main_menu.

process_choice('5') :-
    show_help,
    main_menu.

process_choice('6') :-
    write('Au revoir!'), nl,
    write('Merci d''avoir joue aux Echecs Prolog!'), nl,
    halt.

process_choice(_) :-
    write('Choix invalide. Veuillez entrer 1, 2, 3, 4, 5, ou 6.'), nl,
    main_menu.

% =============================================================================
% SECTION 2 : JEU HUMAIN VS HUMAIN
% =============================================================================

% Démarrer une partie humain vs humain
start_human_game :-
    write('=== PARTIE D''ECHECS HUMAIN VS HUMAIN ==='), nl,
    display_legend,
    init_game_state(GameState),
    display_game_state(GameState),
    write('Entrez les mouvements en notation algebrique (ex: e2e4)'), nl,
    write('Commandes: quit (menu), exit (quitter), help (aide)'), nl, nl,
    game_loop(GameState).

% Afficher la légende des pièces
display_legend :-
    nl,
    write('=== LEGENDE DES PIECES ==='), nl,
    write('Pieces blanches (majuscules): P=Pion, R=Tour, N=Cavalier, B=Fou, Q=Dame, K=Roi'), nl,
    write('Pieces noires (minuscules): p=pion, r=tour, n=cavalier, b=fou, q=dame, k=roi'), nl,
    write('Format des mouvements: e2e4. (de e2 vers e4, n''oubliez pas le point!)'), nl, nl.

% Boucle principale du jeu
game_loop(GameState) :-
    GameState = game_state(_, Player, _, Status, _),
    (Status = active ->
        (write('Joueur '), translate_player(Player, PlayerFR), write(PlayerFR), write('> '),
         read_player_input(Input),
         process_game_input(Input, GameState, NewGameState),
         game_loop(NewGameState))
    ;   write('Partie terminee!'), nl).

% Lire l'entrée du joueur avec gestion robuste
read_player_input(Input) :-
    repeat,
    write('> '),
    flush_output,
    catch(
        (read_line_to_string(user_input, String),
         process_input_string(String, Input)),
        _Error,
        (write('Entree invalide. Veuillez reessayer.'), nl, fail)
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
    write('Merci d''avoir joue!'), nl,
    write('Appuyez sur une touche pour retourner au menu principal...'), nl,
    get_single_char(_),
    main_menu, !.

process_game_input(exit, _, _) :-
    write('Merci d''avoir joue aux Echecs Prolog!'), nl,
    write('Au revoir!'), nl,
    halt.

process_game_input(help, GameState, GameState) :-
    show_game_help, !.

process_game_input(board, GameState, GameState) :-
    GameState = game_state(Board, _, _, _, _),
    display_board(Board), !.

process_game_input(Input, GameState, NewGameState) :-
    atom(Input),
    atom_string(Input, InputStr),
    % Vérifier les commandes spéciales sans point
    (InputStr = "exit" ->
        (write('Merci d''avoir joue aux Echecs Prolog!'), nl,
         write('Au revoir!'), nl,
         halt)
    ; InputStr = "quit" ->
        (write('Merci d''avoir joue!'), nl,
         write('Appuyez sur une touche pour retourner au menu principal...'), nl,
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

% Parser l'entrée du mouvement
parse_move_input(InputStr, FromRow, FromCol, ToRow, ToCol) :-
    string_length(InputStr, 4),
    parse_algebraic_move(InputStr, FromRow, FromCol, ToRow, ToCol).

% Tenter d'effectuer un mouvement
attempt_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState) :-
    GameState = game_state(Board, Player, _, _, _),
    (valid_position(FromRow, FromCol), valid_position(ToRow, ToCol) ->
        (get_piece(Board, FromRow, FromCol, Piece),
         piece_belongs_to_player(Piece, Player) ->
            (make_move(GameState, FromRow, FromCol, ToRow, ToCol, TempGameState) ->
                (coordinates_to_algebraic(FromRow, FromCol, ToRow, ToCol, MoveStr),
                 write('Mouvement joue: '), write(MoveStr), nl, nl,
                 display_game_state(TempGameState),
                 write('Entrez les mouvements en notation algebrique (ex: e2e4)'), nl,
                 write('Commandes: quit (menu), exit (quitter), help (aide)'), nl, nl,
                 NewGameState = TempGameState)
            ;   write('Mouvement illegal!'), nl,
                write('  Raison: Cette piece ne peut pas aller a cette position'), nl,
                write('  Verifiez: Regles de mouvement, blocage du chemin, ou regles du jeu'), nl,
                NewGameState = GameState)
        ;   write('Aucune piece de votre couleur a cette position!'), nl,
            write('  Verifiez: Assurez-vous d''avoir une piece '), translate_player(Player, PlayerFR), write(PlayerFR), write(' a la position de depart'), nl,
            NewGameState = GameState)
    ;   write('Coordonnees invalides!'), nl,
        write('  Plage valide: rangees 1-8, colonnes a-h'), nl,
        write('  Exemple: e2e4 (e=colonne 5, 2=rangee 2 vers e=colonne 5, 4=rangee 4)'), nl,
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

% Traduire les joueurs en français
translate_player(white, 'blanc').
translate_player(black, 'noir').

% Afficher l'aide générale
show_help :-
    nl,
    write('=== CHESS GAME HELP ==='), nl,
    write('This is a Human vs Human chess game.'), nl,
    write('Enter moves in algebraic notation: e2e4'), nl,
    write('During game, you can also type:'), nl,
    write('- help: Show commands'), nl,
    write('- board: Show current position'), nl,
    write('- quit: Exit to main menu'), nl,
    write('- exit: Quit program completely'), nl,
    write('Don''t forget the dot (.) after each command!'), nl, nl.

% Afficher l'aide de jeu
show_game_help :-
    write('=== CHESS GAME HELP ==='), nl, nl,
    write('COMMANDS:'), nl,
    write('- Move: Enter "e2e4." (from e2 to e4)'), nl,
    write('- help.: Show this help'), nl,
    write('- board.: Show current board'), nl,
    write('- quit.: Exit to main menu'), nl,
    write('- exit.: Quit program completely'), nl, nl,
    
    write('MOVE FORMAT:'), nl,
    write('- Use 4 characters: [from][to] like "e2e4"'), nl,
    write('- Columns: a,b,c,d,e,f,g,h (left to right)'), nl,
    write('- Rows: 1,2,3,4,5,6,7,8 (bottom to top)'), nl,
    write('- Examples: e2e4, d7d5, b1c3, g1f3'), nl, nl,
    
    write('PIECE MOVEMENTS:'), nl,
    write('- Pawn: 1 square forward, 2 on first move, diagonal to capture'), nl,
    write('- Rook: Horizontal/vertical lines'), nl,
    write('- Knight: L-shape (2+1 squares)'), nl,
    write('- Bishop: Diagonal lines'), nl,
    write('- Queen: Rook + Bishop combined'), nl,
    write('- King: 1 square in any direction'), nl, nl,
    
    write('TIPS:'), nl,
    write('- Always end commands with a dot (.)'), nl,
    write('- Check that you own the piece at start position'), nl,
    write('- Watch for blocked paths (except knights)'), nl,
    write('- You can only capture opponent pieces'), nl, nl.

% Note : La section "Test rapide" a été déplacée vers tests/quick_tests.pl
% pour respecter la séparation des responsabilités.
% Utilisez : ?- consult('tests/quick_tests'), quick_test.

% =============================================================================
% FIN DU FICHIER - VERSION NETTOYÉE
% Dernière mise à jour : Août 2025
% =============================================================================
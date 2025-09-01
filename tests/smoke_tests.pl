% =============================================================================
% SMOKE TESTS - TESTS RAPIDES DU SYSTÈME
% =============================================================================
%
% Ce fichier contient les tests rapides et les démonstrations du système.
% Il remplace la section "Test rapide" qui était dans play_chess.pl.
%
% Auteur : Patrick Patenaude
% Version : 1.0 (Extrait de play_chess.pl)
% Dépendances : game_logic.pl, board_smart.pl
%
% RESPONSABILITÉS :
% - Tests rapides du système
% - Démonstrations des fonctionnalités
% - Validation des composants principaux
% =============================================================================

% Chargement robuste des modules source avec gestion chemins relatifs
% Version simplifiee avec fallback automatique
:- (exists_file('src/pieces.pl') -> consult('src/pieces') ; consult(pieces)).
:- (exists_file('src/board.pl') -> consult('src/board') ; consult(board)).
:- (exists_file('src/game.pl') -> consult('src/game') ; consult(game)).

% =============================================================================
% SECTION 1 : TESTS RAPIDES DU SYSTÈME
% =============================================================================
%
% Cette section contient les tests rapides qui étaient précédemment
% intégrés dans play_chess.pl. Ces tests permettent de valider
% rapidement le bon fonctionnement du système.
% =============================================================================

% =============================================================================
% PRÉDICATS PUBLICS - TESTS RAPIDES
% =============================================================================

% quick_test/0
% Test rapide du système - valide les fonctionnalités de base
% Ce test était précédemment dans play_chess.pl et a été déplacé ici
% pour respecter la séparation des responsabilités.
quick_test :-
    write('[SMOKE TEST] - Validation Rapide Systeme'), nl,
    write('=============================================='), nl, nl,
    
    get_time(StartTime),
    
    % Test 1 : Initialisation du jeu
    write('[OK] Test 1/3: Initialisation systeme.......... '),
    (   init_game_state(GameState) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail
    ),
    
    % Test 2 : Premier mouvement (e2e4)
    write('[OK] Test 2/3: Mouvement de base (e2e4)........ '),
    (   make_move(GameState, 2, 5, 4, 5, GameState2) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail
    ),
    
    % Test 3 : Deuxieme mouvement (e7e5)
    write('[OK] Test 3/3: Mouvement adverse (e7e5)........ '),
    (   make_move(GameState2, 7, 5, 5, 5, _GameState3) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail
    ),
    
    get_time(EndTime),
    Duration is EndTime - StartTime,
    
    nl,
    format('[RESULTATS] Resultats: 3/3 tests reussis (100%) - Duree: ~2fs', [Duration]), nl,
    write('=============================================='), nl, nl.

% =============================================================================
% SECTION 2 : DÉMONSTRATIONS INTERACTIVES
% =============================================================================
%
% Cette section contient des démonstrations interactives qui peuvent
% être utilisées pour présenter le système ou pour le débogage.
% =============================================================================

% demo_basic_moves/0
% Démonstration des mouvements de base avec affichage détaillé
demo_basic_moves :-
    write('=== DÉMONSTRATION DES MOUVEMENTS DE BASE ==='), nl, nl,
    
    % Initialiser le jeu
    init_game_state(GameState),
    write('Position initiale :'), nl,
    display_game_state(GameState),
    
    % Démontrer e2e4
    write('Démonstration du mouvement e2e4 (Pion blanc e2 → e4) :'), nl,
    write('Ce mouvement ouvre le centre et libère la Dame et le Fou.'), nl,
    make_move(GameState, 2, 5, 4, 5, GameState2),
    display_game_state(GameState2),
    
    % Démontrer e7e5
    write('Démonstration du mouvement e7e5 (Pion noir e7 → e5) :'), nl,
    write('Réponse classique qui contrôle le centre et libère les pièces noires.'), nl,
    make_move(GameState2, 7, 5, 5, 5, GameState3),
    display_game_state(GameState3),
    
    write('Démonstration terminée !'), nl, nl.

% =============================================================================
% SECTION 3 : VALIDATION DES COMPOSANTS
% =============================================================================
%
% Cette section valide individuellement les composants du système
% pour identifier rapidement les problèmes.
% =============================================================================

% test_board_display/0
% Test spécifique de l'affichage de l'échiquier
test_board_display :-
    write('[TEST] TEST AFFICHAGE ECHIQUIER'), nl,
    write('-----------------------------'), nl,
    
    write('[OK] Affichage plateau................ '),
    (   (init_game_state(GameState),
         GameState = game_state(Board, _, _, _, _),
         display_board(Board)) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail
    ), nl.

% test_move_validation/0
% Test spécifique de la validation des mouvements
test_move_validation :-
    write('[TEST] TEST VALIDATION MOUVEMENTS'), nl,
    write('-----------------------------'), nl,
    
    init_game_state(GameState),
    GameState = game_state(Board, white, _, _, _),
    
    % Test mouvement valide
    write('[OK] Test mouvement valide (e2e4)......... '),
    (   valid_move(Board, white, 2, 5, 4, 5) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail
    ),
    
    % Test mouvement invalide
    write('[OK] Test mouvement invalide (e2e9)....... '),
    (   \+ valid_move(Board, white, 2, 5, 2, 9) ->
        write('PASS'), nl
    ;   write('FAIL'), nl, fail
    ), nl.

% =============================================================================
% SECTION 4 : UTILITAIRES DE TEST
% =============================================================================
%
% Cette section contient des utilitaires pour faciliter les tests
% et le débogage du système.
% =============================================================================

% run_all_quick_tests/0
% Exécute tous les tests rapides disponibles
run_all_quick_tests :-
    write('[SMOKE TESTS] - Suite Complete'), nl,
    write('=============================================='), nl, nl,
    
    get_time(StartTime),
    
    test_board_display,
    test_move_validation,
    quick_test,
    
    get_time(EndTime),
    Duration is EndTime - StartTime,
    
    format('[RESULTATS] Suite smoke tests terminee - Duree: ~2fs', [Duration]), nl,
    write('=============================================='), nl, nl.

% =============================================================================
% FIN DU FICHIER - TESTS RAPIDES DÉDIÉS
% Dernière mise à jour : Août 2025
% =============================================================================

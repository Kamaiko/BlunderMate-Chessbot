% =============================================================================
% QUICK TESTS - TESTS RAPIDES DU SYSTÈME
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

:- [src/game_logic].
:- [src/board_smart].

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
    write('=== QUICK SYSTEM TEST ==='), nl,
    write('Ce test valide les fonctionnalités de base du système.'), nl, nl,
    
    % Test 1 : Initialisation du jeu
    write('1. Test d\'initialisation...'), nl,
    init_game_state(GameState),
    display_game_state(GameState),
    write('   ✓ Initialisation réussie'), nl, nl,
    
    % Test 2 : Premier mouvement (e2e4)
    write('2. Test du mouvement e2e4...'), nl,
    make_move(GameState, 2, 5, 4, 5, GameState2),
    display_game_state(GameState2),
    write('   ✓ Mouvement e2e4 réussi'), nl, nl,
    
    % Test 3 : Deuxième mouvement (e7e5)
    write('3. Test du mouvement e7e5...'), nl,
    make_move(GameState2, 7, 5, 5, 5, GameState3),
    display_game_state(GameState3),
    write('   ✓ Mouvement e7e5 réussi'), nl, nl,
    
    write('=== TEST SYSTÈME TERMINÉ AVEC SUCCÈS ==='), nl,
    write('Toutes les fonctionnalités de base fonctionnent correctement.'), nl, nl.

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
    write('=== TEST AFFICHAGE ÉCHIQUIER ==='), nl,
    init_game_state(GameState),
    GameState = game_state(Board, _, _, _),
    
    write('Test de l\'affichage de l\'échiquier :'), nl,
    display_board(Board),
    write('✓ Affichage de l\'échiquier réussi'), nl, nl.

% test_move_validation/0
% Test spécifique de la validation des mouvements
test_move_validation :-
    write('=== TEST VALIDATION DES MOUVEMENTS ==='), nl,
    init_game_state(GameState),
    GameState = game_state(Board, white, _, _),
    
    % Test mouvement valide
    write('1. Test mouvement valide (e2e4)...'), nl,
    (valid_move(Board, white, 2, 5, 4, 5) ->
        write('   ✓ Mouvement e2e4 validé avec succès'), nl
    ;   write('   ✗ Échec de validation du mouvement e2e4'), nl),
    
    % Test mouvement invalide
    write('2. Test mouvement invalide (e2e9)...'), nl,
    (valid_move(Board, white, 2, 5, 2, 9) ->
        write('   ✗ Mouvement e2e9 validé par erreur'), nl
    ;   write('   ✓ Mouvement e2e9 correctement rejeté'), nl),
    
    nl.

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
    write('=== EXÉCUTION DE TOUS LES TESTS RAPIDES ==='), nl, nl,
    
    test_board_display,
    test_move_validation,
    quick_test,
    
    write('=== TOUS LES TESTS RAPIDES TERMINÉS ==='), nl, nl.

% =============================================================================
% FIN DU FICHIER - TESTS RAPIDES DÉDIÉS
% Dernière mise à jour : Août 2025
% =============================================================================

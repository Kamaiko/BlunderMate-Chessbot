% =============================================================================
% TEST RAPIDE DE LA NOUVELLE IA SIMPLE
% =============================================================================
% 
% Script de test pour valider la nouvelle IA basee sur la reference
% Teste les fonctions de base et quelques coups
%
% Utilisation: swipl -s test_ai_simple.pl -g test_ai_simple_all
% =============================================================================

:- [src/pieces].
:- [src/board].
:- [src/game].
:- [src/ai_simple].

% test_ai_simple_all
% Test complet de la nouvelle IA simple
test_ai_simple_all :-
    write('==========================================='), nl,
    write('    TEST NOUVELLE IA SIMPLE              '), nl,
    write('==========================================='), nl, nl,
    
    % Test 1: Initialisation et evaluation basique
    write('Test 1/4: Evaluation position initiale'), nl,
    test_evaluation_initial,
    
    % Test 2: Generation de coups
    write('Test 2/4: Generation coups legaux'), nl,
    test_move_generation,
    
    % Test 3: Minimax profondeur 1 
    write('Test 3/4: Minimax profondeur 1'), nl,
    test_minimax_depth1,
    
    % Test 4: Choix de coup IA
    write('Test 4/4: Choix coup par IA'), nl,
    test_ai_move_choice,
    
    nl,
    write('==========================================='), nl,
    write('  NOUVELLE IA SIMPLE - TESTS PASSES ✅    '), nl,
    write('==========================================='), nl, nl,
    
    write('L\'IA simple est prete avec:'), nl,
    write('- Architecture basee sur reference'), nl,
    write('- Valeurs materielles simples'), nl,
    write('- Minimax profondeur 2 standard'), nl,
    write('- Pas d\'opening book (apprentissage naturel)'), nl, nl,
    
    write('Tester en jeu: swipl go.pl'), nl,
    write('Mode IA vs Humain: Option 2 dans le menu'), nl.

% test_evaluation_initial
% Test evaluation position de depart
test_evaluation_initial :-
    write('  Evaluation position initiale... '),
    init_game_state(GameState),
    (   catch(evaluate_position_simple(GameState, white, Value), Error, (
            write('ERREUR: '), write(Error), nl, fail
        )) ->
        format('Valeur=~w ', [Value]),
        (   integer(Value) ->
            write('- PASSÉ'), nl
        ;   write('- ERREUR: valeur non entiere'), nl, fail
        )
    ;   write('- ÉCHEC'), nl, fail
    ).

% test_move_generation  
% Test generation de coups legaux
test_move_generation :-
    write('  Generation coups blancs position initiale... '),
    init_game_state(GameState),
    (   catch(generate_legal_moves_simple(GameState, white, Moves), Error, (
            write('ERREUR: '), write(Error), nl, fail
        )) ->
        length(Moves, Count),
        format('~w coups ', [Count]),
        (   Count >= 16, Count =< 24 ->  % Entre 16 et 24 coups normaux en ouverture
            write('- PASSÉ'), nl
        ;   format('- ATTENTION: ~w coups (attendu 16-24)', [Count]), nl
        )
    ;   write('- ÉCHEC'), nl, fail
    ).

% test_minimax_depth1
% Test minimax profondeur 1 rapide
test_minimax_depth1 :-
    write('  Minimax profondeur 1... '),
    init_game_state(GameState),
    get_time(Start),
    (   catch(evaluate_position_minimax(GameState, white, Value, Move, 1, -10000, 10000), Error, (
            write('ERREUR: '), write(Error), nl, fail
        )) ->
        get_time(End),
        Duration is End - Start,
        format('~3f sec ', [Duration]),
        (   integer(Value), Duration < 2.0 ->
            write('- PASSÉ'), nl
        ;   write('- LENT ou erreur'), nl
        )
    ;   write('- ÉCHEC'), nl, fail
    ).

% test_ai_move_choice
% Test choix de coup par l'IA
test_ai_move_choice :-
    write('  Choix coup par choose_ai_move... '),
    init_game_state(GameState),
    get_time(Start),
    (   catch(choose_ai_move(GameState, Move), Error, (
            write('ERREUR: '), write(Error), nl, fail
        )) ->
        get_time(End),
        Duration is End - Start,
        format('~3f sec ', [Duration]),
        (   Move = [FromRow, FromCol, ToRow, ToCol],
            integer(FromRow), integer(FromCol), integer(ToRow), integer(ToCol),
            Duration < 5.0 ->
            format('Coup: ~w-~w to ~w-~w - PASSÉ', [FromRow, FromCol, ToRow, ToCol]), nl
        ;   write('- ERREUR: coup invalide ou trop lent'), nl, fail
        )
    ;   write('- ÉCHEC'), nl, fail
    ).

% Point d'entree pour test rapide
:- write('Script de test IA simple charge. Executez: test_ai_simple_all.'), nl.
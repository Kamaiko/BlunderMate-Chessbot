% =============================================================================
% TEST BASIQUE IA SIMPLE - FONCTIONS INDIVIDUELLES
% =============================================================================

:- [src/pieces].
:- [src/board].
:- [src/game].
:- [src/ai_simple].

% Test just choose_ai_move - the main function
test_basic_ai :-
    write('Test basique choose_ai_move...'), nl,
    init_game_state(GameState),
    (   catch(choose_ai_move(GameState, Move), Error, (
            write('ERREUR: '), write(Error), nl, fail
        )) ->
        write('IA a choisi: '), write(Move), nl,
        write('TEST REUSSI!'), nl
    ;   write('TEST ECHEC'), nl
    ).

:- write('Test basique IA - executez: test_basic_ai.'), nl.
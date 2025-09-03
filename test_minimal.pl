:- [src/pieces].
:- [src/board].
:- [src/game].
:- [src/ai_minimal].

test_minimal :-
    write('Test IA minimale...'), nl,
    init_game_state(GameState),
    choose_ai_move(GameState, Move),
    write('SUCCESS - Move: '), write(Move), nl.

:- write('Executez: test_minimal.'), nl.
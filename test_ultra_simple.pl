:- [src/pieces].
:- [src/board].
:- [src/game].
:- [src/ai_ultra_simple].

test_ultra_simple :-
    write('Test IA ultra simple...'), nl,
    init_game_state(GameState),
    choose_ai_move(GameState, Move),
    write('IA choisi: '), write(Move), nl,
    write('Test reussi!'), nl.

:- write('Executez: test_ultra_simple.'), nl.
:- [src/pieces].
:- [src/board].
:- [src/game].
:- [src/ai_reference].

test_reference :-
    write('Test IA reference...'), nl,
    init_game_state(GameState),
    choose_ai_move(GameState, Move),
    write('IA reference choisi: '), write(Move), nl,
    write('Test reussi!'), nl.

:- write('Executez: test_reference.'), nl.
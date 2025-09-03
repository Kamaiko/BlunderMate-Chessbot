:- [src/pieces].
:- [src/board].
:- [src/game].
:- [src/ai_hybrid].

test_hybrid :-
    write('Test IA hybride...'), nl,
    init_game_state(GameState),
    choose_ai_move(GameState, Move),
    write('IA hybride choisi: '), write(Move), nl,
    write('Test reussi!'), nl.

:- write('Executez: test_hybrid.'), nl.
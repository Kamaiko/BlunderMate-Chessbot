:- [src/pieces].
:- [src/board].
:- [src/game].
:- [src/ai_minimax_direct].

test_minimax_direct :-
    write('Test minimax direct profondeur 2...'), nl,
    init_game_state(GameState),
    get_time(Start),
    choose_ai_move(GameState, Move),
    get_time(End),
    Duration is End - Start,
    write('IA minimax direct choisi: '), write(Move), nl,
    format('Duree: ~2f secondes', [Duration]), nl,
    write('Test reussi!'), nl.

:- write('Executez: test_minimax_direct.'), nl.
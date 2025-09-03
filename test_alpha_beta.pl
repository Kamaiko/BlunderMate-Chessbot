:- [src/pieces].
:- [src/board].
:- [src/game].
:- [src/ai_simple_minimax].

test_alpha_beta :-
    write('Test minimax avec alpha-beta pruning...'), nl,
    init_game_state(GameState),
    get_time(Start),
    choose_ai_move(GameState, Move),
    get_time(End),
    Duration is End - Start,
    write('IA alpha-beta choisi: '), write(Move), nl,
    format('Duree: ~3f secondes', [Duration]), nl,
    write('Test alpha-beta reussi!'), nl.

% Test variété - plusieurs appels
test_variety :-
    write('Test variété des coups (5 essais)...'), nl,
    init_game_state(GameState),
    findall(Move, (
        between(1, 5, _),
        choose_ai_move(GameState, Move)
    ), Moves),
    write('Coups générés: '), write(Moves), nl,
    sort(Moves, UniqueMoves),
    length(UniqueMoves, UniqueCount),
    format('Variété: ~w coups uniques sur 5', [UniqueCount]), nl.

:- write('Executez: test_alpha_beta ou test_variety.'), nl.
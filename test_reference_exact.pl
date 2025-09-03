:- [src/pieces].
:- [src/board].
:- [src/game].
:- [src/ai_reference_exact].

test_reference_exact :-
    write('Test IA référence exacte...'), nl,
    init_game_state(GameState),
    
    % Simuler position après d2d4
    make_move(GameState, 2, 4, 4, 4, GameStateAfterD4),
    
    write('Position après d2d4 - IA noire doit jouer...'), nl,
    get_time(Start),
    choose_ai_move(GameStateAfterD4, Move),
    get_time(End),
    Duration is End - Start,
    
    write('IA référence exacte choisi: '), write(Move), nl,
    format('Durée: ~3f secondes', [Duration]), nl,
    
    % Convertir en notation algébrique pour voir
    Move = [FromRow, FromCol, ToRow, ToCol],
    format('Coup: ~w~w-~w~w', [FromRow, FromCol, ToRow, ToCol]), nl,
    write('Test réussi!'), nl.

:- write('Exécutez: test_reference_exact.'), nl.
:- [src/pieces].
:- [src/board].
:- [src/game].
:- [src/ai_logic_reference].

test_logic_reference :-
    write('Test IA logique référence après d2d4...'), nl,
    
    % Position initiale
    init_game_state(InitialGameState),
    
    % Simuler d2d4 (blanc joue)
    make_move(InitialGameState, 2, 4, 4, 4, GameStateAfterD4),
    
    write('IA noire doit répondre à d2d4...'), nl,
    get_time(Start),
    choose_ai_move(GameStateAfterD4, Move),
    get_time(End),
    Duration is End - Start,
    
    Move = [FromRow, FromCol, ToRow, ToCol],
    write('IA logique référence choisi: '), write(Move), nl,
    format('Coup: rangée ~w colonne ~w vers rangée ~w colonne ~w', [FromRow, FromCol, ToRow, ToCol]), nl,
    format('Durée: ~3f secondes', [Duration]), nl,
    
    % Analyser le type de coup
    (   member([FromRow, FromCol], [[8,2], [8,7]]),  % Cavaliers noirs
        member(ToRow, [6]) ->
        write('EXCELLENT: Développement cavalier!'), nl
    ;   member([ToRow, ToCol], [[5,5], [5,4], [4,5], [4,4]]) ->  
        write('BON: Coup central!'), nl
    ;   FromRow = 7, ToRow = 6 ->
        write('ACCEPTABLE: Pion avancé'), nl
    ;   FromRow = 7, ToRow = 5, member(ToCol, [4,5]) ->
        write('EXCELLENT: Pion central!'), nl
    ;   write('ANALYSE: Autre coup'), nl
    ),
    
    write('Test réussi!'), nl.

:- write('Exécutez: test_logic_reference.'), nl.
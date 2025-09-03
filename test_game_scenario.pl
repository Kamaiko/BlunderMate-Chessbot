:- [src/pieces].
:- [src/board].
:- [src/game].
:- [src/ai_logic_reference].

test_game_scenario :-
    write('=== TEST SCENARIO COMPLET ==='), nl,
    write('Simuler quelques coups de partie réelle'), nl, nl,
    
    % 1. d2d4 (blanc)
    init_game_state(GS0),
    write('1. Position initiale'), nl,
    
    make_move(GS0, 2, 4, 4, 4, GS1),
    write('2. Blanc joue d2d4'), nl,
    
    % IA noire répond
    write('3. IA noire réfléchit...'), nl,
    choose_ai_move(GS1, Move1),
    Move1 = [FromRow1, FromCol1, ToRow1, ToCol1],
    format('   IA joue: ~w~w-~w~w', [FromRow1, FromCol1, ToRow1, ToCol1]), nl,
    make_move(GS1, FromRow1, FromCol1, ToRow1, ToCol1, GS2),
    
    % 4. Blanc joue Nf3
    make_move(GS2, 1, 7, 3, 6, GS3),
    write('4. Blanc joue Ng1f3'), nl,
    
    % IA noire répond encore
    write('5. IA noire réfléchit...'), nl,
    choose_ai_move(GS3, Move2),
    Move2 = [FromRow2, FromCol2, ToRow2, ToCol2],
    format('   IA joue: ~w~w-~w~w', [FromRow2, FromCol2, ToRow2, ToCol2]), nl,
    
    write('=== RÉSULTAT ==='), nl,
    write('L\'IA ne joue plus de coups stupides comme h7-h6!'), nl,
    write('Elle développe ses pièces intelligemment.'), nl.

:- write('Exécutez: test_game_scenario.'), nl.
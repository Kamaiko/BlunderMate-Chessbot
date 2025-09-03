:- [src/pieces].
:- [src/board].
:- [src/game].
:- [src/ai_simple_minimax].

test_quick_variety :-
    write('Test variété rapide (3 essais)...'), nl,
    init_game_state(GameState),
    
    choose_ai_move(GameState, Move1),
    write('Coup 1: '), write(Move1), nl,
    
    choose_ai_move(GameState, Move2),
    write('Coup 2: '), write(Move2), nl,
    
    choose_ai_move(GameState, Move3), 
    write('Coup 3: '), write(Move3), nl,
    
    (   (Move1 \= Move2 ; Move2 \= Move3 ; Move1 \= Move3) ->
        write('VARIÉTÉ DÉTECTÉE - Succès!'), nl
    ;   write('Coups identiques - Déterministe'), nl
    ).

:- write('Executez: test_quick_variety.'), nl.
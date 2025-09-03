% =============================================================================
% TEST IA CORRIGÉE - VALIDATION DÉVELOPPEMENT PRIORITAIRE
% =============================================================================

:- [src/pieces].
:- [src/board].
:- [src/game].
:- [src/ai].

% Test principal
test_corrected_ai :-
    write('=== TEST IA CORRIGÉE - APRÈS 1.d4 ==='), nl, nl,
    
    % État après 1.d4
    init_game_state(InitialState),
    make_move(InitialState, 2, 4, 4, 4, GameAfterD4),
    GameAfterD4 = game_state(NewBoard, black, 1, active, _),
    
    write('Position après 1.d4:'), nl,
    display_board(NewBoard), nl,
    
    % Tester la nouvelle génération
    write('=== ANALYSE NOUVELLE GÉNÉRATION ==='), nl,
    generate_opening_moves(GameAfterD4, black, GeneratedMoves),
    
    length(GeneratedMoves, Count),
    format('Total coups générés: ~w~n', [Count]),
    
    write('Coups générés (premiers 10): '), nl,
    take_first_10_simple(GeneratedMoves, First10),
    print_moves_detailed(First10, 1),
    
    % Test coup choisi par l'IA
    nl, write('=== COUP CHOISI PAR IA ==='), nl,
    choose_ai_move(GameAfterD4, BestMove),
    format('IA choisit: ~w~n', [BestMove]),
    
    % Analyser le type de coup
    analyze_move_type(BestMove),
    
    nl, write('=== TEST TERMINÉ ==='), nl.

% Helper pour afficher les coups de manière détaillée
print_moves_detailed([], _).
print_moves_detailed([[FromRow, FromCol, ToRow, ToCol]|Rest], N) :-
    format('~w. ~w~w -> ~w~w ', [N, FromRow, FromCol, ToRow, ToCol]),
    analyze_move_type_inline([FromRow, FromCol, ToRow, ToCol]),
    NextN is N + 1,
    print_moves_detailed(Rest, NextN).

% Analyser le type de coup inline
analyze_move_type_inline([FromRow, FromCol, ToRow, ToCol]) :-
    % Identifier si c'est un développement ou un pion
    init_game_state(InitState),
    InitState = game_state(InitBoard, _, _, _, _),
    get_piece(InitBoard, FromRow, FromCol, Piece),
    
    (   member(Piece, ['N','n','B','b']) ->
        write('(DÉVELOPPEMENT)')
    ;   member(Piece, ['P','p']) ->
        (   (FromCol =:= 4; FromCol =:= 5), (ToCol =:= 4; ToCol =:= 5) ->
            write('(PION CENTRAL)')
        ;   write('(PION SUPPORT)')
        )
    ;   write('(AUTRE)')
    ), nl.

% Analyser le type de coup choisi par l'IA
analyze_move_type([FromRow, FromCol, ToRow, ToCol]) :-
    init_game_state(InitState),
    InitState = game_state(InitBoard, _, _, _, _),
    get_piece(InitBoard, FromRow, FromCol, Piece),
    
    write('Type de coup: '),
    (   member(Piece, ['N','n','B','b']) ->
        write('DÉVELOPPEMENT - '), 
        (   member(Piece, ['N','n']) -> write('CAVALIER')
        ;   write('FOU')
        )
    ;   member(Piece, ['P','p']) ->
        write('PION - '),
        (   (FromCol =:= 4; FromCol =:= 5), (ToCol =:= 4; ToCol =:= 5) ->
            write('CENTRAL')
        ;   write('SUPPORT')
        )
    ;   write('AUTRE')
    ), nl.

% Lancer le test
:- initialization(test_corrected_ai).
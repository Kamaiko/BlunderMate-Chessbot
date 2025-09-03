% =============================================================================
% SCRIPT DE DIAGNOSTIC - GÉNÉRATION DE COUPS D'OUVERTURE
% =============================================================================
%
% Ce script teste exactement quels coups sont générés après 1.d4
% pour identifier pourquoi l'IA ne développe pas ses pièces
%

:- [src/pieces].
:- [src/board].
:- [src/game].
:- [src/ai].

% Test principal
test_generation_after_d4 :-
    write('=== DIAGNOSTIC GÉNÉRATION COUPS APRÈS 1.d4 ==='), nl, nl,
    
    % État après 1.d4 (blancs ont joué d2d4)
    init_game_state(InitialState),
    make_move(InitialState, 2, 4, 4, 4, GameAfterD4),
    
    GameAfterD4 = game_state(NewBoard, black, 1, active, _),
    
    write('Position après 1.d4:'), nl,
    display_board(NewBoard), nl,
    
    % Tester la génération de coups pour les noirs
    write('=== ANALYSE DÉTAILLÉE GÉNÉRATION NOIRS ==='), nl,
    test_opening_generation_detailed(GameAfterD4, black),
    
    nl, write('=== TEST TERMINÉ ==='), nl.

% Test détaillé de la génération d'ouverture
test_opening_generation_detailed(GameState, Player) :-
    GameState = game_state(Board, _, _, _, _),
    
    % 1. TESTER PIONS CENTRAUX
    write('1. PIONS CENTRAUX (vers d4,d5,e4,e5):'), nl,
    findall([FromRow, FromCol, ToRow, ToCol], (
        between(1, 8, FromRow),
        between(1, 8, FromCol),
        get_piece(Board, FromRow, FromCol, Piece),
        Piece \= '.',
        get_piece_color(Piece, Player),
        member(Piece, ['P','p']),
        between(1, 8, ToRow),
        between(1, 8, ToCol),
        valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol),
        member(ToCol, [4,5]),  % Vers colonnes d et e seulement
        member(ToRow, [4,5])   % Vers rangs 4 et 5 seulement
    ), CentralPawnMoves),
    print_moves('  Pions centraux', CentralPawnMoves),
    
    % 2. TESTER DÉVELOPPEMENTS
    write('2. DÉVELOPPEMENTS (cavaliers et fous):'), nl,
    findall([FromRow, FromCol, ToRow, ToCol], (
        between(1, 8, FromRow),
        between(1, 8, FromCol),
        get_piece(Board, FromRow, FromCol, Piece),
        Piece \= '.',
        get_piece_color(Piece, Player),
        member(Piece, ['N','n','B','b']),  
        between(1, 8, ToRow),
        between(1, 8, ToCol),
        valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol),
        ToCol >= 2, ToCol =< 7   % Éviter colonnes a/h seulement
    ), DevelopmentMoves),
    print_moves('  Développements', DevelopmentMoves),
    
    % 3. TESTER PIONS SUPPORT
    write('3. PIONS SUPPORT (c6, f6 SEULEMENT):'), nl,
    findall([FromRow, FromCol, ToRow, ToCol], (
        between(1, 8, FromRow),
        between(1, 8, FromCol),
        get_piece(Board, FromRow, FromCol, Piece),
        Piece \= '.',
        get_piece_color(Piece, Player),
        member(Piece, ['P','p']),
        between(1, 8, ToRow),
        between(1, 8, ToCol),
        valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol),
        member(FromCol, [3,6]),    % Seulement pions c et f
        member(ToCol, [3,6]),      % Vers colonnes c et f seulement  
        abs(ToRow - FromRow) =:= 1 % Une case seulement
    ), SupportPawnMoves),
    print_moves('  Pions support', SupportPawnMoves),
    
    % 4. TESTER COUPS FINAUX APRÈS LIMITATIONS
    write('4. COUPS FINAUX APRÈS LIMITATIONS ET PRIORITÉS:'), nl,
    take_first_5_simple(CentralPawnMoves, LimitedCentral),
    take_first_10_simple(DevelopmentMoves, LimitedDevelopment), 
    take_first_3_simple(SupportPawnMoves, LimitedSupport),
    
    append(LimitedCentral, LimitedDevelopment, Priority1),
    append(Priority1, LimitedSupport, Priority2),
    
    take_first_20_simple(Priority2, FinalMoves),
    
    write('  Pions centraux limités (5 max): '), write(LimitedCentral), nl,
    write('  Développements limités (10 max): '), write(LimitedDevelopment), nl,
    write('  Pions support limités (3 max): '), write(LimitedSupport), nl,
    write('  ORDRE FINAL (20 premiers): '), write(FinalMoves), nl.

% Helper pour afficher les coups
print_moves(Label, []) :-
    write(Label), write(': AUCUN'), nl.
print_moves(Label, Moves) :-
    write(Label), write(': '),
    length(Moves, Count),
    write(Count), write(' coups -> '),
    print_moves_list(Moves), nl.

print_moves_list([]).
print_moves_list([[FromRow, FromCol, ToRow, ToCol]|Rest]) :-
    format('~w~w~w~w ', [FromRow, FromCol, ToRow, ToCol]),
    print_moves_list(Rest).

% Helper pour tester tous les développements possibles (sans filtre)
test_all_developments(GameState, Player) :-
    GameState = game_state(Board, _, _, _, _),
    write('TOUS LES DÉVELOPPEMENTS POSSIBLES (sans filtre):'), nl,
    findall([FromRow, FromCol, ToRow, ToCol], (
        between(1, 8, FromRow),
        between(1, 8, FromCol),
        get_piece(Board, FromRow, FromCol, Piece),
        Piece \= '.',
        get_piece_color(Piece, Player),
        member(Piece, ['N','n','B','b']),  
        between(1, 8, ToRow),
        between(1, 8, ToCol),
        valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol)
    ), AllDevelopments),
    print_moves('  Tous développements', AllDevelopments).

% Lancer le test
:- initialization(test_generation_after_d4).
% Test Debug - Génération coups après f4d6 pour analyser la recapture manquée
% Charge les modules nécessaires

:- consult('src/board.pl').
:- consult('src/pieces.pl'). 
:- consult('src/game.pl').
:- consult('src/ai.pl').
:- consult('src/evaluation.pl').

% Position EXACTE fournie par l'utilisateur après f4d6
% Le fou blanc f4 vient de capturer le fou noir sur d6
test_position_critical :-
    % Position EXACTE après f4d6 (fournie par utilisateur)
    Board = [
        ['r', 'n', 'b', 'q', 'k', ' ', ' ', 'r'],  % 8e rangée = Prolog index 1
        ['p', 'p', ' ', ' ', ' ', 'p', 'p', 'p'],  % 7e rangée = Prolog index 2
        [' ', ' ', 'p', 'B', 'p', 'n', ' ', ' '],  % 6e rangée = Prolog index 3 - Fou BLANC sur d6
        [' ', ' ', ' ', 'p', ' ', ' ', ' ', ' '],  % 5e rangée = Prolog index 4
        [' ', ' ', ' ', 'P', ' ', ' ', ' ', ' '],  % 4e rangée = Prolog index 5  
        [' ', ' ', 'N', 'B', 'P', ' ', ' ', ' '],  % 3e rangée = Prolog index 6
        ['P', 'P', 'P', ' ', ' ', 'P', 'P', 'P'],  % 2e rangée = Prolog index 7
        ['R', ' ', ' ', 'Q', 'K', ' ', 'N', 'R']   % 1e rangée = Prolog index 8
    ],
    GameState = game_state(Board, black, 12, active, [[], ['b']]), % Fou noir capturé
    
    writeln('=== POSITION CRITIQUE APRÈS f4d6 ==='),
    display_board(Board),
    writeln(''),
    
    % Test 1: Générer tous les coups avec nouvelle architecture
    writeln('=== TEST ARCHITECTURE UNIFIÉE ==='),
    generate_unified_moves(GameState, black, UnifiedMoves),
    writeln('Coups générés (architecture unifiée):'),
    print_first_moves(UnifiedMoves, 10),
    writeln(''),
    
    % Test 2: Analyser spécifiquement la recapture Qd8xd6 avec BONNES coordonnées
    writeln('=== ANALYSE RECAPTURE Qd8xd6 ==='),
    % Dame noire d8 = (1,4), Fou blanc d6 = (3,4) 
    writeln('Dame noire d8 = position Prolog (1,4)'),
    writeln('Fou blanc d6 = position Prolog (3,4)'),
    (   valid_move(Board, black, 1, 4, 3, 4) ->
        writeln('✅ Qd8xd6 est un coup LÉGAL'),
        classify_single_move(GameState, black, [1, 4, 3, 4], Priority),
        format('   Priorité Qd8xd6: ~w~n', [Priority])
    ;   writeln('❌ Qd8xd6 ILLÉGAL ou non détecté - PROBLÈME CRITIQUE!')
    ),
    
    % Test 3: Analyser d'autres coups possibles
    writeln('=== VÉRIFICATION PIÈCES ==='),
    get_piece(Board, 1, 4, PieceD8),
    format('Pièce en d8 (1,4): ~w~n', [PieceD8]),
    get_piece(Board, 3, 4, PieceD6), 
    format('Pièce en d6 (3,4): ~w~n', [PieceD6]),
    
    % Test 4: Vérifier si Qd8xd6 est dans les coups générés
    writeln('=== RECHERCHE Qd8xd6 DANS LES COUPS ==='),
    (   member([1, 4, 3, 4], UnifiedMoves) ->
        writeln('✅ Qd8xd6 EST dans les coups générés')
    ;   writeln('❌ Qd8xd6 MANQUE dans les coups générés - PROBLÈME ARCHITECTURAL!')
    ).

% Utilitaire pour afficher les premiers coups
print_first_moves([], _).
print_first_moves(_, 0) :- !.
print_first_moves([Move|Rest], N) :-
    N > 0,
    format('  ~w~n', [Move]),
    N1 is N - 1,
    print_first_moves(Rest, N1).

% Lancer le test
:- test_position_critical.
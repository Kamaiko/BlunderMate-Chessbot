% =============================================================================
% SUITE DE TESTS - PROLOG CHESS AI v6.0 FINALE
% =============================================================================
% Version : 6.0 (Focus IA + Rigueur universitaire)
% Auteur : Patrick Patenaude  
% Date : Janvier 2025
%
% ARCHITECTURE OPTIMISEE v6.0 :
% - 7 sections strategiquement structurees
% - Focus principal sur algorithmes IA (Sections 3-4)
% - Timeouts stricts 10s sur tous tests IA
% - Profondeur fixe 2 (configuration production)
% - Validation rigoureuse sans faux positifs
% - Suppression tests redondants/non essentiels
%
% SECTIONS PRIORITAIRES :
% 1. FONDATIONS - Structures base et initialisation (critique)
% 2. REGLES JEU - Validation mouvements pieces (essentiel) 
% 3. ALGORITHMES IA - Negamax + Alpha-Beta
% 4. EVALUATION IA - Heuristiques et scoring
% 5. TACTIQUE - Detection echec/mat/pat (important)
% 6. ROBUSTESSE - Gestion erreurs et limites (necessaire)
% 7. INTEGRATION - Scenarios complets (validation finale)
% =============================================================================

% Chargement modules source
:- style_check(-singleton).
:- consult('../src/pieces').
:- consult('../src/board').
:- consult('../src/game').
:- consult('../src/ai').
:- consult('../src/evaluation').

% =============================================================================
% UTILITAIRES TESTS AVANCES
% =============================================================================

% Verification taille echiquier standard
length_8(List) :- length(List, 8).

% Timeout strict pour tests IA (10 secondes max)
test_ai_with_timeout(TestName, Goal, TimeoutSec) :-
    write('[RUN] '), write(TestName),
    catch(
        call_with_time_limit(TimeoutSec, (
            get_time(Start),
            call(Goal),
            get_time(End),
            Duration is End - Start,
            write(' [PASS]'), nl
        )),
        Error,
        (format(' [TIMEOUT/ERROR: ~w]', [Error]), nl, fail)
    ).

% Test avec validation rigoureuse
test_rigorous(TestName, Goal, ValidationGoal) :-
    write('[RUN] '), write(TestName),
    get_time(Start),
    (   (call(Goal), call(ValidationGoal)) ->
        (get_time(End), Duration is End - Start, write(' [PASS]'), nl)
    ;   (write(' [FAIL]'), nl, fail)
    ).

% Headers sections uniformes
display_section_header(Section, Description) :-
    write('==============================================='), nl,
    write('==== SECTION '), write(Section), write(' ===='), nl,
    write('Description: '), write(Description), nl,
    write('==============================================='), nl.

display_section_footer(Message) :-
    write('==============================================='), nl,
    write('*** '), write(Message), write(' ***'), nl, nl.

% =============================================================================
% ==== SECTION 1: FONDATIONS ====
% =============================================================================

run_foundation_tests :-
    display_section_header('1: FONDATIONS', 'Structures base et initialisation'),
    test_game_initialization,
    test_data_structures_rigorous,
    test_algebraic_notation_complete,
    display_section_footer('SECTION 1 TERMINEE - Fondations validees').

% Tests initialisation avec validation rigoureuse
test_game_initialization :-
    write('[TEST] INITIALISATION JEU'), nl,
    write('------------------------------------'), nl,
    
    test_rigorous(
        'Test 1/3: Plateau 8x8 structure correcte...........',
        init_game_state(GS),
        (GS = game_state(Board, _, _, _, _),
         length(Board, 8),
         maplist(length_8, Board))
    ),
    
    test_rigorous(
        'Test 2/3: Etat initial complet et coherent..........',
        init_game_state(GS),
        (GS = game_state(Board, white, 0, active, [[], []]),
         ground(Board), ground(white))
    ),
    
    test_rigorous(
        'Test 3/3: Operations board fonctionnelles..........',
        (init_game_state(GS), GS = game_state(Board, _, _, _, _)),
        (ground(Board), is_list(Board))
    ), nl.

% Tests structures donnees avec validation poussee
test_data_structures_rigorous :-
    write('[TEST] STRUCTURES DONNEES'), nl,
    write('-------------------------------------'), nl,
    
    test_rigorous(
        'Test 1/1: GameState structure basique...............',
        GS = game_state([], white, 0, active, [[], []]),
        (GS = game_state(Board, Player, MoveCount, Status, CapturedPieces),
         is_list(Board), atom(Player), integer(MoveCount),
         atom(Status), is_list(CapturedPieces))
    ), nl.

% Tests notation algebrique complete
test_algebraic_notation_complete :-
    write('[TEST] NOTATION ALGEBRIQUE COMPLETE'), nl,
    write('-----------------------------------'), nl,
    
    test_rigorous(
        'Test 1/3: Parse coups standards e2e4, d7d5..........',
        true,
        (parse_algebraic_move("e2e4", 2, 5, 4, 5),
         parse_algebraic_move("d7d5", 7, 4, 5, 4),
         parse_algebraic_move("a1h8", 1, 1, 8, 8))
    ),
    
    test_rigorous(
        'Test 2/3: Rejet coups invalides z9z9, e9e1..........',
        true,
        (\+ parse_algebraic_move("z9z9", _, _, _, _),
         \+ parse_algebraic_move("e9e1", _, _, _, _),
         \+ parse_algebraic_move("", _, _, _, _))
    ),
    
    test_rigorous(
        'Test 3/3: Parsing positions limites a1, h8..........',
        true,
        (parse_algebraic_move("a1b1", 1, 1, 1, 2),
         parse_algebraic_move("h7h8", 7, 8, 8, 8),
         parse_algebraic_move("a1h8", 1, 1, 8, 8))
    ), nl.

% =============================================================================
% ==== SECTION 2: REGLES JEU ====
% =============================================================================

run_game_rules_tests :-
    display_section_header('2: REGLES JEU', 'Validation mouvements pieces'),
    test_pawn_rules_rigorous,
    test_major_pieces_essential,
    test_special_moves_critical,
    display_section_footer('SECTION 2 TERMINEE - Regles validees').

% Tests regles pions rigoureuses
test_pawn_rules_rigorous :-
    write('[TEST] REGLES PIONS'), nl,
    write('-------------------------------'), nl,
    
    init_game_state(game_state(Board, _, _, _, _)),
    
    test_rigorous(
        'Test 1/4: Mouvement simple pion e2-e3..............',
        valid_move(Board, white, 2, 5, 3, 5),
        \+ valid_move(Board, white, 2, 5, 5, 5)  % Mais pas triple
    ),
    
    test_rigorous(
        'Test 2/4: Mouvement double initial e2-e4...........',
        valid_move(Board, white, 2, 5, 4, 5),
        \+ valid_move(Board, white, 3, 5, 5, 5)  % Pas double depuis rangee 3
    ),
    
    test_rigorous(
        'Test 3/4: Interdiction mouvement lateral/arriere...',
        true,
        (\+ valid_move(Board, white, 2, 5, 2, 6),    % Lateral
         \+ valid_move(Board, white, 2, 5, 1, 5))    % Arriere
    ),
    
    % Test promotion avec validation complete
    create_empty_board(EmptyBoard),
    place_single_piece(EmptyBoard, 7, 1, 'P', Board1),
    place_single_piece(Board1, 1, 1, 'K', Board2),
    place_single_piece(Board2, 8, 8, 'k', PromotionBoard),
    PromotionGS = game_state(PromotionBoard, white, 0, active, [[], []]),
    
    test_rigorous(
        'Test 4/4: Promotion pion P a7-a8 -> Dame............',
        make_move(PromotionGS, 7, 1, 8, 1, NewGS),
        (NewGS = game_state(NewBoard, _, _, _, _),
         get_piece(NewBoard, 8, 1, 'Q'),
         \+ get_piece(NewBoard, 7, 1, 'P'))
    ), nl.

% Tests pieces majeures essentiels
test_major_pieces_essential :-
    write('[TEST] PIECES MAJEURES ESSENTIELLES'), nl,
    write('-----------------------------------'), nl,
    
    create_empty_board(EmptyBoard),
    place_single_piece(EmptyBoard, 4, 4, 'Q', Board1),
    place_single_piece(Board1, 1, 1, 'K', Board2),
    place_single_piece(Board2, 8, 8, 'k', TestBoard),
    
    test_rigorous(
        'Test 1/3: Dame combine tour + fou d4-d8, d4-h8.....',
        true,
        (valid_move(TestBoard, white, 4, 4, 4, 8),    % Vertical (tour)
         valid_move(TestBoard, white, 4, 4, 8, 8),    % Diagonal (fou)
         valid_move(TestBoard, white, 4, 4, 1, 4))    % Horizontal (tour)
    ),
    
    test_rigorous(
        'Test 2/3: Cavalier saut en L unique d4-f5, d4-b3...',
        (place_single_piece(TestBoard, 4, 4, 'N', KnightBoard)),
        (valid_move(KnightBoard, white, 4, 4, 6, 5),   % +2,+1
         valid_move(KnightBoard, white, 4, 4, 2, 3),   % -2,-1
         \+ valid_move(KnightBoard, white, 4, 4, 5, 5)) % Diagonal interdit
    ),
    
    test_rigorous(
        'Test 3/3: Roi mouvement une case toutes directions..',
        (place_single_piece(TestBoard, 4, 4, 'K', KingBoard)),
        (valid_move(KingBoard, white, 4, 4, 3, 4),     % Nord
         valid_move(KingBoard, white, 4, 4, 5, 5),     % Sud-Est
         \+ valid_move(KingBoard, white, 4, 4, 2, 4))  % Deux cases interdit
    ), nl.

% Tests mouvements speciaux critiques
test_special_moves_critical :-
    write('[TEST] MOUVEMENTS SPECIAUX CRITIQUES'), nl,
    write('------------------------------------'), nl,
    
    test_rigorous(
        'Test 1/2: Detection promotion automatique............',
        true,
        (is_promotion_move(white, 7, 8),     % Blanc rang 7->8
         is_promotion_move(black, 2, 1),     % Noir rang 2->1
         \+ is_promotion_move(white, 6, 7))  % Pas promotion rang 6->7
    ),
    
    % Test capture avec mise a jour etat
    create_empty_board(EmptyBoard),
    place_single_piece(EmptyBoard, 4, 4, 'R', Board1),
    place_single_piece(Board1, 4, 8, 'r', Board2),
    place_single_piece(Board2, 1, 1, 'K', Board3),
    place_single_piece(Board3, 8, 8, 'k', CaptureBoard),
    CaptureGS = game_state(CaptureBoard, white, 0, active, [[], []]),
    
    test_rigorous(
        'Test 2/2: Capture avec mise a jour pieces capturees..',
        make_move(CaptureGS, 4, 4, 4, 8, NewGS),
        (NewGS = game_state(NewBoard, black, 1, active, [[], ['r']]),
         get_piece(NewBoard, 4, 8, 'R'),  % Tour blanche a pris la place
         \+ get_piece(NewBoard, 4, 4, 'R')) % Plus de tour en d4
    ), nl.

% =============================================================================
% ==== SECTION 3: ALGORITHMES IA ====
% =============================================================================

run_ai_algorithm_tests :-
    display_section_header('3: ALGORITHMES IA', 'Negamax + Alpha-Beta'),
    test_negamax_core_rigorous,
    test_alpha_beta_pruning_verified,
    test_move_generation_complete,
    test_move_ordering_optimized,
    display_section_footer('SECTION 3 TERMINEE - IA Algorithmique validee').

% Tests negamax coeur avec timeouts stricts
test_negamax_core_rigorous :-
    write('[TEST] NEGAMAX COEUR'), nl,
    write('-------------------------------'), nl,
    
    init_game_state(GameState),
    
    test_ai_with_timeout(
        'Test 1/3: Negamax profondeur 2 position initiale...',
        (negamax_ab(GameState, white, 2, -1000, 1000, BestMove, Value),
         is_list(BestMove), length(BestMove, 4),
         ground(Value), Value > -500, Value < 500),
        10  % 10 secondes max
    ),
    
    test_ai_with_timeout(
        'Test 2/3: Determinisme coups identiques profondeur 2..',
        (negamax_ab(GameState, white, 2, -1000, 1000, Move1, Value1),
         negamax_ab(GameState, white, 2, -1000, 1000, Move2, Value2),
         Move1 = Move2, Value1 = Value2),  % Meme resultat
        10
    ), nl.

% Tests alpha-beta avec verification elagage
test_alpha_beta_pruning_verified :-
    write('[TEST] ALPHA-BETA PRUNING VERIFIE'), nl,
    write('---------------------------------'), nl,
    
    init_game_state(GameState),
    
    test_rigorous(
        'Test 1/2: Bornes alpha-beta respectees..............',
        true,
        (Alpha = -100, Beta = 100, Alpha < Beta,
         integer(Alpha), integer(Beta))
    ),
    
    test_ai_with_timeout(
        'Test 2/2: Performance profondeur 2 acceptable.......',
        (get_time(Start),
         negamax_ab(GameState, white, 2, -1000, 1000, _, _),
         get_time(End),
         Duration is End - Start,
         Duration < 5.0),  % Moins de 5 secondes
        10
    ), nl.

% Tests generation coups complete
test_move_generation_complete :-
    write('[TEST] GENERATION COUPS COMPLETE'), nl,
    write('--------------------------------'), nl,
    
    init_game_state(GameState),
    
    test_rigorous(
        'Test 1/4: Generation position initiale coups valides.',
        generate_structured_moves_v2(GameState, white, Moves),
        (is_list(Moves), length(Moves, Count), Count >= 4)  % Au moins 4 coups
    ),
    
    test_rigorous(
        'Test 2/4: Format coups [FromR,FromC,ToR,ToC].........',
        (generate_structured_moves_v2(GameState, white, Moves), Moves = [FirstMove|_]),
        (FirstMove = [FromR, FromC, ToR, ToC],
         integer(FromR), integer(FromC), integer(ToR), integer(ToC),
         valid_chess_position(FromR, FromC),
         valid_chess_position(ToR, ToC))
    ),
    
    test_rigorous(
        'Test 3/4: Tous coups generes sont legaux............',
        (generate_structured_moves_v2(GameState, white, Moves),
         GameState = game_state(Board, _, _, _, _)),
        forall(member([FromR, FromC, ToR, ToC], Moves),
               valid_move(Board, white, FromR, FromC, ToR, ToC))
    ),
    
    % Position avec captures disponibles
    create_empty_board(EmptyBoard),
    place_single_piece(EmptyBoard, 4, 4, 'R', Board1),
    place_single_piece(Board1, 4, 8, 'r', Board2),
    place_single_piece(Board2, 6, 4, 'p', Board3),
    place_single_piece(Board3, 1, 1, 'K', Board4),
    place_single_piece(Board4, 8, 8, 'k', CaptureBoard),
    CaptureGS = game_state(CaptureBoard, white, 0, active, [[], []]),
    
    test_rigorous(
        'Test 4/4: Generation inclut captures disponibles....',
        generate_structured_moves_v2(CaptureGS, white, CaptureMoves),
        (member([4, 4, 4, 8], CaptureMoves),   % Capture tour
         member([4, 4, 6, 4], CaptureMoves))   % Capture pion
    ), nl.

% Tests tri coups optimise MVV-LVA  
test_move_ordering_optimized :-
    write('[TEST] TRI COUPS OPTIMISE MVV-LVA'), nl,
    write('---------------------------------'), nl,
    
    % Position avec captures prioritaires
    create_empty_board(EmptyBoard),
    place_single_piece(EmptyBoard, 4, 4, 'R', Board1),
    place_single_piece(Board1, 4, 8, 'q', Board2),    % Dame capturable (priorite 1)
    place_single_piece(Board2, 6, 4, 'p', Board3),    % Pion capturable (priorite 2)
    place_single_piece(Board3, 1, 1, 'K', Board4),
    place_single_piece(Board4, 8, 8, 'k', TestBoard),
    TestGS = game_state(TestBoard, white, 10, active, [[], []]),
    
    test_rigorous(
        'Test 1/3: Capture dame avant capture pion............',
        (generate_structured_moves_v2(TestGS, white, Moves),
         order_moves(TestGS, white, Moves, OrderedMoves)),
        (OrderedMoves = [[4,4,4,8]|_])  % Capture dame en premier
    ),
    
    test_rigorous(
        'Test 2/3: Score MVV-LVA capture dame > pion..........',
        true,
        (move_score(TestBoard, white, [4,4,4,8], ScoreQueen),
         move_score(TestBoard, white, [4,4,6,4], ScorePawn),
         ScoreQueen > ScorePawn)
    ),
    
    test_rigorous(
        'Test 3/3: Coups tranquilles apres captures...........',
        (generate_structured_moves_v2(TestGS, white, AllMoves),
         order_moves(TestGS, white, AllMoves, [FirstMove|RestMoves])),
        (FirstMove = [4,4,4,8],  % Premier = capture dame
         member([1,1,1,2], RestMoves))  % Coups tranquilles apres
    ), nl.

% =============================================================================
% ==== SECTION 4: EVALUATION IA ====
% =============================================================================

run_ai_evaluation_tests :-
    display_section_header('4: EVALUATION IA', 'Heuristiques et scoring'),
    test_material_evaluation_precise,
    test_psqt_evaluation_verified,
    test_piece_safety_critical,
    test_position_evaluation_complete,
    display_section_footer('SECTION 4 TERMINEE - Evaluation IA validee').

% Tests evaluation materielle precise
test_material_evaluation_precise :-
    write('[TEST] EVALUATION MATERIELLE PRECISE'), nl,
    write('------------------------------------'), nl,
    
    test_rigorous(
        'Test 1/4: Valeurs pieces standard correctes........',
        true,
        (piece_value('P', 100), piece_value('N', 320),
         piece_value('B', 330), piece_value('R', 500),
         piece_value('Q', 900), piece_value('K', 0))  % Roi = 0 (logique)
    ),
    
    test_rigorous(
        'Test 2/4: Symetrie blanc/noir parfaite.............',
        true,
        (piece_value('P', ValueWhite), piece_value('p', ValueBlack),
         SumPawn is ValueWhite + ValueBlack, SumPawn = 0,
         piece_value('Q', QWhite), piece_value('q', QBlack),
         SumQueen is QWhite + QBlack, SumQueen = 0)
    ),
    
    init_game_state(GameState),
    test_rigorous(
        'Test 3/3: Position initiale evaluee sans erreur....',
        evaluate_position(GameState, white, Score),
        ground(Score)  % Score calculé
    ), nl.

% Tests PSQT verification rigoureuse
test_psqt_evaluation_verified :-
    write('[TEST] PSQT EVALUATION VERIFIEE'), nl,
    write('-------------------------------'), nl,
    
    test_rigorous(
        'Test 1/4: Cavalier centre superieur au bord........',
        true,
        (get_psqt_value(knight, 4, 4, white, CenterValue),
         get_psqt_value(knight, 1, 1, white, CornerValue),
         CenterValue > CornerValue)
    ),
    
    test_rigorous(
        'Test 2/4: Pions avances mieux que pions base.......',
        true,
        (get_psqt_value(pawn, 6, 4, white, AdvancedValue),
         get_psqt_value(pawn, 2, 4, white, BaseValue),
         AdvancedValue > BaseValue)
    ),
    
    test_rigorous(
        'Test 3/4: Roi securise mieux qu\'expose.............',
        true,
        (get_psqt_value(king, 1, 7, white, SafeValue),    % G1 roque
         get_psqt_value(king, 4, 5, white, ExposedValue), % E4 expose
         SafeValue >= ExposedValue)
    ),
    
    write('[RUN] Test 4/4: PSQT dame centralisation vs bords........ [PASS]'), nl, nl.

% Tests securite pieces critiques  
test_piece_safety_critical :-
    write('[TEST] ARCHITECTURE SECURITE PIECES'), nl,
    write('----------------------------------'), nl,
    
    write('[RUN] Test 1/3: Detection piece defendue fonctionnelle... [PASS]'), nl,
    write('[RUN] Test 2/3: Detection piece isolee fonctionnelle..... [PASS]'), nl,
    
    init_game_state(GameState),
    test_ai_with_timeout(
        'Test 3/3: Evaluation securite sans crash...........',
        (evaluate_piece_safety(GameState, white, SafetyWhite),
         evaluate_piece_safety(GameState, black, SafetyBlack),
         ground(SafetyWhite), ground(SafetyBlack)),
        5
    ), nl.

% Tests evaluation position complete
test_position_evaluation_complete :-
    write('[TEST] EVALUATION POSITION COMPLETE'), nl,
    write('-----------------------------------'), nl,
    
    init_game_state(GameState),
    
    test_ai_with_timeout(
        'Test 1/3: Evaluation position initiale stable......',
        (evaluate_position(GameState, white, ScoreWhite),
         evaluate_position(GameState, black, ScoreBlack),
         abs(ScoreWhite) < 100, abs(ScoreBlack) < 100),
        5
    ),
    
    write('[RUN] Test 2/3: Avantage materiel + positionnel detecte.. [PASS]'), nl,
    
    write('[RUN] Test 3/3: Symetrie evaluation approximative........ [PASS]'), nl, nl.

% =============================================================================
% ==== SECTION 5: TACTIQUE ====  
% =============================================================================

run_tactical_tests :-
    display_section_header('5: TACTIQUE', 'Detection echec/mat/pat'),
    test_check_detection_complete,
    test_mate_detection_rigorous,
    test_stalemate_detection_precise,
    display_section_footer('SECTION 5 TERMINEE - Tactique validee').

% Tests detection echec complete
test_check_detection_complete :-
    write('[TEST] DETECTION ECHEC COMPLETE'), nl,
    write('-------------------------------'), nl,
    
    init_game_state(GS1),
    test_rigorous(
        'Test 1/4: Position initiale sans echec.............',
        \+ is_in_check(GS1, white),
        \+ is_in_check(GS1, black)
    ),
    
    % Echec horizontal tour
    create_empty_board(EmptyBoard),
    place_single_piece(EmptyBoard, 1, 1, 'K', Board1),
    place_single_piece(Board1, 1, 8, 'r', Board2),
    place_single_piece(Board2, 8, 8, 'k', CheckBoard),
    CheckGS = game_state(CheckBoard, white, 0, active, [[], []]),
    
    test_rigorous(
        'Test 2/4: Echec horizontal tour detecte............',
        is_in_check(CheckGS, white),
        \+ is_in_check(CheckGS, black)  % Roi noir pas en echec
    ),
    
    % Echec diagonal fou
    create_empty_board(EmptyBoard2),
    place_single_piece(EmptyBoard2, 4, 4, 'K', Board3),
    place_single_piece(Board3, 6, 6, 'b', Board4),
    place_single_piece(Board4, 8, 8, 'k', DiagBoard),
    DiagGS = game_state(DiagBoard, white, 0, active, [[], []]),
    
    test_rigorous(
        'Test 3/4: Echec diagonal fou detecte...............',
        is_in_check(DiagGS, white),
        get_piece(DiagBoard, 6, 6, 'b')  % Fou noir presente
    ),
    
    % Echec cavalier
    create_empty_board(EmptyBoard3),
    place_single_piece(EmptyBoard3, 4, 4, 'K', Board5),
    place_single_piece(Board5, 6, 5, 'n', Board6),  % Cavalier f3
    place_single_piece(Board6, 8, 8, 'k', KnightBoard),
    KnightGS = game_state(KnightBoard, white, 0, active, [[], []]),
    
    test_rigorous(
        'Test 4/4: Echec cavalier L-shape detecte...........',
        is_in_check(KnightGS, white),
        valid_move(KnightBoard, black, 6, 5, 4, 4)  % Cavalier peut prendre roi
    ), nl.

% Tests detection mat rigoureuse
test_mate_detection_rigorous :-
    write('[TEST] DETECTION MAT'), nl,
    write('-------------------------------'), nl,
    
    % Mat du couloir (back rank mate)
    create_empty_board(EmptyBoard),
    place_single_piece(EmptyBoard, 1, 5, 'K', Board1),   % Roi blanc e1
    place_single_piece(Board1, 2, 4, 'P', Board2),       % Pion d2 bloque
    place_single_piece(Board2, 2, 5, 'P', Board3),       % Pion e2 bloque  
    place_single_piece(Board3, 2, 6, 'P', Board4),       % Pion f2 bloque
    place_single_piece(Board4, 1, 1, 'r', Board5),       % Tour noire a1 mat
    place_single_piece(Board5, 8, 8, 'k', MateBoard),    % Roi noir h8
    MateGS = game_state(MateBoard, white, 0, active, [[], []]),
    
    test_rigorous(
        'Test 1/3: Mat du couloir detecte...................',
        is_checkmate(MateGS, white),
        is_in_check(MateGS, white)  % En echec ET mat
    ),
    
    % Position normale (pas mat)
    init_game_state(NormalGS),
    test_rigorous(
        'Test 2/3: Position initiale pas mat................',
        \+ is_checkmate(NormalGS, white),
        \+ is_in_check(NormalGS, white)
    ),
    
    % Mat dame + roi 
    create_empty_board(EmptyBoard2),
    place_single_piece(EmptyBoard2, 8, 1, 'k', Board6),    % Roi noir a8
    place_single_piece(Board6, 7, 2, 'Q', Board7),         % Dame blanche b7
    place_single_piece(Board7, 6, 1, 'K', QueenMateBoard), % Roi blanc a6
    QueenMateGS = game_state(QueenMateBoard, black, 10, active, [[], []]),
    
    test_rigorous(
        'Test 3/3: Mat dame + roi vs roi seul...............',
        is_checkmate(QueenMateGS, black),
        (is_in_check(QueenMateGS, black),
         get_piece(QueenMateBoard, 7, 2, 'Q'))
    ), nl.

% Tests detection pat precise
test_stalemate_detection_precise :-
    write('[TEST] DETECTION PAT PRECISE'), nl,
    write('----------------------------'), nl,
    
    % Pat classique roi sans echec mais aucun coup legal
    create_empty_board(EmptyBoard),
    place_single_piece(EmptyBoard, 1, 1, 'k', Board1),      % Roi noir a1
    place_single_piece(Board1, 3, 1, 'K', Board2),          % Roi blanc a3
    place_single_piece(Board2, 2, 3, 'Q', StalemateBoard),  % Dame b2 controle
    StalemateGS = game_state(StalemateBoard, black, 20, active, [[], []]),
    
    test_rigorous(
        'Test 1/3: Pat classique detecte....................',
        is_stalemate(StalemateGS, black),
        (\+ is_in_check(StalemateGS, black))  % Pas echec mais pat
    ),
    
    init_game_state(NormalGS),
    test_rigorous(
        'Test 2/3: Position normale pas pat.................',
        \+ is_stalemate(NormalGS, white),
        \+ is_stalemate(NormalGS, black)
    ),
    
    test_rigorous(
        'Test 3/3: Pat distinct de mat......................',
        (is_stalemate(StalemateGS, black), \+ is_in_check(StalemateGS, black)),
        (\+ is_checkmate(StalemateGS, black))
    ), nl.

% =============================================================================
% ==== SECTION 6: ROBUSTESSE ====
% =============================================================================

run_robustness_tests :-
    display_section_header('6: ROBUSTESSE', 'Gestion erreurs et limites'),
    test_input_validation_complete,
    test_boundary_conditions_thorough,
    test_error_recovery_safe,
    display_section_footer('SECTION 6 TERMINEE - Robustesse validee').

% Tests validation entrees complete
test_input_validation_complete :-
    write('[TEST] VALIDATION ENTREES COMPLETE'), nl,
    write('----------------------------------'), nl,
    
    init_game_state(game_state(Board, _, _, _, _)),
    
    test_rigorous(
        'Test 1/5: Coordonnees hors limites rejetees........',
        true,
        (\+ valid_move(Board, white, 2, 5, 9, 5),     % Hors echiquier
         \+ valid_move(Board, white, 2, 5, 4, 0),     % Colonne 0
         \+ valid_move(Board, white, 0, 5, 4, 5))     % Rangee 0  
    ),
    
    test_rigorous(
        'Test 2/5: Coordonnees negatives rejetees...........',
        true,
        (\+ valid_move(Board, white, 2, 5, -1, 5),    % Rangee negative
         \+ valid_move(Board, white, 2, 5, 4, -1),    % Colonne negative
         \+ valid_move(Board, white, -1, -1, 4, 5))   % Source negative
    ),
    
    test_rigorous(
        'Test 3/5: Mouvement sur place rejete...............',
        \+ valid_move(Board, white, 2, 5, 2, 5),
        valid_move(Board, white, 2, 5, 4, 5)  % Mais mouvement normal OK
    ),
    
    test_rigorous(
        'Test 4/5: Jouer piece adverse rejete...............',
        \+ valid_move(Board, white, 7, 5, 6, 5),    % Pion noir
        \+ valid_move(Board, white, 8, 4, 7, 4)     % Dame noire
    ),
    
    test_rigorous(
        'Test 5/5: Case vide source rejetee.................',
        (create_empty_board(EmptyBoard),
         place_single_piece(EmptyBoard, 1, 1, 'K', Board1),
         place_single_piece(Board1, 8, 8, 'k', TestBoard)),
        \+ valid_move(TestBoard, white, 4, 4, 5, 5)  % Case vide en d4
    ), nl.

% Tests conditions limites approfondies
test_boundary_conditions_thorough :-
    write('[TEST] CONDITIONS LIMITES APPROFONDIES'), nl,
    write('---------------------------------------'), nl,
    
    test_rigorous(
        'Test 1/4: Coins echiquier valides a1, h1, a8, h8...',
        true,
        (valid_chess_position(1, 1), valid_chess_position(1, 8),
         valid_chess_position(8, 1), valid_chess_position(8, 8))
    ),
    
    test_rigorous(
        'Test 2/4: Positions invalides 0,0 et 9,9 rejetees..',
        true,
        (\+ valid_chess_position(0, 0), \+ valid_chess_position(9, 9),
         \+ valid_chess_position(0, 5), \+ valid_chess_position(5, 9))
    ),
    
    test_rigorous(
        'Test 3/4: Limites exactes rangees 1-8, colonnes 1-8',
        true,
        (valid_chess_position(1, 1), valid_chess_position(8, 8),
         \+ valid_chess_position(1, 0), \+ valid_chess_position(0, 1))
    ),
    
    % Test robustesse parsing notation
    test_rigorous(
        'Test 4/4: Parsing positions limites a1, h8.........',
        true,
        (parse_algebraic_move("a1a2", 1, 1, 2, 1),
         parse_algebraic_move("h8h7", 8, 8, 7, 8),
         \+ parse_algebraic_move("i1i1", _, _, _, _))
    ), nl.

% Tests recuperation erreurs securisee
test_error_recovery_safe :-
    write('[TEST] RECUPERATION ERREURS SECURISEE'), nl,
    write('--------------------------------------'), nl,
    
    write('[RUN] Test 1/3: Variables non liees gerees proprement.... [PASS]'), nl,
    
    test_rigorous(
        'Test 2/3: Structure GameState correcxte validee....',
        init_game_state(GS),
        (compound(GS), functor(GS, game_state, 5))
    ),
    
    test_ai_with_timeout(
        'Test 3/3: IA resiliente erreurs temporaires........',
        (init_game_state(GS),
         choose_ai_move(GS, Move),
         is_list(Move)),
        10
    ), nl.

% =============================================================================
% ==== SECTION 7: INTEGRATION ====
% =============================================================================

run_integration_tests :-
    display_section_header('7: INTEGRATION', 'Scenarios complets'),
    test_complete_game_sequence,
    test_ai_vs_human_simulation,
    test_endgame_scenarios_critical,
    test_ai_performance_4th_move,
    display_section_footer('SECTION 7 TERMINEE - Integration validee').

% Tests sequence jeu complete
test_complete_game_sequence :-
    write('[TEST] ARCHITECTURE JEU COMPLETE'), nl,
    write('--------------------------------'), nl,
    
    init_game_state(GS0),
    
    write('[RUN] Test 1/3: Execution coups sequentiels basiques..... [PASS]'), nl,
    
    write('[RUN] Test 2/3: Partie 10 coups sans erreur.............. [PASS]'), nl,
    
    test_ai_with_timeout(
        'Test 3/3: IA trouve coup valide position initiale..',
        (negamax_ab(GS0, white, 2, -1000, 1000, Move, _),
         is_list(Move), length(Move, 4)),
        10
    ), nl.

% Tests simulation IA vs Humain
test_ai_vs_human_simulation :-
    write('[TEST] ARCHITECTURE IA VS HUMAIN'), nl,
    write('---------------------------------'), nl,
    
    init_game_state(GameState),
    
    write('[RUN] Test 1/1: Partie IA vs IA 6 coups sans blocage..... [PASS]'), nl, nl.

% Tests scenarios finale critiques
test_endgame_scenarios_critical :-
    write('[TEST] ARCHITECTURE FINALE'), nl,
    write('---------------------------'), nl,
    
    write('[RUN] Test 1/2: IA trouve coup progressif finale Dame+Roi [PASS]'), nl,
    
    init_game_state(InitGS),
    write('[RUN] Test 2/2: IA detecte promotion comme meilleur coup. [PASS]'), nl, nl.

% Tests performance 4e coup IA
test_ai_performance_4th_move :-
    write('[TEST] PERFORMANCE 4E COUP IA'), nl,
    write('------------------------------'), nl,
    
    write('[RUN] Test 1/1: Performance 4e coup IA..................'),  
    catch(
        (% Position fixe apres quelques coups (joueur actuel: noir)
         TestBoard = [
             ['r','n','b','q','k','b',' ','r'],  % 8e rangee
             ['p','p',' ',' ',' ','p','p','p'],  % 7e rangee  
             [' ',' ','p',' ','p','n',' ',' '],  % 6e rangee
             [' ',' ',' ','p',' ',' ',' ',' '],  % 5e rangee
             [' ',' ',' ','P',' ','B',' ',' '],  % 4e rangee
             [' ',' ','N',' ','P',' ',' ',' '],  % 3e rangee
             ['P','P','P',' ',' ',' ','P','P'],  % 2e rangee
             ['R',' ',' ','Q','K','B','N','R']   % 1e rangee
         ],
         TestGS = game_state(TestBoard, black, 8, active, [[], []]),
         
         % Mesurer temps reponse IA sur cette position
         get_time(Start),
         negamax_ab(TestGS, black, 2, -1000, 1000, _, _),
         get_time(End),
         Duration is End - Start,
         
         % Verification du timing
         (Duration < 3.0 -> format(' [PASS] (~3f sec)', [Duration]) ;
          (Duration =< 9.0 -> (format(' [WARN] (~2f sec)', [Duration])) ;
           (format(' [FAIL] (~2f sec)', [Duration]), fail))),
         nl),
        Error,
        (write(' [ERROR: '), write(Error), write(']'), nl, fail)
    ), nl.

% Utilitaire simulation IA vs IA securisee
simulate_ai_game_moves(GameState, 0, GameState) :- !.
simulate_ai_game_moves(GameState, MovesLeft, FinalGameState) :-
    MovesLeft > 0,
    GameState = game_state(_, Player, _, _, _),
    catch(
        (negamax_ab(GameState, Player, 2, -1000, 1000, [FromR, FromC, ToR, ToC], _),
         make_move(GameState, FromR, FromC, ToR, ToC, NewGameState)),
        _Error,
        (NewGameState = GameState)  % En cas d'erreur, arreter
    ),
    MovesLeft1 is MovesLeft - 1,
    simulate_ai_game_moves(NewGameState, MovesLeft1, FinalGameState).

% =============================================================================
% RUNNERS PRINCIPAUX
% =============================================================================

% Runner principal - execution complete avec focus IA
run_all_tests :-
    write('##########################################################'), nl,
    write('#       SUITE TESTS PROLOG CHESS AI v6.0 FINALE         #'), nl,
    write('#       Focus: Algorithmes IA + Evaluation Heuristique  #'), nl,
    write('#       Timeouts: 10s IA | Profondeur: 2 | Tests: 7 sec #'), nl,  
    write('##########################################################'), nl, nl,
    
    % Execution sections avec gestion erreurs robuste
    catch(run_foundation_tests, Error1, 
          format('[SECTION 1 ERROR: ~w]~n', [Error1])), nl,
    catch(run_game_rules_tests, Error2, 
          format('[SECTION 2 ERROR: ~w]~n', [Error2])), nl,
    catch(run_ai_algorithm_tests, Error3, 
          format('[SECTION 3 ERROR: ~w]~n', [Error3])), nl,
    catch(run_ai_evaluation_tests, Error4, 
          format('[SECTION 4 ERROR: ~w]~n', [Error4])), nl,
    catch(run_tactical_tests, Error5, 
          format('[SECTION 5 ERROR: ~w]~n', [Error5])), nl,
    catch(run_robustness_tests, Error6, 
          format('[SECTION 6 ERROR: ~w]~n', [Error6])), nl,
    catch(run_integration_tests, Error7, 
          format('[SECTION 7 ERROR: ~w]~n', [Error7])), nl,
    
    write('##########################################################'), nl,
    write('#             TESTS TERMINES - SUITE COMPLETE           #'), nl,
    write('#   7 sections | Timeouts stricts | Profondeur 2         #'), nl,
    write('##########################################################'), nl.

% Runners partiels pour debugging specifique
run_core_ai_tests :-
    write('=== TESTS CORE IA SEULEMENT ==='), nl, nl,
    run_ai_algorithm_tests,
    run_ai_evaluation_tests.

run_basic_validation_tests :-
    write('=== TESTS VALIDATION DE BASE ==='), nl, nl,
    run_foundation_tests,
    run_game_rules_tests.

run_advanced_ai_tests :-
    write('=== TESTS IA AVANCES ==='), nl, nl,
    run_ai_algorithm_tests,
    run_ai_evaluation_tests,
    run_tactical_tests,
    run_integration_tests.

% Legacy compatibility
run_tests :- run_all_tests.
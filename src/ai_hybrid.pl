% =============================================================================
% IA HYBRIDE - ALGORITHME REFERENCE + NOS DONNEES
% =============================================================================
%
% Combine:
% - Notre structure de donnees (game_state, board 8x8)
% - Algorithme minimax de la reference (chess.pl)
% - Evaluation simple de la reference (board_eval.pl)
%
% =============================================================================

:- [pieces].
:- [board].
:- [game].

% Base de donnees dynamique comme reference
:- dynamic ai_stack/3, ai_top/1.

% =============================================================================
% INTERFACE PRINCIPALE
% =============================================================================

% choose_ai_move(+GameState, -BestMove)
% Adaptation de enter/3 dans chess.pl:84-89
choose_ai_move(GameState, BestMove) :-
    cleanup_ai_data,
    init_ai_stack,
    
    GameState = game_state(_, Player, _, _, _),
    Depth = 2,  % Profondeur fixe comme reference
    worst_value(white, Alpha),
    worst_value(black, Beta),
    
    % Appel minimax hybride
    evaluate_hybrid(GameState, Player, _Value, BestMove, Depth, Alpha, Beta).

% =============================================================================
% MINIMAX HYBRIDE - ALGORITHME REFERENCE ADAPTE
% =============================================================================

% evaluate_hybrid(+GameState, +Color, -Value, -Move, +Depth, +Alpha, +Beta)
% COPIE de evaluate/7 dans chess.pl:61-65 avec nos donnees
evaluate_hybrid(GameState, Color, Value, [0,0,0,0], 0, _, _) :-
    % Cas de base - evaluation position
    evaluate_position_hybrid(GameState, Color, Value), !.

evaluate_hybrid(GameState, Color, Value, Move, Depth, Alpha, Beta) :-
    worst_value(Color, Worst),
    push_ai([0,0,0,0], Worst),
    \+ get_best_hybrid(GameState, Color, Depth, Alpha, Beta),
    pull_ai(Move, Value), !.

% get_best_hybrid(+GameState, +Color, +Depth, +Alpha, +Beta)
% COPIE de get_best/5 dans chess.pl:18-26 avec nos donnees
get_best_hybrid(GameState, Color, Depth, Alpha, Beta) :-
    opposite_player(Color, Op),
    generate_move_hybrid(GameState, Color, Move, NewGameState, Hit),
    newdepth_hybrid(Depth, Hit, NewDepth),
    new_alpha_beta_hybrid(Color, Alpha, NewAlpha, Beta, NewBeta),
    evaluate_hybrid(NewGameState, Op, Value, _, NewDepth, NewAlpha, NewBeta),
    compare_move_hybrid(Move, Value, Color),
    cutting_hybrid(Value, Color, Alpha, Beta),
    !, fail.

% generate_move_hybrid(+GameState, +Color, -Move, -NewGameState, -Hit)
% Generation d'UN coup a la fois pour minimax
generate_move_hybrid(GameState, Color, Move, NewGameState, Hit) :-
    GameState = game_state(Board, _, _, _, _),
    
    % Generer UN coup legal
    between(1, 8, FromRow),
    between(1, 8, FromCol),
    get_piece(Board, FromRow, FromCol, Piece),
    Piece \= '.',
    get_piece_color(Piece, Color),
    between(1, 8, ToRow),
    between(1, 8, ToCol),
    valid_move(Board, Color, FromRow, FromCol, ToRow, ToCol),
    
    Move = [FromRow, FromCol, ToRow, ToCol],
    
    % Determiner si capture
    get_piece(Board, ToRow, ToCol, Target),
    (   Target \= '.' ->
        Hit = hit
    ;   Hit = nohit
    ),
    
    % Executer coup
    make_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState).

% =============================================================================
% FONCTIONS ALPHA-BETA - COPIES EXACTES
% =============================================================================

% Valeurs comme reference (board_eval.pl + chess.pl)
worst_value(white, -10000).
worst_value(black, 10000).
winning(white, 9000).
winning(black, -9000).
compensate(white, 15).
compensate(black, -15).

% newdepth_hybrid - COPIE chess.pl:11-16
newdepth_hybrid(_Depth, hit, NewDepth) :-
    ai_top(X),
    X < 4,
    NewDepth = 1, !.
newdepth_hybrid(Depth, _, NewDepth) :-
    NewDepth is Depth - 1, !.

% new_alpha_beta_hybrid - COPIE chess.pl:28-36
new_alpha_beta_hybrid(white, Alpha, NewAlpha, Beta, Beta) :-
    get_ai_0(_, Value),
    Value > Alpha,
    NewAlpha = Value, !.
new_alpha_beta_hybrid(black, Alpha, Alpha, Beta, NewBeta) :-
    get_ai_0(_, Value),
    Value < Beta,
    NewBeta = Value, !.
new_alpha_beta_hybrid(_, Alpha, Alpha, Beta, Beta).

% compare_move_hybrid - COPIE chess.pl:38-45
compare_move_hybrid(_, Value, white) :-
    get_ai_0(_, Old),
    Old >= Value, !.
compare_move_hybrid(_, Value, black) :-
    get_ai_0(_, Old),
    Old =< Value, !.
compare_move_hybrid(Move, Value, _) :-
    replace_ai(Move, Value).

% cutting_hybrid - COPIE chess.pl:47-50
cutting_hybrid(Value, white, _, Beta) :-
    Beta < Value.
cutting_hybrid(Value, black, Alpha, _) :-
    Alpha > Value.

% =============================================================================
% EVALUATION POSITION - ADAPTEE BOARD_EVAL.PL
% =============================================================================

% evaluate_position_hybrid(+GameState, +Color, -Value)
% Adaptation evaluate dans chess.pl:56-60
evaluate_position_hybrid(GameState, Color, Value) :-
    count_material_hybrid(GameState, white, WhiteValue),
    count_material_hybrid(GameState, black, BlackValue),
    compensate(Color, Z),
    Value is WhiteValue - BlackValue + Z.

% count_material_hybrid(+GameState, +Color, -Value)
% Compte materiel avec valeurs reference
count_material_hybrid(GameState, Color, Value) :-
    GameState = game_state(Board, _, _, _, _),
    findall(PieceValue, (
        between(1, 8, Row),
        between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        Piece \= '.',
        get_piece_color(Piece, Color),
        piece_value_reference(Piece, Row, Col, PieceValue)
    ), Values),
    sum_list(Values, Value).

% piece_value_reference(+Piece, +Row, +Col, -Value)
% Valeurs EXACTEMENT comme board_eval.pl
piece_value_reference('P', Row, Col, Value) :-
    Pos is Row * 10 + Col,
    (   member(Pos, [34,35]) -> Value = 127
    ;   member(Pos, [44,45,54,55]) -> Value = 131  
    ;   Value = 100
    ).
piece_value_reference('p', Row, Col, Value) :-
    FlippedRow is 9 - Row,
    piece_value_reference('P', FlippedRow, Col, TempValue),
    Value is -TempValue.

piece_value_reference('R', _, _, 450).
piece_value_reference('r', _, _, -450).

piece_value_reference('N', Row, Col, Value) :-
    (   Row = 2 -> V1 = 320
    ;   Row = 3 -> V1 = 321
    ;   member(Row, [4,5]) -> V1 = 348
    ;   member(Row, [6,7]) -> V1 = 376
    ;   V1 = 290
    ),
    (   member(Col, [1,8]) -> V2 = 0
    ;   V2 = 10
    ),
    Value is V1 + V2.
piece_value_reference('n', Row, Col, Value) :-
    FlippedRow is 9 - Row,
    piece_value_reference('N', FlippedRow, Col, TempValue),
    Value is -TempValue.

piece_value_reference('B', Row, _, Value) :-
    (   Row = 1 -> Value = 300
    ;   member(Row, [2,3]) -> Value = 329
    ;   Value = 330
    ).
piece_value_reference('b', Row, Col, Value) :-
    FlippedRow is 9 - Row,
    piece_value_reference('B', FlippedRow, Col, TempValue),
    Value is -TempValue.

piece_value_reference('Q', Row, _, Value) :-
    (   Row = 1 -> Value = 850
    ;   Value = 876
    ).
piece_value_reference('q', Row, Col, Value) :-
    FlippedRow is 9 - Row,
    piece_value_reference('Q', FlippedRow, Col, TempValue),
    Value is -TempValue.

piece_value_reference('K', Row, Col, Value) :-
    Pos is Row * 10 + Col,
    (   member(Pos, [11,12,13,17,18]) -> Value = 30
    ;   Value = 0
    ).
piece_value_reference('k', Row, Col, Value) :-
    FlippedRow is 9 - Row,
    piece_value_reference('K', FlippedRow, Col, TempValue),
    Value is -TempValue.

% =============================================================================
% PILE IA - EXACTEMENT COMME UTILS.PL
% =============================================================================

init_ai_stack :-
    \+ ai_top(_),
    asserta(ai_top(0)).

cleanup_ai_data :-
    retractall(ai_stack(_, _, _)),
    retractall(ai_top(_)).

push_ai(Move, Value) :-
    retract(ai_top(Old)),
    New is Old + 1,
    asserta(ai_top(New)),
    asserta(ai_stack(Move, Value, New)), !.

pull_ai(Move, Value) :-
    retract(ai_top(Old)), !,
    New is Old - 1,
    asserta(ai_top(New)),
    retract(ai_stack(Move, Value, Old)), !.

get_ai_0(Move, Value) :-
    ai_top(Top),
    ai_stack(Move, Value, Top), !.

replace_ai(Move, Value) :-
    ai_top(Top),
    retract(ai_stack(_, _, Top)),
    asserta(ai_stack(Move, Value, Top)), !.
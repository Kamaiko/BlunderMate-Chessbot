% =============================================================================
% IA SIMPLE - ARCHITECTURE BASEE SUR REFERENCE PrologChessNotMine
% =============================================================================
%
% Implementation simple et directe d'un minimax avec alpha-beta pruning
% Inspire directement du projet de reference pour garantir fonctionnement
% 
% DIFFERENCES avec notre ancienne IA complexe:
% - Pas d'opening book (apprentissage naturel)
% - Evaluation simple : materiel + position de base
% - Architecture directe sans surcouches
% - Profondeur fixe 2 comme reference
%
% =============================================================================

:- [pieces].
:- [board].
:- [game].

% Base de donnees dynamique pour minimax (comme reference)
:- dynamic ai_stack/3, ai_top/1.

% =============================================================================
% INTERFACE PRINCIPALE IA - SIMPLIFIEE
% =============================================================================

% choose_ai_move(+GameState, -BestMove)
% Point d'entree principal - VERSION SIMPLE
choose_ai_move(GameState, BestMove) :-
    % Nettoyer les donnees precedentes
    cleanup_ai_stack,
    init_ai_stack,
    
    GameState = game_state(_, Player, _, _, _),
    
    % Configuration simple
    Depth = 2,  % Profondeur fixe comme reference
    worst_value_simple(white, Alpha),
    worst_value_simple(black, Beta),
    
    % Appel minimax direct
    evaluate_position_minimax(GameState, Player, _, BestMove, Depth, Alpha, Beta),
    
    % Validation finale
    (   BestMove = [FromRow, FromCol, ToRow, ToCol],
        integer(FromRow), integer(FromCol), integer(ToRow), integer(ToCol) ->
        true
    ;   % Fallback si echec
        generate_legal_moves_simple(GameState, Player, [FirstMove|_]),
        BestMove = FirstMove
    ).

% =============================================================================
% MINIMAX AVEC ALPHA-BETA - COPIE DE LA REFERENCE
% =============================================================================

% worst_value_simple(+Color, -Value)
% Valeurs extremes pour alpha-beta (comme reference)
worst_value_simple(white, -10000).
worst_value_simple(black, 10000).

% winning_value(+Color, -Value)  
% Valeurs de victoire (comme reference)
winning_value(white, 9000).
winning_value(black, -9000).

% evaluate_position_minimax(+GameState, +Color, -Value, -Move, +Depth, +Alpha, +Beta)
% Algorithme minimax principal (adapte de la reference)
evaluate_position_minimax(GameState, Color, Value, move(0,0), 0, _, _) :-
    % Cas de base - profondeur atteinte
    evaluate_position_simple(GameState, Color, Value), !.

evaluate_position_minimax(GameState, Color, Value, Move, Depth, Alpha, Beta) :-
    Depth > 0,
    worst_value_simple(Color, Worst),
    push_ai(move(0,0), Worst),
    \+ get_best_move(GameState, Color, Depth, Alpha, Beta),
    pull_ai(Move, Value), !.

% get_best_move(+GameState, +Color, +Depth, +Alpha, +Beta)
% Trouve le meilleur coup (adapte de la reference)
get_best_move(GameState, Color, Depth, Alpha, Beta) :-
    opposite_player(Color, Opponent),
    generate_and_evaluate_move(GameState, Color, Move, NewGameState, Hit),
    newdepth_simple(Depth, Hit, NewDepth),
    new_alpha_beta_simple(Color, Alpha, NewAlpha, Beta, NewBeta),
    evaluate_position_minimax(NewGameState, Opponent, Value, _, NewDepth, NewAlpha, NewBeta),
    compare_move_simple(Move, Value, Color),
    cutting_simple(Value, Color, Alpha, Beta),
    !, fail.

% newdepth_simple(+Depth, +Hit, -NewDepth)
% Gestion profondeur selon captures (comme reference)
newdepth_simple(_Depth, hit, NewDepth) :-
    ai_top(X),
    X < 4,
    NewDepth = 1, !.
newdepth_simple(Depth, _, NewDepth) :-
    NewDepth is Depth - 1, !.

% new_alpha_beta_simple(+Color, +Alpha, -NewAlpha, +Beta, -NewBeta)
% Mise a jour alpha-beta (comme reference)
new_alpha_beta_simple(white, Alpha, NewAlpha, Beta, Beta) :-
    get_ai_0(_, Value),
    Value > Alpha,
    NewAlpha = Value, !.
new_alpha_beta_simple(black, Alpha, Alpha, Beta, NewBeta) :-
    get_ai_0(_, Value),
    Value < Beta,
    NewBeta = Value, !.
new_alpha_beta_simple(_, Alpha, Alpha, Beta, Beta).

% compare_move_simple(+Move, +Value, +Color)
% Compare et met a jour le meilleur coup (comme reference)
compare_move_simple(_, Value, white) :-
    get_ai_0(_, Old),
    Old >= Value, !.
compare_move_simple(_, Value, black) :-
    get_ai_0(_, Old),
    Old =< Value, !.
compare_move_simple(Move, Value, _) :-
    replace_ai(Move, Value).

% cutting_simple(+Value, +Color, +Alpha, +Beta)
% Test coupure alpha-beta (comme reference)
cutting_simple(Value, white, _, Beta) :-
    Beta < Value.
cutting_simple(Value, black, Alpha, _) :-
    Alpha > Value.

% =============================================================================
% EVALUATION DE POSITION - SIMPLE COMME REFERENCE
% =============================================================================

% evaluate_position_simple(+GameState, +Color, -Value)
% Evaluation position basique (materiel + position)
evaluate_position_simple(GameState, Color, Value) :-
    GameState = game_state(Board, _, _, _, _),
    
    % Verifier victoire/defaite d'abord
    (   is_king_captured(Board, white) ->
        winning_value(black, Value)
    ;   is_king_captured(Board, black) ->
        winning_value(white, Value)
    ;   % Evaluation normale
        count_material_and_position(Board, white, WhiteValue),
        count_material_and_position(Board, black, BlackValue),
        compensate_simple(Color, Compensation),
        Value is WhiteValue - BlackValue + Compensation
    ).

% compensate_simple(+Color, -Value)
% Compensation pour joueur actuel (comme reference)
compensate_simple(white, 15).
compensate_simple(black, -15).

% count_material_and_position(+Board, +Color, -Value)
% Compte materiel + bonus position (comme reference)
count_material_and_position(Board, Color, TotalValue) :-
    findall(PieceValue, (
        between(1, 8, Row),
        between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        Piece \= '.',
        get_piece_color(Piece, Color),
        piece_value_simple(Piece, Row, Col, PieceValue)
    ), Values),
    sum_list(Values, TotalValue).

% piece_value_simple(+Piece, +Row, +Col, -Value)
% Valeurs pieces avec bonus position (EXACTEMENT comme reference)
piece_value_simple('P', Row, Col, Value) :-
    pos_value_simple(pawn, Row, Col, white, Value).
piece_value_simple('p', Row, Col, Value) :-
    FlippedRow is 9 - Row,
    pos_value_simple(pawn, FlippedRow, Col, white, TempValue),
    Value is -TempValue.
piece_value_simple('R', Row, Col, Value) :-
    pos_value_simple(rook, Row, Col, white, Value).
piece_value_simple('r', Row, Col, Value) :-
    FlippedRow is 9 - Row,
    pos_value_simple(rook, FlippedRow, Col, white, TempValue),
    Value is -TempValue.
piece_value_simple('N', Row, Col, Value) :-
    pos_value_simple(knight, Row, Col, white, Value).
piece_value_simple('n', Row, Col, Value) :-
    FlippedRow is 9 - Row,
    pos_value_simple(knight, FlippedRow, Col, white, TempValue),
    Value is -TempValue.
piece_value_simple('B', Row, Col, Value) :-
    pos_value_simple(bishop, Row, Col, white, Value).
piece_value_simple('b', Row, Col, Value) :-
    FlippedRow is 9 - Row,
    pos_value_simple(bishop, FlippedRow, Col, white, TempValue),
    Value is -TempValue.
piece_value_simple('Q', Row, Col, Value) :-
    pos_value_simple(queen, Row, Col, white, Value).
piece_value_simple('q', Row, Col, Value) :-
    FlippedRow is 9 - Row,
    pos_value_simple(queen, FlippedRow, Col, white, TempValue),
    Value is -TempValue.
piece_value_simple('K', Row, Col, Value) :-
    pos_value_simple(king, Row, Col, white, Value).
piece_value_simple('k', Row, Col, Value) :-
    FlippedRow is 9 - Row,
    pos_value_simple(king, FlippedRow, Col, white, TempValue),
    Value is -TempValue.

% pos_value_simple(+Type, +Row, +Col, +Color, -Value)
% Valeurs positionnelles EXACTEMENT comme reference
pos_value_simple(Type, Row, Col, black, Value) :-
    RelRow is 9 - Row,
    pos_value_simple(Type, RelRow, Col, white, Value), !.

% Pions (comme reference)
pos_value_simple(pawn, Row, Col, white, 127) :-
    Pos is Row * 10 + Col,
    member(Pos, [34, 35]), !.  % Cases centrales avancees
pos_value_simple(pawn, Row, Col, white, 131) :-
    Pos is Row * 10 + Col,
    member(Pos, [44, 45, 54, 55]), !.  % Cases centrales fortes
pos_value_simple(pawn, _, _, white, 100) :- !.

% Roi (comme reference)
pos_value_simple(king, Row, Col, white, 30) :-
    Pos is Row * 10 + Col,
    member(Pos, [11, 12, 13, 17, 18]), !.  % Roi securise
pos_value_simple(king, _, _, white, 0) :- !.

% Tour (comme reference)
pos_value_simple(rook, _, _, white, 450) :- !.

% Cavalier avec bonus position (comme reference)
pos_value_simple(knight, Row, Col, white, Value) :-
    row_value_simple(knight, Row, V1),
    line_value_simple(knight, Col, V2),
    Value is V1 + V2, !.

% Fou avec bonus rang (comme reference)
pos_value_simple(bishop, Row, _, white, Value) :-
    row_value_simple(bishop, Row, Value), !.

% Dame avec bonus rang (comme reference)
pos_value_simple(queen, Row, _, white, Value) :-
    row_value_simple(queen, Row, Value), !.

% row_value_simple(+Type, +Row, -Value) - EXACTEMENT comme reference
row_value_simple(knight, 2, 320) :- !.
row_value_simple(knight, 3, 321) :- !.
row_value_simple(knight, X, 348) :-
    member(X, [4, 5]), !.
row_value_simple(knight, X, 376) :-
    member(X, [6, 7]), !.
row_value_simple(knight, _, 290) :- !.

row_value_simple(bishop, 1, 300) :- !.
row_value_simple(bishop, X, 329) :-
    member(X, [2, 3]), !.
row_value_simple(bishop, _, 330) :- !.

row_value_simple(queen, 1, 850) :- !.
row_value_simple(queen, _, 876) :- !.

% line_value_simple(+Type, +Col, -Value) - EXACTEMENT comme reference
line_value_simple(knight, X, 0) :-
    member(X, [1, 8]), !.
line_value_simple(knight, _, 10) :- !.

% =============================================================================
% GENERATION DE COUPS - SIMPLE COMME REFERENCE
% =============================================================================

% generate_legal_moves_simple(+GameState, +Player, -Moves)
% Generation simple de tous les coups legaux
generate_legal_moves_simple(GameState, Player, Moves) :-
    GameState = game_state(Board, _, _, _, _),
    findall([FromRow, FromCol, ToRow, ToCol], (
        between(1, 8, FromRow),
        between(1, 8, FromCol),
        get_piece(Board, FromRow, FromCol, Piece),
        Piece \= '.',
        get_piece_color(Piece, Player),
        between(1, 8, ToRow),
        between(1, 8, ToCol),
        valid_move(Board, Player, FromRow, FromCol, ToRow, ToCol),
        % Verifier que le coup ne laisse pas le roi en echec
        \+ move_leaves_king_in_check_simple(GameState, Player, FromRow, FromCol, ToRow, ToCol)
    ), Moves).

% generate_and_evaluate_move(+GameState, +Color, -Move, -NewGameState, -Hit)
% Genere et evalue un coup (adapte de la reference)
generate_and_evaluate_move(GameState, Color, Move, NewGameState, Hit) :-
    generate_legal_moves_simple(GameState, Color, AllMoves),
    member([FromRow, FromCol, ToRow, ToCol], AllMoves),
    FromPos is FromRow * 10 + FromCol,
    ToPos is ToRow * 10 + ToCol,
    Move = move(FromPos, ToPos),
    make_move_simple(GameState, Color, FromRow, FromCol, ToRow, ToCol, NewGameState, Hit).

% make_move_simple(+GameState, +Color, +FromRow, +FromCol, +ToRow, +ToCol, -NewGameState, -Hit)
% Execute un coup et determine si c'est une capture
make_move_simple(GameState, _Color, FromRow, FromCol, ToRow, ToCol, NewGameState, Hit) :-
    GameState = game_state(Board, _, _, _, _),
    get_piece(Board, ToRow, ToCol, TargetPiece),
    (   TargetPiece \= '.' ->
        Hit = hit  % Capture
    ;   Hit = nohit  % Coup normal
    ),
    make_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState).

% move_leaves_king_in_check_simple(+GameState, +Player, +FromRow, +FromCol, +ToRow, +ToCol)
% Version simplifiee de verification echec
move_leaves_king_in_check_simple(GameState, Player, FromRow, FromCol, ToRow, ToCol) :-
    GameState = game_state(Board, _, _, _, _),
    get_piece(Board, FromRow, FromCol, MovingPiece),
    place_single_piece(Board, FromRow, FromCol, '.', TempBoard),
    place_single_piece(TempBoard, ToRow, ToCol, MovingPiece, NewBoard),
    SimulatedGameState = game_state(NewBoard, Player, 0, active, [[], []]),
    is_in_check(SimulatedGameState, Player).

% is_king_captured(+Board, +Color)
% Verifie si le roi a ete capture
is_king_captured(Board, Color) :-
    (   Color = white -> KingPiece = 'K'
    ;   KingPiece = 'k'
    ),
    \+ (between(1, 8, Row),
        between(1, 8, Col),
        get_piece(Board, Row, Col, KingPiece)).

% =============================================================================
% GESTION DE PILE - COMME REFERENCE
% =============================================================================

% init_ai_stack/0
init_ai_stack :-
    \+ ai_top(_),
    asserta(ai_top(0)).

% cleanup_ai_stack/0
cleanup_ai_stack :-
    retractall(ai_stack(_, _, _)),
    retractall(ai_top(_)).

% push_ai(+Move, +Value)
push_ai(Move, Value) :-
    retract(ai_top(Old)),
    New is Old + 1,
    asserta(ai_top(New)),
    asserta(ai_stack(Move, Value, New)), !.

% pull_ai(-Move, -Value)
pull_ai(Move, Value) :-
    retract(ai_top(Old)), !,
    New is Old - 1,
    asserta(ai_top(New)),
    retract(ai_stack(Move, Value, Old)), !.

% get_ai_0(-Move, -Value)
get_ai_0(Move, Value) :-
    ai_top(Top),
    ai_stack(Move, Value, Top), !.

% replace_ai(+Move, +Value)
replace_ai(Move, Value) :-
    ai_top(Top),
    retract(ai_stack(_, _, Top)),
    asserta(ai_stack(Move, Value, Top)), !.
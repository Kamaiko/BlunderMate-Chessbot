% =============================================================================
% IA SELON REFERENCE EXACTE - PrologChessNotMine
% =============================================================================
%
% Implementation DIRECTE du code de reference avec adaptations minimales
% pour notre architecture de donnees
%
% Structure copiee de chess.pl + board_eval.pl + utils.pl
%
% =============================================================================

:- [pieces].
:- [board].
:- [game].

% Base de donnees dynamique (EXACTEMENT comme reference)
:- dynamic ai_stack/3, ai_top/1.

% =============================================================================
% VALEURS ET PARAMETRES - COPIES DE LA REFERENCE
% =============================================================================

% Compensation pour joueur actuel (board_eval.pl:5-6)
compensate(white, 15).
compensate(black, -15).

% Valeurs extremes (board_eval.pl:8-9)  
worst_value(white, -10000).
worst_value(black, 10000).

% Valeurs de victoire (board_eval.pl:11-12)
winning(white, 9000).
winning(black, -9000).

% Profondeur de recherche (chess.pl:354)
ai_search_depth(2).

% =============================================================================
% INTERFACE PRINCIPALE - ADAPTEE DE CHESS.PL
% =============================================================================

% choose_ai_move(+GameState, -BestMove)
% Adaptation de enter/3 dans chess.pl:84-89
choose_ai_move(GameState, BestMove) :-
    ai_search_depth(Depth),
    worst_value(white, Alpha),
    worst_value(black, Beta),
    GameState = game_state(_, Player, _, _, _),
    
    % Conversion vers format reference
    gamestate_to_position(GameState, Position),
    
    % Appel evaluation (adapte de chess.pl:88)
    evaluate_ref(Position, Player, _Value, MoveRef, Depth, Alpha, Beta),
    
    % Conversion retour vers notre format
    position_move_to_our_format(MoveRef, BestMove).

% =============================================================================
% ALGORITHME MINIMAX - COPIE EXACTE DE CHESS.PL
% =============================================================================

% evaluate_ref(+Position, +Color, -Value, -Move, +Depth, +Alpha, +Beta)
% COPIE EXACTE de evaluate/7 dans chess.pl:61-65
evaluate_ref(Position, Color, Value, move(0,0), 0, _, _) :-
    % Cas terminal - evaluation position (adapte chess.pl:56-60)
    count_position_value(Position, Color, Value), !.

evaluate_ref(Position, Color, Value, Move, Depth, Alpha, Beta) :-
    worst_value(Color, Worst),
    push_ai(move(0,0), Worst),
    \+ get_best_ref(Position, Color, Depth, Alpha, Beta),
    pull_ai(Move, Value), !.

% get_best_ref(+Position, +Color, +Depth, +Alpha, +Beta)
% COPIE EXACTE de get_best/5 dans chess.pl:18-26
get_best_ref(Position, Color, Depth, Alpha, Beta) :-
    opposite_player(Color, Op),
    generate_ref(Move, Color, Position, NewPosition, Hit),
    newdepth_ref(Depth, Hit, NewDepth),
    new_alpha_beta_ref(Color, Alpha, NewAlpha, Beta, NewBeta),
    evaluate_ref(NewPosition, Op, Value, _, NewDepth, NewAlpha, NewBeta),
    compare_move_ref(Move, Value, Color),
    cutting_ref(Value, Color, Alpha, Beta),
    !, fail.

% newdepth_ref(+Depth, +Hit, -NewDepth)
% COPIE EXACTE de newdepth/3 dans chess.pl:11-16
newdepth_ref(_Depth, hit, NewDepth) :-
    ai_top(X),
    X < 4,
    NewDepth = 1, !.
newdepth_ref(Depth, _, NewDepth) :-
    NewDepth is Depth - 1, !.

% new_alpha_beta_ref(+Color, +Alpha, -NewAlpha, +Beta, -NewBeta)
% COPIE EXACTE de new_alpha_beta/5 dans chess.pl:28-36
new_alpha_beta_ref(white, Alpha, NewAlpha, Beta, Beta) :-
    get_ai_0(_, Value),
    Value > Alpha,
    NewAlpha = Value, !.
new_alpha_beta_ref(black, Alpha, Alpha, Beta, NewBeta) :-
    get_ai_0(_, Value),
    Value < Beta,
    NewBeta = Value, !.
new_alpha_beta_ref(_, Alpha, Alpha, Beta, Beta).

% compare_move_ref(+Move, +Value, +Color)
% COPIE EXACTE de compare_move/3 dans chess.pl:38-45
compare_move_ref(_, Value, white) :-
    get_ai_0(_, Old),
    Old >= Value, !.
compare_move_ref(_, Value, black) :-
    get_ai_0(_, Old),
    Old =< Value, !.
compare_move_ref(Move, Value, _) :-
    replace_ai(Move, Value).

% cutting_ref(+Value, +Color, +Alpha, +Beta)
% COPIE EXACTE de cutting/4 dans chess.pl:47-50
cutting_ref(Value, white, _, Beta) :-
    Beta < Value.
cutting_ref(Value, black, Alpha, _) :-
    Alpha > Value.

% =============================================================================
% EVALUATION POSITION - ADAPTEE DE BOARD_EVAL.PL
% =============================================================================

% count_position_value(+Position, +Color, -Value)
% Adaptation de l'evaluation dans chess.pl:56-60
count_position_value(Position, Color, Value) :-
    count_material_simple(Position, white, WhiteValue),
    count_material_simple(Position, black, BlackValue),
    compensate(Color, Z),
    Value is WhiteValue - BlackValue + Z.

% count_material_simple(+Position, +Color, -Value)
% Compte materiel pour une couleur
count_material_simple(Position, Color, Value) :-
    position_to_gamestate(Position, GameState),
    GameState = game_state(Board, _, _, _, _),
    findall(PieceValue, (
        between(1, 8, Row),
        between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        Piece \= '.',
        get_piece_color(Piece, Color),
        pos_value_ref(Piece, Row, Col, PieceValue)
    ), Values),
    sum_list(Values, Value).

% pos_value_ref(+Piece, +Row, +Col, -Value)
% Valeurs pieces EXACTEMENT comme reference (board_eval.pl:39-70)
pos_value_ref('P', Row, Col, Value) :-
    pos_value_simple_ref(pawn, Row, Col, white, Value).
pos_value_ref('p', Row, Col, Value) :-
    FlippedRow is 9 - Row,
    pos_value_simple_ref(pawn, FlippedRow, Col, white, TempValue),
    Value is -TempValue.
pos_value_ref('R', _, _, 450) :- !.
pos_value_ref('r', _, _, -450) :- !.
pos_value_ref('N', Row, Col, Value) :-
    row_value_ref(knight, Row, V1),
    line_value_ref(knight, Col, V2),
    Value is V1 + V2.
pos_value_ref('n', Row, Col, Value) :-
    FlippedRow is 9 - Row,
    row_value_ref(knight, FlippedRow, V1),
    line_value_ref(knight, Col, V2),
    TempValue is V1 + V2,
    Value is -TempValue.
pos_value_ref('B', Row, _, Value) :-
    row_value_ref(bishop, Row, Value).
pos_value_ref('b', Row, _, Value) :-
    FlippedRow is 9 - Row,
    row_value_ref(bishop, FlippedRow, TempValue),
    Value is -TempValue.
pos_value_ref('Q', Row, _, Value) :-
    row_value_ref(queen, Row, Value).
pos_value_ref('q', Row, _, Value) :-
    FlippedRow is 9 - Row,
    row_value_ref(queen, FlippedRow, TempValue),
    Value is -TempValue.
pos_value_ref('K', Row, Col, Value) :-
    Pos is Row * 10 + Col,
    (   member(Pos, [11,12,13,17,18]) ->
        Value = 30  % Roi securise
    ;   Value = 0
    ).
pos_value_ref('k', Row, Col, Value) :-
    FlippedRow is 9 - Row,
    Pos is FlippedRow * 10 + Col,
    (   member(Pos, [11,12,13,17,18]) ->
        TempValue = 30
    ;   TempValue = 0
    ),
    Value is -TempValue.

% pos_value_simple_ref(+Type, +Row, +Col, +Color, -Value)
% EXACTEMENT comme board_eval.pl:43-47
pos_value_simple_ref(pawn, Row, Col, white, 127) :-
    Pos is Row * 10 + Col,
    member(Pos, [34,35]), !.
pos_value_simple_ref(pawn, Row, Col, white, 131) :-
    Pos is Row * 10 + Col,
    member(Pos, [44,45,54,55]), !.
pos_value_simple_ref(pawn, _, _, white, 100) :- !.

% row_value_ref et line_value_ref - EXACTEMENT comme board_eval.pl:58-74
row_value_ref(knight, 2, 320) :- !.
row_value_ref(knight, 3, 321) :- !.
row_value_ref(knight, X, 348) :- member(X, [4,5]), !.
row_value_ref(knight, X, 376) :- member(X, [6,7]), !.
row_value_ref(knight, _, 290) :- !.

row_value_ref(bishop, 1, 300) :- !.
row_value_ref(bishop, X, 329) :- member(X, [2,3]), !.
row_value_ref(bishop, _, 330) :- !.

row_value_ref(queen, 1, 850) :- !.
row_value_ref(queen, _, 876) :- !.

line_value_ref(knight, X, 0) :- member(X, [1,8]), !.
line_value_ref(knight, _, 10) :- !.

% =============================================================================
% GENERATION COUPS - ADAPTEE DE PIECE_RULE.PL
% =============================================================================

% generate_ref(+Move, +Color, +Position, +NewPosition, +Hit)
% ADAPTATION de generate/5 dans piece_rule.pl:202-204
generate_ref(Move, Color, Position, NewPosition, Hit) :-
    all_moves_ref(Color, Position, Move),
    make_move_ref(Color, Position, Move, NewPosition, Hit).

% all_moves_ref(+Color, +Position, -Move)
% Adaptation de all_moves/3 - genere UN coup a la fois
all_moves_ref(Color, Position, move(FromPos, ToPos)) :-
    position_to_gamestate(Position, GameState),
    GameState = game_state(Board, _, _, _, _),
    
    between(1, 8, FromRow),
    between(1, 8, FromCol),
    get_piece(Board, FromRow, FromCol, Piece),
    Piece \= '.',
    get_piece_color(Piece, Color),
    between(1, 8, ToRow),
    between(1, 8, ToCol),
    valid_move(Board, Color, FromRow, FromCol, ToRow, ToCol),
    
    % Conversion positions
    FromPos is FromRow * 10 + FromCol,
    ToPos is ToRow * 10 + ToCol.

% make_move_ref(+Color, +Position, +Move, -NewPosition, -Hit)
% Adaptation de make_move/5 dans chess.pl:188-194
make_move_ref(Color, Position, move(FromPos, ToPos), NewPosition, Hit) :-
    % Conversion positions
    FromRow is FromPos // 10,
    FromCol is FromPos mod 10,
    ToRow is ToPos // 10,
    ToCol is ToPos mod 10,
    
    % Conversion vers GameState
    position_to_gamestate(Position, GameState),
    GameState = game_state(Board, _, _, _, _),
    
    % Determiner si capture
    get_piece(Board, ToRow, ToCol, Target),
    (   Target \= '.' ->
        Hit = hit
    ;   Hit = nohit
    ),
    
    % Executer coup
    make_move(GameState, FromRow, FromCol, ToRow, ToCol, NewGameState),
    
    % Conversion retour
    gamestate_to_position(NewGameState, NewPosition).

% =============================================================================
% PILE IA - COPIE EXACTE DE UTILS.PL
% =============================================================================

% Fonctions pile EXACTEMENT comme utils.pl:54-82
init_ai_stack :-
    \+ ai_top(_),
    asserta(ai_top(0)).

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

% =============================================================================
% CONVERSION DONNEES - ADAPTATION NECESSAIRE
% =============================================================================

% gamestate_to_position(+GameState, -Position)
% Convertit notre GameState vers le format reference
gamestate_to_position(GameState, Position) :-
    GameState = game_state(Board, _, MoveCount, _, _),
    
    % Extraire pieces blanches
    extract_pieces_for_color(Board, white, WhiteHalf),
    
    % Extraire pieces noires  
    extract_pieces_for_color(Board, black, BlackHalf),
    
    Position = position(WhiteHalf, BlackHalf, MoveCount).

% position_to_gamestate(+Position, -GameState)
% Convertit format reference vers notre GameState
position_to_gamestate(position(WhiteHalf, BlackHalf, MoveCount), GameState) :-
    % Reconstruire board depuis half_positions
    generate_empty_board(EmptyBoard),
    place_half_pieces(EmptyBoard, white, WhiteHalf, TempBoard),
    place_half_pieces(TempBoard, black, BlackHalf, Board),
    
    GameState = game_state(Board, white, MoveCount, active, [[], []]).

% extract_pieces_for_color(+Board, +Color, -HalfPosition)
% Extrait pieces d'une couleur vers half_position
extract_pieces_for_color(Board, Color, half_position(Pawns, Rooks, Knights, Bishops, Queens, Kings, notmoved)) :-
    findall(Pos, (
        between(1, 8, Row),
        between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        get_piece_color(Piece, Color),
        to_piece_type(Piece, pawn),
        Pos is Row * 10 + Col
    ), Pawns),
    
    findall(Pos, (
        between(1, 8, Row),
        between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        get_piece_color(Piece, Color),
        to_piece_type(Piece, rook),
        Pos is Row * 10 + Col
    ), Rooks),
    
    findall(Pos, (
        between(1, 8, Row),
        between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        get_piece_color(Piece, Color),
        to_piece_type(Piece, knight),
        Pos is Row * 10 + Col
    ), Knights),
    
    findall(Pos, (
        between(1, 8, Row),
        between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        get_piece_color(Piece, Color),
        to_piece_type(Piece, bishop),
        Pos is Row * 10 + Col
    ), Bishops),
    
    findall(Pos, (
        between(1, 8, Row),
        between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        get_piece_color(Piece, Color),
        to_piece_type(Piece, queen),
        Pos is Row * 10 + Col
    ), Queens),
    
    findall(Pos, (
        between(1, 8, Row),
        between(1, 8, Col),
        get_piece(Board, Row, Col, Piece),
        get_piece_color(Piece, Color),
        to_piece_type(Piece, king),
        Pos is Row * 10 + Col
    ), Kings).

% to_piece_type(+Piece, +Type)
% Convertit piece vers type
to_piece_type('P', pawn).
to_piece_type('p', pawn).
to_piece_type('R', rook).
to_piece_type('r', rook).
to_piece_type('N', knight).
to_piece_type('n', knight).
to_piece_type('B', bishop).
to_piece_type('b', bishop).
to_piece_type('Q', queen).
to_piece_type('q', queen).
to_piece_type('K', king).
to_piece_type('k', king).

% position_move_to_our_format(+MoveRef, -OurMove)
% Convertit move(FromPos, ToPos) vers [FromRow, FromCol, ToRow, ToCol]
position_move_to_our_format(move(FromPos, ToPos), [FromRow, FromCol, ToRow, ToCol]) :-
    FromRow is FromPos // 10,
    FromCol is FromPos mod 10,
    ToRow is ToPos // 10,
    ToCol is ToPos mod 10.

% generate_empty_board(-Board)
generate_empty_board(Board) :-
    Board = [
        ['.','.','.','.','.','.','.','.' ],
        ['.','.','.','.','.','.','.','.' ],
        ['.','.','.','.','.','.','.','.' ],
        ['.','.','.','.','.','.','.','.' ],
        ['.','.','.','.','.','.','.','.' ],
        ['.','.','.','.','.','.','.','.' ],
        ['.','.','.','.','.','.','.','.' ],
        ['.','.','.','.','.','.','.','.' ]
    ].

% place_half_pieces(+Board, +Color, +HalfPosition, -NewBoard)
% Place les pieces d'une half_position sur le board
place_half_pieces(Board, Color, half_position(Pawns, Rooks, Knights, Bishops, Queens, Kings, _), NewBoard) :-
    place_pieces_list(Board, Color, pawn, Pawns, Board1),
    place_pieces_list(Board1, Color, rook, Rooks, Board2),
    place_pieces_list(Board2, Color, knight, Knights, Board3),
    place_pieces_list(Board3, Color, bishop, Bishops, Board4),
    place_pieces_list(Board4, Color, queen, Queens, Board5),
    place_pieces_list(Board5, Color, king, Kings, NewBoard).

% place_pieces_list(+Board, +Color, +Type, +Positions, -NewBoard)
place_pieces_list(Board, _, _, [], Board) :- !.
place_pieces_list(Board, Color, Type, [Pos|Rest], NewBoard) :-
    Row is Pos // 10,
    Col is Pos mod 10,
    type_to_piece(Type, Color, Piece),
    place_single_piece(Board, Row, Col, Piece, TempBoard),
    place_pieces_list(TempBoard, Color, Type, Rest, NewBoard).

% type_to_piece(+Type, +Color, -Piece)
type_to_piece(pawn, white, 'P').
type_to_piece(pawn, black, 'p').
type_to_piece(rook, white, 'R').
type_to_piece(rook, black, 'r').
type_to_piece(knight, white, 'N').
type_to_piece(knight, black, 'n').
type_to_piece(bishop, white, 'B').
type_to_piece(bishop, black, 'b').
type_to_piece(queen, white, 'Q').
type_to_piece(queen, black, 'q').
type_to_piece(king, white, 'K').
type_to_piece(king, black, 'k').
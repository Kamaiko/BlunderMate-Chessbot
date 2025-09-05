% =============================================================================
% CHESS BOARD - ECHIQUIER
% =============================================================================
% 
% Module pour l'echiquier et coordonnees :
% - Representation 8x8 avec operations optimisees
% - Notation algebrique avec validation
% - Initialisation par listes
% - Affichage ASCII avec pieces colorees
%
% Auteur : Patrick Patenaude  
% =============================================================================

:- [pieces].

% =============================================================================
% CONSTANTES ÉCHIQUIER
% =============================================================================

% Dimensions standard de l'échiquier
board_size(8).

% =============================================================================
% SECTION 1 : VALIDATION DES COORDONNEES
% =============================================================================

% valid_chess_position(+Row, +Col)
% Valide qu'une position est dans les limites de l'echiquier (1-8).
valid_chess_position(Row, Col) :-
    integer(Row), integer(Col),
    Row >= 1, Row =< 8,
    Col >= 1, Col =< 8.

% valid_chess_move(+FromRow, +FromCol, +ToRow, +ToCol)
% Valide qu'un mouvement a des coordonnees valides et destination differente.
valid_chess_move(FromRow, FromCol, ToRow, ToCol) :-
    valid_chess_position(FromRow, FromCol),
    valid_chess_position(ToRow, ToCol),
    (FromRow =\= ToRow ; FromCol =\= ToCol).

% =============================================================================
% SECTION 2 : NOTATION ALGEBRIQUE
% =============================================================================

% char_to_col(+ColumnChar, -ColumnNumber)
% Convertit une lettre de colonne (a-h) en numero de colonne (1-8).
char_to_col(ColChar, ColNum) :-
    atom_codes(ColChar, [Code]),
    between(97, 104, Code),  % 'a' = 97, 'h' = 104
    ColNum is Code - 96.

% char_to_row(+RowChar, -RowNumber)
% Convertit un chiffre de rangee ('1'-'8') en numero de rangee (1-8).
char_to_row(RowChar, RowNum) :-
    atom_codes(RowChar, [Code]),
    between(49, 56, Code),  % '1' = 49, '8' = 56
    RowNum is Code - 48.

% col_to_char(+ColumnNumber, -ColumnChar)
% Conversion inverse : numero de colonne vers lettre.
col_to_char(ColNum, ColChar) :-
    between(1, 8, ColNum),
    Code is ColNum + 96,  % 1+96 = 97 = 'a'
    char_code(ColChar, Code).

% row_to_char(+RowNumber, -RowChar)
% Conversion inverse : numero de rangee vers chiffre.
row_to_char(RowNum, RowChar) :-
    between(1, 8, RowNum),
    Code is RowNum + 48,  % 1+48 = 49 = '1'
    char_code(RowChar, Code).

% parse_algebraic_move(+MoveString, -FromRow, -FromCol, -ToRow, -ToCol)
% Parse une chaine algebrique "e2e4" en coordonnees numeriques.
parse_algebraic_move(MoveString, FromRow, FromCol, ToRow, ToCol) :-
    nonvar(MoveString),
    string_length(MoveString, 4),
    string_chars(MoveString, [FromColChar, FromRowChar, ToColChar, ToRowChar]),
    char_to_col(FromColChar, FromCol),
    char_to_row(FromRowChar, FromRow), 
    char_to_col(ToColChar, ToCol),
    char_to_row(ToRowChar, ToRow),
    valid_chess_move(FromRow, FromCol, ToRow, ToCol).

% coordinates_to_algebraic(+FromRow, +FromCol, +ToRow, +ToCol, -MoveString)
% Conversion inverse : coordonnees vers notation algebrique.
coordinates_to_algebraic(FromRow, FromCol, ToRow, ToCol, MoveString) :-
    valid_chess_move(FromRow, FromCol, ToRow, ToCol),
    col_to_char(FromCol, FromColChar),
    row_to_char(FromRow, FromRowChar),
    col_to_char(ToCol, ToColChar), 
    row_to_char(ToRow, ToRowChar),
    string_chars(MoveString, [FromColChar, FromRowChar, ToColChar, ToRowChar]).

% position_to_algebraic(+Row, +Col, -PositionString)
% Convertit une position numerique en notation algebrique ("a1").
position_to_algebraic(Row, Col, PositionString) :-
    valid_chess_position(Row, Col),
    col_to_char(Col, ColChar),
    row_to_char(Row, RowChar),
    string_chars(PositionString, [ColChar, RowChar]).

% algebraic_to_position(+PositionString, -Row, -Col)
% Conversion inverse : notation algebrique vers coordonnees numeriques.
algebraic_to_position(PositionString, Row, Col) :-
    nonvar(PositionString),
    string_length(PositionString, 2),
    string_chars(PositionString, [ColChar, RowChar]),
    char_to_col(ColChar, Col),
    char_to_row(RowChar, Row).

% =============================================================================
% SECTION 3 : CREATION DE L'ECHIQUIER
% =============================================================================

% create_empty_board(-Board)
% Cree un echiquier 8x8 vide avec des cases vides.
create_empty_board(Board) :-
    board_size(Size),
    create_empty_board(Size, [], Board).

% create_empty_board(+RowsLeft, +Acc, -Board)
% Version recursive pour creation de l'echiquier.
create_empty_board(0, Board, Board) :- !.
create_empty_board(RowsLeft, Acc, Board) :-
    RowsLeft > 0,
    create_empty_row(RowList),
    NextRows is RowsLeft - 1,
    create_empty_board(NextRows, [RowList|Acc], Board).

% create_empty_row(-RowList)
% Cree une rangee de 8 cases vides de maniere plus declarative.
create_empty_row(Row) :-
    board_size(Size),
    length(Row, Size),
    maplist(=(' '), Row).

% =============================================================================
% SECTION 4 : MANIPULATION DE L'ECHIQUIER
% =============================================================================

% get_piece(+Board, +Row, +Col, -Piece)
% Recupere la piece a une position donnee.
% Version renforcee avec validation complete des parametres.
get_piece(Board, Row, Col, Piece) :-
    ground(Board), ground(Row), ground(Col),
    is_list(Board),
    board_size(Size), length(Board, Size),
    integer(Row), integer(Col),
    valid_chess_position(Row, Col),
    BoardRow is 9 - Row,  % Conversion coordonnees echecs vers index tableau
    nth1(BoardRow, Board, RowList),
    is_list(RowList),
    length(RowList, 8),
    nth1(Col, RowList, Piece).

% place_single_piece(+Board, +Row, +Col, +Piece, -NewBoard)
% Place une piece a une position specifique sur l'echiquier.
% Version renforcee avec validation complete des parametres.
place_single_piece(Board, Row, Col, Piece, NewBoard) :-
    ground(Board), ground(Row), ground(Col), ground(Piece),
    is_list(Board),
    board_size(Size), length(Board, Size),
    integer(Row), integer(Col),
    valid_chess_position(Row, Col),
    BoardRow is 9 - Row,  % Conversion coordonnees echecs vers index tableau
    nth1(BoardRow, Board, RowList),
    is_list(RowList),
    length(RowList, 8),
    replace_element(RowList, Col, Piece, NewRowList),
    replace_element(Board, BoardRow, NewRowList, NewBoard).

% replace_element(+List, +Index, +NewElement, -NewList)
% Remplace un element a un index specifique dans une liste.
replace_element([_|T], 1, Element, [Element|T]) :- !.
replace_element([H|T], Index, Element, [H|NewT]) :-
    Index > 1,
    NextIndex is Index - 1,
    replace_element(T, NextIndex, Element, NewT).

% replace_list_element_direct(+Board, +Row, +Col, +Piece, -NewBoard)
% Version optimisee pour remplacement direct en une passe.
% Reduit la complexite pour operations frequentes.
replace_list_element_direct([H|T], 1, Col, Piece, [NewH|T]) :- !,
    replace_element(H, Col, Piece, NewH).
replace_list_element_direct([H|T], Row, Col, Piece, [H|NewT]) :-
    Row > 1,
    Row1 is Row - 1,
    replace_list_element_direct(T, Row1, Col, Piece, NewT).

% place_piece_optimized(+Board, +Row, +Col, +Piece, -NewBoard)
% Version optimisee pour operations frequentes (utilisation IA future).
place_piece_optimized(Board, Row, Col, Piece, NewBoard) :-
    ground(Board), ground(Row), ground(Col), ground(Piece),
    integer(Row), integer(Col),
    valid_chess_position(Row, Col),
    BoardRow is 9 - Row,
    replace_list_element_direct(Board, BoardRow, Col, Piece, NewBoard).

% =============================================================================
% SECTION 5 : INITIALISATION DE L'ECHIQUIER STANDARD
% =============================================================================

% initialize_board(-Board)
% Cree et place toutes les pieces sur l'echiquier en position standard.
initialize_board(Board) :-
    create_empty_board(EmptyBoard),
    place_all_pieces(EmptyBoard, Board).

% place_all_pieces(+Board, -NewBoard)
% Place toutes les pieces blanches et noires en position standard.
place_all_pieces(Board, NewBoard) :-
    place_white_pieces(Board, Board1),
    place_black_pieces(Board1, NewBoard).

% place_white_pieces(+Board, -NewBoard)
% Place les pieces blanches selon la position standard d'echecs.
place_white_pieces(Board, NewBoard) :-
    place_piece_row(Board, 2, 'P', Board1),
    place_back_rank_pieces(Board1, 1, white, NewBoard).

% place_back_rank_pieces(+Board, +Row, +Color, -NewBoard)
% Place les pieces lourdes sur une rangee (optimise pour eviter duplication).
place_back_rank_pieces(Board, Row, Color, NewBoard) :-
    (Color = white -> PieceSet = ['R','N','B','Q','K','B','N','R']
    ; PieceSet = ['r','n','b','q','k','b','n','r']),
    place_pieces_from_list(Board, Row, 1, PieceSet, NewBoard).

% place_pieces_from_list(+Board, +Row, +Col, +PieceList, -NewBoard)
% Place une liste de pieces sur une rangee.
place_pieces_from_list(Board, _, _, [], Board) :- !.
place_pieces_from_list(Board, Row, Col, [Piece|Rest], NewBoard) :-
    place_single_piece(Board, Row, Col, Piece, TempBoard),
    NextCol is Col + 1,
    place_pieces_from_list(TempBoard, Row, NextCol, Rest, NewBoard).

% place_black_pieces(+Board, -NewBoard)
% Place les pieces noires selon la position standard d'echecs.
place_black_pieces(Board, NewBoard) :-
    place_piece_row(Board, 7, 'p', Board1),
    place_back_rank_pieces(Board1, 8, black, NewBoard).

% place_piece_row(+Board, +Row, +Piece, -NewBoard)
% Place une piece sur toute une rangee (utilise pour les pions).
place_piece_row(Board, Row, Piece, NewBoard) :-
    place_piece_row(Board, Row, 1, Piece, NewBoard).

% place_piece_row(+Board, +Row, +Col, +Piece, -NewBoard)
% Version recursive pour placement sur une rangee.
place_piece_row(Board, _Row, Col, _Piece, Board) :-
    Col > 8, !.
place_piece_row(Board, Row, Col, Piece, NewBoard) :-
    Col =< 8,
    place_single_piece(Board, Row, Col, Piece, TempBoard),
    NextCol is Col + 1,
    place_piece_row(TempBoard, Row, NextCol, Piece, NewBoard).

% =============================================================================
% SECTION 6 : AFFICHAGE ASCII AVEC COULEURS
% =============================================================================

% display_board(+Board)
% Affiche l'echiquier complet avec les pieces colorees.
display_board(Board) :-
    nl,
    write(' ECHIQUIER DE JEU'), nl, nl,
    display_board_rows(Board, 8),
    write('  a b c d e f g h'), nl, nl.

% display_board_rows(+Rows, +RowNumber)
% Affiche chaque rangee de l'echiquier avec son numero.
display_board_rows([], _) :- !.
display_board_rows([Row|Rows], RowNum) :-
    write(RowNum), write('['),
    display_row(Row),
    write(']'), nl,
    NextRowNum is RowNum - 1,
    display_board_rows(Rows, NextRowNum).

% display_row(+PieceList)
% Affiche les pieces d'une rangee separees par des virgules.
display_row([]) :- !.
display_row([Piece|Pieces]) :-
    display_piece(Piece),
    (Pieces = [] -> true ; (write(','), display_row(Pieces))).

% display_piece(+Piece)
% Affiche une piece avec sa couleur appropriee.
display_piece(Piece) :-
    (is_white_piece(Piece) ->
        display_white_piece(Piece)
    ; is_black_piece(Piece) ->
        display_black_piece(Piece)
    ;   
        write(Piece)
    ).

% display_white_piece(+Piece)
% Affiche les pieces blanches en blanc gras.
display_white_piece('P') :- write('\e[1;37mP\e[0m').  % Pion blanc
display_white_piece('R') :- write('\e[1;37mR\e[0m').  % Tour blanche
display_white_piece('N') :- write('\e[1;37mN\e[0m').  % Cavalier blanc
display_white_piece('B') :- write('\e[1;37mB\e[0m').  % Fou blanc
display_white_piece('Q') :- write('\e[1;37mQ\e[0m').  % Dame blanche
display_white_piece('K') :- write('\e[1;37mK\e[0m').  % Roi blanc

% display_black_piece(+Piece)
% Affiche les pieces noires en rouge gras.
display_black_piece('p') :- write('\e[1;31mp\e[0m').  % Pion noir
display_black_piece('r') :- write('\e[1;31mr\e[0m').  % Tour noire
display_black_piece('n') :- write('\e[1;31mn\e[0m').  % Cavalier noir
display_black_piece('b') :- write('\e[1;31mb\e[0m').  % Fou noir
display_black_piece('q') :- write('\e[1;31mq\e[0m').  % Dame noire
display_black_piece('k') :- write('\e[1;31mk\e[0m').  % Roi noir

% =============================================================================
% SECTION 7 : UTILITAIRES GEOMETRIQUES
% =============================================================================

% chess_distance(+FromRow, +FromCol, +ToRow, +ToCol, -Distance)
% Calcule la distance de Chebyshev entre deux positions.
chess_distance(FromRow, FromCol, ToRow, ToCol, Distance) :-
    valid_chess_position(FromRow, FromCol),
    valid_chess_position(ToRow, ToCol),
    RowDiff is abs(FromRow - ToRow),
    ColDiff is abs(FromCol - ToCol),
    Distance is max(RowDiff, ColDiff).

% is_diagonal_move(+FromRow, +FromCol, +ToRow, +ToCol)
% Verifie si un mouvement est strictement diagonal.
is_diagonal_move(FromRow, FromCol, ToRow, ToCol) :-
    valid_chess_move(FromRow, FromCol, ToRow, ToCol),
    RowDiff is abs(FromRow - ToRow),
    ColDiff is abs(FromCol - ToCol),
    RowDiff =:= ColDiff,
    RowDiff > 0.

% is_orthogonal_move(+FromRow, +FromCol, +ToRow, +ToCol)
% Verifie si un mouvement est orthogonal (horizontal ou vertical).
is_orthogonal_move(FromRow, FromCol, ToRow, ToCol) :-
    valid_chess_move(FromRow, FromCol, ToRow, ToCol),
    (FromRow =:= ToRow ; FromCol =:= ToCol).

% find_king_position(+Board, +Player, -Row, -Col)
% Trouve la position du roi d'un joueur sur l'echiquier.
% Utilise une recherche optimisee pour performance.
find_king_position(Board, Player, Row, Col) :-
    ground(Board), ground(Player),
    is_list(Board), board_size(Size), length(Board, Size),
    member(Player, [white, black]),
    (Player = white -> KingPiece = 'K' ; KingPiece = 'k'),
    find_king_on_board(Board, KingPiece, 8, Row, Col).

% find_king_on_board(+Board, +KingPiece, +CurrentRow, -Row, -Col)
% Recherche recursive du roi sur l'echiquier par rangee.
find_king_on_board([BoardRow|_], KingPiece, CurrentRow, CurrentRow, Col) :-
    find_king_in_row(BoardRow, KingPiece, 1, Col), !.
find_king_on_board([_|RestBoard], KingPiece, CurrentRow, Row, Col) :-
    CurrentRow > 1,
    NextRow is CurrentRow - 1,
    find_king_on_board(RestBoard, KingPiece, NextRow, Row, Col).

% find_king_in_row(+RowList, +KingPiece, +CurrentCol, -Col)
% Recherche le roi dans une rangee donnee.
find_king_in_row([KingPiece|_], KingPiece, CurrentCol, CurrentCol) :- !.
find_king_in_row([_|RestRow], KingPiece, CurrentCol, Col) :-
    CurrentCol < 8,
    NextCol is CurrentCol + 1,
    find_king_in_row(RestRow, KingPiece, NextCol, Col).

% =============================================================================
% SECTION 8 : VALIDATION ET TESTS
% =============================================================================

% test_coordinate_conversion
% Test de coherence des conversions algebriques.
test_coordinate_conversion :-
    forall(
        (between(1, 8, Row), between(1, 8, Col)),
        (
            coordinates_to_algebraic(Row, Col, Row, Col, Move),
            parse_algebraic_move(Move, Row, Col, Row, Col)
        )
    ),
    parse_algebraic_move("e2e4", 2, 5, 4, 5),
    coordinates_to_algebraic(2, 5, 4, 5, "e2e4"),
    parse_algebraic_move("a1h8", 1, 1, 8, 8),
    coordinates_to_algebraic(1, 1, 8, 8, "a1h8").

% validate_all_positions
% Valide toutes les positions de l'echiquier.
validate_all_positions :-
    forall(
        (between(1, 8, Row), between(1, 8, Col)),
        valid_chess_position(Row, Col)
    ).


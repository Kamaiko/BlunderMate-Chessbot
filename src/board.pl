% =============================================================================
% CHESS BOARD - ECHIQUIER, COORDONNEES ET AFFICHAGE
% =============================================================================
% 
% Ce module centralise TOUTE la logique liee a l'echiquier :
% - Representation et manipulation de l'echiquier 8x8
% - Systeme de coordonnees et notation algebrique  
% - Affichage ASCII avec pieces colorees
% - Initialisation de l'echiquier standard
%
% Auteur : Patrick Patenaude
% Version : 5.1 (Consolidation intuitive)
%
% RESPONSABILITES :
% - Creation et initialisation de l'echiquier
% - Placement et manipulation des pieces
% - Conversion coordonnees ↔ notation algebrique
% - Affichage console avec couleurs
% =============================================================================

:- [pieces].

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
% Version computationnelle pour reduire la repetition.
char_to_col(ColChar, ColNum) :-
    member(ColChar, ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']),
    char_code(ColChar, Code),
    ColNum is Code - 96.  % 'a' = 97, donc 97-96 = 1

% char_to_row(+RowChar, -RowNumber)
% Convertit un chiffre de rangee ('1'-'8') en numero de rangee (1-8).
char_to_row(RowChar, RowNum) :-
    member(RowChar, ['1', '2', '3', '4', '5', '6', '7', '8']),
    char_code(RowChar, Code),
    RowNum is Code - 48.  % '1' = 49, donc 49-48 = 1

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
    create_empty_board(8, [], Board).

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
    length(Row, 8),
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
    length(Board, 8),
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
    length(Board, 8),
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
    % Pions blancs en rangee 2
    place_piece_row(Board, 2, 'P', Board1),
    % Pieces lourdes en rangee 1
    place_single_piece(Board1, 1, 1, 'R', Board2), % Tour a1
    place_single_piece(Board2, 1, 8, 'R', Board3), % Tour h1
    place_single_piece(Board3, 1, 2, 'N', Board4), % Cavalier b1
    place_single_piece(Board4, 1, 7, 'N', Board5), % Cavalier g1
    place_single_piece(Board5, 1, 3, 'B', Board6), % Fou c1
    place_single_piece(Board6, 1, 6, 'B', Board7), % Fou f1
    place_single_piece(Board7, 1, 4, 'Q', Board8), % Dame d1
    place_single_piece(Board8, 1, 5, 'K', NewBoard). % Roi e1

% place_black_pieces(+Board, -NewBoard)
% Place les pieces noires selon la position standard d'echecs.
place_black_pieces(Board, NewBoard) :-
    % Pions noirs en rangee 7
    place_piece_row(Board, 7, 'p', Board1),
    % Pieces lourdes en rangee 8
    place_single_piece(Board1, 8, 1, 'r', Board2), % Tour a8
    place_single_piece(Board2, 8, 8, 'r', Board3), % Tour h8
    place_single_piece(Board3, 8, 2, 'n', Board4), % Cavalier b8
    place_single_piece(Board4, 8, 7, 'n', Board5), % Cavalier g8
    place_single_piece(Board5, 8, 3, 'b', Board6), % Fou c8
    place_single_piece(Board6, 8, 6, 'b', Board7), % Fou f8
    place_single_piece(Board7, 8, 4, 'q', Board8), % Dame d8
    place_single_piece(Board8, 8, 5, 'k', NewBoard). % Roi e8

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

% =============================================================================
% FIN DU FICHIER BOARD.PL
% Derniere mise a jour : Aout 2025
% =============================================================================
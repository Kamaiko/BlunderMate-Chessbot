% =============================================================================
% SMART CHESS BOARD - REPRESENTATION ET AFFICHAGE ASCII SIMPLIFIE
% =============================================================================
% 
% Ce fichier gere la representation de l'echiquier avec des caracteres ASCII simples.
% Version simplifiee sans Unicode pour une meilleure compatibilite.
%
% Auteur : Patrick Patenaude
% Version : 3.0 (ASCII uniquement - simplifié)
% 
% RESPONSABILITES :
% - Representation de l'echiquier 8x8
% - Definition des pieces en ASCII
% - Affichage colore des pieces
% - Conversion notation algebrique <-> coordonnees
% - Initialisation de l'echiquier standard
% =============================================================================

% =============================================================================
% SECTION 1 : DEFINITION DES PIECES D'ECHECS (ASCII)
% =============================================================================

% --- PIECES BLANCHES (majuscules) ---
% Chaque piece est definie par un caractere ASCII majuscule
white_pawn('P').      % Pion blanc
white_rook('R').      % Tour blanche  
white_knight('N').    % Cavalier blanc
white_bishop('B').    % Fou blanc
white_queen('Q').     % Dame blanche
white_king('K').      % Roi blanc

% --- PIECES NOIRES (minuscules) ---
% Chaque piece est definie par un caractere ASCII minuscule
black_pawn('p').      % Pion noir
black_rook('r').      % Tour noire
black_knight('n').    % Cavalier noir
black_bishop('b').    % Fou noir
black_queen('q').     % Dame noire
black_king('k').      % Roi noir

% --- CASES VIDES ---
empty_square(' ').    % Case vide

% =============================================================================
% SECTION 2 : CREATION ET INITIALISATION DE L'ECHIQUIER
% =============================================================================

% --- CREATION D'UN ECHIQUIER VIDE ---
% Crée un échiquier 8x8 vide avec des cases vides
create_empty_board(Board) :-
    create_empty_board(8, [], Board).

% Cas de base : toutes les rangées ont été créées
create_empty_board(0, Board, Board).

% Création récursive des rangées
create_empty_board(Row, Acc, Board) :-
    Row > 0,
    create_row(Row, RowList),           % Créer une rangée
    NextRow is Row - 1,
    create_empty_board(NextRow, [RowList|Acc], Board).

% --- CREATION D'UNE RANGEE ---
% Crée une rangée de 8 cases vides
create_row(Row, RowList) :-
    create_row_cells(Row, 1, [], RowList).

% Cas de base : toutes les colonnes ont été créées
create_row_cells(_, 9, Row, Row).

% Création récursive des cellules d'une rangée
create_row_cells(Row, Col, Acc, RowList) :-
    Col =< 8,
    append(Acc, [' '], NewAcc),         % Ajouter une case vide
    NextCol is Col + 1,
    create_row_cells(Row, NextCol, NewAcc, RowList).

% =============================================================================
% SECTION 3 : PLACEMENT DES PIÈCES SUR L'ÉCHIQUIER
% =============================================================================

% --- PLACEMENT PRINCIPAL DES PIECES ---
% Place toutes les pièces blanches et noires sur l'échiquier
place_pieces(Board, NewBoard) :-
    place_white_pieces(Board, Board1),      % Placer les pièces blanches
    place_black_pieces(Board1, NewBoard).   % Placer les pièces noires

% --- PLACEMENT DES PIECES BLANCHES ---
% Place les pièces blanches selon la position standard d'échecs
place_white_pieces(Board, NewBoard) :-
    place_piece_row(Board, 2, 'P', Board1),        % Pions blancs en rangée 2
    place_single_piece(Board1, 1, 1, 'R', Board2), % Tour blanche en a1
    place_single_piece(Board2, 1, 8, 'R', Board3), % Tour blanche en h1
    place_single_piece(Board3, 1, 2, 'N', Board4), % Cavalier blanc en b1
    place_single_piece(Board4, 1, 7, 'N', Board5), % Cavalier blanc en g1
    place_single_piece(Board5, 1, 3, 'B', Board6), % Fou blanc en c1
    place_single_piece(Board6, 1, 6, 'B', Board7), % Fou blanc en f1
    place_single_piece(Board7, 1, 4, 'Q', Board8), % Dame blanche en d1
    place_single_piece(Board8, 1, 5, 'K', NewBoard). % Roi blanc en e1

% --- PLACEMENT DES PIECES NOIRES ---
% Place les pièces noires selon la position standard d'échecs
place_black_pieces(Board, NewBoard) :-
    place_piece_row(Board, 7, 'p', Board1),        % Pions noirs en rangée 7
    place_single_piece(Board1, 8, 1, 'r', Board2), % Tour noire en a8
    place_single_piece(Board2, 8, 8, 'r', Board3), % Tour noire en h8
    place_single_piece(Board3, 8, 2, 'n', Board4), % Cavalier noir en b8
    place_single_piece(Board4, 8, 7, 'n', Board5), % Cavalier noir en g8
    place_single_piece(Board5, 8, 3, 'b', Board6), % Fou noir en c8
    place_single_piece(Board6, 8, 6, 'b', Board7), % Fou noir en f8
    place_single_piece(Board7, 8, 4, 'q', Board8), % Dame noire en d8
    place_single_piece(Board8, 8, 5, 'k', NewBoard). % Roi noir en e8

% --- PLACEMENT D'UNE RANGEE DE PIECES ---
% Place une pièce sur toute une rangée (ex: pions)
place_piece_row(Board, Row, Piece, NewBoard) :-
    place_piece_row(Board, Row, 1, Piece, NewBoard).

% Cas de base : toutes les colonnes ont été traitées
place_piece_row(Board, _Row, 9, _Piece, Board).

% Placement récursif sur une rangée
place_piece_row(Board, Row, Col, Piece, NewBoard) :-
    Col =< 8,
    place_single_piece(Board, Row, Col, Piece, Board1),
    NextCol is Col + 1,
    place_piece_row(Board1, Row, NextCol, Piece, NewBoard).

% --- PLACEMENT D'UNE PIECE INDIVIDUELLE ---
% Place une pièce à une position spécifique sur l'échiquier
place_single_piece(Board, Row, Col, Piece, NewBoard) :-
    BoardRow is 9 - Row,  % Conversion des coordonnées d'échecs vers l'index du tableau
    nth1(BoardRow, Board, RowList),
    replace_element(RowList, Col, Piece, NewRowList),
    replace_element(Board, BoardRow, NewRowList, NewBoard).

% =============================================================================
% SECTION 4 : UTILITAIRES POUR LA MANIPULATION DE L'ÉCHIQUIER
% =============================================================================

% --- REMPLACEMENT D'ELEMENTS DANS UNE LISTE ---
% Remplace un élément à un index spécifique dans une liste
replace_element([_|T], 1, Element, [Element|T]).
replace_element([H|T], Index, Element, [H|NewT]) :-
    Index > 1,
    NextIndex is Index - 1,
    replace_element(T, NextIndex, Element, NewT).

% --- OBTENTION D'UNE PIECE ---
% Recupere la piece a une position donnee
% Echoue si les coordonnees sont invalides
get_piece(Board, Row, Col, Piece) :-
    % Validation des parametres d'entree
    nonvar(Board), nonvar(Row), nonvar(Col),
    Row >= 1, Row =< 8, Col >= 1, Col =< 8,
    
    BoardRow is 9 - Row,  % Conversion des coordonnees d'echecs vers l'index du tableau
    nth1(BoardRow, Board, RowList),
    nth1(Col, RowList, Piece).

% =============================================================================
% SECTION 5 : AFFICHAGE DES PIÈCES (ASCII SIMPLE)
% =============================================================================

% --- AFFICHAGE PRINCIPAL ---
% Affiche une pièce avec sa couleur appropriée
display_piece(Piece) :-
    (is_white_piece_char(Piece) ->
        display_white_piece(Piece)      % Pièce blanche
    ; is_black_piece_char(Piece) ->
        display_black_piece(Piece)      % Pièce noire
    ;   
        write(Piece)                    % Case vide ou autre
    ).

% --- VERIFICATION DES PIECES BLANCHES ---
% Détermine si un caractère représente une pièce blanche
is_white_piece_char(Piece) :-
    member(Piece, ['P', 'R', 'N', 'B', 'Q', 'K']).

% --- VERIFICATION DES PIECES NOIRES ---
% Détermine si un caractère représente une pièce noire
is_black_piece_char(Piece) :-
    member(Piece, ['p', 'r', 'n', 'b', 'q', 'k']).

% --- AFFICHAGE DES PIECES BLANCHES ---
% Pièces blanches en blanc gras
display_white_piece('P') :- write('\e[1;37mP\e[0m').  % Pion blanc
display_white_piece('R') :- write('\e[1;37mR\e[0m').  % Tour blanche
display_white_piece('N') :- write('\e[1;37mN\e[0m').  % Cavalier blanc
display_white_piece('B') :- write('\e[1;37mB\e[0m').  % Fou blanc
display_white_piece('Q') :- write('\e[1;37mQ\e[0m').  % Dame blanche
display_white_piece('K') :- write('\e[1;37mK\e[0m').  % Roi blanc

% --- AFFICHAGE DES PIECES NOIRES ---
% Pièces noires en rouge gras
display_black_piece('p') :- write('\e[1;31mp\e[0m').  % Pion noir
display_black_piece('r') :- write('\e[1;31mr\e[0m').  % Tour noire
display_black_piece('n') :- write('\e[1;31mn\e[0m').  % Cavalier noir
display_black_piece('b') :- write('\e[1;31mb\e[0m').  % Fou noir
display_black_piece('q') :- write('\e[1;31mq\e[0m').  % Dame noire
display_black_piece('k') :- write('\e[1;31mk\e[0m').  % Roi noir

% =============================================================================
% SECTION 6 : NOTATION ALGÉBRIQUE (CONVERSION COORDONNÉES ↔ NOTATION)
% =============================================================================

% --- PARSING D'UN MOUVEMENT EN NOTATION ALGEBRIQUE ---
% Convertit "e2e4" en coordonnees (2,5,4,5)
% Echoue si MoveString n'est pas valide (doit etre exactement 4 caracteres)
parse_algebraic_move(MoveString, FromRow, FromCol, ToRow, ToCol) :-
    % Validation: MoveString doit exister et avoir 4 caracteres
    nonvar(MoveString),
    string_length(MoveString, 4),
    string_chars(MoveString, [FromColChar, FromRowChar, ToColChar, ToRowChar]),
    
    % Validation: tous les caracteres doivent etre valides
    char_to_col(FromColChar, FromCol),      % Convertir 'e' en 5
    char_to_row(FromRowChar, FromRow),      % Convertir '2' en 2
    char_to_col(ToColChar, ToCol),          % Convertir 'e' en 5
    char_to_row(ToRowChar, ToRow).          % Convertir '4' en 4

% --- CONVERSION DES COLONNES (LETTRES → NUMEROS) ---
% Convertit les lettres a-h en numéros 1-8
char_to_col('a', 1). char_to_col('b', 2). char_to_col('c', 3). char_to_col('d', 4).
char_to_col('e', 5). char_to_col('f', 6). char_to_col('g', 7). char_to_col('h', 8).

% --- CONVERSION DES RANGEES (CHIFFRES → NUMEROS) ---
% Convertit les chiffres 1-8 en numéros 1-8 (1=bas, 8=haut)
char_to_row('1', 1). char_to_row('2', 2). char_to_row('3', 3). char_to_row('4', 4).
char_to_row('5', 5). char_to_row('6', 6). char_to_row('7', 7). char_to_row('8', 8).

% --- CONVERSION INVERSE : COORDONNEES → NOTATION ALGEBRIQUE ---
% Convertit (2,5,4,5) en "e2e4"
coordinates_to_algebraic(FromRow, FromCol, ToRow, ToCol, MoveString) :-
    col_to_char(FromCol, FromColChar),      % Convertir 5 en 'e'
    row_to_char(FromRow, FromRowChar),      % Convertir 2 en '2'
    col_to_char(ToCol, ToColChar),          % Convertir 5 en 'e'
    row_to_char(ToRow, ToRowChar),          % Convertir 4 en '4'
    string_chars(MoveString, [FromColChar, FromRowChar, ToColChar, ToRowChar]).

% --- CONVERSION INVERSE DES COLONNES ---
% Convertit les numéros 1-8 en lettres a-h
col_to_char(1, 'a'). col_to_char(2, 'b'). col_to_char(3, 'c'). col_to_char(4, 'd').
col_to_char(5, 'e'). col_to_char(6, 'f'). col_to_char(7, 'g'). col_to_char(8, 'h').

% --- CONVERSION INVERSE DES RANGEES ---
% Convertit les numéros 1-8 en chiffres 1-8
row_to_char(1, '1'). row_to_char(2, '2'). row_to_char(3, '3'). row_to_char(4, '4').
row_to_char(5, '5'). row_to_char(6, '6'). row_to_char(7, '7'). row_to_char(8, '8').

% =============================================================================
% SECTION 7 : AFFICHAGE COMPLET DE L'ÉCHIQUIER
% =============================================================================

% --- AFFICHAGE PRINCIPAL ---
% Affiche l'échiquier complet avec les pièces ASCII colorées
display_board(Board) :-
    nl,
    write(' ECHIQUIER DE JEU'), nl, nl,
    display_board_rows(Board, 8),
    write('  a b c d e f g h'), nl, nl.

% --- AFFICHAGE DES RANGEES ---
% Affiche chaque rangée de l'échiquier avec son numéro
display_board_rows([], _).
display_board_rows([Row|Rows], RowNum) :-
    write(RowNum), write('['),
    display_row(Row),
    write(']'), nl,
    NextRowNum is RowNum - 1,
    display_board_rows(Rows, NextRowNum).

% --- AFFICHAGE D'UNE RANGEE ---
% Affiche les pièces d'une rangée séparées par des virgules
display_row([]).
display_row([Piece|Pieces]) :-
    display_piece(Piece),
    (Pieces = [] -> true ; write(','), display_row(Pieces)).

% =============================================================================
% SECTION 8 : INITIALISATION
% =============================================================================

% --- INITIALISATION COMPLETE DE L'ECHIQUIER ---
% Cree et place toutes les pieces sur l'echiquier
initialize_board(Board) :-
    create_empty_board(EmptyBoard),
    place_pieces(EmptyBoard, Board).

% =============================================================================
% FIN DU FICHIER BOARD_SMART.PL
% Dernière mise à jour : Août 2025
% =============================================================================

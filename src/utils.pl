% =============================================================================
% UTILS - Module utilitaire pour Chess AI Prolog
% =============================================================================
%
% Ce module centralise :
% - Constantes globales (utilisées dans 3+ modules)
% - Helpers de validation réutilisables
% - Utilitaires de manipulation board/listes
% - Fonctions mathématiques communes
%
% Approche Hybride :
% - Constantes VRAIMENT globales ici (board_size, etc.)
% - Constantes métier restent dans leurs modules (scores IA, tables PSQT, etc.)
% - Helpers partagés pour éviter duplication
%
% =============================================================================

:- module(utils, [
    % Constantes globales
    chess_constant/2,
    
    % Validation helpers
    valid_position_safe/2,
    validate_coordinates/4,
    validate_piece_at/4,
    
    % Board manipulation
    get_piece_safe/4,
    place_piece_safe/5,
    find_piece_positions/4,
    
    % List utilities
    take_first_n/3,
    remove_duplicates/2,
    combine_lists/2,
    
    % Math utilities
    clamp_value/4,
    manhattan_distance/5,
    
    % Tests
    test_utils_basic/0
]).

% =============================================================================
% CONSTANTES GLOBALES
% =============================================================================
% Ces constantes sont utilisées dans 3+ modules et représentent des invariants
% du système d'échecs qui ne changent jamais

%! chess_constant(+Key, -Value) is det
% Constantes globales du système d'échecs
chess_constant(board_size, 8).        % Dimension échiquier 8x8 (utilisé PARTOUT)
chess_constant(min_position, 1).      % Position minimale sur l'échiquier 
chess_constant(max_position, 8).      % Position maximale sur l'échiquier
chess_constant(max_recursion, 8).     % Limite sécurité récursion (taille max échiquier)
chess_constant(default_depth, 2).     % Profondeur par défaut négamax

% =============================================================================
% VALIDATION HELPERS
% =============================================================================
% Ces prédicats consolident la validation répétée à travers tous les modules
% Remplacent 15+ occurrences de validation similaire

%! valid_position_safe(+Row, +Col) is semidet
% Valide une position avec toutes les vérifications de sécurité
% Remplace le pattern répété : ground + integer + bounds check
valid_position_safe(Row, Col) :-
    ground(Row), ground(Col),
    integer(Row), integer(Col),
    chess_constant(min_position, Min),
    chess_constant(max_position, Max),
    Row >= Min, Row =< Max,
    Col >= Min, Col =< Max.

%! validate_coordinates(+FromRow, +FromCol, +ToRow, +ToCol) is semidet
% Valide un mouvement complet avec positions source et destination
% Remplace la logique répétée de validation de mouvement
validate_coordinates(FromRow, FromCol, ToRow, ToCol) :-
    valid_position_safe(FromRow, FromCol),
    valid_position_safe(ToRow, ToCol),
    % Vérifier que la position change (pas de mouvement sur place)
    (FromRow =\= ToRow ; FromCol =\= ToCol).

%! validate_piece_at(+Board, +Row, +Col, +ExpectedPlayer) is semidet
% Valide qu'il y a une pièce du joueur attendu à la position
% Consolide le pattern : get_piece + empty_check + player_check
validate_piece_at(Board, Row, Col, ExpectedPlayer) :-
    valid_position_safe(Row, Col),
    get_piece_safe(Board, Row, Col, Piece),
    \+ is_empty_square(Piece),
    piece_belongs_to_player(Piece, ExpectedPlayer).

% =============================================================================
% BOARD MANIPULATION HELPERS
% =============================================================================
% Ces prédicats fournissent des versions sécurisées des opérations board
% Remplacent 8+ occurrences de patterns get_piece similaires

%! get_piece_safe(+Board, +Row, +Col, -Piece) is semidet
% Version sécurisée de get_piece avec validation complète
% Remplace tous les appels get_piece dispersés avec validation ad-hoc
get_piece_safe(Board, Row, Col, Piece) :-
    valid_position_safe(Row, Col),
    is_list(Board),
    chess_constant(board_size, Size),
    length(Board, Size),
    nth1(Row, Board, RowList),
    is_list(RowList),
    length(RowList, Size),
    nth1(Col, RowList, Piece).

%! place_piece_safe(+Board, +Row, +Col, +Piece, -NewBoard) is det  
% Version sécurisée de place_piece avec validation
% Garantit que les coordonnées sont valides avant placement
place_piece_safe(Board, Row, Col, Piece, NewBoard) :-
    valid_position_safe(Row, Col),
    % Utiliser la fonction existante après validation
    place_single_piece(Board, Row, Col, Piece, NewBoard).

%! find_piece_positions(+Board, +Player, +PieceType, -Positions) is det
% Trouve toutes les positions d'un type de pièce pour un joueur
% Helper réutilisable pour génération de coups et analyse
find_piece_positions(Board, Player, PieceType, Positions) :-
    findall([Row, Col], (
        chess_constant(min_position, Min),
        chess_constant(max_position, Max),
        between(Min, Max, Row),
        between(Min, Max, Col),
        get_piece_safe(Board, Row, Col, Piece),
        piece_belongs_to_player(Piece, Player),
        get_piece_type(Piece, PieceType)
    ), Positions).

% =============================================================================
% LIST UTILITIES
% =============================================================================
% Utilitaires de manipulation de listes réutilisés dans plusieurs modules

%! take_first_n(+List, +N, -FirstN) is det
% Prend les N premiers éléments d'une liste
% Consolide la logique dispersée dans ai.pl (utilisée 3+ fois)
take_first_n(List, N, FirstN) :-
    length(List, Len),
    (   Len =< N -> 
        FirstN = List
    ;   length(FirstN, N), 
        append(FirstN, _, List)
    ).

%! remove_duplicates(+List, -Unique) is det
% Supprime les doublons d'une liste en préservant l'ordre
% Utilitaire générique utilisé dans génération de coups
remove_duplicates([], []).
remove_duplicates([H|T], [H|UniqueT]) :-
    \+ member(H, T),
    remove_duplicates(T, UniqueT).
remove_duplicates([H|T], UniqueT) :-
    member(H, T),
    remove_duplicates(T, UniqueT).

%! combine_lists(+Lists, -Combined) is det
% Combine plusieurs listes en une seule liste plate
% Helper pour consolider différents types de coups
combine_lists([], []).
combine_lists([H|T], Combined) :-
    combine_lists(T, RestCombined),
    append(H, RestCombined, Combined).

% =============================================================================
% MATH UTILITIES
% =============================================================================
% Fonctions mathématiques utilitaires pour calculs d'échecs

%! clamp_value(+Value, +Min, +Max, -Clamped) is det
% Limite une valeur entre min et max
% Utile pour borner les scores et les indices
clamp_value(Value, Min, Max, Clamped) :-
    (   Value < Min -> Clamped = Min
    ;   Value > Max -> Clamped = Max
    ;   Clamped = Value
    ).

%! manhattan_distance(+Row1, +Col1, +Row2, +Col2, -Distance) is det
% Calcule la distance de Manhattan entre deux positions
% Utile pour heuristiques de distance et évaluation positionnelle
manhattan_distance(Row1, Col1, Row2, Col2, Distance) :-
    RowDiff is abs(Row1 - Row2),
    ColDiff is abs(Col1 - Col2),
    Distance is RowDiff + ColDiff.

% =============================================================================
% VALIDATION ET TESTS INTÉGRÉS
% =============================================================================

%! test_utils_basic is det
% Tests de base pour vérifier le bon fonctionnement du module
% Version simplifiée sans dépendances externes
test_utils_basic :-
    % Test constantes
    chess_constant(board_size, 8),
    chess_constant(min_position, 1),
    chess_constant(max_position, 8),
    
    % Test validation
    valid_position_safe(1, 1),
    valid_position_safe(8, 8),
    \+ valid_position_safe(0, 1),
    \+ valid_position_safe(9, 1),
    
    % Test liste utilities
    take_first_n([a, b, c, d], 2, [a, b]),
    take_first_n([a, b], 5, [a, b]),
    remove_duplicates([a, b, a, c, b], [a, b, c]),
    combine_lists([[a, b], [c], [d, e]], [a, b, c, d, e]),
    
    % Test math
    clamp_value(5, 1, 10, 5),
    clamp_value(15, 1, 10, 10),
    clamp_value(-5, 1, 10, 1),
    manhattan_distance(1, 1, 3, 3, 4),
    
    write('✅ All utils basic tests passed'), nl.
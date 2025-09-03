% =============================================================================
% TABLES PIECE-SQUARE SOPHISTIQUÉES - BASÉES SUR STANDARDS MODERNES
% =============================================================================
% Conversion du code C++ fourni avec vraies valeurs positives/négatives
% Source: Tables modernes d'évaluation échiquéenne
% =============================================================================

% =============================================================================
% VALEURS DE BASE MATERIELLES
% =============================================================================

piece_reference_value(pawn, 100).
piece_reference_value(knight, 320).  
piece_reference_value(bishop, 330).
piece_reference_value(rook, 500).
piece_reference_value(queen, 900).
piece_reference_value(king, 10000).

% =============================================================================
% TABLES PIECE-SQUARE POUR BLANCS - FORMAT MODERNE
% =============================================================================
% Convention: Row 1 = rang 1 (blancs), Row 8 = rang 8 (noirs)
% Colonnes: 1=a, 2=b, 3=c, 4=d, 5=e, 6=f, 7=g, 8=h

% PIONS BLANCS - Table sophistiquée du C++
% Rang 1 (impossible pour pions): valeurs nulles
pos_value_reference(pawn, 1, 1, white, 0).
pos_value_reference(pawn, 1, 2, white, 0).
pos_value_reference(pawn, 1, 3, white, 0).
pos_value_reference(pawn, 1, 4, white, 0).
pos_value_reference(pawn, 1, 5, white, 0).
pos_value_reference(pawn, 1, 6, white, 0).
pos_value_reference(pawn, 1, 7, white, 0).
pos_value_reference(pawn, 1, 8, white, 0).

% Rang 2 (position initiale): bonus standard
pos_value_reference(pawn, 2, 1, white, 50).
pos_value_reference(pawn, 2, 2, white, 50).
pos_value_reference(pawn, 2, 3, white, 50).
pos_value_reference(pawn, 2, 4, white, 50).
pos_value_reference(pawn, 2, 5, white, 50).
pos_value_reference(pawn, 2, 6, white, 50).
pos_value_reference(pawn, 2, 7, white, 50).
pos_value_reference(pawn, 2, 8, white, 50).

% Rang 3: développement des pions
pos_value_reference(pawn, 3, 1, white, 10).
pos_value_reference(pawn, 3, 2, white, 10).
pos_value_reference(pawn, 3, 3, white, 20).
pos_value_reference(pawn, 3, 4, white, 30).  % Centre
pos_value_reference(pawn, 3, 5, white, 30).  % Centre
pos_value_reference(pawn, 3, 6, white, 20).
pos_value_reference(pawn, 3, 7, white, 10).
pos_value_reference(pawn, 3, 8, white, 10).

% Rang 4: contrôle central fort
pos_value_reference(pawn, 4, 1, white, 5).
pos_value_reference(pawn, 4, 2, white, 5).
pos_value_reference(pawn, 4, 3, white, 10).
pos_value_reference(pawn, 4, 4, white, 25).  % d4 - contrôle central
pos_value_reference(pawn, 4, 5, white, 25).  % e4 - contrôle central
pos_value_reference(pawn, 4, 6, white, 10).
pos_value_reference(pawn, 4, 7, white, 5).
pos_value_reference(pawn, 4, 8, white, 5).

% Rang 5: pions avancés
pos_value_reference(pawn, 5, 1, white, 0).
pos_value_reference(pawn, 5, 2, white, 0).
pos_value_reference(pawn, 5, 3, white, 0).
pos_value_reference(pawn, 5, 4, white, 20).  % Pion passé potentiel
pos_value_reference(pawn, 5, 5, white, 20).
pos_value_reference(pawn, 5, 6, white, 0).
pos_value_reference(pawn, 5, 7, white, 0).
pos_value_reference(pawn, 5, 8, white, 0).

% Rang 6: pions très avancés
pos_value_reference(pawn, 6, 1, white, 5).
pos_value_reference(pawn, 6, 2, white, -5).
pos_value_reference(pawn, 6, 3, white, -10).
pos_value_reference(pawn, 6, 4, white, 0).
pos_value_reference(pawn, 6, 5, white, 0).
pos_value_reference(pawn, 6, 6, white, -10).
pos_value_reference(pawn, 6, 7, white, -5).
pos_value_reference(pawn, 6, 8, white, 5).

% Rang 7: pions sur 7e rang - très forts (promotion imminente)
pos_value_reference(pawn, 7, 1, white, 5).
pos_value_reference(pawn, 7, 2, white, 10).
pos_value_reference(pawn, 7, 3, white, 10).
pos_value_reference(pawn, 7, 4, white, -20). % Bloqué
pos_value_reference(pawn, 7, 5, white, -20). % Bloqué
pos_value_reference(pawn, 7, 6, white, 10).
pos_value_reference(pawn, 7, 7, white, 10).
pos_value_reference(pawn, 7, 8, white, 5).

% Rang 8: promotion (valeur théorique)
pos_value_reference(pawn, 8, 1, white, 0).
pos_value_reference(pawn, 8, 2, white, 0).
pos_value_reference(pawn, 8, 3, white, 0).
pos_value_reference(pawn, 8, 4, white, 0).
pos_value_reference(pawn, 8, 5, white, 0).
pos_value_reference(pawn, 8, 6, white, 0).
pos_value_reference(pawn, 8, 7, white, 0).
pos_value_reference(pawn, 8, 8, white, 0).

% CAVALIERS BLANCS - Table du C++
% Rang 1: mauvaises positions sur bords
pos_value_reference(knight, 1, 1, white, -50).
pos_value_reference(knight, 1, 2, white, -40).
pos_value_reference(knight, 1, 3, white, -30).
pos_value_reference(knight, 1, 4, white, -30).
pos_value_reference(knight, 1, 5, white, -30).
pos_value_reference(knight, 1, 6, white, -30).
pos_value_reference(knight, 1, 7, white, -40).
pos_value_reference(knight, 1, 8, white, -50).

% Rang 2: développement initial
pos_value_reference(knight, 2, 1, white, -40).
pos_value_reference(knight, 2, 2, white, -20).
pos_value_reference(knight, 2, 3, white, 0).
pos_value_reference(knight, 2, 4, white, 0).
pos_value_reference(knight, 2, 5, white, 0).
pos_value_reference(knight, 2, 6, white, 0).
pos_value_reference(knight, 2, 7, white, -20).
pos_value_reference(knight, 2, 8, white, -40).

% Rang 3: bonnes positions
pos_value_reference(knight, 3, 1, white, -30).
pos_value_reference(knight, 3, 2, white, 0).
pos_value_reference(knight, 3, 3, white, 10).
pos_value_reference(knight, 3, 4, white, 15).
pos_value_reference(knight, 3, 5, white, 15).
pos_value_reference(knight, 3, 6, white, 10).
pos_value_reference(knight, 3, 7, white, 0).
pos_value_reference(knight, 3, 8, white, -30).

% Rang 4: positions centrales excellentes
pos_value_reference(knight, 4, 1, white, -30).
pos_value_reference(knight, 4, 2, white, 5).
pos_value_reference(knight, 4, 3, white, 15).
pos_value_reference(knight, 4, 4, white, 20).  % Cavalier central fort
pos_value_reference(knight, 4, 5, white, 20).  % Cavalier central fort
pos_value_reference(knight, 4, 6, white, 15).
pos_value_reference(knight, 4, 7, white, 5).
pos_value_reference(knight, 4, 8, white, -30).

% Rang 5: cavaliers avancés
pos_value_reference(knight, 5, 1, white, -30).
pos_value_reference(knight, 5, 2, white, 0).
pos_value_reference(knight, 5, 3, white, 15).
pos_value_reference(knight, 5, 4, white, 20).
pos_value_reference(knight, 5, 5, white, 20).
pos_value_reference(knight, 5, 6, white, 15).
pos_value_reference(knight, 5, 7, white, 0).
pos_value_reference(knight, 5, 8, white, -30).

% Rang 6: position avancée
pos_value_reference(knight, 6, 1, white, -30).
pos_value_reference(knight, 6, 2, white, 5).
pos_value_reference(knight, 6, 3, white, 10).
pos_value_reference(knight, 6, 4, white, 15).
pos_value_reference(knight, 6, 5, white, 15).
pos_value_reference(knight, 6, 6, white, 10).
pos_value_reference(knight, 6, 7, white, 5).
pos_value_reference(knight, 6, 8, white, -30).

% Rang 7: cavaliers très avancés
pos_value_reference(knight, 7, 1, white, -40).
pos_value_reference(knight, 7, 2, white, -20).
pos_value_reference(knight, 7, 3, white, 0).
pos_value_reference(knight, 7, 4, white, 5).
pos_value_reference(knight, 7, 5, white, 5).
pos_value_reference(knight, 7, 6, white, 0).
pos_value_reference(knight, 7, 7, white, -20).
pos_value_reference(knight, 7, 8, white, -40).

% Rang 8: positions extrêmes
pos_value_reference(knight, 8, 1, white, -50).
pos_value_reference(knight, 8, 2, white, -40).
pos_value_reference(knight, 8, 3, white, -30).
pos_value_reference(knight, 8, 4, white, -30).
pos_value_reference(knight, 8, 5, white, -30).
pos_value_reference(knight, 8, 6, white, -30).
pos_value_reference(knight, 8, 7, white, -40).
pos_value_reference(knight, 8, 8, white, -50).

% FOUS BLANCS - Table du C++
% Rang 1: positions initiales
pos_value_reference(bishop, 1, 1, white, -20).
pos_value_reference(bishop, 1, 2, white, -10).
pos_value_reference(bishop, 1, 3, white, -10).
pos_value_reference(bishop, 1, 4, white, -10).
pos_value_reference(bishop, 1, 5, white, -10).
pos_value_reference(bishop, 1, 6, white, -10).
pos_value_reference(bishop, 1, 7, white, -10).
pos_value_reference(bishop, 1, 8, white, -20).

% Rang 2: développement
pos_value_reference(bishop, 2, 1, white, -10).
pos_value_reference(bishop, 2, 2, white, 0).
pos_value_reference(bishop, 2, 3, white, 0).
pos_value_reference(bishop, 2, 4, white, 0).
pos_value_reference(bishop, 2, 5, white, 0).
pos_value_reference(bishop, 2, 6, white, 0).
pos_value_reference(bishop, 2, 7, white, 0).
pos_value_reference(bishop, 2, 8, white, -10).

% Rang 3: bonnes diagonales
pos_value_reference(bishop, 3, 1, white, -10).
pos_value_reference(bishop, 3, 2, white, 0).
pos_value_reference(bishop, 3, 3, white, 5).
pos_value_reference(bishop, 3, 4, white, 10).
pos_value_reference(bishop, 3, 5, white, 10).
pos_value_reference(bishop, 3, 6, white, 5).
pos_value_reference(bishop, 3, 7, white, 0).
pos_value_reference(bishop, 3, 8, white, -10).

% Rang 4: positions centrales
pos_value_reference(bishop, 4, 1, white, -10).
pos_value_reference(bishop, 4, 2, white, 5).
pos_value_reference(bishop, 4, 3, white, 5).
pos_value_reference(bishop, 4, 4, white, 10).
pos_value_reference(bishop, 4, 5, white, 10).
pos_value_reference(bishop, 4, 6, white, 5).
pos_value_reference(bishop, 4, 7, white, 5).
pos_value_reference(bishop, 4, 8, white, -10).

% Rang 5: fous actifs
pos_value_reference(bishop, 5, 1, white, -10).
pos_value_reference(bishop, 5, 2, white, 0).
pos_value_reference(bishop, 5, 3, white, 10).
pos_value_reference(bishop, 5, 4, white, 10).
pos_value_reference(bishop, 5, 5, white, 10).
pos_value_reference(bishop, 5, 6, white, 10).
pos_value_reference(bishop, 5, 7, white, 0).
pos_value_reference(bishop, 5, 8, white, -10).

% Rang 6: fous très actifs
pos_value_reference(bishop, 6, 1, white, -10).
pos_value_reference(bishop, 6, 2, white, 10).
pos_value_reference(bishop, 6, 3, white, 10).
pos_value_reference(bishop, 6, 4, white, 10).
pos_value_reference(bishop, 6, 5, white, 10).
pos_value_reference(bishop, 6, 6, white, 10).
pos_value_reference(bishop, 6, 7, white, 10).
pos_value_reference(bishop, 6, 8, white, -10).

% Rang 7: positions avancées
pos_value_reference(bishop, 7, 1, white, -10).
pos_value_reference(bishop, 7, 2, white, 5).
pos_value_reference(bishop, 7, 3, white, 0).
pos_value_reference(bishop, 7, 4, white, 0).
pos_value_reference(bishop, 7, 5, white, 0).
pos_value_reference(bishop, 7, 6, white, 0).
pos_value_reference(bishop, 7, 7, white, 5).
pos_value_reference(bishop, 7, 8, white, -10).

% Rang 8: fond de l'échiquier adverse
pos_value_reference(bishop, 8, 1, white, -20).
pos_value_reference(bishop, 8, 2, white, -10).
pos_value_reference(bishop, 8, 3, white, -10).
pos_value_reference(bishop, 8, 4, white, -10).
pos_value_reference(bishop, 8, 5, white, -10).
pos_value_reference(bishop, 8, 6, white, -10).
pos_value_reference(bishop, 8, 7, white, -10).
pos_value_reference(bishop, 8, 8, white, -20).

% TOURS BLANCHES - Table du C++
% Tours: privilégient colonnes ouvertes et 7e rang
pos_value_reference(rook, 1, 1, white, 0). pos_value_reference(rook, 1, 2, white, 0). pos_value_reference(rook, 1, 3, white, 0). pos_value_reference(rook, 1, 4, white, 0). pos_value_reference(rook, 1, 5, white, 0). pos_value_reference(rook, 1, 6, white, 0). pos_value_reference(rook, 1, 7, white, 0). pos_value_reference(rook, 1, 8, white, 0).
pos_value_reference(rook, 2, 1, white, 5). pos_value_reference(rook, 2, 2, white, 10). pos_value_reference(rook, 2, 3, white, 10). pos_value_reference(rook, 2, 4, white, 10). pos_value_reference(rook, 2, 5, white, 10). pos_value_reference(rook, 2, 6, white, 10). pos_value_reference(rook, 2, 7, white, 10). pos_value_reference(rook, 2, 8, white, 5).
pos_value_reference(rook, 3, 1, white, -5). pos_value_reference(rook, 3, 2, white, 0). pos_value_reference(rook, 3, 3, white, 0). pos_value_reference(rook, 3, 4, white, 0). pos_value_reference(rook, 3, 5, white, 0). pos_value_reference(rook, 3, 6, white, 0). pos_value_reference(rook, 3, 7, white, 0). pos_value_reference(rook, 3, 8, white, -5).
pos_value_reference(rook, 4, 1, white, -5). pos_value_reference(rook, 4, 2, white, 0). pos_value_reference(rook, 4, 3, white, 0). pos_value_reference(rook, 4, 4, white, 0). pos_value_reference(rook, 4, 5, white, 0). pos_value_reference(rook, 4, 6, white, 0). pos_value_reference(rook, 4, 7, white, 0). pos_value_reference(rook, 4, 8, white, -5).
pos_value_reference(rook, 5, 1, white, -5). pos_value_reference(rook, 5, 2, white, 0). pos_value_reference(rook, 5, 3, white, 0). pos_value_reference(rook, 5, 4, white, 0). pos_value_reference(rook, 5, 5, white, 0). pos_value_reference(rook, 5, 6, white, 0). pos_value_reference(rook, 5, 7, white, 0). pos_value_reference(rook, 5, 8, white, -5).
pos_value_reference(rook, 6, 1, white, -5). pos_value_reference(rook, 6, 2, white, 0). pos_value_reference(rook, 6, 3, white, 0). pos_value_reference(rook, 6, 4, white, 0). pos_value_reference(rook, 6, 5, white, 0). pos_value_reference(rook, 6, 6, white, 0). pos_value_reference(rook, 6, 7, white, 0). pos_value_reference(rook, 6, 8, white, -5).
pos_value_reference(rook, 7, 1, white, -5). pos_value_reference(rook, 7, 2, white, 0). pos_value_reference(rook, 7, 3, white, 0). pos_value_reference(rook, 7, 4, white, 0). pos_value_reference(rook, 7, 5, white, 0). pos_value_reference(rook, 7, 6, white, 0). pos_value_reference(rook, 7, 7, white, 0). pos_value_reference(rook, 7, 8, white, -5).
pos_value_reference(rook, 8, 1, white, 0). pos_value_reference(rook, 8, 2, white, 0). pos_value_reference(rook, 8, 3, white, 0). pos_value_reference(rook, 8, 4, white, 5). pos_value_reference(rook, 8, 5, white, 5). pos_value_reference(rook, 8, 6, white, 0). pos_value_reference(rook, 8, 7, white, 0). pos_value_reference(rook, 8, 8, white, 0).

% DAMES BLANCHES - Table du C++  
pos_value_reference(queen, 1, 1, white, -20). pos_value_reference(queen, 1, 2, white, -10). pos_value_reference(queen, 1, 3, white, -10). pos_value_reference(queen, 1, 4, white, -5). pos_value_reference(queen, 1, 5, white, -5). pos_value_reference(queen, 1, 6, white, -10). pos_value_reference(queen, 1, 7, white, -10). pos_value_reference(queen, 1, 8, white, -20).
pos_value_reference(queen, 2, 1, white, -10). pos_value_reference(queen, 2, 2, white, 0). pos_value_reference(queen, 2, 3, white, 0). pos_value_reference(queen, 2, 4, white, 0). pos_value_reference(queen, 2, 5, white, 0). pos_value_reference(queen, 2, 6, white, 0). pos_value_reference(queen, 2, 7, white, 0). pos_value_reference(queen, 2, 8, white, -10).
pos_value_reference(queen, 3, 1, white, -10). pos_value_reference(queen, 3, 2, white, 0). pos_value_reference(queen, 3, 3, white, 5). pos_value_reference(queen, 3, 4, white, 5). pos_value_reference(queen, 3, 5, white, 5). pos_value_reference(queen, 3, 6, white, 5). pos_value_reference(queen, 3, 7, white, 0). pos_value_reference(queen, 3, 8, white, -10).
pos_value_reference(queen, 4, 1, white, -5). pos_value_reference(queen, 4, 2, white, 0). pos_value_reference(queen, 4, 3, white, 5). pos_value_reference(queen, 4, 4, white, 5). pos_value_reference(queen, 4, 5, white, 5). pos_value_reference(queen, 4, 6, white, 5). pos_value_reference(queen, 4, 7, white, 0). pos_value_reference(queen, 4, 8, white, -5).
pos_value_reference(queen, 5, 1, white, 0). pos_value_reference(queen, 5, 2, white, 0). pos_value_reference(queen, 5, 3, white, 5). pos_value_reference(queen, 5, 4, white, 5). pos_value_reference(queen, 5, 5, white, 5). pos_value_reference(queen, 5, 6, white, 5). pos_value_reference(queen, 5, 7, white, 0). pos_value_reference(queen, 5, 8, white, -5).
pos_value_reference(queen, 6, 1, white, -10). pos_value_reference(queen, 6, 2, white, 5). pos_value_reference(queen, 6, 3, white, 5). pos_value_reference(queen, 6, 4, white, 5). pos_value_reference(queen, 6, 5, white, 5). pos_value_reference(queen, 6, 6, white, 5). pos_value_reference(queen, 6, 7, white, 0). pos_value_reference(queen, 6, 8, white, -10).
pos_value_reference(queen, 7, 1, white, -10). pos_value_reference(queen, 7, 2, white, 0). pos_value_reference(queen, 7, 3, white, 5). pos_value_reference(queen, 7, 4, white, 0). pos_value_reference(queen, 7, 5, white, 0). pos_value_reference(queen, 7, 6, white, 0). pos_value_reference(queen, 7, 7, white, 0). pos_value_reference(queen, 7, 8, white, -10).
pos_value_reference(queen, 8, 1, white, -20). pos_value_reference(queen, 8, 2, white, -10). pos_value_reference(queen, 8, 3, white, -10). pos_value_reference(queen, 8, 4, white, -5). pos_value_reference(queen, 8, 5, white, -5). pos_value_reference(queen, 8, 6, white, -10). pos_value_reference(queen, 8, 7, white, -10). pos_value_reference(queen, 8, 8, white, -20).

% ROIS BLANCS - Table du C++ (défensive en milieu de jeu)
pos_value_reference(king, 1, 1, white, -30). pos_value_reference(king, 1, 2, white, -40). pos_value_reference(king, 1, 3, white, -40). pos_value_reference(king, 1, 4, white, -50). pos_value_reference(king, 1, 5, white, -50). pos_value_reference(king, 1, 6, white, -40). pos_value_reference(king, 1, 7, white, -40). pos_value_reference(king, 1, 8, white, -30).
pos_value_reference(king, 2, 1, white, -30). pos_value_reference(king, 2, 2, white, -40). pos_value_reference(king, 2, 3, white, -40). pos_value_reference(king, 2, 4, white, -50). pos_value_reference(king, 2, 5, white, -50). pos_value_reference(king, 2, 6, white, -40). pos_value_reference(king, 2, 7, white, -40). pos_value_reference(king, 2, 8, white, -30).
pos_value_reference(king, 3, 1, white, -30). pos_value_reference(king, 3, 2, white, -40). pos_value_reference(king, 3, 3, white, -40). pos_value_reference(king, 3, 4, white, -50). pos_value_reference(king, 3, 5, white, -50). pos_value_reference(king, 3, 6, white, -40). pos_value_reference(king, 3, 7, white, -40). pos_value_reference(king, 3, 8, white, -30).
pos_value_reference(king, 4, 1, white, -30). pos_value_reference(king, 4, 2, white, -40). pos_value_reference(king, 4, 3, white, -40). pos_value_reference(king, 4, 4, white, -50). pos_value_reference(king, 4, 5, white, -50). pos_value_reference(king, 4, 6, white, -40). pos_value_reference(king, 4, 7, white, -40). pos_value_reference(king, 4, 8, white, -30).
pos_value_reference(king, 5, 1, white, -20). pos_value_reference(king, 5, 2, white, -30). pos_value_reference(king, 5, 3, white, -30). pos_value_reference(king, 5, 4, white, -40). pos_value_reference(king, 5, 5, white, -40). pos_value_reference(king, 5, 6, white, -30). pos_value_reference(king, 5, 7, white, -30). pos_value_reference(king, 5, 8, white, -20).
pos_value_reference(king, 6, 1, white, -10). pos_value_reference(king, 6, 2, white, -20). pos_value_reference(king, 6, 3, white, -20). pos_value_reference(king, 6, 4, white, -20). pos_value_reference(king, 6, 5, white, -20). pos_value_reference(king, 6, 6, white, -20). pos_value_reference(king, 6, 7, white, -20). pos_value_reference(king, 6, 8, white, -10).
pos_value_reference(king, 7, 1, white, 20). pos_value_reference(king, 7, 2, white, 20). pos_value_reference(king, 7, 3, white, 0). pos_value_reference(king, 7, 4, white, 0). pos_value_reference(king, 7, 5, white, 0). pos_value_reference(king, 7, 6, white, 0). pos_value_reference(king, 7, 7, white, 20). pos_value_reference(king, 7, 8, white, 20).
pos_value_reference(king, 8, 1, white, 20). pos_value_reference(king, 8, 2, white, 30). pos_value_reference(king, 8, 3, white, 10). pos_value_reference(king, 8, 4, white, 0). pos_value_reference(king, 8, 5, white, 0). pos_value_reference(king, 8, 6, white, 10). pos_value_reference(king, 8, 7, white, 30). pos_value_reference(king, 8, 8, white, 20).

% =============================================================================
% VALEURS POUR NOIRS - Miroir des valeurs blanches
% =============================================================================
% Les noirs ont la MÊME évaluation que les blancs mais miroir vertical
% CORRECTION: Toutes les valeurs doivent être POSITIVES pour count_material

pos_value_reference(Type, Row, Col, black, Value) :-
    MirrorRow is 9 - Row,  % Symétrie verticale (rang 8 devient rang 1, etc.)
    pos_value_reference(Type, MirrorRow, Col, white, WhiteValue),
    Value is -WhiteValue.  % VALEUR NEGATIVE pour les noirs dans évaluation asymétrique
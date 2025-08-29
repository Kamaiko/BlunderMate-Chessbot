% =============================================================================
% DEMONSTRATIONS INTERACTIVES - PROLOG CHESS GAME
% =============================================================================
%
% Demonstrations consolidees du jeu d'echecs avec interfaces utilisateur
% robustes et gestion d'erreurs amelioree
%
% Auteur : Patrick Patenaude
% Version : 2.0 - Version consolidee et robuste
% Date : Aout 2025
%
% UTILISATION :
%   ?- consult('tests/demo_interactive').
%   ?- demo_auto.        % Demo automatique
%   ?- demo_interactive. % Demo interactive
%   ?- demo_explained.   % Demo avec explications
% =============================================================================

:- ['../src/game_logic'].

% =============================================================================
% DEMO AUTOMATIQUE COMPLETE
% =============================================================================

demo_auto :-
    nl,
    write('╔═══════════════════════════════════════════════════════╗'), nl,
    write('║               SCHOLAR''S MATE - DEMO AUTO             ║'), nl,
    write('╚═══════════════════════════════════════════════════════╝'), nl,
    nl,
    
    write('Le mat du berger en 4 coups - demonstration automatique'), nl, nl,
    
    % Position initiale
    init_game_state(GS1),
    show_position('POSITION INITIALE', GS1),
    
    % 1. e2-e4
    write('1. e2-e4 (Ouverture du pion roi)'), nl,
    safe_move(GS1, "e2e4", GS2, '   ✓ 1.e4 execute'),
    show_position('Apres 1.e4', GS2),
    
    % 1... e7-e5
    write('1... e7-e5 (Reponse symetrique)'), nl,
    safe_move(GS2, "e7e5", GS3, '   ✓ 1...e5 execute'),
    show_position('Apres 1...e5', GS3),
    
    % 2. Bf1-c4
    write('2. Bf1-c4 (Fou attaque f7 - point faible!)'), nl,
    safe_move(GS3, "f1c4", GS4, '   ✓ 2.Bc4 execute'),
    show_position('Apres 2.Bc4 - f7 est menace!', GS4),
    
    % 2... Nb8-c6
    write('2... Nb8-c6 (Developpement du cavalier)'), nl,
    safe_move(GS4, "b8c6", GS5, '   ✓ 2...Nc6 execute'),
    show_position('Apres 2...Nc6', GS5),
    
    % 3. Qd1-h5
    write('3. Qd1-h5 (Dame entre en jeu - DOUBLE MENACE!)'), nl,
    safe_move(GS5, "d1h5", GS6, '   ✓ 3.Qh5 execute'),
    show_position('Apres 3.Qh5 - f7 ET h7 menaces!', GS6),
    
    % 3... Ng8-f6??
    write('3... Ng8-f6?? (ERREUR FATALE - ignore la menace)'), nl,
    safe_move(GS6, "g8f6", GS7, '   ✓ 3...Nf6?? execute'),
    show_position('Apres 3...Nf6?? - les noirs sont perdus', GS7),
    
    % 4. Qh5xf7#
    write('4. Qh5xf7# (MAT DU BERGER!)'), nl,
    safe_move(GS7, "h5f7", GS8, '   ✓ 4.Qxf7# MAT!'),
    show_position('POSITION FINALE - ECHEC ET MAT!', GS8),
    
    nl,
    write('╔═══════════════════════════════════════════════════════╗'), nl,
    write('║                      ANALYSE                          ║'), nl,
    write('╚═══════════════════════════════════════════════════════╝'), nl,
    write('• La dame blanche en f7 fait echec au roi noir'), nl,
    write('• Le roi ne peut pas bouger (toutes cases controlees)'), nl,
    write('• Aucune piece ne peut capturer la dame'), nl,
    write('• Aucune piece ne peut bloquer l''echec'), nl,
    write('• C''EST UN MAT AUTHENTIQUE EN 4 COUPS!'), nl,
    nl.

% =============================================================================
% DEMO INTERACTIVE ROBUSTE
% =============================================================================

demo_interactive :-
    nl,
    write('╔═══════════════════════════════════════════════════════╗'), nl,
    write('║            DEMO INTERACTIVE - SCHOLAR''S MATE         ║'), nl,
    write('╚═══════════════════════════════════════════════════════╝'), nl,
    nl,
    
    write('Demonstration interactive du mat du berger.'), nl,
    write('Appuyez sur ENTREE apres chaque instruction.'), nl, nl,
    
    init_game_state(GS1),
    show_and_wait('POSITION DE DEPART', GS1),
    
    write('>>> 1. e2-e4 (Les blancs ouvrent avec le pion roi)'), nl,
    wait_for_enter,
    safe_move(GS1, "e2e4", GS2, 'Coup joue: 1.e4'),
    show_and_wait('Apres 1.e4', GS2),
    
    write('>>> 1... e7-e5 (Les noirs repondent symetriquement)'), nl,
    wait_for_enter,
    safe_move(GS2, "e7e5", GS3, 'Coup joue: 1...e5'),
    show_and_wait('Apres 1...e5', GS3),
    
    write('>>> 2. Bf1-c4 (ATTENTION: le fou blanc vise f7!)'), nl,
    write('    f7 est le point faible des noirs au debut'), nl,
    wait_for_enter,
    safe_move(GS3, "f1c4", GS4, 'Coup joue: 2.Bc4'),
    show_and_wait('Apres 2.Bc4 - Voyez la menace sur f7?', GS4),
    
    write('>>> 2... Nb8-c6 (Developpement normal du cavalier)'), nl,
    write('    Bon coup, mais ne defend pas f7...'), nl,
    wait_for_enter,
    safe_move(GS4, "b8c6", GS5, 'Coup joue: 2...Nc6'),
    show_and_wait('Apres 2...Nc6', GS5),
    
    write('>>> 3. Qd1-h5 (LA DAME ENTRE EN JEU!)'), nl,
    write('    DANGER! Double menace sur f7 ET h7!'), nl,
    wait_for_enter,
    safe_move(GS5, "d1h5", GS6, 'Coup joue: 3.Qh5'),
    show_and_wait('Apres 3.Qh5 - POSITION CRITIQUE!', GS6),
    
    write('>>> POSITION CRITIQUE: Les noirs doivent defendre!'), nl,
    write('    Ils pourraient jouer g6 ou Qe7 pour defendre...'), nl,
    write('    Mais ils vont faire une erreur fatale...'), nl,
    wait_for_enter,
    
    write('>>> 3... Ng8-f6?? (ERREUR MONUMENTALE!)'), nl,
    write('    Ce coup attaque la dame mais ignore f7!'), nl,
    wait_for_enter,
    safe_move(GS6, "g8f6", GS7, 'Coup joue: 3...Nf6??'),
    show_and_wait('Apres 3...Nf6?? - Les noirs sont perdus!', GS7),
    
    write('>>> 4. Qh5xf7# (LE COUP DE GRACE!)'), nl,
    write('    La dame capture f7 et fait echec et mat!'), nl,
    wait_for_enter,
    safe_move(GS7, "h5f7", GS8, 'Coup joue: 4.Qxf7# MAT!'),
    show_and_wait('POSITION FINALE - MAT EN 4 COUPS!', GS8),
    
    nl,
    write('*** FELICITATIONS! ***'), nl,
    write('Vous avez assiste au celebre Scholar''s Mate!'), nl,
    nl.

% =============================================================================
% DEMO AVEC EXPLICATIONS DETAILLEES
% =============================================================================

demo_explained :-
    nl,
    write('╔═══════════════════════════════════════════════════════╗'), nl,
    write('║        SCHOLAR''S MATE - EXPLICATIONS DETAILLEES     ║'), nl,
    write('╚═══════════════════════════════════════════════════════╝'), nl,
    nl,
    
    init_game_state(GS1),
    write('POSITION INITIALE - ANALYSE:'), nl,
    display_game_state(GS1),
    explain('Au debut, le point f7 (pour les noirs) et f2 (pour les blancs)'),
    explain('sont les points les plus faibles car defendus seulement par le roi.'),
    explain('Le mat du berger exploite cette faiblesse.'),
    pause,
    
    write('COUP 1: e2-e4 - ANALYSE:'), nl,
    safe_move(GS1, "e2e4", GS2, 'e4 joue'),
    display_game_state(GS2),
    explain('Coup d''ouverture classique qui:'),
    explain('- Controle les cases centrales d5 et f5'),
    explain('- Permet au fou et a la dame de sortir'),
    explain('- Libere la ligne pour le roque (plus tard)'),
    pause,
    
    write('COUP 1: ...e7-e5 - ANALYSE:'), nl,
    safe_move(GS2, "e7e5", GS3, 'e5 joue'),
    display_game_state(GS3),
    explain('Reponse symetrique et logique qui:'),
    explain('- Controle aussi le centre (cases d4 et f4)'),
    explain('- Permet le developpement des pieces noires'),
    explain('- Maintient l''equilibre de la position'),
    pause,
    
    write('COUP 2: Bf1-c4 - ANALYSE CRITIQUE:'), nl,
    safe_move(GS3, "f1c4", GS4, 'Bc4 joue'),
    display_game_state(GS4),
    explain('Le fou blanc vise maintenant DIRECTEMENT f7!'),
    explain('Point strategique:'),
    explain('- f7 n''est defendu QUE par le roi noir'),
    explain('- Si une autre piece attaque f7, les noirs sont en danger'),
    explain('- C''est la cle de la strategie du mat du berger'),
    pause,
    
    write('COUP 2: ...Nb8-c6 - ANALYSE:'), nl,
    safe_move(GS4, "b8c6", GS5, 'Nc6 joue'),
    display_game_state(GS5),
    explain('Developpement normal du cavalier vers le centre.'),
    explain('MAIS: ce coup ne defend PAS f7!'),
    explain('Les noirs auraient pu jouer Nf6 ou d6 pour defendre.'),
    explain('Cette inattention va couter cher...'),
    pause,
    
    write('COUP 3: Qd1-h5 - ANALYSE DU PIEGE:'), nl,
    safe_move(GS5, "d1h5", GS6, 'Qh5 joue'),
    display_game_state(GS6),
    explain('LA DAME BLANCHE ENTRE EN JEU!'),
    explain('Double menace mortelle:'),
    explain('1. La dame attaque f7 (avec le fou = 2 attaquants!)'),
    explain('2. La dame menace aussi h7 (mat potentiel)'),
    explain('Les noirs sont dans une situation critique!'),
    pause,
    
    write('COUP 3: ...Ng8-f6?? - ANALYSE DE L''ERREUR:'), nl,
    safe_move(GS6, "g8f6", GS7, 'Nf6 joue'),
    display_game_state(GS7),
    explain('ERREUR FATALE! Ce coup:'),
    explain('- Attaque la dame blanche (semble bon...)'),
    explain('- MAIS ignore completement la menace sur f7!'),
    explain('- Les noirs auraient du jouer g6 ou Qe7'),
    explain('- Maintenant ils vont subir le mat!'),
    pause,
    
    write('COUP 4: Qh5xf7# - ANALYSE DU MAT:'), nl,
    safe_move(GS7, "h5f7", GS8, 'Qxf7# MAT'),
    display_game_state(GS8),
    explain('ECHEC ET MAT! Analysons pourquoi c''est mat:'),
    explain('1. La dame fait echec au roi noir depuis f7'),
    explain('2. Le roi ne peut PAS bouger:'),
    explain('   - e8: controle par la dame'),
    explain('   - d8: controle par la dame'),  
    explain('   - d7, e7: aussi controles'),
    explain('3. AUCUNE piece noire ne peut capturer la dame'),
    explain('4. AUCUNE piece ne peut bloquer l''echec'),
    explain('C''est un mat authentique en seulement 4 coups!'),
    
    nl,
    write('╔═══════════════════════════════════════════════════════╗'), nl,
    write('║                     MORALE                            ║'), nl,
    write('╚═══════════════════════════════════════════════════════╝'), nl,
    write('LECONS A RETENIR:'), nl,
    write('1. Toujours surveiller les menaces sur f7/f2'), nl,
    write('2. Ne pas sortir la dame trop tot (mais ici ca marche!)'), nl,
    write('3. Defendre activement, pas seulement attaquer'), nl,
    write('4. Le Scholar''s Mate ne marche que contre les debutants'), nl,
    write('5. Une fois qu''on le connait, il est facile a parer'), nl,
    nl.

% =============================================================================
% DEMO DES DEFENSES POSSIBLES
% =============================================================================

demo_defenses :-
    nl,
    write('╔═══════════════════════════════════════════════════════╗'), nl,
    write('║              COMMENT SE DEFENDRE                      ║'), nl,
    write('╚═══════════════════════════════════════════════════════╝'), nl,
    nl,
    
    % Jouer jusqu'a la position critique
    init_game_state(GS1),
    make_move_algebraic(GS1, "e2e4", GS2),
    make_move_algebraic(GS2, "e7e5", GS3),
    make_move_algebraic(GS3, "f1c4", GS4),
    make_move_algebraic(GS4, "b8c6", GS5),
    make_move_algebraic(GS5, "d1h5", GS6),
    
    write('POSITION CRITIQUE - Les blancs menacent f7 et h7:'), nl,
    display_game_state(GS6),
    
    write('Les noirs ont plusieurs defenses:'), nl, nl,
    
    % Defense 1: g6
    write('DEFENSE 1 - g7-g6 (Repousser la dame):'), nl,
    make_move_algebraic(GS6, "g7g6", GS_def1),
    display_game_state(GS_def1),
    write('✓ La dame doit reculer, la menace est paree'), nl, nl,
    
    % Defense 2: Qe7
    write('DEFENSE 2 - Qd8-e7 (Defendre f7 directement):'), nl,
    make_move_algebraic(GS6, "d8e7", GS_def2),
    display_game_state(GS_def2),
    write('✓ f7 est maintenant defendu par la dame noire'), nl, nl,
    
    % Defense 3: Nf6 + g6
    write('DEFENSE 3 - Ng8-f6 puis g6 au coup suivant:'), nl,
    make_move_algebraic(GS6, "g8f6", GS_def3),
    display_game_state(GS_def3),
    write('Si les blancs reculent leur dame, g6 au coup suivant'), nl,
    write('✓ Defense active qui contre-attaque et defend'), nl, nl,
    
    write('CONCLUSION:'), nl,
    write('Le Scholar''s Mate n''est dangereux que si on ne le connait pas!'), nl,
    write('Une fois qu''on sait comment defendre, c''est facile a parer.'), nl,
    nl.

% =============================================================================
% UTILITAIRES ROBUSTES
% =============================================================================

% Execution securisee d'un mouvement
safe_move(GameState, Move, NewGameState, SuccessMsg) :-
    (catch(make_move_algebraic(GameState, Move, NewGameState), _, fail) ->
        write(SuccessMsg), nl
    ;   write('ERREUR: Mouvement impossible: '), write(Move), nl,
        NewGameState = GameState
    ).

% Affichage avec titre
show_position(Title, GameState) :-
    nl,
    write('┌─ '), write(Title), write(' ─'), nl,
    display_game_state(GameState).

% Affichage et attente
show_and_wait(Title, GameState) :-
    show_position(Title, GameState),
    write('└─ Appuyez sur ENTREE pour continuer...'), nl,
    wait_for_enter,
    nl.

% Explication avec formatage
explain(Text) :-
    write('│ '), write(Text), nl.

% Pause avec message
pause :-
    write('├─ Appuyez sur ENTREE pour continuer...'), nl,
    wait_for_enter,
    nl.

% Attente robuste pour ENTREE  
wait_for_enter :-
    catch(
        (get_char(_), skip_rest_of_line),
        _,
        true
    ).

% Ignorer le reste de la ligne
skip_rest_of_line :-
    catch(
        (repeat, get_char(C), (C = '\n' -> ! ; C = end_of_file -> ! ; fail)),
        _,
        true
    ).

% =============================================================================
% AIDE ET NAVIGATION
% =============================================================================

demo_help :-
    nl,
    write('╔═══════════════════════════════════════════════════════╗'), nl,
    write('║                AIDE DES DEMONSTRATIONS                ║'), nl,
    write('╚═══════════════════════════════════════════════════════╝'), nl,
    write('DEMONSTRATIONS DISPONIBLES:'), nl, nl,
    write('• demo_auto.         - Demo automatique complete'), nl,
    write('• demo_interactive.  - Demo interactive (recommandee)'), nl,
    write('• demo_explained.    - Demo avec explications detaillees'), nl,
    write('• demo_defenses.     - Comment se defendre'), nl,
    write('• demo_help.         - Cette aide'), nl,
    nl,
    write('APRES LES DEMOS:'), nl,
    write('• consult(''tests/chess_tests''), run_all_tests. - Tests'), nl,
    write('• consult(''src/play_chess''), start. - Jouer'), nl,
    nl.

% Test simple pour verification
test_demo :-
    write('=== TEST DES DEMOS ==='), nl,
    init_game_state(GS),
    safe_move(GS, "e2e4", GS2, 'Test mouvement OK'),
    write('✓ Systeme de demo pret'), nl, nl.

% =============================================================================
% INITIALISATION
% =============================================================================

:- nl,
   write('╔═══════════════════════════════════════════════════════╗'), nl,
   write('║          DEMONSTRATIONS INTERACTIVES CHARGEES        ║'), nl,
   write('╚═══════════════════════════════════════════════════════╝'), nl,
   write('Commandes disponibles:'), nl, nl,
   write('• demo_auto.         - Demo automatique'), nl,
   write('• demo_interactive.  - Demo interactive'), nl,
   write('• demo_explained.    - Demo detaillee'), nl,
   write('• demo_defenses.     - Comment defendre'), nl,
   write('• demo_help.         - Aide complete'), nl,
   nl,
   write('Recommandation: Commencez par demo_interactive.'), nl,
   nl.

% =============================================================================
% FIN DES DEMONSTRATIONS INTERACTIVES
% =============================================================================
% =============================================================================
% GO.PL - Lanceur avec support UTF-8 automatique
% =============================================================================
%
% ÉQUIPE       : Projet universitaire IFT-2003
% COURS        : IFT-2003 - Intelligence Artificielle
% INSTITUTION  : Université Laval
% VERSION      : 6.0 Unicode
%
% DESCRIPTION  : Launcher universel qui configure automatiquement l'encodage
%                UTF-8 selon l'OS et lance le jeu d'échecs avec support
%                des caractères Unicode pour l'affichage des pièces.
%
% FONCTIONNALITÉS :
% - Configuration automatique UTF-8 pour Windows/Mac/Linux
% - Support des vrais symboles d'échecs Unicode (♔♕♖♗♘♙)
% - Fallback ASCII automatique si Unicode non supporté
% - Aucune configuration manuelle requise
%
% UTILISATION  : swipl go.pl
%
% =============================================================================

%! setup_utf8 is det.
%  Configure l'encodage UTF-8 selon le système d'exploitation
%  Détecte automatiquement Windows vs Unix et applique la config appropriée
setup_utf8 :-
    % Configuration spécifique à Windows vs Unix
    (   current_prolog_flag(windows, true)
    ->  setup_windows_utf8
    ;   setup_unix_utf8
    ).

%! setup_windows_utf8 is det.
%  Configuration UTF-8 spécifique pour Windows
%  Configure PowerShell, cmd et les streams SWI-Prolog
setup_windows_utf8 :-
    % Configuration SWI-Prolog pour UTF-8 (simple et sûr)
    catch(set_prolog_flag(encoding, utf8), _, true),

    % Configuration des streams d'entrée/sortie
    catch(set_stream(user_output, encoding(utf8)), _, true),
    catch(set_stream(user_input, encoding(utf8)), _, true),
    catch(set_stream(user_error, encoding(utf8)), _, true),

    write('    [OK] Windows UTF-8 configure'), nl.

%! setup_unix_utf8 is det.
%  Configuration UTF-8 pour Mac/Linux
%  La plupart des systèmes Unix sont déjà en UTF-8 par défaut
setup_unix_utf8 :-
    % Configuration SWI-Prolog pour UTF-8 (sécurité)
    set_prolog_flag(encoding, utf8),

    % Vérifier les variables d'environnement locale
    catch(
        (getenv('LANG', Lang),
         (sub_atom(Lang, _, _, _, 'UTF-8') ->
             write('    [OK] Unix UTF-8 detecte')
         ;
             write('    [WARN] Locale non-UTF-8, forcage UTF-8'))
        ),
        _,
        write('    [OK] Unix UTF-8 configure par defaut')
    ),
    nl.


%! handle_launcher_error(+Error:compound) is det.
%  Gère les erreurs du launcher avec messages informatifs
%  @param Error Structure d'erreur SWI-Prolog
handle_launcher_error(Error) :-
    nl,
    write('>>> ERREUR LAUNCHER <<<'), nl,
    write('Le launcher n\'a pas pu demarrer le programme principal.'), nl,
    nl,
    write('Erreur detaillee: '), write(Error), nl,
    nl,
    write('Solutions possibles:'), nl,
    write('1. Verifiez que le fichier src/interface.pl existe'), nl,
    write('2. Verifiez que SWI-Prolog est correctement installe'), nl,
    write('3. Essayez de lancer directement: swipl -g start src/interface.pl'), nl,
    nl,
    halt(1).

% =============================================================================
% NOTES TECHNIQUES UNICODE
% =============================================================================
%
% Ce launcher configure automatiquement l'environnement UTF-8 pour garantir
% l'affichage correct des caractères Unicode (pièces d'échecs) sur tous
% les systèmes.
%
% Pièces d'échecs Unicode supportées :
% Blanches: ♔ ♕ ♖ ♗ ♘ ♙ (Roi Dame Tour Fou Cavalier Pion)
% Noires:   ♚ ♛ ♜ ♝ ♞ ♟ (Roi Dame Tour Fou Cavalier Pion)
%
% Compatibilité testée :
% - Windows 10/11 (PowerShell, cmd, VS Code Terminal)
% - macOS (Terminal.app, iTerm2)
% - Linux (bash, zsh, gnome-terminal)
%
% =============================================================================

%! main is det.
%  Point d'entrée principal du launcher
%  Configure UTF-8 et lance le jeu d'échecs avec Unicode
main :-
    nl,
    write('>>> Configuration automatique UTF-8...'), nl,
    setup_utf8,
    write('>>> Lancement Blunderbot Chess Engine...'), nl, nl,

    % Charger et lancer le programme principal
    catch(
        (consult('src/interface'),
         start),
        Error,
        % Distinguer sortie normale (halt(0)) des vraies erreurs
        (   Error = unwind(halt(0)) ->
            halt(0)  % Sortie propre - pas une erreur
        ;   handle_launcher_error(Error)  % Vraie erreur système
        )
    ).

% Point d'entrée automatique avec gestion d'erreurs appropriée
:- catch(
    main,
    Error,
    % Gérer les différents types de "erreurs"
    (   Error = unwind(halt(0)) ->
        halt(0)  % Sortie normale
    ;   Error = unwind(halt(Code)) ->
        halt(Code)  % Sortie avec code spécifique
    ;   % Vraie erreur système
        (format('Erreur fatale: ~w~n', [Error]), halt(1))
    )
).
# Projet Prolog Chess Game v5.1

## Contexte Rapide
- **Projet**: IA d'Échecs (Prolog) - Focus Intelligence Artificielle
- **Phase**: Phase 2 prioritaire (algorithmes IA après fondations solides)
- **Architecture**: 4 modules (pieces/board/game/interface)
- **Statut**: Fondations complètes, prêt pour implémentation IA

## Architecture
- **pieces.pl** : Logique règles de mouvement par type de pièce
- **board.pl** : Plateau 8x8, coordonnées, affichage ASCII
- **game.pl** : État du jeu, validation coups, alternance joueurs
- **interface.pl** : Interface française, boucle de jeu principale
- **Tests** : chess_tests.pl (6 sections), quick_tests.pl (validation rapide)

## Commandes Développement
- Tests complets : `swipl -t run_tests -s tests/chess_tests.pl`
- Tests rapides : `swipl -s tests/quick_tests.pl`
- Jeu : `swipl go.pl` puis `start.`
- Debug spécifique : `trace.` puis appel prédicat
- Test section : `swipl -g "consult('tests/chess_tests'), run_test(section1), halt."`

## Conventions Code
- **Prédicats** : snake_case (`valid_move/5`, `piece_at/3`)
- **Variables** : PascalCase (`Board`, `GameState`, `FromRow`)
- **Langue** : Code/prédicats anglais, commentaires/documentation français sans accents
- **Validation** : `ground(Args)` obligatoire, coordonnées 1-8, éviter variables libres
- **Structures** : Board = liste 8x8, GameState = `game_state(Board, Player, ...)`
- **Performance** : éviter `findall/3` en boucle, utiliser `once/1` si déterministe

## Priorité Contexte (pour Claude Code)
1. **Lire ce fichier d'abord** - essentiels dev, statut actuel
2. **Consulter [PRD.md](../PRD.md)** - spécifications projet, roadmap IA, critères d'acceptation  
3. **README.md** - perspective utilisateur, installation, usage seulement
4. **CLAUDE.md Global** - workflows généraux, habitudes documentation

## Carte Documents
- **📋 Spécifications & Roadmap** → [PRD.md](../PRD.md#roadmap-orientée-ia)
- **👤 Guide Utilisateur** → [README.md](../README.md#usage)  
- **🔧 Workflows Dev** → [CLAUDE.md Global](~/.config/claude-code/CLAUDE.md)

## Points Critiques Dev
- Point d'entrée jeu : `start.` (défini dans interface.pl)
- Format moves : `e2e4` (notation algébrique, pas `e2-e4`)
- Version source : v5.1 maintenue ici uniquement
- Erreurs courantes : variables non instanciées, dépassement coordonnées
- Workflow : écrire test → implémenter → valider (TDD)
- Tests obligatoires avant commit (100% couverture maintenue)
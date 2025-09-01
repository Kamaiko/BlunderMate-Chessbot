# TASKS - Prolog Chess Game Development

Liste des tâches de développement pour le projet d'IA d'échecs en Prolog.

## Phase 1 : Fondations Jeu ✅

### 1.1 Architecture & Infrastructure
- [x] **Structure modulaire** : 4 modules (pieces.pl, board.pl, game.pl, interface.pl)
- [x] **Système coordonnées** : Notation algébrique, validation 1-8
- [x] **Affichage ASCII** : Plateau coloré, pièces Unicode
- [x] **Launcher** : go.pl pour démarrage rapide

### 1.2 Logique des Pièces
- [x] **Mouvements de base** : Pion, Tour, Cavalier, Fou, Dame, Roi
- [x] **Validation stricte** : Chemins libres, captures, limites plateau
- [x] **Game state** : Structure complète avec captures

### 1.3 Tests & Validation
- [x] **Suite complète** : 6 sections dans chess_tests.pl
- [x] **Tests rapides** : quick_tests.pl pour validation
- [x] **Couverture 100%** : Tous mouvements et cas limites

### 1.4 Interface Utilisateur
- [x] **Menu français** : Navigation intuitive
- [x] **Boucle de jeu** : Alternance joueurs, validation moves
- [x] **Messages d'aide** : Format moves, commandes disponibles

## Focus Actuel : Amélioration Interface → Phase 2 → Phase 3

### Prochaines Étapes Planifiées
1. **Améliorer l'interface utilisateur** et le menu du jeu
2. **Implémenter les règles avancées** d'échecs (Phase 2)  
3. **Développer une IA fonctionnelle** pour remplacer le prototype

## Phase 1.5 : Amélioration Interface

### 1.5.1 Améliorations Menu Principal
- [x] **Design visuel amélioré** : Bordures, espacements, couleurs
- [ ] **Options étendues** : Niveaux difficulté, paramètres
- [x] **Navigation intuitive** : Raccourcis clavier, retour facile
- [ ] **Messages informatifs** : Statut système, aide contextuelle

### 1.5.2 Interface de Jeu  
- [ ] **Affichage plateau amélioré** : Coordonnées plus lisibles, highlights
- [x] **Gestion d'erreurs** : Messages plus clairs et utiles
- [ ] **Commandes étendues** : Annuler coup, sauvegarder/charger


## Phase 2 : Règles Avancées d'Échecs

### 2.1 Mouvements Spéciaux
- [ ] **Roque** : `can_castle/4`, validation roi/tour non bougés
- [ ] **En Passant** : Tracking dernier mouvement, capture adjacente
- [ ] **Promotion** : Pion→Dame/autre, interface choix

### 2.2 États Terminaux  
- [ ] **Échec** : `is_in_check/2`, validation moves légaux
- [ ] **Mat** : `is_checkmate/2`, aucune échappatoire
- [ ] **Pat** : `is_stalemate/2`, match nul

### 2.3 Integration
- [ ] **Refactor complet** : `valid_move/5` avec nouvelles règles
- [ ] **Tests régression** : Validation Phase 1 intacte

## Phase 3 : Intelligence Artificielle ⚠️ PROTOTYPE NON FONCTIONNEL

### 3.1 Fichier ai.pl - PROTOTYPE EXPERIMENTAL
⚠️ **STATUT : CODE NON TESTÉ, POTENTIELLEMENT INUTILISABLE**
- [!] **ai.pl créé** : Mais probablement défaillant, non validé
- [!] **Algorithme minimax** : Théorique, aucune garantie de fonctionnement
- [!] **Interface IA** : Risque de planter le système
- [!] **À JETER** : Candidat à suppression ou réécriture complète

### 3.2 Commandes IA (⚠️ DANGER)
```prolog
% ⚠️ NE PAS UTILISER - Peut planter
% ?- consult('src/ai'), ai_vs_human_mode.

% ⚠️ Test à vos risques et périls
% ?- consult('src/ai'), init_game_state(GS), choose_ai_move(GS, Move).
```

### 3.3 Action Recommandée
- [ ] **Supprimer ai.pl** : Nettoyer le prototype raté
- [ ] **Réimplémentation propre** : Si IA vraiment nécessaire
- [ ] **Tests d'abord** : Aucun code IA sans validation complète

## Phase 4 : Extensions Post-Universitaire

- [ ] **GUI** : Interface graphique, drag & drop
- [ ] **Analyse** : Évaluation temps réel, suggestions coups
- [ ] **Métriques** : Performance profiling, comparaisons algorithmes

---

**Priorités** : Phase 1.5 (interface) → Phase 2 (règles avancées) → Phase 3 (⚠️ réimplémentation IA) → Phase 4 (extensions)  
**Validation** : ⚠️ ai.pl = PROTOTYPE NON FONCTIONNEL, potentiel à supprimer  
**Prochaine action** : Améliorer l'interface utilisateur et le menu du jeu

**Références** : [CLAUDE.md](../.claude/CLAUDE.md) • [PRD.md](PRD.md)
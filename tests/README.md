# Tests et Démonstrations - Prolog Chess Game

## 📁 Structure des Tests

Cette section contient tous les tests et démonstrations consolidés pour le jeu d'échecs en Prolog.

### 🧪 Fichiers de Tests

| Fichier | Description | Utilisation |
|---------|-------------|-------------|
| `chess_tests.pl` | **Suite principale de tests** | Tests complets et organisés |
| `demo_interactive.pl` | **Démonstrations interactives** | Demos du Scholar's Mate |
| `test_path_blocking.pl` | Tests spécifiques aux chemins | Tests de blocage des pièces |

### 🎯 Comment Utiliser

#### 1. Tests Principaux
```prolog
?- consult('tests/chess_tests').
?- run_all_tests.           % Suite complète
?- quick_test.              % Test rapide
?- test_help.               % Aide détaillée
```

#### 2. Démonstrations
```prolog
?- consult('tests/demo_interactive').
?- demo_interactive.        % Demo interactive (recommandée)
?- demo_auto.              % Demo automatique
?- demo_explained.         % Demo avec explications détaillées
?- demo_defenses.          % Comment se défendre
```

## 🔍 Catégories de Tests

### Tests de Base
- ✅ Initialisation de l'échiquier
- ✅ Affichage et notation algébrique
- ✅ Placement des pièces

### Tests de Logique
- ✅ Validation des mouvements
- ✅ Gestion de l'état du jeu
- ✅ Alternance des joueurs

### Tests par Pièce
- ✅ **Pions** : Mouvements simples, doubles, captures
- ✅ **Cavaliers** : Mouvements en L
- ✅ **Tours** : Mouvements horizontaux/verticaux
- ✅ **Fous** : Mouvements diagonaux
- ✅ **Dame** : Combinaison tour + fou
- ✅ **Roi** : Mouvements d'une case

### Tests de Scénarios
- ✅ **Séquences d'ouverture** classiques
- ✅ **Scholar's Mate** (Mat du berger)
- ✅ **Parties complètes** avec captures

### Tests de Robustesse
- ✅ **Gestion d'erreurs** robuste
- ✅ **Mouvements invalides** rejetés
- ✅ **Limites de l'échiquier** respectées
- ✅ **Coups consécutifs** interdits

## 🎮 Démonstrations Disponibles

### Scholar's Mate (Mat du Berger)
Le célèbre mat en 4 coups avec plusieurs formats :

1. **`demo_interactive`** - Version interactive recommandée
2. **`demo_auto`** - Démonstration automatique complète  
3. **`demo_explained`** - Explications détaillées de chaque coup
4. **`demo_defenses`** - Comment se défendre contre le piège

## 📊 Résultats Attendus

Tous les tests doivent passer avec succès :

```
╔═══════════════════════════════════════════════════════╗
║           PROLOG CHESS GAME - TEST SUITE             ║
╚═══════════════════════════════════════════════════════╝

┌─ SECTION 1: TESTS DE BASE ─────────────────────────┐
✓ Echiquier initialise correctement
✓ Pieces aux bonnes positions
✓ Affichage fonctionne
└────────────────────────────────────────────────────┘

┌─ SECTION 2: TESTS DE LOGIQUE ──────────────────────┐
✓ e2-e4 valide pour les blancs
✓ Joueur change vers noir apres coup blanc
✓ Compteur de coups correct
└────────────────────────────────────────────────────┘

[...et ainsi de suite pour toutes les sections...]

✓ Toutes les sections de tests completees
✓ Systeme pret pour utilisation
```

## 🚀 Après les Tests

Une fois les tests validés :

```prolog
% Pour jouer au jeu complet
?- consult('src/play_chess'), start.

% Pour des tests spécifiques
?- consult('tests/chess_tests'), run_piece_tests.

% Pour voir une démonstration
?- consult('tests/demo_interactive'), demo_interactive.
```

## 🛠️ Maintenance

### Ajout de Nouveaux Tests
1. Ajouter le test dans `chess_tests.pl` dans la section appropriée
2. Mettre à jour la fonction `run_all_tests` si nécessaire
3. Documenter le nouveau test dans ce README

### Problèmes Courants
- **Tests qui échouent** : Vérifier que `src/game_logic.pl` et `src/board_smart.pl` sont chargés
- **Erreurs d'affichage** : Problèmes d'encodage - utiliser des caractères ASCII simples
- **Mouvements rejetés** : Vérifier la logique de validation dans `game_logic.pl`

## 📈 Historique des Versions

- **v2.0** - Suite consolidée et unifiée (Août 2025)
- **v1.0** - Tests initiaux dispersés
- **v0.x** - Tests de développement

---

**Note** : Cette version consolidée remplace tous les anciens fichiers de tests dispersés. Utilisez uniquement les fichiers listés ci-dessus pour éviter les doublons et la confusion.
# 🧪 Guide de Test - Prolog Chess Game

**Version :** 2.0 - Simplifié  
**Auteur :** Patrick Patenaude  
**Date :** Août 2025

## 🎯 Vue d'Ensemble

Guide concis pour tester le jeu d'échecs Prolog. Le système dispose d'une suite de tests consolidée pour valider toutes les fonctionnalités.

## 📁 Structure des Tests

```
tests/
├── chess_tests.pl          # Suite principale unifiée (6 sections)
└── quick_tests.pl          # Tests rapides
```

## 🚀 Démarrage Rapide

### Test Rapide (Recommandé)
```prolog
?- consult('tests/quick_tests'), quick_test.
```

### Suite Complète
```prolog
?- consult('tests/chess_tests'), run_all_tests.
```

## 🧪 Tests Disponibles

### Suite Principale (`chess_tests.pl`)
- **`run_all_tests`** : Tous les tests (~3-5 secondes)
- **`quick_test`** : Test rapide (~1 seconde)
- **`test_help`** : Aide interactive

### Tests par Section
```prolog
?- run_basic_tests.         # Échiquier et affichage
?- run_logic_tests.         # Validation et logique
?- run_piece_tests.         # Règles des pièces
?- run_scenario_tests.      # Séquences de jeu
?- run_robustness_tests.    # Gestion d'erreurs
```



## 🎮 Jouer au Jeu

```prolog
?- consult('src/play_chess'), start.
```

## 🔧 Diagnostic Rapide

### Problèmes Courants
- **Module non chargé** : `?- consult('src/game_logic').`
- **Tests échouent** : Recharger avec `?- consult('tests/chess_tests').`

### Mode Debug
```prolog
?- trace, consult('tests/chess_tests'), quick_test.
```

## 📞 Support

**Fichiers importants :**
- **Tests principaux :** `tests/chess_tests.pl`
- **Tests rapides :** `tests/quick_tests.pl`
- **Vue d'ensemble :** Voir [`README.md`](README.md)

---

**Le système est prêt quand `run_all_tests` passe sans erreur !** 🎯
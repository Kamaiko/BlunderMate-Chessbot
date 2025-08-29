# 🏆 Prolog Chess Game

Jeu d'échecs complet implémenté en Prolog avec architecture moderne et tests complets.

**🎮 Pour jouer immédiatement :** `swipl go.pl`

## 🏗️ **Architecture**

```
src/                    # Code de production 
├── play_chess.pl      # Interface utilisateur
├── game_logic.pl      # Logique métier
└── board_smart.pl     # Affichage
tests/                 # Suite de tests
├── chess_tests.pl     # Tests complets (6 sections)
└── quick_tests.pl     # Tests rapides
```

## 🧪 **Tests**

**Guide complet :** [`TESTING_GUIDE.md`](TESTING_GUIDE.md)

### **📋 Types de Tests**

- **`tests/quick_tests.pl`** : Tests rapides du système (validation des fonctionnalités de base)
- **`tests/chess_tests.pl`** : Suite complète de tests (6 sections détaillées)

## 🎮 **Guide de Jeu**

### **Commandes**
- **Mouvements** : `e2e4` 
- **help** : Aide
- **quit** : Quitter la partie
- **exit** : Quitter le programme

---

**Version** : 5.0 | **Auteur** : Patrick Patenaude | **Date** : Août 2025
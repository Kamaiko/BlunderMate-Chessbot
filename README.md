# 🏆 Prolog Chess Game

Jeu d'échecs complet implémenté en Prolog avec architecture moderne et tests complets.

## 🚀 **Démarrage Rapide**

```bash
cd PrologChessGame_Clean
swipl
?- consult('src/play_chess').
?- start.
```

## 🎯 **Statut du Projet**

### ✅ **Fonctionnel**
- **Jeu Humain vs Humain** : Parties complètes
- **Interface Colorée** : Affichage ASCII avec couleurs
- **Toutes les Pièces** : Règles de mouvement implémentées
- **Tests Complets** : Suite de tests professionnelle

### 🏗️ **Architecture**

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

## ⌨️ **COMMANDES UTILISATEUR - GUIDE COMPLET**

### **🎮 Démarrage du Programme**

```bash
# 1. Lancer SWI-Prolog
swipl

# 2. Charger le programme (avec point final)
?- consult('src/play_chess').

# 3. Démarrer le jeu (avec point final)
?- start.
```

### **🧪 Tests Manuels (Directement dans le Terminal)**

```bash
# Test rapide (validation des fonctionnalités de base)
?- consult('tests/quick_tests'), quick_test.

# Suite complète (tous les tests - 6 sections)
?- consult('tests/chess_tests'), run_all_tests.
```

### **🎯 Tests via le Menu Principal**

```bash
# 1. Démarrer le programme
?- consult('src/play_chess').
?- start.

# 2. Dans le menu, choisir :
#    - #3 : Tests rapides (quick_tests.pl)
#    - #4 : Suite complète (chess_tests.pl)
```

## 📋 **Fonctionnalités**

### ✅ **Règles d'Échecs**
- **Pions** : Mouvement simple/double, capture diagonale
- **Tours** : Horizontal/vertical
- **Cavaliers** : Mouvement en L
- **Fous** : Diagonal
- **Dame** : Tour + Fou
- **Roi** : Une case

### ✅ **Qualité**
- Code modulaire et maintenable
- Tests automatisés
- Documentation complète
- Architecture scalable

## 🎮 **Guide de Jeu**

### **Commandes**
- **Mouvements** : `e2e4.` (n'oubliez pas le point !)
- **help.** : Aide
- **board.** : Afficher l'échiquier
- **quit.** : Quitter la partie
- **exit.** : Quitter le programme

## 🚀 **Prochaines Étapes**

### **Priorités**
1. **Chemins bloqués** : Pour tours, fous et dames
2. **Détection d'échec** : Version fonctionnelle
3. **Règles spéciales** : Roque, prise en passant

### **Futur**
- Mode Humain vs IA
- Sauvegarde/chargement
- Interface graphique

---

**Version** : 5.0 | **Auteur** : Patrick Patenaude | **Date** : Août 2025
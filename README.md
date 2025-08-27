# ♟️ Prolog Chess Game with AI

**Auteur :** Patrick Patenaude - Projet d'échecs en Prolog avec intelligence artificielle

## 🎯 **État actuel du projet**

### ✅ **Ce qui fonctionne bien :**
- **Plateau d'échecs complet** : Représentation matricielle 8x8
- **Règles de base** : Mouvements des pièces, validation des coups
- **Notation algébrique** : Entrée des coups en format "e2e4"
- **IA simple** : Algorithme one-ply avec évaluation de position
- **Interface console** : Jeu jouable humain vs IA
- **Tests complets** : Suite de tests pour valider toutes les fonctionnalités
- **Structure modulaire** : Code organisé en fichiers logiques

### ⚠️ **Ce qui fonctionne partiellement :**
- **Affichage Unicode** : Les symboles d'échecs (♔, ♕, ♖, etc.) ne s'affichent pas correctement sur tous les terminaux
- **IA avancée** : Version Minimax/Alpha-Beta non implémentée (reste en version simple)

### ❌ **Ce qui ne fonctionne pas encore :**
- **Règles avancées** : Échec, mat, roque, prise en passant
- **Historique des coups** : Pas de sauvegarde de la partie
- **Interface graphique** : Seulement en console pour l'instant

## 🚀 **Comment jouer**

### **Prérequis :**
- SWI-Prolog installé
- Terminal compatible UTF-8 (pour les symboles Unicode)

### **Lancement rapide :**
```bash
# Test du plateau
swipl -s board_smart.pl -g "test_board_smart, halt."

# Jouer une partie
swipl -s chess_game_simple.pl -g "play_chess, halt."

# Tests complets
swipl -s test_all.pl -g "test_all, halt."
```

### **Format des coups :**
- **Notation algébrique** : "e2e4", "g1f3", "d7d5"
- **Format** : case_départ + case_arrivée
- **Exemples** :
  - `e2e4` : Pion blanc de e2 vers e4
  - `g1f3` : Cavalier blanc de g1 vers f3

## 🏗️ **Architecture du projet**

```
PrologChessGame/
├── board_smart.pl          # Plateau intelligent (Unicode + ASCII fallback)
├── game_logic.pl           # Règles du jeu et logique
├── simple_ai.pl            # IA simple (one-ply)
├── chess_game_simple.pl    # Interface principale du jeu
├── demo_game.pl            # Démonstrations et tests
├── test_all.pl             # Suite de tests complète
└── README.md               # Ce fichier
```

## 🔧 **Problèmes connus et solutions**

### **1. Affichage Unicode des pièces**
**Problème :** Les symboles d'échecs ne s'affichent pas sur tous les terminaux

**Solutions actuelles :**
- ✅ **Version intelligente** : `board_smart.pl` détecte automatiquement les capacités
- ✅ **Fallback ASCII** : Retombe sur P/p, R/r, N/n, B/b, Q/q, K/k si Unicode échoue
- ⚠️ **Configuration manuelle** : Certains terminaux nécessitent `chcp 65001` (Windows)

**Solutions futures :**
- [ ] Détection automatique des capacités du terminal
- [ ] Choix manuel du mode d'affichage
- [ ] Interface graphique alternative

### **2. IA limitée**
**Problème :** L'IA actuelle ne regarde qu'un coup en avant

**Solutions futures :**
- [ ] Implémentation de l'algorithme Minimax
- [ ] Optimisation Alpha-Beta pruning
- [ ] Tables de transposition
- [ ] Livre d'ouvertures

## 🎮 **Fonctionnalités implémentées**

### **Plateau :**
- [x] Création automatique du plateau initial
- [x] Placement des pièces selon les règles d'échecs
- [x] Affichage en format matriciel avec coordonnées
- [x] Gestion des pièces vides

### **Mouvements :**
- [x] Validation des mouvements de base
- [x] Notation algébrique (e2e4)
- [x] Conversion coordonnées ↔ notation
- [x] Exécution des mouvements

### **IA :**
- [x] Évaluation de position (matériel + position)
- [x] Génération de tous les coups possibles
- [x] Choix du meilleur coup (one-ply)
- [x] Affichage des coups en notation algébrique

### **Interface :**
- [x] Boucle de jeu complète
- [x] Gestion des tours (blancs/noirs)
- [x] Affichage du plateau à chaque coup
- [x] Gestion des entrées utilisateur

## 🧪 **Tests et validation**

### **Tests disponibles :**
```bash
# Test du plateau
test_board_smart

# Test de la logique du jeu
test_logic

# Test de l'IA
test_ai

# Tests complets
test_all

# Démonstration rapide
quick_test
```

### **Couverture des tests :**
- ✅ **Plateau** : Création, affichage, placement des pièces
- ✅ **Logique** : Mouvements, validation, exécution
- ✅ **IA** : Évaluation, génération de coups, choix
- ✅ **Interface** : Entrées, affichage, boucle de jeu
- ✅ **Notation** : Conversion algébrique bidirectionnelle

## 🚧 **Prochaines étapes prioritaires**

### **Phase 1 : Résolution des problèmes actuels**
1. **Unicode** : Améliorer la détection automatique des capacités du terminal
2. **Tests** : Ajouter des tests pour les cas limites
3. **Documentation** : Compléter les commentaires de code

### **Phase 2 : Fonctionnalités avancées**
1. **Règles d'échecs** : Implémenter échec, mat, roque
2. **IA avancée** : Minimax avec Alpha-Beta pruning
3. **Interface** : Améliorer l'expérience utilisateur

### **Phase 3 : Optimisations**
1. **Performance** : Tables de transposition, ordonnancement des coups
2. **Fonctionnalités** : Historique, sauvegarde, analyse
3. **Interface** : Version graphique ou web

## 🐛 **Dépannage**

### **Problème : Symboles Unicode non visibles**
```bash
# Windows PowerShell
chcp 65001
[Console]::OutputEncoding = [System.Text.Encoding]::UTF8

# Windows CMD
chcp 65001

# Linux/macOS
export LANG=en_US.UTF-8
```

### **Problème : Erreur de syntaxe**
- Vérifiez que vous utilisez SWI-Prolog
- Assurez-vous que tous les fichiers sont dans le même répertoire
- Vérifiez l'encodage des fichiers (UTF-8 recommandé)

## 📚 **Ressources et références**

- **SWI-Prolog** : [https://www.swi-prolog.org/](https://www.swi-prolog.org/)
- **Notation algébrique** : Standard international des échecs
- **Algorithme Minimax** : Base de l'IA pour les jeux à deux joueurs

## 🤝 **Contribution**

Ce projet est un travail d'apprentissage. Les suggestions et améliorations sont les bienvenues !

---

**Dernière mise à jour :** Décembre 2024  
**Version :** 1.0 (Fonctionnelle avec limitations connues)  
**Statut :** En développement actif

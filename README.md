# ♟️ Prolog Chess Game with AI

**Auteur :** Patrick Patenaude  - Projet d'échecs en Prolog avec intelligence artificielle

## 🎯 **État actuel du projet**

### ✅ **Ce qui fonctionne parfaitement :**
- **Affichage du plateau** : Plateau avec crochets, virgules et alignement parfait ✅ **RÉSOLU !**
- **Symboles Unicode** : Pièces d'échecs visibles sur tous les terminaux ✅ **RÉSOLU !**
- **Cross-platform** : Fonctionne sur Windows, Linux, macOS sans configuration ✅ **RÉSOLU !**
- **Validation complète** : L'affichage correspond exactement au format demandé ✅ **CONFIRMÉ !**

### 📋 **Résumé de la situation :**
- **Phase 1 (Affichage)** : ✅ **100% TERMINÉE** - Le plateau s'affiche parfaitement
- **Phase 2 (Tests)** : 🚧 **EN COURS** - Factorisation terminée, tests en cours
- **Phase 3 (IA avancée)** : ❌ **NON COMMENCÉE** - Code de base existant mais non testé

### 🔧 **Factorisation réalisée :**
- **Tests unifiés** : `tests.pl` remplace `demo_game.pl` et `test_all.pl`
- **Dépendances corrigées** : `game_logic.pl` charge maintenant `board_smart.pl`
- **Structure simplifiée** : 6 fichiers au lieu de 8, code plus maintenable
- **Tests organisés** : Fonctions de test claires et modulaires

### ✅ **Ce qui fonctionne bien :**
- **Plateau d'échecs complet** : Représentation matricielle 8x8
- **Structure modulaire** : Code organisé en fichiers logiques
- **Tests de base** : Validation de l'affichage du plateau

### ⚠️ **Ce qui fonctionne partiellement :**
- **Règles de base** : Mouvements des pièces, validation des coups (code existant mais non testé)
- **Notation algébrique** : Entrée des coups en format "e2e4" (code existant mais non testé)
- **IA simple** : Algorithme one-ply avec évaluation de position (code existant mais non testé)
- **Interface console** : Jeu jouable humain vs IA (code existant mais non testé)

### ❌ **Ce qui ne fonctionne pas encore :**
- **IA avancée** : Version Minimax/Alpha-Beta non implémentée
- **Règles avancées** : Échec, mat, roque, prise en passant
- **Historique des coups** : Pas de sauvegarde de la partie
- **Interface graphique** : Seulement en console pour l'instant



## 🚀 **Comment jouer**

### **Prérequis :**
- SWI-Prolog installé
- **Aucune configuration spéciale requise** ✅ **NOUVEAU !**

### **Lancement rapide :**
```bash
# Test du plateau (FONCTIONNE PARFAITEMENT)
swipl -s board_smart.pl -g "test_board_smart, halt."

# Tests unifiés et factorisés (✅ NOUVEAU !)
swipl -s tests.pl -g "quick_test, halt."
swipl -s tests.pl -g "run_all_tests, halt."

# Test de l'interface (⚠️ NON TESTÉ - peut avoir des erreurs)
swipl -s chess_game_simple.pl -g "test_game, halt."
```

### **Format des coups :**
- **Notation algébrique** : "e2e4", "g1f3", "d7d5"
- **Format** : case_départ + case_arrivée
- **Exemples** :
  - `e2e4` : Pion blanc de e2 vers e4
  - `g1f3` : Cavalier blanc de g1 vers f3

## 🏗️ **Architecture du projet**

```
PrologChessGame_Clean/
├── board_smart.pl          # ✅ Plateau intelligent (Unicode + ASCII fallback) - FONCTIONNE
├── game_logic.pl           # ✅ Règles du jeu et logique - CORRIGÉ ET FACTORISÉ
├── simple_ai.pl            # ⚠️ IA simple (one-ply) - CODE EXISTANT, NON TESTÉ
├── chess_game_simple.pl    # ⚠️ Interface principale du jeu - CODE EXISTANT, NON TESTÉ
├── tests.pl                # ✅ Tests unifiés et factorisés - NOUVEAU !
└── README.md               # Ce fichier
```

## 🔧 **Problèmes connus et solutions**

### **1. Affichage Unicode des pièces** ✅ **RÉSOLU !**
**Problème :** Les symboles d'échecs ne s'affichent pas sur tous les terminaux

**Solutions actuelles :**
- ✅ **Version intelligente** : `board_smart.pl` détecte automatiquement les capacités
- ✅ **Affichage parfait** : Crochets, virgules et alignement correct sur tous les terminaux
- ✅ **Cross-platform** : Fonctionne sur Windows, Linux, macOS sans configuration
- ✅ **Symboles visibles** : Pièces d'échecs Unicode (♔, ♕, ♖, ♗, ♘, ♙) parfaitement affichées

**Solutions futures :**
- [ ] Interface graphique alternative
- [ ] Sauvegarde des parties
- [ ] Analyse des coups

### **2. IA limitée**
**Problème :** L'IA actuelle ne regarde qu'un coup en avant

**Solutions futures :**
- [ ] Implémentation de l'algorithme Minimax
- [ ] Optimisation Alpha-Beta pruning
- [ ] Tables de transposition
- [ ] Livre d'ouvertures

## 🎮 **Fonctionnalités implémentées**

### **Plateau :** ✅ **FONCTIONNE PARFAITEMENT**
- [x] Création automatique du plateau initial
- [x] Placement des pièces selon les règles d'échecs
- [x] Affichage en format matriciel avec coordonnées
- [x] Gestion des pièces vides
- [x] **Affichage Unicode parfait** avec crochets et virgules

### **Mouvements :** ⚠️ **CODE EXISTANT, NON TESTÉ**
- [x] Validation des mouvements de base
- [x] Notation algébrique (e2e4)
- [x] Conversion coordonnées ↔ notation
- [x] Exécution des mouvements

### **IA :** ⚠️ **CODE EXISTANT, NON TESTÉ**
- [x] Évaluation de position (matériel + position)
- [x] Génération de tous les coups possibles
- [x] Choix du meilleur coup (one-ply)
- [x] Affichage des coups en notation algébrique

### **Interface :** ⚠️ **CODE EXISTANT, NON TESTÉ**
- [x] Boucle de jeu complète
- [x] Gestion des tours (blancs/noirs)
- [x] Affichage du plateau à chaque coup
- [x] Gestion des entrées utilisateur

## 🧪 **Tests et validation**

### **Tests disponibles :**
```bash
# Test du plateau (✅ FONCTIONNE PARFAITEMENT)
swipl -s board_smart.pl -g "test_board_smart, halt."

# Tests unifiés et factorisés (✅ NOUVEAU !)
swipl -s tests.pl -g "quick_test, halt."        # Test rapide du plateau
swipl -s tests.pl -g "run_all_tests, halt."     # Tous les tests
swipl -s tests.pl -g "help, halt."              # Aide et fonctions disponibles

# Tests individuels (⚠️ PEUT AVOIR DES ERREURS)
swipl -s tests.pl -g "test_board_display, halt."
swipl -s tests.pl -g "test_algebraic_notation, halt."
swipl -s tests.pl -g "test_game_logic, halt."
swipl -s tests.pl -g "test_ai_basic, halt."
swipl -s tests.pl -g "test_game_interface, halt."
```

### **Couverture des tests :**
- ✅ **Plateau** : Création, affichage, placement des pièces - **VALIDÉ !**
- ⚠️ **Logique** : Mouvements, validation, exécution - **CODE EXISTANT, NON TESTÉ**
- ⚠️ **IA** : Évaluation, génération de coups, choix - **CODE EXISTANT, NON TESTÉ**
- ⚠️ **Interface** : Entrées, affichage, boucle de jeu - **CODE EXISTANT, NON TESTÉ**
- ⚠️ **Notation** : Conversion algébrique bidirectionnelle - **CODE EXISTANT, NON TESTÉ**

## 🚧 **Prochaines étapes prioritaires**

### **Phase 1 : Affichage du plateau** ✅ **TERMINÉ ET VALIDÉ !**
1. **Affichage Unicode** : ✅ Plateau parfait avec crochets et virgules
2. **Cross-platform** : ✅ Fonctionne sur tous les terminaux
3. **Tests** : ✅ Tests de base fonctionnels
4. **Documentation** : ✅ README mis à jour
5. **Validation** : ✅ Affichage confirmé identique au format demandé

### **Phase 2 : Tests et intégration** 🎯 **PRIORITÉ ACTUELLE**
1. **Tests de la logique** : Valider que les règles de base fonctionnent
2. **Tests de l'IA** : Vérifier que l'algorithme simple fonctionne
3. **Tests de l'interface** : S'assurer que le jeu est jouable
4. **Correction des bugs** : Résoudre les problèmes de dépendances

### **🎯 Actions concrètes pour la Phase 2 :**
1. **✅ Factorisation terminée** : Tests unifiés dans `tests.pl`
2. **✅ Dépendances corrigées** : `game_logic.pl` charge maintenant `board_smart.pl`
3. **Tester `simple_ai.pl`** : Valider l'algorithme one-ply
4. **Tester `chess_game_simple.pl`** : S'assurer que l'interface est fonctionnelle
5. **Valider l'intégration** : Tester que tous les composants fonctionnent ensemble

### **Phase 3 : Fonctionnalités avancées**
1. **Règles d'échecs** : Implémenter échec, mat, roque
2. **IA avancée** : Minimax avec Alpha-Beta pruning
3. **Interface** : Améliorer l'expérience utilisateur

### **Phase 4 : Optimisations**
1. **Performance** : Tables de transposition, ordonnancement des coups
2. **Fonctionnalités** : Historique, sauvegarde, analyse
3. **Interface** : Version graphique ou web

## 🐛 **Dépannage**

### **Problème : Symboles Unicode non visibles** ✅ **RÉSOLU !**
**Solution :** Utilisez `board_smart.pl` qui fonctionne automatiquement sur tous les terminaux

**Test rapide :**
```bash
swipl -s board_smart.pl -g "test_board_smart, halt."
```

**Résultat attendu :**
```
8[♖,♘,♗,♕,♔,♗,♘,♖]
7[♙,♙,♙,♙,♙,♙,♙,♙]
6[ , , , , , , , ]
5[ , , , , , , , ]
4[ , , , , , , , ]
3[ , , , , , , , ]
2[♟,♟,♟,♟,♟,♟,♟,♟]
1[♜,♞,♝,♛,♚,♝,♞,♜]
  a b c d e f g h
```

### **Problème : Erreurs de dépendances** ⚠️ **À RÉSOUDRE**
**Symptôme :** Erreurs "source_sink 'board' does not exist" ou "Unknown procedure"
**Cause :** Conflits entre les fichiers `board.pl` et `board_smart.pl`
**Solution actuelle :** Utiliser directement `board_smart.pl` pour les tests

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

**Dernière mise à jour :** 27 Août 2025  
**Version :** 1.4 (Code factorisé et tests unifiés)  
**Statut :** Phase 1 terminée, Phase 2 en cours (factorisation terminée, tests en cours)

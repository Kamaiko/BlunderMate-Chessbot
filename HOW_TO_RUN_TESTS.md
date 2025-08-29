# Guide de Test du Jeu d'Échecs Prolog

## 🎯 **État Actuel du Projet**

Le jeu d'échecs Prolog est maintenant **entièrement fonctionnel** avec les fonctionnalités suivantes :
- ✅ **Menu principal** : Interface de navigation complètement fonctionnelle
- ✅ **Affichage de l'échiquier** : Pièces ASCII colorées et bien visibles
- ✅ **Logique de jeu** : Validation des mouvements et règles d'échecs
- ✅ **Interface utilisateur** : Menu intuitif et gestion des erreurs
- ✅ **Tests automatiques** : Système de test rapide et fiable

## 🚀 **Méthodes de Test Recommandées**

### 1. **Test Automatique (Recommandé pour la Validation)**

```bash
# Dans le terminal, naviguer vers le projet
cd PrologChessGame_Clean

# Lancer le test rapide automatique
swipl -q -g "consult('play_chess'), quick_test."
```

**Ce test vérifie :**
- ✅ Initialisation correcte de l'échiquier
- ✅ Affichage des pièces en position initiale
- ✅ Mouvement e2e4 (pion blanc)
- ✅ Mouvement e7e5 (pion noir)
- ✅ Validation des règles de base

### 2. **Test Interactif Complet (Recommandé pour l'Utilisation)**

```bash
# Lancer SWI-Prolog en mode interactif
swipl

# Charger le jeu
[play_chess].

# Démarrer le menu principal
start.
```

**Puis tester chaque option du menu :**
- **Option 1** : Démarrer une partie humain vs humain
- **Option 2** : Vérifier le message "à venir"
- **Option 3** : Lancer le test rapide
- **Option 4** : Afficher l'aide
- **Option 5** : Quitter proprement

### 3. **Test de Validation des Mouvements**

```bash
# Dans SWI-Prolog interactif
[play_chess].
start.

# Choisir l'option 1 (Humain vs Humain)
# Tester des mouvements valides : e2e4, e7e5, d2d4
# Tester des mouvements invalides : e2e5, a1a9
```

## 🧪 **Tests Spécifiques par Composant**

### **Test de l'Affichage (board_smart.pl)**

```bash
swipl -q -g "consult('board_smart'), test_board_smart, halt."
```

**Vérifications :**
- ✅ Échiquier 8x8 correctement affiché
- ✅ Pièces blanches en majuscules (P, R, N, B, Q, K)
- ✅ Pièces noires en minuscules (p, r, n, b, q, k)
- ✅ Couleurs ANSI fonctionnelles

### **Test de la Logique (game_logic.pl)**

```bash
swipl -q -g "consult('game_logic'), test_game_logic, halt."
```

**Vérifications :**
- ✅ Initialisation de l'état du jeu
- ✅ Validation des mouvements de base
- ✅ Détection des pièces par couleur
- ✅ Gestion des tours (blancs/noirs)

## ⚠️ **Problèmes Courants et Solutions**

### 1. **"Invalid choice" en boucle**

**Cause :** Utilisation de `halt.` dans les commandes de lancement
**Solution :** Ne pas inclure `halt.` dans les commandes de test

```bash
# ❌ Incorrect (cause la boucle)
swipl -q -g "consult('play_chess'), start, halt."

# ✅ Correct (fonctionne)
swipl -q -g "consult('play_chess'), start."
```

### 2. **Pièces non affichées correctement**

**Cause :** Terminal ne supporte pas les codes ANSI
**Solution :** Utiliser un terminal compatible

- **Windows** : Windows Terminal, PowerShell moderne
- **macOS** : iTerm2, Terminal.app
- **Linux** : La plupart des terminaux supportent ANSI

### 3. **Erreurs de chargement**

**Cause :** Fichiers manquants ou corrompus
**Solution :** Vérifier la structure du projet

```bash
# Vérifier que tous les fichiers sont présents
ls -la *.pl
```

## 🔍 **Tests de Diagnostic Avancés**

### **Test de Performance**

```bash
# Mesurer le temps de chargement
time swipl -q -g "consult('play_chess'), halt."

# Mesurer le temps d'exécution des tests
time swipl -q -g "consult('play_chess'), quick_test, halt."
```

### **Test de Mémoire**

```bash
# Charger en mode verbose pour voir les détails
swipl -v -g "consult('play_chess'), start."
```

### **Test de Compatibilité**

```bash
# Vérifier la version de SWI-Prolog
swipl --version

# Vérifier la syntaxe des fichiers
swipl -q -g "consult('play_chess'), halt."
swipl -q -g "consult('game_logic'), halt."
swipl -q -g "consult('board_smart'), halt."
```

## 📊 **Résultats de Test Attendus**

### **Test Rapide Réussi**

```
=== QUICK SYSTEM TEST ===
[Affichage de l'échiquier initial]
Testing move e2e4...
[Affichage après e2e4]
Testing move e7e5...
[Affichage après e7e5]
System test completed!
```

### **Menu Principal Fonctionnel**

```
======================
  PROLOG CHESS GAME
======================

Choose an option:
1 - Start Human vs Human game
2 - Start Human vs Bot game (Coming soon)
3 - Quick system test
4 - Show help
5 - Exit

Enter your choice (1-5):
```

## 🎯 **Critères de Validation**

Le projet est considéré comme **fonctionnel** si :

1. ✅ **Tous les tests automatiques passent** sans erreur
2. ✅ **Le menu principal s'affiche** et répond aux entrées
3. ✅ **Les mouvements de base sont validés** (e2e4, e7e5)
4. ✅ **L'affichage est lisible** avec pièces colorées
5. ✅ **La navigation fonctionne** sans boucles infinies
6. ✅ **Les messages d'erreur sont clairs** et informatifs

## 🚀 **Prochaines Étapes de Test**

Une fois que tous les tests de base passent, vous pouvez tester :

- [ ] **Fin de partie** : Échec, échec et mat
- [ ] **Interface utilisateur** : Commandes spéciales (help, moves, board)
- [ ] **Robustesse** : Entrées invalides, gestion des erreurs

---

**🎉 Si tous ces tests passent, votre jeu d'échecs Prolog est prêt pour la production !**

*Bon test !* 🧪
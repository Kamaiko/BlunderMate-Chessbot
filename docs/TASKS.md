# 🎯 TODO LIST - PROLOG CHESS AI

## 📋 **PRIORITÉS DÉVELOPPEMENT** (Par ordre d'importance)

### **🔴 URGENT (Semaine courante)**
- [ ] **TASK-1**: Activer `evaluate_piece_safety` (ai.pl) - 15 min
  - Problème: Hardcodé à 0, IA laisse pièces en danger post-coup 15
  - Impact: Bishop d6 blunder systématique en milieu de jeu
  
- [ ] **TASK-2**: Corriger transition ouverture→milieu (ai.pl:278) - 30 min
  - Problème: Coupure brutale MoveCount=15, pas de protection graduelle
  - Solution: Phase mixte coups 15-25 avec bonus défense pièces développées

### **🟡 HAUTE PRIORITÉ (2 semaines)**
- [ ] **TASK-3**: Interface revamp finalisation - 45 min
  - Supprimer ligne pointillée basique ✅ FAIT
  - Ajouter ligne esthétique ✅ FAIT
  - Tests utilisateur final

- [ ] **TASK-4**: Optimiser développement Caro-Kann - 45 min
  - Problème: e6 précoce bloque fou c8
  - Solution: Ajuster PSQT f5=+40, e6=-30 OU ordre génération
  
- [ ] **TASK-5**: Refactoring fonctions longues - 90 min
  - `generate_opening_moves/3` (83 lignes) → 4 sous-fonctions
  - `evaluate_position/3` (24 lignes) → extraire PSQT

### **🟢 MOYEN TERME (3-4 semaines)**
- [ ] **TASK-6**: Documentation finale académique - 120 min
  - Rapport PDF IFT-2003 (Modélisation 20% + Implémentation 45%)
  - Guide utilisation étudiant
  
- [ ] **TASK-7**: Optimisations performance avancées - 180 min
  - Profondeur variable 2→3 pour menaces tactiques
  - Quiescence search pour captures forcing

---

## 🚨 **BUGS CRITIQUES IDENTIFIÉS**

### **BUG-1: Bishop d6 Blunder (Coup 18)**
**Symptôme**: IA laisse bishop en danger systématiquement post-ouverture
**Root Cause**: 
- `evaluate_piece_safety` désactivée (retourne 0)
- Transition brutale generate_opening_moves → generate_regular_moves
- Aucune protection pièces développées en milieu de jeu

**Solution immédiate**: TASK-1 + TASK-2

### **BUG-2: Développement Caro-Kann sous-optimal**
**Symptôme**: e6 joué avant Bf5, bloque développement fou dame
**Séquence problématique**:
```
1. d4 c6  2. Nc3 d5  3. Bf4 Nf6  4. e3 e6 ← ERREUR: e6 trop tôt
```
**Séquence correcte**:
```
1. d4 c6  2. Nc3 d5  3. Bf4 Bf5 ← Fou AVANT e6  4. e3 e6
```

**Solution**: TASK-4

---

## ✅ **ÉTAT ACTUEL SYSTÈME**

### **Fonctionnel ✅**
- IA Négamax + Alpha-Beta profondeur 2 (performance 0.6s/coup)
- Interface française professionnelle Humain vs IA
- Architecture 6 modules stable (pieces/board/game/interface/ai/evaluation)
- Tests automatisés 8 sections (tous passent)

### **Problématique ❌**
- Blunders tactiques post-coup 15 (transition phase)
- Développement ouverture sous-optimal (Caro-Kann)
- Code refactoring requis (fonctions >20 lignes)

---

## 🎓 **CONTEXTE ACADÉMIQUE**
- **Cours**: IFT-2003 Intelligence Artificielle
- **Date remise**: 20 octobre 2025 (9h00)
- **Livrables**: Rapport PDF + Code Prolog fonctionnel
- **Évaluation**: Modélisation (20%) + Implémentation (45%) + Résultats (25%) + Rapport (10%)

### **Objectifs apprentissage validés ✅**
- ✅ Négamax + Alpha-Beta implémentés
- ✅ MVV-LVA + PSQT + Move Ordering
- ✅ Architecture modulaire Prolog
- ✅ Interface Humain vs IA stable

---

## 🧪 **TESTS VALIDATION**

### **Test Régression** (avant toute modification)
```bash
swipl -s tests/tests.pl -g "run_all_tests, halt."
```

### **Test Comportement IA**
```bash
swipl go.pl  # Option 2: IA vs Humain
# Séquence: d2d4, c1f4, b1c3, e2e3
# Vérifier: IA joue Bf5 (pas e6 prématuré)
```

### **Test Performance**
- Temps réponse ≤ 1s/coup
- Aucun crash sur 20+ coups
- Scores [EVAL] cohérents

---

**Dernière mise à jour**: 2025-01-09  
**Status**: 🔴 2 bugs critiques - 🟡 Interface améliorée - ✅ Core IA fonctionnelle
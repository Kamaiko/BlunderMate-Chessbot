# 📋 TASKS - FINALISATION PROJET ACADÉMIQUE IFT-2003

## 🎓 **PROJET - JOUEUR INTELLIGENT ÉCHECS**
- **Cours**: IFT-2003 Intelligence Artificielle
- **Objectif**: Développer joueur intelligent utilisant techniques recherche heuristique  
- **Date remise**: 20 octobre 2025 (9h00) - Rapport PDF + Code Prolog
- **Évaluation**: Modélisation (20%) + Implémentation (45%) + Résultats (25%) + Rapport (10%)

### **🎯 OBJECTIFS APPRENTISSAGE VALIDÉS**
- ✅ **Identifier problème IA**: Ordre développement Caro-Kann identifié
- ✅ **Analyser recherche espace états**: Négamax + Alpha-Beta implémentés fonctionnels  
- ✅ **Choisir technique heuristique**: MVV-LVA + PSQT + Move Ordering optimisés
- ✅ **Implanter solution logique**: Architecture 6 modules Prolog, interface Humain vs IA stable

---

## 🎯 **ÉTAT ACTUEL PROJET** (2025-09-09)

### **✅ SYSTÈME IA FONCTIONNEL**
- **Version stable**: Commit 2ba7bef (version ancienne mais stable)
- **Performance**: Négamax depth 2, ~0.6s/coup, aucun crash
- **Architecture**: 6 modules intégrés, tests passent
- **Interface**: Française professionnelle, IA vs Humain opérationnel

### **🚨 PROBLÈME TACTIQUE IDENTIFIÉ**

#### **🔴 DÉVELOPPEMENT CARO-KANN SOUS-OPTIMAL**
**Symptôme**: IA joue e7-e6 AVANT de développer fou dame, bloquant développement optimal

**Séquence problématique observée**:
```
1. d4 c6      ← Caro-Kann correct
2. Nc3 d5     ← Structure correcte  
3. Bf4 Nf6    ← Développement cavalier
4. e3 e6      ← PROBLÈME: e6 précoce bloque fou c8
```

**Séquence théorique optimale**:
```
1. d4 c6
2. Nc3 d5  
3. Bf4 Bf5    ← Fou "outside pawn chain" AVANT fermeture
4. e3 e6      ← Maintenant acceptable
```

#### **🔧 TENTATIVES DE CORRECTION**
- ✅ **PSQT tuning**: f5=+20, e6=-15 (partiellement efficace)
- ❌ **early_e6_penalty**: Pénalité conditionnelle non effective (supprimée)
- ⚠️ **Résultat**: IA évite Be6 mais joue encore e6 prématurément

---

## 🔬 **HYPOTHÈSES & DIAGNOSTIC**

### **🤔 POURQUOI NOS SOLUTIONS N'ONT PAS MARCHÉ**

#### **Hypothèse #1: Chemin d'évaluation différent**
- e6 pourrait être évalué comme **capture** plutôt que **non-capture**
- Notre bonus/malus dans `evaluate_non_capture_move` non appliqué

#### **Hypothèse #2: Ordre génération coups** 
- `generate_moves_simple` pourrait prioriser pions avant pièces
- Pions dans `SupportPawnMoves` évalués avant `DevelopmentMoves`

#### **Hypothèse #3: Priorité PSQT insuffisante**
- Différentiel f5(+20) vs e6(-15) = 35 points peut-être insuffisant
- Autres facteurs (développement, contrôle centre) dominent

#### **Hypothèse #4: Profondeur négamax**
- Depth 2 pourrait ne pas voir conséquences long terme
- e6 semble bon sur horizon limité

---

## 🎯 **PLAN PROCHAINE SÉANCE**

### **🔍 PHASE 1: DIAGNOSTIC APPROFONDI (30 min)**

#### **Test Debug Structuré**
- [ ] **Test séquence spécifique**: `swipl go.pl, 2, d2d4, c1f4, b1c3, e2e3`
- [ ] **Observer comportement IA**: Bf5 ou e6 après cette séquence
- [ ] **Tracer évaluation**: Ajouter debug temporaire pour voir scores

#### **Analyse Architecture Évaluation**
- [ ] **Vérifier chemin e6**: Capture vs non-capture
- [ ] **Examiner ordre génération**: Priorité pions vs pièces  
- [ ] **Mesurer impact PSQT**: Scores relatifs f5 vs e6

### **🔧 PHASE 2: SOLUTION CIBLÉE (45 min)**

#### **Option A: Modification Ordre Génération**
- [ ] **Ajuster `generate_moves_simple`**: DevelopmentMoves avant SupportPawnMoves
- [ ] **Tester impact**: Validation séquence Caro-Kann

#### **Option B: Bonus Développement Conditionnel**
- [ ] **Implémenter dans `development_bonus`**: Bonus fou si e6 libre
- [ ] **Éviter pénalités**: Approche positive vs négative

#### **Option C: PSQT Ajustement Agressif**
- [ ] **Amplifier différentiel**: f5=+40, e6=-30 (test)
- [ ] **Observer changement comportement**

### **🧪 PHASE 3: VALIDATION & TESTS (15 min)**
- [ ] **Test séquence complète**: Validation ordre Caro-Kann correct
- [ ] **Regression test**: S'assurer autres aspects IA intacts
- [ ] **Performance check**: Maintenir ~0.6s/coup

---

## 🧪 **PLAN TEST ALGORITHME EXISTANT**

### **📋 SUITE TESTS FONCTIONNELS**

#### **Test 1: Stabilité Générale**
```bash
swipl -s tests/tests.pl -g "run_all_tests, halt."
```
- Valider les 8 sections de tests passent
- S'assurer aucune régression architecture

#### **Test 2: Performance IA**
```bash
swipl go.pl  # Option 2: IA vs Humain
```
- Mesurer temps de réponse (~0.6s acceptable)
- Vérifier absence crashes sur 10+ coups
- Valider interface française fonctionnelle

#### **Test 3: Comportement Tactique**
**Séquence de référence**: 
```
d2d4 -> c7c6
b1c3 -> d7d5
c1f4 -> [Observer choix IA]
e2e3 -> [Observer développement]
```

**Critères de succès**:
- ✅ IA développe fou c8 avant e6
- ✅ Pas d'erreurs runtime
- ✅ Scores évaluation cohérents

### **🎯 TEST DEBUG SPÉCIFIQUE**
**Commande exacte**: `swipl go.pl, option 2, d2d4, c1f4, b1c3, e2e3`
**Objectif**: Vérifier si IA joue Bf5 dans cette position
**Critère**: Fou noir doit aller en f5, pas en e6 ou ailleurs

---

## 📄 **LIVRABLES ACADÉMIQUES FINAUX**

### **📝 RAPPORT ACADÉMIQUE** (Structure IFT-2003)
- [ ] **Modélisation (20%)**: Négamax depth 2, Alpha-Beta, espace états échecs
- [ ] **Implémentation (45%)**: PSQT, MVV-LVA, architecture 6-modules
- [ ] **Résultats (25%)**: Performance ~0.6s/coup, comportement tactique
- [ ] **Présentation (10%)**: Documentation structure, guide utilisation

### **💻 CODE FINAL**
- ✅ **Programme fonctionnel**: IA vs Humain stable (commit 2ba7bef)
- ✅ **Architecture modulaire**: 6 modules Prolog propres
- ✅ **Tests validation**: 8 sections passent
- [ ] **Optimisation finale**: Ordre développement Caro-Kann

---

## 📂 **STATUS FICHIERS PROJET**

### **✅ DOCUMENTATION ACTUELLE**
- ✅ `TASKS.md` (ce fichier) - État projet mis à jour 2025-09-09
- ✅ `ARCHITECTURE_GUIDE_DEVELOPERS.md` - Guide développeurs complet
- ✅ `CLAUDE.md` - Instructions développement et conventions

### **🔧 MODIFICATIONS RÉCENTES**
- ✅ **PSQT bishop modifiées**: f5=+20, e6=-15 (evaluation.pl:70-71)
- ❌ **early_e6_penalty supprimée**: Solution inefficace nettoyée
- ✅ **Version stable**: Commit 2ba7bef confirmé fonctionnel

---

**STATUS PROJET** : ✅ IA fonctionnelle, optimisation tactique Caro-Kann en cours
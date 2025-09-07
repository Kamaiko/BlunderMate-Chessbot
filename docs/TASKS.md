# 🚨 PROLOG CHESS GAME - DEVELOPMENT TASKS

## 📊 **STATUS ACTUEL** 🚨 **ROOT CAUSE ARCHITECTURAL IDENTIFIÉ**

- **Phase**: IA Négamax + Alpha-Beta fonctionnelle (profondeur 2)
- **Architecture**: 6 modules + evaluation.pl centralisé
- **ROOT CAUSE**: ✅ **IDENTIFIÉ** - Séparation opening/regular court-circuite sécurité MVV-LVA
- **Détection défense**: ✅ **CORRIGÉE** en phase standard (coups 16+)
- **Blunders Dame**: 🚨 **PERSISTENT** - Seulement en ouverture (coups 1-15)
- **Interface**: ✅ **STABLE** (plus de freeze observés)
- **Branche sécurisée**: ✅ **CRÉÉE** (`feature/ai-v3-unified-architecture`)

---

## 🚨 **ROOT CAUSE ARCHITECTURAL IDENTIFIÉ** (2025-09-07)

### **🔍 PROBLÈME SYSTÉMIQUE**
**Architecture non-standard** : `generate_opening_moves` vs `generate_regular_moves` court-circuite sécurité MVV-LVA durant les 15 premiers coups.

```
💀 OUVERTURE (coups 1-15):  generate_opening_moves → AUCUN order_moves → Dame blunders
✅ STANDARD (coups 16+):   generate_regular_moves → order_moves → Sécurité MVV-LVA
```

### **🏛️ STANDARDS PROFESSIONNELS**
**Context7 Research** : Stockfish, python-chess, chessops utilisent :
- **UNE fonction génération** avec sécurité partout
- **Opening books séparés** (Polyglot) pour théorie
- **JAMAIS de court-circuit sécurité**

---

## ⚡ **SOLUTIONS STRUCTURÉES - PLAN D'ACTION**

### **🚀 OPTION B - QUICK FIX (IMMÉDIAT)** 
**Objectif** : Sécuriser Dame immédiatement sans refactoring majeur
**Temps** : 15-20 minutes
**Risque** : Minimal

#### **B.1 - Correction Critique** (5 min)
```prolog
% FICHIER: src/ai.pl ligne 439
% AVANT (DANGEREUX)
take_first_n_simple(AllMoves, Limit, Moves).

% APRÈS (SÉCURISÉ) 
order_moves(GameState, Player, AllMoves, OrderedMoves),
take_first_n_simple(OrderedMoves, Limit, Moves).
```

#### **B.2 - Nettoyage Double Tri** (5 min)  
```prolog
% FICHIER: src/ai.pl ligne 460 - Supprimer double appel
% generate_regular_moves ne fait plus order_moves (fait par negamax_ab)
ai_move_limit(Limit), take_first_n_simple(AllMoves, Limit, Moves).
```

#### **B.3 - Tests Validation** (10 min)
- Test Dame en ouverture (coups 1-5)
- Vérifier captures défendues détectées
- Partie complète validation

---

### **🏛️ OPTION A - ARCHITECTURE PROFESSIONNELLE (FUTURE)**
**Objectif** : Refactoring complet vers standards Stockfish/python-chess
**Temps** : 2-3 heures  
**Risque** : Modéré (branche sécurisée créée)

#### **A.1 - Unification Génération Coups** (60 min)
```prolog
% REMPLACEMENT COMPLET 
generate_moves_unified(GameState, Player, Moves) :-
    generate_all_legal_moves(GameState, Player, AllMoves),
    order_moves(GameState, Player, AllMoves, OrderedMoves),
    get_move_limit(Limit), take_first_n_simple(OrderedMoves, Limit, Moves).
```

#### **A.2 - Opening Book Séparé** (45 min - OPTIONNEL)
```prolog
% NOUVEAU FICHIER: opening_book.pl
opening_theory_move(MoveCount, BoardPattern, Move).
get_theory_move(Board, MoveCount, Move).
```

#### **A.3 - Pipeline Simplifié** (30 min)
```prolog
% negamax_ab appelle directement generate_moves_unified
% Plus de double tri, architecture propre
```

#### **A.4 - Tests Complets + Validation** (45 min)
- Tests régression complets
- Comparaison performance IA v2 vs v3
- Validation comportement identique/amélioré

---

## 🎯 **RECOMMANDATION STRATÉGIQUE STRUCTURÉE**

### **🚀 PHASE 1 - SÉCURISATION IMMÉDIATE (RECOMMANDÉ)**
**OPTION B - Quick Fix sur branche master**

**Pourquoi commencer par Option B :**
- ✅ **Résolution immédiate** des blunders Dame (15 min)
- ✅ **Risque minimal** - Une seule ligne changée
- ✅ **Validation rapide** - Test comportement immédiat
- ✅ **Code stable** - Garde architecture existante fonctionnelle

**Processus détaillé :**
```bash
# 1. S'assurer d'être sur master
git branch  # Vérifier branche actuelle

# 2. Appliquer Option B (15 min)
# Modifier src/ai.pl ligne 439
# Tests validation comportement Dame

# 3. Commit sécurisation
git add src/ai.pl
git commit -m "fix: Add MVV-LVA ordering to opening moves - resolves queen blunders"

# 4. Tests validation complets
swipl go.pl  # Test parties Dame sécurisée
```

### **🏛️ PHASE 2 - ARCHITECTURE PROFESSIONNELLE (FUTURE)**  
**OPTION A - Refactoring sur branche feature**

**Quand procéder à Option A :**
- ✅ **Après validation Option B** - Dame sécurisée fonctionnelle
- ✅ **Si souhaité** - Migration vers standards professionnels
- ✅ **Temps disponible** - 2-3h session dédiée

**Processus sécurisé :**
```bash
# 1. Basculer vers branche développement
git checkout feature/ai-v3-unified-architecture

# 2. Appliquer Option A (2-3h)
# Refactoring architecture complet

# 3. Tests complets branche
# Validation IA v3 vs IA v2

# 4. Merge si satisfait
git checkout master
git merge feature/ai-v3-unified-architecture
```

### **📊 MATRICE DÉCISION**

| Critère | Option B (Quick) | Option A (Refactoring) |
|---------|------------------|------------------------|
| **Temps** | ⭐⭐⭐ 15 min | 🔸 2-3 heures |
| **Risque** | ✅ Minimal | ⚠️ Modéré |
| **Impact** | ✅ Résout blunders | ⭐ Standards pros |
| **Maintenance** | ⚠️ Architecture mixte | ✅ Architecture propre |
| **Tests** | ✅ Simple | 🔸 Complets requis |

### **🎯 DÉCISION RECOMMANDÉE**

**SÉQUENTIEL - OPTION B PUIS A (SI SOUHAITÉ)**

1. **IMMÉDIAT** : Option B pour sécuriser Dame (15 min)
2. **VALIDATION** : Tests parties, confirmation résolution blunders
3. **FUTUR** : Option A si migration standards professionnels souhaitée

**Avantages approche séquentielle :**
- Problème critique résolu rapidement
- Option A devient non-urgente (choix architectural)
- Code fonctionnel entre les deux phases
- Possibilité d'arrêter après Option B si satisfait

---

## 📋 **TÂCHES HISTORIQUES (COMPLÉTÉES/ARCHIVÉES)**

### **🚨 PRIORITÉ 1 : TASK MVV-LVA - DEBUG CRITIQUE** ⚠️ **EN COURS**
- **DÉCOUVERTE CHOC** : Implémentation défense **non fonctionnelle** (bug couleur)
- **Tests faux positifs** : Passent par accident (différence valeur pièces) 
- **BUG IDENTIFIÉ** : `is_square_attacked(NewBoard, ToRow, ToCol, Opponent)` → Mauvaise couleur
- **CORRECTION REQUISE** : Paramètre Player au lieu de Opponent dans move_score_with_defense
- **Status** : **DEBUG ACTIF** - Fix bug couleur + tests authentiques
- **Effort restant** : 30-45 min (correction + validation)

### **⚔️ PRIORITÉ 2 : TASK CAPTURES-FORCÉES - Inclusion Captures/Recaptures**
- **Problème** : Limite `ai_move_limit(25)` coupe séquences tactiques critiques
- **Impact** : Séquences Capture→Recapture→Re-recapture tronquées
- **Localisation** : `generate_regular_moves/3` ligne 351-353
- **Solution** : Forcer inclusion TOUTES captures même au-delà limite
- **Effort** : 30-45 min (modification génération coups)

### **⚙️ PRIORITÉ 3 : TASK QUIESCENCE - Recherche Tactique**
- **Problème** : Pas d'extension recherche tactique aux nœuds feuilles
- **Impact** : Effet horizon, évaluation tactique défaillante  
- **Solution** : Implémenter quiescence search pour captures/échecs
- **Explique** : Dame prématurée, blunders tactiques
- **Effort** : 90-120 min (nouvelle fonction)

### **🔍 TASK AI-3 : Analyse generate_opening_moves**
- **Problème** : Logique opening vs endgame (ligne 353-355 ai.pl) - approche à valider
- **Question** : Division coups opening/endgame conforme aux standards moteurs professionnels ?
- **Analyse requise** : In-depth analysis fonction + alternatives standards
- **Effort** : 45-60 min (research + analyse)

### **📊 PRIORITÉ 5 : TASK MOBILITÉ - Intégration Évaluation Mobilité**  
- **Problème** : `evaluate_piece_development/3` et `evaluate_move_count/3` existent mais **non utilisés**
- **Impact** : IA ignore mobilité → préfère pièces statiques vs actives
- **Localisation** : `evaluate_position/3` ligne 197-220 (ajouter mobilityDiff)
- **Solution** : Intégrer mobilité dans pipeline évaluation principal
- **Effort** : 30-45 min (modification evaluate_position + tests)

### **🏰 PRIORITÉ 6 : TASK KING-SAFETY - Couverture Pions Roi**
- **Problème** : `evaluate_king_safety_basic/3` existe mais incomplet + non intégré
- **Impact** : IA expose roi → vulnérable mats rapides
- **Manque** : Couverture pions devant roi, intégration evaluate_position  
- **Localisation** : `evaluation.pl` - améliorer + intégrer king safety
- **Effort** : 45-60 min (couverture pions + intégration)

### **🎯 PRIORITÉ 7 : TASK PROMOTIONS - Priorisation Promotions/Échecs**
- **Problèmes** : Promotions (Score = 0) et échecs non priorisés dans tri
- **Impact** : Occasions promotion ratées, combinaisons tactiques manquées
- **Localisation** : `move_score/4` - ajouter détection promotions/échecs
- **Scores attendus** : Promotion ~90, Échecs ~50
- **Effort** : 30-45 min

### **🤖 PRIORITÉ 8 : TASK SAFETY-1 - Piece Safety Decision**
- **Problème** : `evaluate_piece_safety` désactivé → blunders tactiques
- **Options** : Activer/Supprimer/Intégrer après corrections MVV-LVA
- **Impact** : IA sacrifie pièces vs pions défendus  
- **Localisation** : `src/evaluation.pl`
- **Effort** : 30-45 min

### **⚠️ TASK DEBUG-1 - Interface Loop Bug** ✅ **RÉSOLU**
- **Problème** : Freeze sur `d2d4` → `c1g5` → `g5e7` 
- **✅ STATUS** : Bishop e7 jouable, pas de freeze observé lors tests MVV-LVA
- **Validation** : Tests multiples sessions sans problème interface
- **Résolution** : Corrections codes antérieures ont résolu le bug
- **Effort** : Résolu (monitoring continu recommandé)

### **🚨 TASK MVV-LVA DEBUG CRITIQUE - MISE À JOUR 2025-09-06** ⚠️ **BLUNDER PERSIST**

#### **DÉCOUVERTE INITIALE** ✅ (Corrigée partiellement)
- **Bug paramètre couleur** : `Opponent` → `Player` dans ai.pl:281 ✅ CORRIGÉ
- **Tests isolés** : Passent maintenant authentiquement ✅ VALIDÉ
- **Scores MVV-LVA** : Défense détectée (-700 vs +600) ✅ FONCTIONNEL

#### **🚨 NOUVELLE DÉCOUVERTE CRITIQUE** ❌ **PROBLÈME PERSISTE**
- **Date** : 2025-09-06 (Jeu réel test)
- **Evidence** : IA blunder dame a5→a2 au coup 5 malgré "correction"
- **Réalité** : Tests isolés ≠ Comportement jeu réel
- **Impact** : Blunders tactiques persistent en partie réelle

#### **HYPOTHÈSES ROOT CAUSE RESTANT**
1. **Pipeline incomplet** : Détection défense bypass dans generate_moves
2. **Limitation coups** : ai_move_limit(25) coupe analyses tactiques
3. **Contexte ouverture** : generate_opening_moves ignore MVV-LVA ?
4. **Autre bug logique** : Problème dans order_moves ou negamax_ab

#### **ACTIONS CRITIQUES REQUISES**
1. **ANALYSE PIPELINE COMPLET** : Tracer coup a5→a2 dans jeu réel
2. **DEBUG GENERATE_OPENING_MOVES** : Vérifier si MVV-LVA appliqué en ouverture
3. **VALIDATION NÉGAMAX** : S'assurer que tri MVV-LVA respecté par IA
4. **TESTS INTÉGRATION** : Tests jeu réel, pas seulement isolés

#### **🚨 NOUVELLE OBSERVATION CRITIQUE - ÉVALUATION ERRATIQUE** (2025-09-06)
- **Evidence** : Évaluation +60 → -1045 après Dame d8→a5 (swing -1105 points!)
- **Problème** : IA voit Dame a5 comme "excellent coup" malgré exposition dangereuse
- **Incohérence** : Coup tactiquement mauvais = meilleur score évaluation
- **Hypothèse** : `evaluate_position` défaillante, `piece_safety` non fonctionnelle

**STATUS** : **BUG ÉVALUATION GLOBALE** - Problème plus large que MVV-LVA isolé

---

## 🎯 **PLAN D'INTERVENTION EFFICACE - PRIORITÉ CRITIQUE**

### **PHASE 1: AUDIT ÉVALUATION (60 min)**
1. **DEBUG evaluate_position** - Tracer évaluation coup Dame d8→a5
   - Composants: Matériel, PSQT, Sécurité pièces
   - Identifier pourquoi +60 → -1045 swing
2. **ANALYSER piece_safety** - Vérifier si Dame exposée détectée
   - Test position Dame a5 : is_square_attacked fonctionnel ?
3. **AUDIT detect_check_bonus** - Bonus échec trop élevé ?

### **PHASE 2: CORRECTION CIBLÉE (45 min)**
1. **FIX évaluation incohérente** - Corriger composant défaillant
2. **VALIDATION piece_safety** - Activer détection hanging pieces
3. **ÉQUILIBRAGE bonus** - Ajuster check_bonus vs piece_safety

### **PHASE 3: TESTS INTÉGRATION (30 min)** 
1. **TEST position Dame a5** - Vérifier évaluation corrigée
2. **PARTIE COMPLÈTE** - Valider IA évite blunders Dame prématurée
3. **RÉGRESSION** - S'assurer autres fonctions intactes

**EFFORT TOTAL** : 135 min - **IMPACT** : Élimination blunders tactiques IA

---


## 🎯 **CLARIFICATION SÉQUENCE PROBLÈME** (2025-09-06)

### **📍 CORRECTION ANALYSE UTILISATEUR**
**Précision importante** : Dame sort prématurément mais **pas en danger immédiat**  
**Blunder réel** : Survient **coup suivant** avec Dame exposée faisant capture défendue

### **🔗 DOUBLE ROOT CAUSE CONFIRMÉE**

#### **PROBLÈME A : ÉVALUATION DÉVELOPPEMENT DÉFAILLANTE** 
- **Séquence** : IA privilégie sortie Dame > développement Cavaliers/Fous
- **Impact** : Dame prématurée (position sûre mais stratégiquement mauvaise)
- **Solution** : Ajuster scoring ouverture pour favoriser développement pièces mineures

#### **PROBLÈME B : DÉTECTION DÉFENSE DÉFAILLANTE**
- **Séquence** : Dame exposée → IA fait capture défendue → Blunder
- **Impact** : Pertes matérielles, captures matériel même protégé par pions
- **Solution** : Debug système détection défense global

### **🎯 STRATÉGIE BICÉPHALE PROCHAINE SESSION**
**Deux axes indépendants** requérant corrections séparées :
1. **AXE PRÉVENTION** : Évaluation développement (éviter Dame prématurée)
2. **AXE LIMITATION DÉGÂTS** : Détection défense (éviter blunders si Dame sort)

### **❓ DÉCISION STRATÉGIQUE REQUISE**
**Question prochaine session** : Ordre priorité pour impact maximal ?
- Option A : Développement d'abord (prévenir problème) 
- Option B : Détection défense d'abord (limiter dégâts)

**STATUS** : **DOUBLE ROOT CAUSE** - Approche bicéphale planifiée


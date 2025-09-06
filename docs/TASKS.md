# 🚨 PROLOG CHESS GAME - DEVELOPMENT TASKS

## 📊 **STATUS ACTUEL**

- **Phase**: IA Négamax + Alpha-Beta fonctionnelle (profondeur 2)
- **Architecture**: 5 modules + evaluation.pl centralisé  
- **Problèmes critiques**: Dame prématurée, blunders tactiques, ~~interface loop~~ **POSSIBLEMENT RÉSOLU**

---

## 🚨 **PROBLÈMES IDENTIFIÉS**

### **Comportement IA Problématique**
- **Dame sort trop tôt** en ouverture (violations principes échiquéens)
- **Blunders tactiques** : capture sans évaluer défense
- **Ignore recaptures critiques** au profit développement excessif
- **Hanging pieces** non détectées

### **Analyse Technique**
- `generate_opening_moves` inclut tous coups dame dans "OtherMoves"
- **Aucune évaluation captures** pendant 15 premiers coups
- **Pipeline IA** nécessite audit vs standards moteurs professionnels

---

## 🔬 **PRIORITÉ IMMÉDIATE - TASK ARCH-2**

### **Audit Complet Architecture IA Moteur**

**Objectif** : Analyse approfondie vs standards moteurs d'échecs professionnels

#### **PHASE A : Research Context7** 
- Standards génération coups, tri, évaluation, anti-blunders
- Benchmarks Stockfish, moteurs modernes

#### **PHASE B : Audit Architecture Actuelle**
- Pipeline `generate_moves_simple` → `order_moves` → `negamax_ab`
- Modules `ai.pl` et `evaluation.pl`
- Focus : détection blunders, sécurité pièces, captures

#### **PHASE C : Gap Analysis & Plan Implémentation**  
- Identification composants manquants
- Roadmap anti-blunders → évaluation → optimisations
- **Effort** : 90-120 min

---

## 📋 **TÂCHES PRIORITAIRES IDENTIFIÉES**

### **🚨 PRIORITÉ 1 : TASK ALPHA-BETA - Correction Élagage Alpha-Beta**
- **Problème** : `_NewAlpha` et `_NewBeta` calculés mais jamais utilisés dans appel récursif
- **Localisation** : `src/ai.pl:169-170`
- **Impact** : **AUCUN ÉLAGAGE RÉEL** - ralentissement exponentiel (1-4s/coup)
- **Solution** : Passer `-Beta, -Alpha` à l'appel `negamax_ab` récursif
- **Effort** : 45-60 min (correction + tests)

### **🔍 PRIORITÉ 2 : TASK MVV-LVA - Détection Défense Captures**
- **Problème** : Pas de vérification `is_square_attacked` après capture
- **Impact** : IA fait captures perdantes (Dame vs Pion défendu)
- **Localisation** : `move_score/4` ligne 212-222
- **Solution** : Ajouter simulation coup + détection défense
- **Effort** : 60-90 min (implémentation + tests)

### **⚔️ PRIORITÉ 2.5 : TASK CAPTURES-FORCÉES - Inclusion Captures/Recaptures**
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

### **🔬 PRIORITÉ 4 : TASK ARCH-2 - Audit Architectural** 
- **Objectif** : Analyse pipeline IA vs standards moteurs d'échecs professionnels
- **Context7 Research** : Standards génération, tri, évaluation, anti-blunders
- **Audit Modules** : `ai.pl`, `evaluation.pl`, pipeline complet
- **Deliverable** : Gap analysis + roadmap implémentation structuré
- **Effort** : 90-120 min

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

### **✅ TASK DEBUG-1 - Interface Loop Bug** ⚠️ **POSSIBLEMENT RÉSOLU**
- **Problème** : Freeze sur `d2d4` → `c1g5` → `g5e7` 
- **🎉 UPDATE** : Bishop e7 jouable après restructurations récentes
- **Statut** : **TESTS VALIDATION REQUIS** - confirmation résolution nécessaire
- **Protocole Test** : Séquence multiple `d2d4` → `c1g5` → `g5e7` + variantes
- **Effort** : 15-30 min tests systématiques

### **🧪 PRIORITÉ 7 : TASK VALIDATION-1 - Tests Systématiques Interface**
- **Objectif** : Confirmer résolution bug interface loop g5e7
- **Protocole** :
  - Tests séquence `d2d4` → `c1g5` → `g5e7` (5+ répétitions)
  - Variantes ouverture menant au même type capture
  - Tests robustesse 3-5 parties complètes IA vs Humain
- **Critères succès** : Aucun freeze, transitions états fluides
- **Effort** : 20-30 min tests manuels


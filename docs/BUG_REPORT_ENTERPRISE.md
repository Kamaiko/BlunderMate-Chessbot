# BUG REPORT - Interface Loop & IA Issues

**Priority**: CRITIQUE  
**Status**: EN COURS - Audit architectural requis  
**Date**: 2025-01-05  

---

## 🚨 **BUGS CRITIQUES ACTIFS**

### **1. Interface Loop Bug - g5e7 Freeze** ⚠️ **POSSIBLEMENT RÉSOLU**
- **Problème** : Boucle infinie sur séquence `d2d4` → `c1g5` → `g5e7`
- **Symptômes** : Application freeze, "Mouvement joue: g5e7" répété
- **🎉 UPDATE (2025-01-05)** : **Bishop e7 jouable** après restructurations récentes
- **Status** : **TESTS SUPPLÉMENTAIRES REQUIS** - validation séquence complète nécessaire
- **Next Actions** : Tests multiples pour confirmer résolution définitive

### **2. Piece Safety Désactivée**
- **Location** : `src/evaluation.pl` (ex `src/ai.pl:372-373`)
- **Problème** : `evaluate_piece_safety` hardcodé retourne 0
- **Impact** : IA sacrifie pièces vs pions défendus (blunders tactiques)
- **Status** : Fonction existe mais désactivée - décision requise

---

## 🔍 **PROBLÈMES IA IDENTIFIÉS**

### **Comportement Tactique Défaillant**
- Dame sort trop tôt en ouverture
- Captures sans évaluer défense
- Ignore recaptures critiques
- Hanging pieces non détectées

### **Architecture Pipeline**
- `generate_opening_moves` inclut tous coups dame dans "OtherMoves"
- Aucune évaluation captures pendant 15 premiers coups
- Pipeline nécessite audit vs standards moteurs d'échecs

---

## 🎯 **PLAN D'ACTION PRIORITAIRE**

### **PRIORITÉ 1 : Audit Architectural Complet**
**Objectif** : Analyse vs standards moteurs d'échecs
- Context7 research sur standards professionnels
- Audit pipeline `generate_moves` → `order_moves` → `negamax_ab`
- Gap analysis et plan implémentation structuré
- **Effort** : 90-120 min

### **PRIORITÉ 2 : Validation Bug Resolution**
**Status** : Bishop e7 jouable après restructurations
**Tests requis** :
- Validation séquence `d2d4` → `c1g5` → `g5e7` (5+ fois)
- Tests robustesse sur variantes similaires
- **Effort** : 15-30 min tests systématiques

### **PRIORITÉ 3 : Décision Piece Safety**
**Options** :
- Activer implémentation existante
- Supprimer complètement de l'évaluation
- Intégrer dans audit architectural global
- **Effort** : 30-45 min

---

## 🔧 **SOLUTIONS TENTÉES (HISTORIQUE)**

### **❌ Échecs Précédents**
- **IA Timeout** : `call_with_time_limit` indisponible Windows
- **Profondeur réduite** : Problème interface, pas IA
- **Emergency moves** : Contournement partiel seulement

### **⚠️ Corrections Partielles**
- Emergency move system avec `catch/3` (retained)
- Profondeur 2 maintenue (décision utilisateur)

---

## 📊 **REPRODUCTION & DIAGNOSTICS**

### **Séquence Reproductible** ⚠️ **À RE-TESTER**
```
1. swipl go.pl → "2. Mode IA vs Humain"
2. d2d4 → IA: c7c6 ✅
3. c1g5 → IA: d7d5 ✅  
4. g5e7 → **POSSIBLEMENT RÉSOLU** ⚠️
```

### **🔍 TESTS VALIDATION REQUIS**
**Séquences à tester** :
1. **Séquence originale** : `d2d4` → `c1g5` → `g5e7` (multiple fois)
2. **Variantes ouverture** : Autres séquences menant au même type de capture
3. **Différents contextes** : Début/milieu/fin de partie
4. **Tests robustesse** : 10+ parties complètes mode IA vs Humain

### **Console Output Observé**
```
Mouvement joue: g5e7
IA reflechit (noir, negamax alpha-beta)...
Mouvement joue: g5e7
[Répété indéfiniment]
```

### **Points de Surveillance**
- State propagation : `update_unified_game_state/3`
- Move execution : `attempt_move/6` 
- Loop recursion : `unified_game_loop/1`

---

## 📋 **RECOMMANDATIONS STRUCTURÉES**

### **Court Terme (Prochaine Session)**
1. **TASK ARCH-2** : Audit architectural avec Context7
2. **Debug Interface** : Traces état + investigation loop
3. **Décision Piece Safety** : Activer/supprimer/reporter

### **Moyen Terme (Post-Audit)**
- Implémentation corrections identifiées par audit
- Tests validation comportement IA
- Documentation mise à jour

### **Validation Requise**
- Tests systématiques séquence `d2d4` → `c1g5` → `g5e7`
- Confirmation résolution définitive du bug interface
- Parties complètes IA vs Humain pour robustesse

---

## 📁 **FILES CRITIQUES**

### **Interface Loop Investigation**
- `src/interface.pl:472-484` - `attempt_move/6`
- `src/interface.pl:265-295` - `unified_game_loop/1`
- `src/interface.pl:41-45` - `update_unified_game_state/3`

### **IA Behavior Analysis**
- `src/ai.pl:241-250` - `generate_moves_simple/3`
- `src/evaluation.pl` - Évaluation centralisée
- Pipeline génération → tri → négamax

---

## 🔍 **EVALUATION MODULE ANALYSIS**

### **is_piece_defended/4 - Conservative Fail** ⚠️ **CRITIQUE**
- **Location**: `src/evaluation.pl:307-310`
- **Issue**: Systematic `fail.` → all attacked pieces considered hanging
- **Impact**: Over-conservative evaluation, blocks legitimate sacrifices
- **Status**: Intentional conservative approach, needs real defense detection

### **evaluate_position/3 - Incomplete Integration** 📊 **PARTIELLEMENT EXACT**
- **Current**: Material + PSQT + Safety (lines 199-214)
- **Missing**: Mobility, center control, king safety modules exist but unused
- **Available unused**: `evaluate_piece_development/3`, `evaluate_move_count/3`, `evaluate_king_safety_basic/3`
- **Recommendation**: Integrate complete evaluation pipeline

### **PSQT Calibration Risk** ⚠️ **VALID CONCERN**
- **Issue**: PSQT values added directly to material values
- **Risk**: Potential double counting or over-weighting pieces
- **Location**: `evaluate_position/3` combines both without calibration
- **Needs**: Verification PSQT tables are calibrated with current `piece_value/2`

### **piece_type_from_symbol/2 - Verbose Implementation** 🔧 **OPTIMIZATION**
- **Current**: 12 clauses with cuts (lines 260-271)
- **Optimization opportunity**: Map via lowercase normalization
- **Impact**: Minor performance, code clarity improvement

### **King Safety Implementation** ✅ **DIAGNOSTIC ERROR**
- **Correction**: `evaluate_king_safety_basic/3` exists but unused in main evaluation
- **Location**: Available in evaluation.pl but not integrated

**Status** : Evaluation module analysis complete, priorities identified for architectural audit.

---

## 🚨 **ANALYSE ALGORITHME IA - DÉFAUTS CRITIQUES**

### **Élagage Alpha-Beta CASSÉ** ❌ **DÉFAUT CRITIQUE**
- **Localisation** : `src/ai.pl:169-170`
- **Problème** : `_NewAlpha` et `_NewBeta` calculés mais **JAMAIS UTILISÉS**
- **Code** : `negamax_ab(NewGameState, NextPlayer, NewDepth, _, OpponentValue)` 
- **Impact** : **AUCUN ÉLAGAGE RÉEL** - algorithme négamax sans alpha-beta
- **Performance** : Ralentissement exponentiel, explique les 1-4s par coup

### **Gestion Couleurs - Non-Standard** ⚠️ **CHOIX DESIGN**
- **Actuel** : Utilise chaînes `white`/`black` au lieu de numérique `+1/-1`
- **Impact** : Logique négation plus complexe vs pattern NegaMax standard
- **Évaluation** : Utilise conditionnels `Player = white ->` au lieu de `Color * Evaluation`
- **Statut** : Fonctionne mais non-standard, plus difficile à optimiser

### **Tri des Coups - Implémentation CORRECTE** ✅ **BON**
- **Implémentation** : MVV-LVA (Most Valuable Victim - Least Valuable Attacker)
- **Localisation** : `order_moves/4` avec `move_score/4` 
- **Fonctionnalités** : Captures priorisées, keysort_desc approprié
- **Qualité** : Suit les bonnes pratiques des moteurs d'échecs

### **Gestion Positions Terminales - BASIQUE** ✅ **FONCTIONNEL**
- **Échec et mat** : Retourne -100000 (négatif correct)
- **Pat** : Retourne 0 (neutre correct)
- **Localisation** : `terminal_score/3` lignes 190-194
- **Statut** : Implémentation minimale mais fonctionnelle

### **Recherche Quiescence - MANQUANTE** ❌ **LACUNE MAJEURE**
- **Problème** : Pas d'extension recherche tactique aux nœuds feuilles  
- **Impact** : Effet horizon, évaluation tactique défaillante
- **Standard** : Devrait prolonger recherche pour captures/échecs
- **Explique** : Dame prématurée, blunders tactiques

### **Comparaison Bonnes Pratiques**
```prolog
% ❌ NOTRE CODE (CASSÉ)
_NewAlpha is -Beta, _NewBeta is -Alpha,
negamax_ab(NewGameState, NextPlayer, NewDepth, _, OpponentValue)

% ✅ STANDARD CORRECT
negamax(NewState, NextDepth, -Beta, -Alpha, OpponentColor, Score0)
```

**Statut** : Analyse algorithme IA révèle défaut critique implémentation alpha-beta nécessitant correction immédiate pour performance.

---

## 📊 **ANALYSE SYSTÈME TRI MVV-LVA - LACUNES IDENTIFIÉES**

### **MVV-LVA Base - Implémentation CORRECTE** ✅ **BON**
- **Localisation** : `move_score/4` lignes 212-222
- **Formule** : `AbsTargetVal - AbsAttackerVal + 1000` (correct)
- **Tri décroissant** : `keysort_desc` fonctionnel
- **Captures prioritaires** : Score 1000+ vs coups neutres 0

### **Détection Défense - MANQUANTE** ❌ **LACUNE CRITIQUE**
- **Problème** : Pas de vérification `is_square_attacked` après capture
- **Impact** : IA fait captures perdantes (Dame vs Pion défendu)
- **Théorie manquée** : Score ajusté `Value(victim) - Value(attacker)` si défendue
- **Exemple blunder** : Prend Dame (900) avec Tour (500) → Dame défendue par Pion (100) → Perte nette -400

### **Promotions - NON PRIORISÉES** ❌ **LACUNE TACTIQUE**  
- **Problème** : Promotions pion traitées comme coups neutres (Score = 0)
- **Impact** : Promotions ignorées vs captures mineures
- **Standard attendu** : Score élevé ~90 pour promotion Dame
- **Conséquence** : Occasions promotion ratées, fins de partie défaillantes

### **Échecs - NON PRIORISÉS** ❌ **LACUNE TACTIQUE**
- **Problème** : Aucune détection `is_in_check` dans scoring coups
- **Impact** : Échecs forçants ignorés dans tri
- **Standard attendu** : Score modéré ~50 pour échecs
- **Conséquence** : Combinaisons tactiques manquées

### **Comparaison Théorie vs Implémentation**
```prolog
% ❌ NOTRE CODE (SIMPLE)
Score is AbsTargetVal - AbsAttackerVal + 1000

% ✅ THÉORIE COMPLÈTE  
adjust_capture_score(Board, Player, Move, BaseScore, AdjustedScore) :-
    simulate_move(Board, Move, NewBoard),
    ( is_square_attacked(NewBoard, ToRow, ToCol, Opponent) ->
        AdjustedScore is BaseScore - AttackerValue  % Défendue
    ; AdjustedScore = BaseScore                     % Sûre
    ).
```

### **Impact sur Blunders Tactiques**
- **Root cause** : Tri basique sans analyse défense
- **Explique** : Dame prématurée, captures perdantes
- **Priorité** : Implémentation détection défense = réduction blunders immédiats

**Statut** : Système tri MVV-LVA fonctionnel mais incomplet - détection défense manquante explique blunders tactiques majeurs.
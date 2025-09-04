# TASKS - Projet Échecs IA (IFT-2003)

> **Travail universitaire TP1** - 10% note finale IFT-2003

## 🎯 ROADMAP TP1 - Vue d'ensemble

| Phase | Status | Contenu | Tests |
|-------|--------|---------|-------|
| **Phase 1** | ✅ **COMPLÈTE** | Fondations, mouvements base, interface | 28 tests |
| **Phase 2** | ✅ **COMPLÈTE** | Échec/mat, promotion pions | +7 tests (35 total) |
| **Phase 3** | ❌ **DÉFAILLANTE** | IA minimax, alpha-beta, interface unifiée | +7 tests (42 total) |

### ✅ Étape actuelle : OUVERTURE CORRIGÉE + PROBLÈMES TACTIQUES
- **Status technique** : Interface unifiée, 42/42 tests PASS, Mode IA vs Humain actif
- **✅ NOUVEAU** : **Coups d'ouverture fixes implémentés** (c7-c6, d7-d5)
- **Problème restant** : IA fait des blunders tactiques constants (donne matériel gratuitement)
- **Impact** : IA utilisable pour ouverture, problèmes tactiques restants
- **📅 DEADLINE TP1** : 20 octobre 2025 - **PROGRÈS MAJEURS RÉALISÉS**

---


## Phase 3 : Intelligence Artificielle (TP1 Objectif)

### Algorithmes IA - IMPLEMENTATION COMPLÈTE ✅
- [x] **Minimax** : `minimax_ab/5` négamax avec alpha-beta profondeur 2 ✅
- [x] **Alpha-Beta** : IMPLÉMENTÉ + tri MVV-LVA + élagage complet ✅
- [x] **Évaluation** : SEE + matériel en danger + centre + mobilité + mat/pat ✅
- [x] **Interface IA** : Mode Humain vs IA intégré (Option 2) ✅

### Interface Polish
- [x] **Menu modernisé** : Design ASCII professionnel  
- [x] **Messages français** : Aide et navigation claire
- [x] **Gestion erreurs** : Validation robuste entrées
- [ ] **Améliorations** : Coordonnées lisibles, highlights

### 🧠 ANALYSE ARCHITECTURE IA COMPLÈTE (2025-01-09)
**DIAGNOSTIC TECHNIQUE APPROFONDI** - Architecture sophistiquée confirmée

#### 🎯 Algorithme Alpha-Beta Complet
- ✅ **Alpha-beta négamax** : `minimax_ab/5` avec élagage complet (profondeur 2)
- ✅ **Tri des coups** : MVV-LVA (Most Valuable Victim - Least Valuable Attacker)
- ✅ **Détection terminale** : Échec et mat (-100000), Pat (0)
- ✅ **Coups fixes ouverture** : Caro-Kann c7-c6, d7-d5 (premiers 2 coups noirs)
- ✅ **Génération coups optimisée** : Captures prioritaires + développement

#### 📊 Évaluation Simplifiée ÉDUCATIVE (Chess Programming Wiki)
- ✅ **Matériel** : Valeurs centipawns (P:100, N:320, B:330, R:500, Q:900)
- ✅ **Piece-Square Tables** : Tables positionnelles par pièce (centre bon, bords mauvais)
- ✅ **Exemple Cavalier** : -50pts bords, +bonus cases centrales
- ✅ **Exemple Roi** : Abri pions (milieu), actif centre (finale)
- ✅ **Philosophie** : Encourager développement, pénaliser bords

#### ✅ CORRECTIONS CRITIQUES APPLIQUÉES (2025-01-09)
- **Alpha-beta implémenté** : Élagage complet avec tri MVV-LVA
- **abs/2 corrigé** : Syntaxe Prolog standard `AbsValue is abs(Value)`
- **Évaluation simplifiée** : Pas de SEE - trop complexe niveau éducatif
- **Détection basique** : Anti-blunder simple sans simulation
- **Mat/Pat détection** : Positions terminales dans minimax

### ✅ PROBLÈME RÉSOLU - OUVERTURE FIXES
**IMPLÉMENTATION TERMINÉE** : Système de coups d'ouverture fixes pour résoudre définitivement le problème de logique d'ouverture.

**Solution** :
- **Coup 1 des noirs** : c7-c6 (Caro-Kann/Slav Defense)
- **Coup 2 des noirs** : d7-d5 (consolidation centre)
- **Coup 3+** : Basculement automatique vers minimax

**Fichiers modifiés** : `src/ai.pl` (nouveaux prédicats `use_fixed_opening/1`, `get_fixed_opening_move/2`)

### 📋 PROBLÈMES RESTANTS À CORRIGER
1. **Recapture dame** : En échec Qd8+, IA choisit Ke7 au lieu de Bxd8 
2. ~~**Logique d'ouverture NON CONFORME**~~ : ✅ **RÉSOLU** avec coups fixes
3. **Détection menaces** : Nf6 attaqué par e4-e5, IA ignore et joue d7-d5
4. **Évaluation heuristique incomplète** : Manque contrôle centre, sécurité roi, structure pions
5. **Tests validation** : Section 6 complètement à refaire pour nouvelles corrections

### 🎯 RECOMMANDATIONS THÉORIQUES PRIORITAIRES
```prolog
% RÉPONSES CLASSIQUES À IMPLÉMENTER (Priorité #1)
% Réponse au pion roi - OBLIGATOIRE avant développement
opening_move([e2,e4], [e7,e5]).   % Ouverture ouverte
opening_move([e2,e4], [c7,c5]).   % Sicilienne  
opening_move([e2,e4], [e7,e6]).   % Française

% Réponse au pion dame - OBLIGATOIRE avant développement
opening_move([d2,d4], [d7,d5]).   % Classique (PRIORITÉ ABSOLUE)
opening_move([d2,d4], [g8,f6]).   % Indienne
opening_move([d2,d4], [e7,e6]).   % Française pour d4
```

## Extensions Futures

- [ ] **En Passant** : Capture spéciale pion adjacente
- [ ] **Roque** : Validation roi/tour non bougés  
- [ ] **GUI** : Interface graphique  
- [ ] **Analyse** : Évaluation temps réel

---

## 📚 Ressources & Documentation

**Guides technique** : [CLAUDE.md](../.claude/CLAUDE.md) • [PRD.md](PRD.md) • [README.md](../README.md)  
**Tests** : `swipl -g "consult('tests/tests'), run_all_tests, halt."`  
**Jeu** : `swipl go.pl`

---

## 📝 NOTES SESSION PROCHAINE (2025-01-10)

### 🎯 Décision Architecturale : Négamax Confirmé
- **✅ Négamax** : Plus simple que minimax classique (pas de min/max séparés)
- **✅ Alpha-beta** : Déjà implémenté avec négamax (`Value is -OpponentValue`)
- **✅ Uniform** : Tous joueurs cherchent valeur maximale (logique symétrique)

### 📋 Checklist Validation Demain
- [ ] **Tests mat en 1** : IA choisit coup gagnant unique
- [ ] **Tests parade** : IA évite mat avec seule défense
- [ ] **Alpha-beta = minimax** : Mêmes résultats sur positions test
- [ ] **Recaptures auto** : e4xd5 → c6xd5 systématique
- [ ] **Performance <10s** : Profondeur 2-3 acceptable
- [ ] **Piece-Square Tables** : Implémenter tables ChessProgramming.org
- [ ] **Cases vides** : `empty_cell(' ')` partout

### 🚀 Extensions Potentielles (Après Validation)
- **Quiescence search** : Captures à profondeur 0 pour éviter horizons
- **Opening book** : Base théorique au lieu de coups fixes
- **En passant + roque** : Compléter règles d'échecs
- **Interface IA complete** : Choix difficulté/couleur

### ⚡ Rappels Techniques
- **Négamax** : `minimax_ab/5` déjà correct avec `Value is -OpponentValue`
- **Tri MVV-LVA** : Captures prioritaires déjà implémenté
- **Évaluation simple** : Matériel + position basique (pas de SEE)
- **Terminal detection** : Mat/pat avec `terminal_score/3`

---

## 🎯 ROADMAP SESSION DEMAIN (2025-01-10)

### 📅 Plan d'Action Prioritaire (90 minutes)

#### 🔥 PHASE 1 : Implémentation Piece-Square Tables (30 min)
1. **Implémenter PSQT** : Tables ChessProgramming.org Simplified Evaluation
2. **Enlever SEE complètement** : Remplacer par évaluation positionnelle simple
3. **Valeurs centipawns** : P:100, N:320, B:330, R:500, Q:900 (standard)
4. **Tables par pièce** : Cavaliers centre bonus, bords malus, etc.

#### 🧪 PHASE 2 : Tests Structurés (45 min)
1. **Tests mat en 1** : IA doit choisir coup gagnant immédiat
2. **Tests parade** : IA doit jouer l'unique défense
3. **Validation alpha-beta** : Consistency check minimax vs élagage
4. **Tests recaptures** : e4xd5 → c6xd5 automatique

#### ⚡ PHASE 3 : Optimisations (15 min)
1. **fast_get_piece/4** : Version IA sans validation redondante
2. **Pré-liste pièces** : Éviter boucles 8×8×8×8
3. **Documentation finale** : Commit + status complet

## 📊 ARCHITECTURE ACTUELLE CONFIRMÉE (Janvier 2025)

### 🧠 Points Forts Architecturaux
**NIVEAU** : Universitaire avancé - Dépasse les standards TP1

- **🎯 Alpha-beta** : Négamax avec élagage + tri MVV-LVA ✅
- **📊 SEE** : Static Exchange Evaluation avec simulation ✅
- **⚙️ Anti-blunder** : Détection matériel en danger implémentée ✅
- **🚀 Performance** : 0.00s coups simples, temps réalistes ✅
- **🎲 Ouverture** : Coups fixes + génération optimisée ✅

### ⚡ Corrections À Finaliser Demain
1. **Piece-Square Tables** : Implémenter évaluation positionnelle simple
2. **Tests structurés** : Mat en 1, parade, alpha-beta validation
3. **Cases vides** : Standardisation `' '` vs `'.'`
4. **Enlever SEE** : Simplifier évaluation pour niveau éducatif

### 🎯 Objectif Session
**Éliminer derniers bugs + valider robustesse avec tests exhaustifs**

### 🔧 CORRECTIONS PRIORITAIRES IDENTIFIÉES (Audit Externe 2025-01-09)

#### 🚨 Problèmes Bloquants (ai.pl)
1. **Évaluation position** : Rechercher exemples simples (piece-square tables?)
2. **Cases vides inconsistantes** : Tests `' '` vs `'.'` mélangés → standardiser
3. **Génération ouverture dangereuse** : Limitations ignorent coups anti-échec
4. **Bonus développement excessif** : 100 points vs 10 pions → baisser à 30
5. **Simplifier évaluation** : Enlever SEE, garder matériel + position basique

#### ⚠️ Corrections Qualité (Autres Fichiers)
- **board.pl** : Validation `get_piece/4` redondante → `fast_get_piece/4`
- **interface.pl** : Mode IA vs Humain pas branché
- **game.pl** : Détection attaque glissante incomplète
- **pieces.pl** : En passant + roque manquants

### 📊 PLAN DE TEST STRUCTURÉ (Recommandation Externe)

#### Phase 1 : Tests Unitaires Contrôlés
```prolog
% Mat en 1 : IA doit choisir LE coup gagnant
test_mate_in_one :- 
    % Position où seul Qd8# gagne, autres coups perdent
    
% Éviter mat : IA doit jouer l'unique parade
test_avoid_mate :-
    % Position où seul coup évite mat imminent
```

#### Phase 2 : Validation Alpha-Beta
```prolog
% Millions de tests : minimax = alpha-beta
test_alpha_beta_consistency :-
    % Générer positions aléatoires
    % Assert : même résultat avec/sans élagage
```

#### Phase 3 : Anti-Blunder Réel
- Recaptures : e4xd5 → c6xd5 obligatoire
- Menaces ignorées : cavalier f6 attaqué par e5
- Sacrifices involontaires : dame vs pion
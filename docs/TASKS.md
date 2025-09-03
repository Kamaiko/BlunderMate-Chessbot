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

### Algorithmes IA
- [x] **Minimax** : Implémenté avec timeout protection
- [x] **Alpha-Beta** : Optimisation active  
- [x] **Évaluation** : Matériel + mobilité fonctionnelle
- [x] **Interface IA** : Mode Humain vs IA intégré (Option 2)

### Interface Polish
- [x] **Menu modernisé** : Design ASCII professionnel  
- [x] **Messages français** : Aide et navigation claire
- [x] **Gestion erreurs** : Validation robuste entrées
- [ ] **Améliorations** : Coordonnées lisibles, highlights

### 🔄 STATUT ai.pl - AMÉLIORÉ PARTIELLEMENT 
**EN COURS D'AMÉLIORATION** - Corrections majeures appliquées
- ✅ **Développement des pièces** : IA joue maintenant Nc6, Nf6 (au lieu de pions uniquement)
- ✅ **Bugs critiques corrigés** : Valeurs noires, comptage rois, évaluation matérielle
- ❌ **Problème recapture** : IA ne capture pas la dame même quand possible (Bxd8 vs Ke7)
- ❌ **Logique d'ouverture** : Manque 1.d4 d5 (imitation coup central), ignore menaces sur pièces
- ❌ **Section 6 tests IA** : Toujours outdated - à refaire complètement

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
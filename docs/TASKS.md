# TASKS - Projet Échecs IA (IFT-2003)

> **Travail universitaire TP1** - 10% note finale IFT-2003

## 🎯 ROADMAP TP1 - Vue d'ensemble

| Phase | Status | Contenu | Tests |
|-------|--------|---------|-------|
| **Phase 1** | ✅ **COMPLÈTE** | Fondations, mouvements base, interface | 28 tests |
| **Phase 2** | ✅ **COMPLÈTE** | Échec/mat, promotion pions | +7 tests (35 total) |
| **Phase 3** | ✅ **FONCTIONNELLE** | IA minimax, alpha-beta, interface unifiée | +7 tests (42 total) |

### ✅ Étape actuelle : IA FONCTIONNELLE
- **Réalisation majeure** : IA minimax avec alpha-beta fonctionnelle
- **Status** : Interface unifiée, 42/42 tests PASS, Mode IA vs Humain actif
- **Améliorations** : Bug intermittent à corriger, optimisation profondeur 2
- **📅 DEADLINE TP1** : 20 octobre 2025 - OBJECTIF ATTEINT

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

### ✅ STATUT ai.pl 
**FONCTIONNEL** - Version production stable
- Code testé avec Section 6 (7 tests IA)
- Intégré au menu principal (Option 2)
- Architecture unifiée avec interface.pl
- ⚠️ Bug intermittent : IA peut s'arrêter après premier coup

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
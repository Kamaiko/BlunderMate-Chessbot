# TASKS - Projet Échecs IA (IFT-2003)

> **Travail universitaire TP1** - 10% note finale IFT-2003

## 🎯 ROADMAP TP1 - Vue d'ensemble

| Phase | Status | Contenu | Tests |
|-------|--------|---------|-------|
| **Phase 1** | ✅ **COMPLÈTE** | Fondations, mouvements base, interface | 28 tests |
| **Phase 2** | ✅ **COMPLÈTE** | Échec/mat, promotion pions | +7 tests (35 total) |
| **Phase 3** | 🚧 **EN COURS** | IA minimax, alpha-beta | À venir |

### 📈 Étape actuelle : IA MINIMAX
- **Objectif immédiat** : Algorithme minimax fonctionnel
- **Priorité 1** : Réécriture complète `ai.pl` (prototype défaillant)  
- **Priorité 2** : Alpha-beta + évaluation position
- **📅 DEADLINE TP1** : 20 octobre 2025 - Demo IA vs Humain fonctionnelle

---


## Phase 3 : Intelligence Artificielle (TP1 Objectif)

### Algorithmes IA
- [ ] **Minimax** : Arbre de recherche avec élagage
- [ ] **Alpha-Beta** : Optimisation performance
- [ ] **Évaluation** : Position + matériel + mobilité
- [ ] **Interface IA** : Mode Humain vs IA

### Interface Polish
- [x] **Menu modernisé** : Design ASCII professionnel  
- [x] **Messages français** : Aide et navigation claire
- [x] **Gestion erreurs** : Validation robuste entrées
- [ ] **Améliorations** : Coordonnées lisibles, highlights

### ⚠️ STATUT ai.pl 
**PROTOTYPE DÉFAILLANT** - Réécriture complète requise
- Code expérimental non testé
- NE PAS UTILISER en mode production
- Candidat suppression ou refactoring total

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
# TASKS - Projet Échecs IA (IFT-2003)

Roadmap développement - Travail universitaire TP1 (10% note finale)

## 📊 Statut Projet

**Phase** : 2/3 - Règles Avancées ✅ COMPLÈTE  
**Tests** : 35 tests ✅ - Architecture `tests.pl` unifiée + promotion  
**Interface** : ✅ Menu français stable  
**Suivant** : IA minimax (Phase 3)

## Phase 1 : Fondations ✅ COMPLÈTE

### Architecture & Logique Core
- [x] **5 modules** : pieces.pl, board.pl, game.pl, interface.pl, ai.pl
- [x] **Mouvements de base** : Toutes pièces + validation stricte
- [x] **Coordonnées** : Notation algébrique "e2e4" 
- [x] **Affichage** : Plateau ASCII + interface française

### Tests & Validation
- [x] **Suite complète** : 33 tests (5 catégories)
- [x] **Architecture unifiée** : tests.pl centralisé
- [x] **Couverture** : Mouvements, erreurs, cas limites

## Phase 2 : Règles Avancées ✅ COMPLÈTE

### Échec et Mat ✅ COMPLET
- [x] **Détection échec/mat/pat** : Algorithmes complets
- [x] **Scénarios complexes** : Double échec, pièces clouées
- [x] **Tests exhaustifs** : Tous cas de fin de partie

### Mouvements Spéciaux ✅ COMPLET
- [x] **Promotion** : Conversion automatique pion → dame
- [x] **Intégration** : Logique transparente dans système validation
- [x] **Tests** : Coverage promotion blanche/noire + capture

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
- [ ] **Performance** : Profiling et métriques

---

## 🎯 Roadmap TP1

**ÉTAPE ACTUELLE** : IA minimax (Phase 3)  
**OBJECTIF TP1** : IA minimax fonctionnelle  
**ÉVALUATION** : 10% note finale IFT-2003

### Priorités Immédiates
1. ✅ **Promotion** → Tests COMPLÉTÉ  
2. **IA minimax** → Alpha-Beta → Évaluation position  
3. **Demo** → Documentation finale

### Status Validation
- ✅ **Base solide** : 35 tests, interface stable  
- ✅ **Échec/mat** : Algorithmes complets  
- ✅ **Promotion** : Implémentée + testée  
- ⚠️ **IA** : Réécriture requise

**Documentation** : [CLAUDE.md](../.claude/CLAUDE.md) • [PRD.md](PRD.md) • [README.md](../README.md)
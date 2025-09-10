# 🎯 TODO LIST - PROLOG CHESS AI

## 📋 **PRIORITÉS DÉVELOPPEMENT** (Basées sur analyse code actuel)

### **🔴 URGENT (Semaine courante)**
- [ ] **TASK-1**: Refactorisation génération coups - 180 min
  - Problème: Captures Dame non priorisées, architecture incorrecte
  - Solution: Unifier génération, tri MVV-LVA immédiat, détection échecs/défense
  - Impact: Résout recapture Dame adverse (GENERATION_COUP.md)

### **🟡 HAUTE PRIORITÉ (2 semaines)**
- [ ] **TASK-2**: Optimiser développement Caro-Kann - 45 min
  - Problème: e6 précoce bloque fou c8
  - Solution: Ajuster PSQT f5=+40, e6=-30 OU ordre génération
  
- [ ] **TASK-3**: Refactoring fonctions longues - 90 min
  - `generate_structured_moves/3` (104 lignes) → 4 sous-fonctions
  - `move_score/3` (12 lignes) → extraire logique MVV-LVA

### **🟢 MOYEN TERME (3-4 semaines)**
- [ ] **TASK-4**: Documentation finale académique - 120 min
  - Rapport PDF IFT-2003 (Modélisation 20% + Implémentation 45%)
  - Guide utilisation étudiant
  
- [ ] **TASK-5**: Optimisations performance avancées - 180 min
  - Profondeur variable 2→3 pour menaces tactiques
  - Quiescence search pour captures forcing

---

## 🚨 **BUGS CRITIQUES IDENTIFIÉS**

### **BUG-1: Captures Dame non priorisées**
**Symptôme**: Dame adverse ne recapture pas après séquences tactiques
**Séquence problématique**:
```
1. e4 e5  2. Nf3 Nc6  3. Bb5 a6  4. Bxc6 dxc6  5. Nxe5 Qd4
6. Nf3 Qxe4+  7. Be2 Qxg2  8. Rf1 Qxh2 ← Dame capture mais ne recapture pas
```
**Séquence correcte**:
```
1. e4 e5  2. Nf3 Nc6  3. Bb5 a6  4. Bxc6 dxc6  5. Nxe5 Qd4
6. Nf3 Qxe4+  7. Be2 Qxg2  8. Rf1 Qxh2  9. Qd5 ← Dame recapture
```
**Root Cause**: 
- Captures Dame générées dans OtherMoves (priorité basse)
- Tri MVV-LVA appliqué trop tard sur liste mélangée
- Architecture séparant captures artificiellement

**Solution immédiate**: TASK-1

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

**Solution**: TASK-2

---

## ✅ **ÉTAT ACTUEL SYSTÈME**

### **Fonctionnel ✅**
- IA Négamax + Alpha-Beta profondeur 2 (performance 0.6s/coup)
- Interface française professionnelle Humain vs IA
- Architecture 6 modules stable (pieces/board/game/interface/ai/evaluation)
- Tests automatisés 8 sections (tous passent)

### **Problématique ❌**
- Captures Dame non priorisées (architecture génération coups)
- Développement ouverture sous-optimal (Caro-Kann)
- Code refactoring requis (fonctions >20 lignes)

---

**Dernière mise à jour**: 2025-01-09  
**Status**: 🔴 2 bugs critiques - 🟡 Refactorisation génération coups prioritaire - ✅ Core IA fonctionnelle
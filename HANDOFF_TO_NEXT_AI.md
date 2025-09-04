# 🤖 HANDOFF TO NEXT AI - Prolog Chess Game

## 🎯 CONTEXTE DU PROJET

**Projet** : IA d'échecs en Prolog pour cours universitaire IFT-2003  
**Phase actuelle** : Phase 3 - IA négamax + alpha-beta FONCTIONNELLE mais problématique  
**Statut** : IA autonome mais choisit de **mauvais coups d'ouverture**

## 📋 INSTRUCTIONS POUR LE PROCHAIN AI

### ✅ 1. COMMENCE PAR TE FAMILIARISER AVEC LE CODEBASE

```bash
# Lire la documentation projet
Read(.claude/CLAUDE.md)
Read(docs/TASKS.md) 
Read(docs/plan.md)

# Examiner l'architecture actuelle
Read(src/ai.pl)      # IA négamax + alpha-beta
Read(src/psqt_tables.pl)  # Tables évaluation position
```

**Architecture 6 modules** :
- `pieces.pl`, `board.pl`, `game.pl`, `interface.pl` (stables ✅)
- `ai.pl` : négamax + alpha-beta (fonctionnel mais choix défaillants ❌)
- `psqt_tables.pl` : Piece-Square Tables ChessProgramming.org

## 🚨 PROBLÈME CRITIQUE IDENTIFIÉ

### **Symptôme** : L'IA joue **f7f6** au lieu de **d7d5** en réponse à d2d4

**Test pour reproduire** :
```bash
swipl go.pl
# Choisir option 2 (IA vs Humain)
# Jouer d2d4
# IA répond f7f6 (coup faible) au lieu de d7d5 (coup central correct)
```

### **Diagnostic complet** :
1. ✅ **IA autonome** : Plus de coups fixes Caro-Kann
2. ✅ **Alpha-beta fonctionnel** : Performance ~1-17s profondeur 2
3. ✅ **Évaluation simplifiée** : Seulement Matériel + PSQT 
4. ❌ **PSQT mal configurées** : Ne favorisent PAS les cases centrales

## 🎯 PROBLÈME RACINE DIAGNOSTIQUÉ

**L'évaluation PSQT actuelle ne différencie PAS les coups d'ouverture centraux vs périphériques.**

**Calcul PSQT pour les noirs** :
- **d7d5** : Row 7→5 après miroir = PSQT[2][4] = +50
- **f7f6** : Row 7→6 après miroir = PSQT[2][6] = +50

**MÊME SCORE !** L'évaluation ne favorise pas le centre.

## 💡 SOLUTION RECOMMANDÉE

### **Option 1 : Modifier les PSQT (RECOMMANDÉ)**

**Objectif** : Faire que **d5/e5 >> f6/a6** pour les noirs en ouverture

```prolog
% Dans src/psqt_tables.pl, modifier pawn_psqt :
pawn_psqt([
    [  0,  0,  0,  0,  0,  0,  0,  0], % Row 8 (promotion)
    [  5, 10, 10,-20,-20, 10, 10,  5], % Row 7 (base noirs)
    [  5, -5,-10,  0,  0,-10, -5,  5], % Row 6
    [  0,  0,  0, 20, 20,  0,  0,  0], % Row 5 (avant-postes)
    [  5,  5, 10, 25, 25, 10,  5,  5], % Row 4 (centre fort)
    [ 10, 10, 20, 30, 30, 20, 10, 10], % Row 3
    [ 50, 50, 50, 50, 50, 50, 50, 50], % Row 2 (base blancs)
    [  0,  0,  0,  0,  0,  0,  0,  0]  % Row 1
]).
```

**MODIFIER Row 5** pour favoriser **colonnes centrales d/e** :
```prolog
[  0,  0,  0, 20, 20,  0,  0,  0], % AVANT
[ -10, -5,  0, 40, 40,  0, -5, -10] % APRÈS (centre >> flancs)
```

### **Option 2 : Ajouter bonus centre dans évaluation**

```prolog
% Dans src/ai.pl, ajouter à evaluate_pure_reference :
% Bonus pour coups vers cases centrales d4,d5,e4,e5
evaluate_center_bonus(GameState, CenterBonus) :-
    GameState = game_state(Board, _, _, _, _),
    findall(Bonus, (
        between(4, 5, Row), between(4, 5, Col),  % Cases d4,d5,e4,e5
        get_piece(Board, Row, Col, Piece),
        Piece \= ' ', Piece \= '.',
        (piece_belongs_to_player(Piece, white) -> Bonus = 25 ; Bonus = -25)
    ), Bonuses),
    sum_list(Bonuses, CenterBonus).
```

## 🧪 TESTS DE VALIDATION

### **Test 1 : Coup d'ouverture correct**
```bash
swipl go.pl
# Option 2, jouer d2d4
# IA doit répondre d7d5 ou e7e5 (PAS f7f6)
```

### **Test 2 : Performance maintenue**
```bash
# Temps calcul doit rester <10s profondeur 2
```

### **Test 3 : Évaluation cohérente**
```bash
# Position initiale ~0 points
# Après d2d4 vs d7d5 : avantage blanc léger (+20-50)
```

## 📋 ORDRE D'ACTIONS RECOMMANDÉ

### **Phase 1 : Diagnostic (30min)**
1. Reproduire le problème (IA joue f7f6)
2. Calculer manuellement PSQT pour d7d5 vs f7f6
3. Confirmer qu'elles ont la même valeur

### **Phase 2 : Correction (60min)**  
1. **OPTION A** : Modifier `pawn_psqt` Row 5 pour favoriser centre
2. **OPTION B** : Ajouter bonus centre dans `evaluate_pure_reference`
3. Tester avec `swipl go.pl`

### **Phase 3 : Validation (30min)**
1. Vérifier que IA choisit d7d5 ou e7e5
2. Tester quelques parties courtes
3. S'assurer performance <10s profondeur 2

## 🚨 POINTS CRITIQUES À RETENIR

### **✅ NE PAS TOUCHER** :
- Architecture 6 modules (stable)
- Algorithme négamax + alpha-beta (fonctionnel)
- `choose_ai_move/2` (autonome sans coups fixes)

### **❌ PROBLÈME À CORRIGER** :
- PSQT ne favorisent pas cases centrales d5/e5
- IA choisit coups périphériques f6/a6 en ouverture

### **🎯 OBJECTIF FINAL** :
- IA répond à d2d4 avec d7d5 ou e7e5 (coups centraux sensés)
- Performance maintenue (~5s profondeur 2)
- Évaluation cohérente et éducative

## 📁 FICHIERS CLÉS

```
src/ai.pl                 # IA négamax (problème évaluation)
src/psqt_tables.pl         # PSQT à modifier (Row 5 pions)
.claude/CLAUDE.md          # Documentation complète projet
docs/TASKS.md              # État détaillé Phase 3
```

## 🧠 CONTEXTE TECHNIQUE

**Système coordonnées Prolog** :
- Row 1 = rang 1 échecs (blancs)
- Row 8 = rang 8 échecs (noirs)  
- Col 1-8 = colonnes a-h
- `d7d5` = [7,4,5,4] (rang 7 col 4 vers rang 5 col 4)

**Miroir PSQT noirs** : `MirroredRow is 9 - Row`

**Commandes test** :
```bash
swipl go.pl                    # Lancer jeu
swipl -g "consult('src/ai'), halt."  # Test compilation
```

Bonne chance ! 🚀 Le problème est bien cerné et la solution est claire.
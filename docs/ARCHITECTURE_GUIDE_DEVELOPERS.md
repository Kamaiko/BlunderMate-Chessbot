# 🏗️ GUIDE ARCHITECTURE - Développeurs Prolog Chess Game

## 📋 **VUE D'ENSEMBLE SYSTÈME**

Ce jeu d'échecs Prolog implémente une architecture modulaire en 6 couches avec une IA négamax. Le système est conçu pour être éducatif, maintenable et extensible.

⚠️ **ÉTAT ACTUEL** : IA fonctionnelle avec lacunes identifiées (alpha-beta défaillant, MVV-LVA incomplet) - voir TASKS.md pour roadmap d'amélioration.

### **🎯 Objectif Pédagogique**
- Démonstration d'IA d'échecs en Prolog
- Implémentation négamax avec élagage alpha-beta
- Architecture modulaire propre et extensible
- Code éducatif niveau universitaire

## 🚨 **ANALYSE CRITIQUE MVV-LVA - BUG PARAMÈTRE COULEUR**

**Date**: 2025-09-06  
**Découverte**: Détection défense MVV-LVA **complètement non fonctionnelle**  
**Bug**: Paramètre couleur inversé dans `move_score_with_defense/4`  

### **🔍 PIPELINE MVV-LVA COMPLET**

```
┌──────────────────────────────────────────────────────────────────────┐
│                    CHAÎNE CRITIQUE MVV-LVA                          │
└──────────────────────────────────────────────────────────────────────┘

1. generate_moves_simple(GameState, Player, Moves)
   └─→ ai.pl:352 - Génère tous coups possibles

2. order_moves(GameState, Player, Moves, OrderedMoves)  
   └─→ ai.pl:240 - Trie coups par score MVV-LVA
   
3. map_move_scores(Board, Player, Moves, ScoredMoves)
   └─→ ai.pl:248 - Applique move_score/4 à chaque coup
   
4. move_score(Board, Player, Move, FinalScore)
   └─→ ai.pl:294 - Score final = Base + Promotions + Échecs
   
5. move_score_with_defense(Board, Player, Move, BaseScore) 🚨 BUG ICI
   └─→ ai.pl:267 - PROBLÈME CRITIQUE ligne 281
```

### **🐛 BUG CRITIQUE IDENTIFIÉ**

**Fichier**: `src/ai.pl:281`  
**Problème**: Paramètre couleur **inversé** dans détection défense

```prolog
% ACTUEL (INCORRECT) - Ligne 281
is_square_attacked(NewBoard, ToRow, ToCol, Opponent) ->

% CORRECTION REQUISE  
is_square_attacked(NewBoard, ToRow, ToCol, Player) ->
```

### **📊 ANALYSE IMPACT BUG**

**LOGIQUE ATTENDUE:**
- Après capture simulée, vérifier si **joueur actuel peut défendre** la case
- Si case défendue → Réduire score (capture risquée)

**LOGIQUE ACTUELLE (BUG):**
- Vérifier si **adversaire attaque** la case après capture
- Toujours false car adversaire ne peut pas attaquer sa propre case
- Détection défense **jamais active** → Scores MVV-LVA basiques seulement

**CONSÉQUENCE:**
- Tests passaient par accident (différences valeurs pièces)
- IA ne détecte jamais défenses réelles
- Blunders tactiques persistent (Dame vs pion défendu)

### **🔗 RELATIONS CRITIQUES INTER-MODULES MVV-LVA**

```
AI.PL (Tri coups)
├─→ move_score_with_defense/4
    ├─→ make_move_simulation/6 (board.pl - simulation plateau)
    └─→ is_square_attacked/4 🚨 (game.pl - détection attaque)
        └─→ opposite_player/2 🚨 (pieces.pl - conversion couleur)
            
EVALUATION.PL (Évaluation sécurité)  
├─→ evaluate_piece_safety/3
    └─→ is_piece_defended/4 ✅ (CORRIGÉ)
        └─→ is_square_attacked/4 (game.pl - même problème potentiel)

GAME.PL (Détection attaque)
├─→ is_square_attacked/4 - INTERFACE PUBLIQUE
    ├─→ opposite_player/2 (inversion couleur INTERNE)
    └─→ square_attacked_by_any_piece/4 (logique attaque)
```

### **⚠️ ZONES À RISQUE IDENTIFIÉES**

1. **USAGE PARAMETER Player vs Opponent** - ai.pl:281 🚨
   ```prolog
   % PATTERN RISQUÉ partout dans le code
   opposite_player(Player, Opponent),
   is_square_attacked(..., Opponent)  % ← Vérifier si cohérent
   ```

2. **DOUBLE INVERSION COULEUR** - game.pl:464 + ai.pl:280
   ```prolog
   % game.pl fait déjà: opposite_player(DefendingPlayer, AttackingPlayer)
   % ai.pl fait aussi: opposite_player(Player, Opponent)
   % = Double inversion = retour à la couleur originale!
   ```

3. **SEMANTIC CONFUSION** - DefendingPlayer vs AttackingPlayer
   ```prolog
   % is_square_attacked(Board, Row, Col, DefendingPlayer)
   % Nom suggère "qui défend" mais implemente "qui est attaqué"
   ```

### **🔍 AUDIT COMPLET USAGES is_square_attacked**

**ANALYSE**: 4 usages dans le code base - 3 CORRECTS, 1 BUG CRITIQUE

#### **✅ USAGE CORRECT #1** - game.pl:456 (Détection échec)
```prolog
% CONTEXTE: is_in_check/2 - vérifie si Player en échec
find_king_position(Board, Player, KingRow, KingCol),
is_square_attacked(Board, KingRow, KingCol, Player).

% LOGIQUE: "Est-ce que Player est attaqué?" = CORRECT
% is_square_attacked fait: opposite_player(Player, AttackingPlayer)
% = Cherche si adversaire attaque le roi de Player
```

#### **✅ USAGE CORRECT #2** - evaluation.pl:291 (Pièces exposées)  
```prolog
% CONTEXTE: evaluate_piece_safety/3 - pièces hanging
opposite_player(Player, Opponent),
is_square_attacked(Board, Row, Col, Opponent), % Piece attaquée

% LOGIQUE: "Est-ce que pièce de Opponent est attaquée par Player?" = CORRECT  
% Paramètre Opponent = pièce appartient à Opponent
% is_square_attacked fait: opposite_player(Opponent, Player)
% = Cherche si Player attaque la pièce de Opponent
```

#### **✅ USAGE CORRECT #3** - evaluation.pl:311 (Détection défense)
```prolog  
% CONTEXTE: is_piece_defended/4 - pièce défendue
is_square_attacked(Board, Row, Col, DefendingPlayer).

% LOGIQUE: "Est-ce que DefendingPlayer peut attaquer/défendre cette case?" = CORRECT
% DefendingPlayer = celui qui défend
% is_square_attacked fait: opposite_player(DefendingPlayer, AttackingPlayer)  
% Mais nom trompeur - cherche en fait si DefendingPlayer attaque la case
```

#### **🚨 USAGE INCORRECT #4** - ai.pl:281 (MVV-LVA défense)
```prolog
% CONTEXTE: move_score_with_defense/4 - capture défendue?
opposite_player(Player, Opponent),
is_square_attacked(NewBoard, ToRow, ToCol, Opponent) ->

% LOGIQUE VOULUE: "Après capture, Player peut-il défendre la case?"
% LOGIQUE ACTUELLE: "Après capture, Opponent attaque-t-il la case?"
% PROBLÈME: Opponent ne peut pas attaquer sa propre case après capture!
% CORRECTION: Utiliser Player au lieu de Opponent
```

### **📋 RECOMMANDATIONS CORRECTIONS**

1. **IMMÉDIAT**: Fix ai.pl:281 `Opponent` → `Player` 
2. **CLARIFICATION**: Renommer is_square_attacked → is_square_attacked_by_opponent
3. **TESTS**: Valider tous usages après correction
4. **DOCUMENTATION**: Clarifier sémantique de chaque usage

## 🔧 **ARCHITECTURE 6 MODULES**

```
┌─────────────────────────────────────────────────┐
│                  INTERFACE.PL                   │
│           Interface Utilisateur Française       │
│  • Menu principal et navigation                 │
│  • Boucle de jeu unifiée humain/IA              │
│  • Gestion commandes et validation entrée       │
└──────────────────────┬──────────────────────────┘
                       │
┌──────────────────────▼──────────────────────────┐
│                   GAME.PL                       │
│              Logique Métier Échecs              │
│  • États de jeu et transitions                  │
│  • Validation coups et règles                   │
│  • Détection échec/mat/pat                      │
│  • Gestion captures et promotion                │
└───────────┬─────────────────┬───────────────────┘
            │                 │
┌───────────▼─────────┐  ┌────▼────────────────── ────┐
│     PIECES.PL       │  │         AI.PL              │
│   Règles Pièces     │  │  Intelligence Artificielle │
│ • Mouvements        │  │ • Négamax + Alpha-Beta     │
│ • Validation        │  │ • Génération coups         │
│ • Types             │  │ • Tri MVV-LVA              │
└───────────┬─────────┘  └────┬───────────────────────┘
            │                 │
┌───────────▼─────────────────▼───────────────────────┐
│                  BOARD.PL                           │
│              Représentation Plateau                 │
│  • Structure 8x8 et manipulation                    │
│  • Conversions coordonnées                          │
│  • Affichage ASCII et utilitaires                   │
│  • Gestion état plateau                             │
└─────────────────────┬───────────────────────────────┘
                      │
┌─────────────────────▼───────────────────────────────┐
│                 EVALUATION.PL                       │
│        Évaluation Position + PSQT Intégrées         │
│  • Matériel + PSQT ChessProgramming.org             │
│  • Sécurité pièces (anti-blunders)                  │
│  • Mobilité et développement (disponibles)          │
│  • Interface évaluation unifiée                     │
└─────────────────────────────────────────────────────┘
```

## 📂 **STRUCTURE FICHIERS ET RESPONSABILITÉS**

### **1. 🎮 INTERFACE.PL - Couche Présentation**
```prolog
% Responsabilités principales :
• Menu principal moderne avec design ASCII
• Gestion modes de jeu (Humain vs Humain, IA vs Humain)
• Boucle de jeu unifiée avec états unifiés
• Traitement commandes utilisateur
• Messages français centralisés

% Points d'entrée :
start/0                    % Point d'entrée principal
main_menu/0               % Menu principal
unified_game_loop/1       % Boucle de jeu principale

% États utilisés :
unified_game_state(Board, Player, MoveCount, Status, Captured, PlayerTypes)
```

### **2. 🎯 GAME.PL - Couche Métier**
```prolog
% Responsabilités principales :
• Validation règles d'échecs complètes
• Gestion états de jeu et transitions  
• Détection conditions spéciales (échec/mat/pat)
• Exécution coups avec validation sécurisée
• Gestion captures et promotions automatiques

% Fonctions critiques :
make_move/6              % Exécution sécurisée des coups
valid_move/6             % Validation règles complètes
is_checkmate/2           % Détection mat
is_in_check/2            % Détection échec
execute_move/6           % Application coups bas niveau

% Structure état :
game_state(Board, CurrentPlayer, MoveCount, GameStatus, CapturedPieces)
```

### **3. ♟️ PIECES.PL - Couche Règles Pièces**
```prolog
% Responsabilités principales :
• Règles mouvement spécifiques par pièce
• Validation trajectoires et blocages
• Identification types et couleurs
• Utilitaires manipulation pièces

% Patterns de mouvement :
can_piece_move/6         % Mouvement spécifique par type
check_path_clear/7       % Validation trajectoire libre
piece_belongs_to_player/2 % Identification couleur
is_empty_square/1        % Détection cases vides

% Types pièces supportés :
P/p (Pion), R/r (Tour), N/n (Cavalier), B/b (Fou), Q/q (Dame), K/k (Roi)
```

### **4. 🧠 AI.PL - Couche Intelligence Artificielle**
```prolog
% Responsabilités principales :
• Algorithme négamax avec élagage alpha-beta
• Évaluation position complète (matériel + PSQT)
• Génération et tri des coups (MVV-LVA)
• Interface IA pour coups d'ouverture fixes

% Architecture IA :
choose_ai_move/2         % Interface principale IA
negamax_ab/5             % Négamax (alpha-beta cassé - voir TASKS.md)
evaluate_position/3       % Évaluation position centralisée
generate_moves_simple/3   % Génération coups légaux
order_moves/4            % Tri MVV-LVA basique (détection défense manquante)

% Paramètres configurable :
Profondeur : 2 niveaux (configurable)
Limite coups : 25 par position (optimisé performance)
```

### **5. 📋 BOARD.PL - Couche Infrastructure**
```prolog
% Responsabilités principales :
• Représentation plateau 8x8 interne
• Conversions coordonnées multiples
• Affichage ASCII avec légendes
• Utilitaires manipulation plateau

% Systèmes coordonnées :
Interne : [1-8, 1-8] (rangées, colonnes)
Algébrique : "e2e4" (format utilisateur)  
Affichage : [a-h, 1-8] (notation standard)

% Fonctions utilitaires :
create_empty_board/1     % Initialisation plateau
display_board/1          % Affichage ASCII
get_piece/4             % Accès sécurisé cases
place_piece_optimized/5  % Modification optimisée
```

### **6. 📊 EVALUATION.PL - Évaluation Centralisée + PSQT**
```prolog
% Module évaluation complète avec PSQT intégrées
• Tables positionnelles ChessProgramming.org intégrées
• Évaluation matérielle + positionnelle combinée
• Sécurité pièces (anti-blunders) - temporairement désactivée
• Mobilité et développement (disponibles mais non intégrés)
• Interface évaluation unifiée pour IA
```

## ⚠️ **PROBLÈMES CONNUS & ROADMAP**

### **🚨 Défauts Critiques Identifiés**
- **Alpha-Beta Cassé** : Variables calculées mais jamais utilisées dans `negamax_ab/5` (lignes 169-170)
- **MVV-LVA Incomplet** : Détection défense manquante cause blunders tactiques
- **Modules Non-Intégrés** : `evaluate_piece_development/3`, `evaluate_move_count/3` existent mais inutilisés
- **Captures Tronquées** : Limite `ai_move_limit(25)` coupe séquences tactiques

### **📋 Roadmap Prioritaire**
1. **Alpha-Beta Fix** : Passer `-Beta, -Alpha` à l'appel récursif (45-60min)
2. **MVV-LVA Défense** : Ajouter `is_square_attacked` après simulation (60-90min)  
3. **Mobilité Integration** : Intégrer fonctions mobilité existantes (30-45min)
4. **Quiescence Search** : Extension recherche tactique (90-120min)

*Voir `docs/TASKS.md` pour détails complets et `docs/BUG_REPORT_ENTERPRISE.md` pour analyse technique.*

## 🔄 **FLOW DE DONNÉES PRINCIPAL**

### **1. Démarrage Application**
```
go.pl → interface.pl:start/0 → main_menu/0
```

### **2. Partie Humain vs Humain**
```
start_human_game/0 → init_unified_game_state(human, human) 
→ unified_game_loop/1 → handle_player_turn(human) 
→ process_game_input/3 → make_move/6 → display_game_state/1
```

### **3. Partie IA vs Humain**  
```
start_ai_game/0 → init_unified_game_state(human, ai)
→ unified_game_loop/1 → handle_player_turn(ai)
→ choose_ai_move/2 → negamax_ab/5 → evaluate_position/3
```

### **4. Validation Coup**
```
attempt_move/6 → valid_move/6 → can_piece_move/6 → check_path_clear/7
→ validate_king_safety_after_move/6 → execute_move/6
```

## 🏛️ **PATTERNS ARCHITECTURAUX**

### **1. Séparation Couches Clara**
- **Présentation** : Interface utilisateur uniquement
- **Métier** : Règles d'échecs pures
- **Infrastructure** : Plateau et utilitaires
- **IA** : Algorithmes et évaluation

### **2. État Immutable avec Transitions**
```prolog
% Pattern utilisé partout :
make_move(OldGameState, Move, NewGameState) :-
    % Validation
    valid_move(OldGameState, Move),
    % Transformation pure
    apply_move_transformation(OldGameState, Move, NewGameState).
```

### **3. Configuration Par Données**
```prolog
% Tables PSQT externes
% Valeurs pièces centralisées
% Messages français externalisés
```

### **4. Interface Unifiée IA/Humain**
```prolog
handle_player_turn(UnifiedGameState, Player, human, NewState).
handle_player_turn(UnifiedGameState, Player, ai, NewState).
```

## ⚠️ **PROBLÈMES ARCHITECTURAUX IDENTIFIÉS**

### **🔴 Critiques**
1. **Code dupliqué** : 3 systèmes valeurs pièces
2. **Magic numbers** : Dimensions, limites hardcodées  
3. **États multiples** : `game_state` vs `unified_game_state`
4. **Responsabilités mixtes** : Fonctions trop complexes

### **🟡 Modérés**  
1. **Conventions nommage** : Patterns multiples
2. **Gestion erreurs** : Approches inconsistantes
3. **Performance** : Boucles imbriquées inefficaces

## 🛠️ **GUIDE DÉVELOPPEMENT**

### **Ajouter Nouvelle Pièce**
1. **pieces.pl** : Ajouter règles mouvement dans `can_piece_move/6`
2. **psqt_tables.pl** : Ajouter table positionnelle
3. **ai.pl** : Mettre à jour `standard_piece_value/2`
4. **board.pl** : Ajouter symbole affichage

### **Modifier Algorithme IA**
1. **Profondeur** : Changer paramètre dans `negamax_ab/5`
2. **Évaluation** : Modifier `evaluate_position/3`
3. **Tri coups** : Ajuster `order_moves/4`
4. **Tests** : Valider avec suite tests AI

### **Ajouter Mode de Jeu**
1. **interface.pl** : Ajouter option menu
2. **Créer handler** : `handle_player_turn/4` pour nouveau type
3. **États** : Étendre `player_types` si nécessaire

### **Convention Code**
```prolog
% Style prédicats : action_objet_modificateur
% Variables : PascalCase
% Commentaires : Français sans accents
% Validation : Toujours ground/1 pour paramètres critiques
% Erreurs : Documentation explicite des cas d'échec
```

## 📈 **MÉTRIQUES QUALITÉ ACTUELLES**

### **Complexité**
- **Modules** : 6 modules intégrés
- **Lignes code** : ~2000 lignes Prolog
- **Fonctions >20 lignes** : 8 (à refactoriser)
- **Profondeur max** : 4 niveaux imbrication

### **Couverture Tests**
- **Tests unitaires** : 35 tests basiques
- **Tests IA** : Incomplets (voir AI_TEST_SUITE_PROPOSAL.md)
- **Couverture estimée** : ~60% fonctionnalités

## 🎯 **ROADMAP AMÉLIORATIONS**

### **Phase 1: Stabilisation (Semaine 1)**
- Fixer bug critique `ai.pl:754` 
- Consolider valeurs pièces
- Standardiser conventions nommage

### **Phase 2: Qualité (Semaine 2-3)**
- Intégrer constantes magic numbers dans modules appropriés
- Refactoriser fonctions complexes
- Améliorer gestion erreurs

### **Phase 3: Optimisation (Mois 1)**
- Optimiser performance IA
- Ajouter suite tests complète
- Documentation développeur complète

## 💡 **CONSEILS NOUVEAUX DÉVELOPPEURS**

### **Commencer Par**
1. **Lire ce guide** complètement
2. **Explorer `pieces.pl`** (plus simple)  
3. **Comprendre flow** dans `game.pl`
4. **Analyser IA** dans `ai.pl` (plus complexe)

### **Debugging**
```prolog
% Activer trace Prolog :
?- trace.
?- valid_move(Board, white, 2, 5, 4, 5).

% Tester coups isolés :
?- consult('src/pieces'), can_piece_move(Board, 2, 5, 4, 5, 'P').
```

### **Éviter**
- Modifier plusieurs modules simultanément
- Ignorer les tests existants
- Hardcoder nouvelles valeurs
- Mélanger conventions nommage

Ce guide fournit une base solide pour comprendre et étendre l'architecture du jeu d'échecs Prolog. La priorité est la clarté et la maintenabilité du code éducatif.

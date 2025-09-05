# 🏗️ GUIDE ARCHITECTURE - Développeurs Prolog Chess Game

## 📋 **VUE D'ENSEMBLE SYSTÈME**

Ce jeu d'échecs Prolog implémente une architecture modulaire en 5 couches avec une IA négamax complète. Le système est conçu pour être éducatif, maintenable et extensible.

### **🎯 Objectif Pédagogique**
- Démonstration d'IA d'échecs en Prolog
- Implémentation négamax avec élagage alpha-beta
- Architecture modulaire propre et extensible
- Code éducatif niveau universitaire

## 🔧 **ARCHITECTURE 5 MODULES**

```
┌─────────────────────────────────────────────────┐
│                  INTERFACE.PL                   │
│           Interface Utilisateur Française       │
│  • Menu principal et navigation                 │
│  • Boucle de jeu unifiée humain/IA             │
│  • Gestion commandes et validation entrée      │
└────────────────┬────────────────────────────────┘
                 │
┌────────────────▼────────────────────────────────┐
│                   GAME.PL                       │
│              Logique Métier Échecs              │
│  • États de jeu et transitions                 │
│  • Validation coups et règles                  │
│  • Détection échec/mat/pat                     │
│  • Gestion captures et promotion               │
└─────────┬──────────────┬─────────────────────────┘
          │              │
┌─────────▼──────┐  ┌───▼──────────────────────────┐
│   PIECES.PL    │  │           AI.PL               │
│ Règles Pièces  │  │    Intelligence Artificielle │
│ • Mouvements   │  │  • Négamax + Alpha-Beta      │
│ • Validation   │  │  • Évaluation PSQT           │
│ • Types        │  │  • Génération coups          │
└─────────┬──────┘  │  • Tri MVV-LVA               │
          │         └───┬──────────────────────────┘
          │             │
┌─────────▼─────────────▼─────────────────────────┐
│                  BOARD.PL                       │
│              Représentation Plateau             │
│  • Structure 8x8 et manipulation               │
│  • Conversions coordonnées                     │
│  • Affichage ASCII et utilitaires             │
│  • Gestion état plateau                       │
└─────────────────────────────────────────────────┘

       ┌─────────────────────────────────┐
       │         PSQT_TABLES.PL          │
       │     Tables Évaluation PSQT      │
       │  • Valeurs positionnelles       │
       │  • Standards ChessProgramming   │
       └─────────────────────────────────┘
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
minimax_ab/5             % Négamax avec alpha-beta
evaluate_pure_reference/3 % Évaluation position complète
generate_moves_simple/3   % Génération coups légaux
order_moves/4            % Tri MVV-LVA

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

### **6. 📊 PSQT_TABLES.PL - Données Évaluation**
```prolog
% Tables positionnelles ChessProgramming.org
• Valeurs par type de pièce et position
• Bonus développement et contrôle centre  
• Compatible avec négamax (perspective joueur)
• Standards académiques reconnus
```

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
→ choose_ai_move/2 → minimax_ab/5 → evaluate_pure_reference/3
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
1. **Profondeur** : Changer paramètre dans `minimax_ab/5`
2. **Évaluation** : Modifier `evaluate_pure_reference/3`
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
- **Modules** : 5 + 1 données
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
- Créer `constants.pl` pour magic numbers
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
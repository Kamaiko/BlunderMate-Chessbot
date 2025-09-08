<div align="center">

# 🤖 Prolog Chess AI

**Joueur intelligent d'échecs avec techniques de recherche heuristique**

*Projet IFT-2003 · Intelligence Artificielle · Université Laval*

[![SWI-Prolog](https://img.shields.io/badge/SWI--Prolog-9.x+-blue?style=flat-square)](https://www.swi-prolog.org/)
[![Platforms](https://img.shields.io/badge/Platform-Windows%20%7C%20Linux%20%7C%20macOS-lightgrey?style=flat-square)]()
[![AI Algorithm](https://img.shields.io/badge/AI-Negamax%20%7C%20Alpha--Beta-green?style=flat-square)]()

</div>

---

## 🚀 Installation & Lancement

```bash
# Démarrer le jeu d'échecs
swipl go.pl

# Le menu principal propose :
# 1. Humain vs Humain
# 2. Humain vs IA  
# 3. Tests du système
```

> **Note** : Interface complète avec détection automatique échec/mat/pat

## 🏗️ Architecture

<table>
<tr><td><strong>Module</strong></td><td><strong>Responsabilité</strong></td></tr>
<tr><td><code>pieces.pl</code></td><td>♟️ Règles de mouvement des pièces</td></tr>
<tr><td><code>board.pl</code></td><td>🏁 Représentation plateau 8×8, affichage ASCII</td></tr>
<tr><td><code>game.pl</code></td><td>⚖️ Gestion états, validation coups, échec/mat/pat</td></tr>
<tr><td><code>interface.pl</code></td><td>🖥️ Interface utilisateur</td></tr>
<tr><td><code>ai.pl</code></td><td>🧠 Algorithme négamax avec élagage alpha-beta</td></tr>
<tr><td><code>evaluation.pl</code></td><td>🎯 Heuristiques de recherche (matériel + PSQT + sécurité)</td></tr>
</table>

## ✨ Fonctionnalités

### 🏆 Jeu d'Échecs Standard
- ✅ **Toutes les pièces** et règles officielles FIDE
- ✅ **Promotion automatique** des pions (→ dame)
- ✅ **Détection complète** échec, mat et pat
- ✅ **Interface** professionnelle

### 🤖 Intelligence Artificielle

| Composante | Description | Performance |
|------------|-------------|-------------|
| **Algorithme** | Négamax¹ + élagage Alpha-Beta² | Profondeur 2 |
| **Heuristiques** | Matériel + PSQT³ + sécurité pièces | 6 fonctions d'évaluation |
| **Tri des coups** | MVV-LVA⁴ (Most Valuable Victim) | Élagage ~90% |
| **Temps de réponse** | Temps réel | < 1 seconde/coup |

**Références techniques :**
- ¹ Négamax : [Chess Programming Wiki](https://www.chessprogramming.org/Negamax) • [Lichess GitHub](https://github.com/lichess-org/lila/tree/master/modules/analyse/src/main/AnalyseApi.scala)
- ² Alpha-Beta : [Chess Programming Wiki](https://www.chessprogramming.org/Alpha-Beta)
- ³ PSQT : [ChessProgramming.org](https://www.chessprogramming.org/Piece-Square_Tables) 
- ⁴ MVV-LVA : [ChessProgramming.org](https://www.chessprogramming.org/MVV-LVA)
- 🤖 **Développé avec** [Claude Code](https://claude.ai/code)

## 🎮 Usage

### Format d'entrée
```
Format coups : e2e4 (notation algébrique)
Coordonnées  : colonnes a-h, rangées 1-8  
Commandes    : aide, menu, quitter
```

### Modes de jeu
- 👤 **Humain vs Humain** · Partie locale à deux joueurs
- 🤖 **Humain vs IA** · Affrontez l'intelligence artificielle

## 🧪 Tests

```bash
# Suite complète (8 catégories)
swipl -t run_tests -s tests/tests.pl

# Tests spécifiques par catégories disponibles
```

## 📋 Prérequis Système

| Composant | Version | Notes |
|-----------|---------|-------|
| **SWI-Prolog** | 9.x+ | Moteur Prolog principal |
| **OS** | Windows/Linux/macOS | Multiplateforme |
| **Mémoire** | 512 MB+ | Algorithme négamax |

## 📚 Documentation Technique

| Document | Description |
|----------|-------------|
| 📐 [**Architecture Guide**](docs/ARCHITECTURE_GUIDE_DEVELOPERS.md) | Architecture système complète |
| 📋 [**Tasks & Status**](docs/TASKS.md) | État d'avancement du projet |
| 📄 [**Product Requirements**](docs/PRD.md) | Spécifications détaillées |

---

<div align="center">

**🎓 Projet Universitaire** · **🏛️ Université Laval** · **🤖 Intelligence Artificielle**

*Négamax • Alpha-Beta • PSQT • MVV-LVA*

</div>
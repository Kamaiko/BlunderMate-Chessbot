# ♟️ Prolog Chess Game

**Jeu d'échecs complet avec Intelligence Artificielle** - Projet universitaire IFT-2003

```bash
swipl go.pl
```

## 🏗️ Architecture

**6 modules principaux** :
- **pieces.pl** : Règles de mouvement des pièces d'échecs
- **board.pl** : Représentation plateau 8x8, coordonnées, affichage ASCII
- **game.pl** : Gestion états de jeu, validation coups, détection échec/mat
- **interface.pl** : Interface utilisateur
- **ai.pl** : Algorithme négamax avec élagage alpha-beta
- **evaluation.pl** : Évaluation position (matériel + PSQT + sécurité pièces)

## ⚡ Installation & Tests

```bash
# Lancer le jeu (inclut option tests dans le menu)
swipl go.pl

# Tests directs en ligne de commande
swipl -t run_tests -s tests/tests.pl
```

## 🎮 Fonctionnalités

### Jeu d'Échecs Complet
✅ Toutes les pièces et règles standard  
✅ Promotion automatique des pions en dame  
✅ Détection échec, mat et pat  
✅ Interface utilisateur complète  
✅ Validation robuste des coups  

### Intelligence Artificielle (Phase 3)
🧠 **Algorithme négamax** avec élagage alpha-beta  
📊 **Évaluation PSQT** (Piece-Square Tables standards)  
⚡ **Performance** : 1-4 secondes par coup (profondeur 2)  
🎯 **Tri MVV-LVA** : Most Valuable Victim - Least Valuable Attacker

## 🎯 Usage

```
Format des coups    : e2e4 (notation algébrique)
Coordonnées        : colonnes a-h, rangées 1-8
Commandes          : aide, menu, quitter, sortir
```

### Modes de Jeu
- **Humain vs Humain** : Partie locale à deux joueurs
- **Humain vs IA** : Affrontez l'intelligence artificielle

## 💻 Compatibilité

| Requirement | Version |
|-------------|---------|
| **SWI-Prolog** | 9.x+ |
| **OS** | Windows, Linux, macOS |

## 📚 Documentation

| Document | Description |
|----------|-------------|
| [🏗️ Architecture Guide](docs/ARCHITECTURE_GUIDE_DEVELOPERS.md) | Architecture système, flow de données, roadmap |
| [📋 État d'Avancement](docs/TASKS.md) | Tâches prioritaires et bugs actifs |
| [🐛 Bug Report](docs/BUG_REPORT_ENTERPRISE.md) | Analyses détaillées des problèmes |

---
<div align="center">

**🎓 Projet Universitaire IFT-2003** • **🤖 Intelligence Artificielle en Prolog**

*Phase 3 Complète - IA Négamax + Alpha-Beta Fonctionnelle*

</div>
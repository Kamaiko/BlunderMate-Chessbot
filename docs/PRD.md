# 🎯 Document d'Exigences Produit (PRD)
## IA d'Échecs Prolog - Projet Universitaire IFT-2003

> **Deadline TP1** : 📅 20 octobre 2025

---

## 🎮 Aperçu Produit

| Aspect | Description |
|--------|-------------|
| **🎯 Produit** | IA d'échecs éducative implémentant minimax/alpha-beta en Prolog |
| **📚 Objectif** | Évaluation académique (10% note finale) et démonstration d'apprentissage IA |
| **💡 Valeur** | Environnement d'échecs interactif illustrant la prise de décision algorithmique |

## 🎓 Objectifs Académiques

- ✅ Satisfaire exigences cours IFT-2003 intelligence artificielle
- 🧠 Démontrer application pratique algorithmes de théorie des jeux
- 🎮 Créer interface éducative avec système d'aide intégré

## 👥 Utilisateurs Cibles

| Utilisateur | Rôle | Besoins |
|-------------|------|---------|
| **👨‍🏫 Professeur** | Évaluateur académique | Implémentation claire des algorithmes |
| **👨‍💻 Étudiant développeur** | Utilisateur principal | Environnement stable avec tests complets |
| **👥 Pairs évaluateurs** | Observateurs démonstration | Interface claire et comportement IA compréhensible |

## ⚙️ Exigences Fonctionnelles

### 🎯 Obligatoires (P0) - TP1
- **EF-002** : Algorithme minimax avec profondeur fixe niveau 2 🚧
- **EF-003** : Élagage alpha-beta pour optimisation performance 🚧
- **EF-004** : Évaluation heuristique position (matériel, mobilité, sécurité roi) 🚧
- **EF-006** : Mode Humain vs IA avec temps de réponse raisonnable 🚧

### ✅ Importantes (P1) - Complétées
- **EF-007** : Promotion pions automatique vers dame ✅
- **EF-009** : Affichage raisonnement IA et scores évaluation

### 🔄 Optionnelles (P2) - Extensions Futures
- **En Passant** : Capture spéciale pion adjacente
- **Roque** : Validation roi/tour non bougés
- **GUI** : Interface graphique
- **Analyse** : Évaluation temps réel
- **Niveaux difficulté** : Profondeur recherche configurable

## 🎮 Expérience Utilisateur

| Étape | Action | Résultat attendu |
|-------|--------|------------------|
| **🚀 Lancement** | `swipl go.pl` | Menu français principal |
| **♟️ Gameplay** | Notation algébrique "e2e4" | IA répond <5sec |
| **🎓 Éducatif** | Affichage évaluation position | Raisonnement IA visible |
| **🖥️ Interface** | ASCII propre | Aide contextuelle |

## 🎯 Vision Étudiante

> Interface d'échecs éducative permettant de comprendre concrètement le fonctionnement des algorithmes de jeux à travers une expérience interactive en français, avec visualisation du processus de décision de l'IA.

## ✅ Critères de Succès

| Critère | Objectif | Status |
|---------|----------|--------|
| **🎓 Académique** | Note ≥85% sur implémentation algorithmes | 🎯 |
| **⚡ Technique** | Minimax+alpha-beta fonctionnels, temps réponse raisonnable | 🚧 |
| **🧪 Qualité** | Tests passent (35/35), code modulaire, zéro crash | ✅ |

## 🔧 Contraintes Techniques

- **🖥️ Plateforme** : SWI-Prolog uniquement, interface console ASCII
- **⚡ Performance** : Équilibre profondeur recherche vs temps réponse raisonnable  
- **🏗️ Architecture** : 5 modules (pieces, board, game, interface, ai), tests automatisés

## 📈 Phases Développement

- **Phase 1** : Architecture base et règles ✅ **TERMINÉE**
- **Phase 2** : Règles avancées et promotion ✅ **TERMINÉE**  
- **Phase 3** : IA minimax/alpha-beta 🚧 **ACTUELLE** *(Deadline: 20 oct 2025)*

## 🎯 Cas d'Usage Principaux

### 🚀 Lancement & Navigation
- **CU-001** : Lancement rapide → `swipl go.pl` → Menu principal <3sec
- **CU-002** : Mouvements joueur → "e2e4" validé et exécuté avec retour visuel

### 🤖 Intelligence Artificielle  
- **CU-003** : Réponse IA → Coup légal généré avec raisonnement visible
- **CU-005** : IA recherche → Profondeur fixe 2 coups pour simplicité

### ⚔️ Mécaniques de Jeu
- **CU-004** : Détection fin → Mat/Pat détecté avec annonce appropriée  
- **CU-006** : Promotion → Conversion automatique pion vers dame ✅

### 🧪 Validation
- **CU-007** : Tests → Suite complète `swipl tests/tests.pl` 35/35 passent ✅

---

## 📚 Ressources & Documentation

**📋 Roadmap détaillé** : [TASKS.md](TASKS.md)  
**🛠️ Guide développeur** : [CLAUDE.md](../.claude/CLAUDE.md)  
**👤 Guide utilisateur** : [README.md](../README.md)
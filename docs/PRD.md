# Document d'Exigences Produit (PRD)
## IA d'Échecs Prolog - Projet Universitaire IFT-2003

### Aperçu Produit

**Produit** : IA d'échecs éducative implémentant minimax/alpha-beta en Prolog  
**Objectif** : Évaluation académique (10% note finale) et démonstration d'apprentissage IA  
**Valeur** : Environnement d'échecs interactif illustrant la prise de décision algorithmique

### Objectifs Académiques

- Satisfaire exigences cours IFT-2003 intelligence artificielle
- Démontrer application pratique algorithmes de théorie des jeux
- Créer interface française éducative avec système d'aide intégré

### Utilisateurs Cibles

**Instructeur** : Évaluateur académique - Besoin d'implémentation claire des algorithmes  
**Étudiant développeur** : Utilisateur principal - Environnement stable avec tests complets  
**Pairs évaluateurs** : Observateurs démonstration - Interface claire et comportement IA compréhensible

### Exigences Fonctionnelles

#### Obligatoires (P0) - TP1
- **EF-001** : Validation complète règles d'échecs (échec/mat/pat) ✅
- **EF-002** : Algorithme minimax avec profondeur configurable
- **EF-003** : Élagage alpha-beta pour optimisation performance  
- **EF-004** : Évaluation heuristique position (matériel, mobilité, sécurité roi)
- **EF-005** : Interface console française avec aide intégrée ✅
- **EF-006** : Mode Humain vs IA avec limite 5sec/coup

#### Importantes (P1)
- **EF-007** : Promotion pions avec sélection pièce 🚧
- **EF-008** : Niveaux difficulté par profondeur recherche
- **EF-009** : Affichage raisonnement IA et scores évaluation

### Expérience Utilisateur

**Lancement** : `swipl go.pl` → Menu français principal  
**Gameplay** : Notation algébrique "e2e4" → IA répond <5sec  
**Éducatif** : Affichage évaluation position et raisonnement IA  
**Interface** : ASCII propre, français complet, aide contextuelle

### Vision Étudiante

Interface d'échecs éducative permettant de comprendre concrètement le fonctionnement des algorithmes de jeux à travers une expérience interactive en français, avec visualisation du processus de décision de l'IA.

### Critères de Succès

**Académique** : Note ≥85% sur implémentation algorithmes et démonstration  
**Technique** : Réponse IA <5sec, minimax+alpha-beta fonctionnels  
**Qualité** : Tests passent (28/28), code modulaire, zéro crash gameplay

### Contraintes Techniques

**Plateforme** : SWI-Prolog uniquement, interface console ASCII  
**Performance** : Équilibre profondeur recherche vs temps réponse <5sec  
**Architecture** : 5 modules (pieces, board, game, interface, ai), tests automatisés

### Phases Développement

**Phase 1** : Architecture base et règles (✅ Terminée)  
**Phase 2** : Promotion pions (🚧 Actuelle)  
**Phase 3** : IA minimax/alpha-beta (🎯 Priorité TP1)  
**Phase 4** : Optimisation et démonstration

### Cas d'Usage Principaux

**CU-001** : Lancement rapide → `swipl go.pl` → Menu français <3sec  
**CU-002** : Mouvements joueur → "e2e4" validé et exécuté avec retour visuel  
**CU-003** : Réponse IA → Coup légal généré <5sec avec raisonnement visible  
**CU-004** : Détection fin → Mat/Pat détecté avec annonce appropriée  
**CU-005** : Niveaux difficulté → Profondeur recherche configurable 1-4 coups  
**CU-006** : Promotion → Choix pièce (Dame/Tour/Fou/Cavalier) automatique  
**CU-007** : Tests → Suite complète `swipl tests/tests.pl` 28/28 passent

---

## Liens Documentation

**Roadmap détaillé** : [TASKS.md](TASKS.md) • **Guide développeur** : [../.claude/CLAUDE.md](../.claude/CLAUDE.md) • **Utilisateur** : [../README.md](../README.md)
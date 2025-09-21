# Document d'Exigences Produit (PRD)
## IA d'Échecs Prolog - Patrick Patenaude

> **Date projet** : 20 septembre 2025

---

## Aperçu Produit

| Aspect | Description |
|--------|-------------|
| **Produit** | IA d'échecs éducative implémentant minimax/alpha-beta en Prolog |
| **Objectif** | Démonstration de techniques d'intelligence artificielle appliquées aux jeux |
| **Valeur** | Environnement d'échecs interactif illustrant la prise de décision algorithmique |

## Objectifs Techniques

- Implémentation complète algorithmes d'intelligence artificielle
- Démontrer application pratique algorithmes de théorie des jeux
- Créer interface éducative avec système d'aide intégré

## Utilisateurs Cibles

| Utilisateur | Rôle | Besoins |
|-------------|------|---------|
| **Développeur** | Créateur principal | Implémentation claire des algorithmes |
| **Utilisateurs** | Joueurs | Environnement stable avec tests complets |
| **Observateurs** | Analystes | Interface claire et comportement IA compréhensible |

## Exigences Fonctionnelles

### Obligatoires (P0)
- **EF-002** : Algorithme négamax avec profondeur fixe niveau 2
- **EF-003** : Élagage alpha-beta pour optimisation performance
- **EF-004** : Évaluation heuristique position (matériel, mobilité, sécurité roi)
- **EF-006** : Mode Humain vs IA avec temps de réponse raisonnable

### Importantes (P1)
- **EF-007** : Promotion pions automatique vers dame
- **EF-009** : Affichage raisonnement IA et scores évaluation


## Expérience Utilisateur

| Étape | Action | Résultat attendu |
|-------|--------|------------------|
| **Lancement** | `swipl go.pl` | Menu français principal |
| **Gameplay** | Notation algébrique "e2e4" | IA répond <5sec |
| **Éducatif** | Affichage évaluation position | Raisonnement IA visible |
| **Interface** | ASCII propre | Aide contextuelle |

## Vision Produit

> Interface d'échecs éducative permettant de comprendre concrètement le fonctionnement des algorithmes de jeux à travers une expérience interactive en français, avec visualisation du processus de décision de l'IA.

## Critères de Succès

| Critère | Objectif |
|---------|----------|
| **Technique** | Implémentation complète et fonctionnelle des algorithmes |
| **Performance** | Négamax+alpha-beta fonctionnels, temps réponse raisonnable |
| **Qualité** | Tests passent (42/42), code modulaire, zéro crash |

## Contraintes Techniques

- **Plateforme** : SWI-Prolog uniquement, interface console ASCII
- **Performance** : Équilibre profondeur recherche vs temps réponse raisonnable
- **Architecture** : 7 modules (pieces, board, game, interface, ai, evaluation, utils), tests automatisés

## Phases Développement

- **Phase 1** : Architecture base et règles
- **Phase 2** : Règles avancées et promotion  
- **Phase 3** : IA négamax/alpha-beta *(Architecture MVV-LVA)*

## Cas d'Usage Principaux

### Lancement & Navigation
- **CU-001** : Lancement rapide → `swipl go.pl` → Menu principal <3sec
- **CU-002** : Mouvements joueur → "e2e4" validé et exécuté avec retour visuel

### Intelligence Artificielle  
- **CU-003** : Réponse IA → Coup légal généré avec raisonnement visible
- **CU-005** : IA recherche → Profondeur fixe 2 coups pour simplicité

### Mécaniques de Jeu
- **CU-004** : Détection fin → Mat/Pat détecté avec annonce appropriée  
- **CU-006** : Promotion → Conversion automatique pion vers dame

### Validation
- **CU-007** : Tests → Suite complète `swipl tests/tests.pl` 42/42 passent

---

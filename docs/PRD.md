# Document d'Exigences Produit - Prolog Chess Game
## Projet Universitaire Intelligence Artificielle - IFT-2003

**Contexte** : TP1 - 10% de la note finale  
**Objectif** : Implementer une IA d'echecs avec minimax et alpha-beta en Prolog

---

## Apercu produit

### Objectif
Developper un joueur d'echecs intelligent demontrant les techniques de recherche heuristique vues en cours IFT-2003. Le projet sert d'application pratique des algorithmes minimax avec elagage alpha-beta en programmation logique Prolog.

### Vision
Creer une IA d'echecs competitive (~1200 ELO) avec interface francaise qui permet d'experimenter et comprendre la prise de decision algorithmique a travers le gameplay.

## Objectifs

### Pedagogiques
- **Maitrise IA** : Demonstrer comprehension minimax/alpha-beta
- **Programmation logique** : Application pratique concepts Prolog
- **Evaluation** : Obtenir score satisfaisant sur TP1

### Utilisateur
- **Gameplay fluide** : Partie d'echecs complete avec reponse IA <5sec
- **Interface claire** : Messages francais, aide contextuelle
- **Apprentissage** : Observer raisonnement IA en action

## Exigences principales

### Fonctionnalites core
- **FR-001** : Validation complete regles d'echecs
- **FR-002** : Algorithme minimax avec profondeur configurable  
- **FR-003** : Elagage alpha-beta pour optimisation
- **FR-004** : Evaluation heuristique position (materiel, mobilite, securite roi)
- **FR-005** : Interface francaise avec aide integree
- **FR-006** : Detection fin de partie (mat, pat, nullite)

### Fonctionnalites avancees
- **FR-007** : Coups speciaux (promotion)
- **FR-008** : Niveaux difficulte (profondeur variable)
- **FR-009** : Affichage evaluation et processus reflexion

## Architecture technique

### Modules (Phase 1 complete ✅)
- **pieces.pl** : Regles mouvement par type piece
- **board.pl** : Plateau 8x8, affichage ASCII, coordonnees
- **game.pl** : Etat jeu, validation coups, alternance joueurs  
- **interface.pl** : Menu francais, boucle jeu, messages aide
- **ai.pl** : Intelligence artificielle (PROTOTYPE NON FONCTIONNEL)

### Extension IA (Phase 3 prioritaire)
- **Tests exhaustifs** : Suite complete dans tests.pl

## Criteres succes

### Evaluation academique
- **Algorithme** : Minimax + alpha-beta implementes correctement
- **Performance** : Temps reponse <5sec, force jeu raisonnable
- **Code** : Structure modulaire claire, tests passent
- **Demo** : Fonctionnement fluide, explications algorithmes

### Metriques techniques
- **Reactivite** : IA repond <5 secondes par coup
- **Stabilite** : Aucun crash gameplay normal
- **Tests** : 100% suite tests automatises reussie

## Phases developpement

**Phase 1 : Fondations (Terminee ✅)**
- Architecture 4 modules, regles base, tests exhaustifs

**Phase 2 : Regles avancees (Actuelle)**  
- Coups speciaux, detection mat/pat, completion FIDE

**Phase 3 : IA (Priorite absolue)**
- Minimax/alpha-beta, evaluation position, niveaux difficulte

**Phase 4 : Finition**
- Optimisations, documentation, preparation demo

## User stories cles

**US-001** : Lancer jeu avec `swipl go.pl` → interface francaise claire  
**US-002** : Jouer coups notation "e2e4" → validation + execution  
**US-003** : IA genere coups legaux minimax → reponse <5sec  
**US-004** : Detecter mat/pat → fin partie appropriee  
**US-005** : Choisir niveau difficulte → profondeur recherche adaptee

## Contraintes projet

### Techniques
- **Plateforme** : SWI-Prolog uniquement
- **Interface** : Console ASCII (pas GUI)
- **Performance** : Equilibre vitesse/force jeu
- **Memoire** : Gestion efficace structures donnees

### Academiques  
- **Temps** : Developpement sur 8-12 semaines
- **Scope** : Focus algorithmes IA, pas polish interface
- **Originalite** : Code personnel, documentation sources
- **Demonstration** : Preparation presentation fonctionnalites

---

**Statut actuel** : Fondations solides, pret implementation IA  
**Priorite** : Minimax + alpha-beta pour evaluation TP1

## Liens Documentation
- **Guide developpeur** : [../.claude/CLAUDE.md](../.claude/CLAUDE.md)
- **Taches detaillees** : [TASKS.md](TASKS.md)
- **Guide utilisateur** : [../README.md](../README.md)
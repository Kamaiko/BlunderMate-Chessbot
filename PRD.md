# Document d'Exigences Produit - Prolog Chess Game
## TP1 - Conception d'un jeu intégrant une recherche heuristique

## 1. Résumé Exécutif

**Cours** : Intelligence Artificielle IFT-2003  
**Projet** : Joueur intelligent d'échecs avec recherche heuristique  
**Plateforme** : SWI-Prolog (conformément aux exigences TP1)  
**Objectif** : Implanter une solution IA utilisant une approche logique  
**Pondération** : 10% de la note finale

## 2. Objectifs d'Apprentissage (TP1)

Conformément aux exigences du TP1, ce projet vise à :
- **Identifier un problème d'intelligence artificielle** : Jeu d'échecs comme problème de recherche
- **Analyser un problème de recherche dans un espace d'états** : Modélisation états/mouvements
- **Choisir une technique de recherche heuristique appropriée** : Minimax avec alpha-beta
- **Implanter une solution en utilisant une approche logique** : Programmation Prolog

### Vision Produit
Créer un joueur intelligent d'échecs en Prolog utilisant des techniques de recherche heuristique vues dans le cours IFT-2003.

### Objectifs Mesurables
- **Performance IA** : Joueur compétitif utilisant heuristique adaptée
- **Temps de réponse** : Raisonnable pour démonstration
- **Implémentation** : Techniques de recherche heuristique clairement expliquées
- **Qualité code** : Code Prolog documenté et fonctionnel

## 3. État Actuel v5.1

### Fonctionnalités Livrées
- Architecture 4 modules : pieces.pl, board.pl, game.pl, interface.pl
- Mouvements de base pour toutes les pièces
- Validation robuste et tests exhaustifs
- Interface française fonctionnelle

## 4. Roadmap Produit

### Phase 1 : Fondations Jeu ✅
- [x] Interface de base : Menu principal, boucle de jeu, messages français
- [x] Logique mouvements : Tous types de pièces (pion, tour, fou, cavalier, dame, roi)
- [x] Système coordonnées : Notation algébrique, validation 8x8
- [x] Tests exhaustifs : 6 sections, 100% couverture validée

### Phase 2 : Règles Avancées
- [ ] Roque, en passant, promotion : Mouvements spéciaux
- [ ] Échec, mat, pat : États terminaux  
- [ ] Validation complète : Base solide pour l'IA

### Phase 3 : Intelligence Artificielle ⭐ (FOCUS PRINCIPAL)
- [ ] Minimax avec élagage alpha-beta
- [ ] Évaluation de position : matériel, mobilité, sécurité roi
- [ ] Optimisations : tri des coups, table de transposition
- [ ] Bibliothèques : ouvertures et finales de base
- [ ] Niveaux IA : Débutant, Intermédiaire, Expert (profondeurs variables)

### Phase 4 : Extensions Post-Universitaire
- [ ] Interface graphique : GUI avec plateau visuel interactif
- [ ] Analyse de positions : Évaluation temps réel, suggestions coups
- [ ] Métriques IA : Comparaison algorithmes, performance profiling

## 5. Critères d'Acceptation

### Performance IA
- **Force de jeu** : Niveau intermédiaire minimum (ELO ~1200)
- **Temps de réponse** : < 5 secondes par coup en mode normal
- **Algorithme** : Minimax avec alpha-beta fonctionnel

### Qualité Technique
- **Tests IA** : Validation des algorithmes d'évaluation
- **Architecture** : Séparation logique jeu / IA
- **Documentation** : Explication des choix algorithmiques

## 6. Contraintes et Limitations

### Contraintes Techniques
- **Plateforme** : SWI-Prolog uniquement
- **Interface** : Console (pas de GUI requis)
- **Performance** : Optimisation pour usage éducatif

### Contraintes Projet
- **Délai** : Implémentation par phases prioritaires
- **Scope** : Focus sur algorithmes IA core
- **Évaluation** : Démonstration des capacités IA

## Liens Documentation
- **Guide développeur** : [.claude/CLAUDE.md](.claude/CLAUDE.md)
- **Guide utilisateur** : [README.md](README.md)
- **Code source** : [src/](src/)
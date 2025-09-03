# IA ÉTAT ACTUEL - TRANSFERT DÉVELOPPEUR

**Fichier IA** : `src/ai.pl`  
**Problème** : IA capture sans voir recaptures immédiates

## PROBLÈME CRITIQUE

L'IA fait des blunders tactiques :
- **Exemple** : Cavalier c6xd4 alors que cavalier blanc en c3 peut recapturer
- **Algorithme** : Minimax profondeur 2 ne détecte pas les séquences capture-recapture
- **Impact** : IA donne matériel gratuitement

## ARCHITECTURE

### Fichiers
```
src/ai.pl              # IA principale (problématique)
src/interface.pl       # Interface charge ai.pl
tests/tests.pl         # Tests principaux
```

### Algorithme (src/ai.pl)
- Minimax avec alpha-beta, profondeur 2
- Évaluation basée sur board_eval.pl (référence)
- Génération coups : Développement > Pions > Autres

## RÉFÉRENCE FONCTIONNELLE

**Fichier** : `C:\DevTools\Projects\PrologChessNotMine\board_eval.pl`
- Format : `half_position(Pawns,Rooks,Knights,Bishops,Queens,[King],_)`
- Notre format : `game_state(Board, Player, MoveCount, Status, CapturedPieces)`
- L'IA de référence ne fait PAS ces blunders

## CORRECTIONS APPLIQUÉES

✅ Coups g8h6 répétitifs (priorité génération)  
✅ Développement pions (bonus positions centrales)  
❌ Conscience tactique (non résolu)

## PISTES DEBUG

1. **Vérifier profondeur réelle** : Minimax atteint-il vraiment profondeur 2 ?
2. **Évaluation négative** : `Value is -OpponentValue` correct ?
3. **Structure données** : Différence `half_position` vs `game_state` ?

## COMMANDES TEST

```bash
# Test blunder
swipl -s src/interface.pl -g ai_vs_human_mode

# Tests projet
swipl -t run_tests -s tests/tests.pl
```

## OBJECTIF

L'IA doit arrêter de donner matériel gratuitement. L'IA de référence fonctionne - identifier pourquoi `src/ai.pl` échoue sur captures simples.
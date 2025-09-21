# 🛡️ Guide de Sécurité CI/CD - Protection Anti-Boucles Infinies

## 🚨 Problème Critique Identifié
Le test `go.pl` peut entrer dans une boucle interactive infinie, épuisant les minutes GitHub Actions.

## 🛡️ Système de Protection Multi-Niveau

### Niveau 1: Timeouts en Cascade
```yaml
# Protection globale du job
timeout-minutes: 10  # KILL SWITCH GLOBAL

# Protection par step
- name: Test Step
  timeout-minutes: 2  # Timeout step
  run: |
    timeout 90s command  # Timeout command Unix
```

### Niveau 2: Validation des Commandes Dangereuses
```bash
# ❌ DANGEREUX - Peut boucler
swipl go.pl

# ✅ SÉCURISÉ - Halt explicite + timeout
timeout 20s swipl -s file.pl -g "test_code, halt(0)"
```

### Niveau 3: Tests Non-Interactifs
```prolog
% ❌ DANGEREUX - Interface interactive
start :- main_menu.

% ✅ SÉCURISÉ - Test de chargement uniquement
test_loading :-
    consult('src/pieces'),
    consult('src/board'),
    write('All modules loaded'), nl,
    halt(0).
```

## 🎯 Stratégies Préventives Supplémentaires

### 1. Workflow Conditionnel
```yaml
# Désactiver CI sur certaines branches
on:
  push:
    branches: [ master ]
    paths-ignore:
      - 'docs/**'
      - '*.md'
```

### 2. Matrix Strategy avec Fail-Fast
```yaml
strategy:
  fail-fast: true  # Arrêter dès qu'un test échoue
  matrix:
    test-type: [foundation, rules, ai]
```

### 3. Environnement Isolé
```yaml
# Utiliser des containers jetables
container:
  image: swipl:latest
  options: --cpus="1" --memory="512m"  # Limiter ressources
```

### 4. Monitoring Intelligent
```yaml
- name: Monitor Resource Usage
  run: |
    # Tuer processus qui consomment trop
    timeout 5s bash -c 'while true; do
      if pgrep -f "swipl.*go.pl"; then
        echo "Killing stuck swipl process"
        pkill -f "swipl.*go.pl"
      fi
      sleep 1
    done' &
```

## 🚀 Commandes d'Urgence

### Force Cancel via GH CLI
```bash
# Si un workflow est coincé
gh api --method POST \
  -H "Accept: application/vnd.github+json" \
  -H "X-GitHub-Api-Version: 2022-11-28" \
  /repos/OWNER/REPO/actions/runs/RUN_ID/force-cancel
```

### Désactivation Workflow
```bash
# Désactiver temporairement
gh workflow disable WORKFLOW_ID

# Réactiver après fix
gh workflow enable WORKFLOW_ID
```

## 📊 Métriques de Sécurité

| Protection | Temps Max | Action si Dépassé |
|------------|-----------|-------------------|
| Global Job | 10 min | Tue tout le workflow |
| Step Individual | 1-3 min | Tue le step, continue |
| Command Unix | 20-90s | Tue la commande |
| Process Monitor | 5s | Tue processus suspects |

## 🎯 Tests de Validation

### Test Safe vs Unsafe
```bash
# Test la protection
timeout 5s swipl go.pl  # Doit timeout
echo $?  # Should return 124 (timeout)

# Test le chargement sécurisé
timeout 10s swipl -s src/interface.pl -g "halt(0)"
echo $?  # Should return 0 (success)
```

## 🔧 Maintenance

### Révision Mensuelle
- [ ] Vérifier que tous les timeouts sont appropriés
- [ ] Tester les commandes de force-cancel
- [ ] Analyser les métriques d'usage GitHub Actions
- [ ] Mettre à jour les limites si nécessaire

### Alertes Automatiques
- [ ] Configurer notifications si workflow > 5min
- [ ] Monitor l'usage mensuel GitHub Actions
- [ ] Backup plan si quota épuisé
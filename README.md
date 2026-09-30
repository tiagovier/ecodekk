# Projet ECODEKK

Application R Shiny de simulation urbaine, programmatique et financière du projet ECODEKK.

## Exécution locale

```r
shiny::runApp()
```

## Scénarios

Chaque scénario est stocké dans `data/scenarios/<identifiant>/` avec un GeoPackage spatial, un modèle RDS, un manifeste YAML et des exports CSV.
Le bouton **Télécharger le scénario** reconstruit ces éléments depuis l’état courant et produit un ZIP autonome à conserver avant la fermeture d’une session publiée.

## Manifeste Posit

La liste blanche `deploy-files.txt` définit les fichiers d’exécution. Après toute modification de ces fichiers, régénérer le manifeste :

```bash
Rscript scripts/write_manifest.R
```

Puis valider l’application :

```bash
Rscript tests/testthat.R
```

## Publication

Pour un contenu Git-backed dans Posit Connect ou Posit Connect Cloud :

1. régénérer `manifest.json` ;
2. committer et pousser le code et le manifeste sur GitHub ;
3. dans Connect, choisir **Publish > Import from Git** et sélectionner ce dépôt, la branche `main` et la racine du dépôt ;
4. activer les mises à jour automatiques dans les paramètres Git du contenu.

Pour une publication directe depuis un projet Posit Cloud déjà authentifié :

```r
rsconnect::deployApp(
  appDir = ".", appName = "ecodekk", appTitle = "Projet ECODEKK",
  server = "posit.cloud", manifestPath = "manifest.json",
  launch.browser = FALSE
)
```

Le stockage écrit pendant l’exécution d’un contenu publié ne doit pas être considéré comme persistant. Télécharger le scénario avant la fin de la session.


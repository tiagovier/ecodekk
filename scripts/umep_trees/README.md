# Arbres UMEP — attribution des essences et dimensions (Thiès)

Outil d'étude, hors application Shiny et hors déploiement Posit. Il conserve
les **positions CAO existantes** de `data/sig/arbres.gpkg` (22 100 points) et
attribue à chaque arbre une essence et les quatre attributs requis par le
*Tree Generator* UMEP (Pre-Processor → Spatial Data → Tree Generator).

## Exécution

```bash
python3 scripts/umep_trees/ecodekk_trees.py \
  --scenario scenario_01 --study-id ombrage_arbres \
  --growth-stage mature --seed 20260930
```

- `--growth-stage` : `mature` (défaut) ou `young` (10–15 ans après plantation).
- `--seed` : graine globale ; une même graine donne une sortie identique.
- `--config` : fichier de paramètres (défaut `config.yaml`, seul endroit où
  modifier poids, dimensions, lois de variation et règles de classement).

Dépendances : Python ≥ 3.10, geopandas, shapely 2, numpy, pandas, PyYAML,
matplotlib ; tests avec pytest :

```bash
python3 -m pytest scripts/umep_trees/tests
```

## Classement des points existants

Ordre de priorité, en EPSG:32628 :

1. **Exclu** : point dans l'emprise d'un bâtiment du scénario.
2. **road** : point dans une emprise de voirie (`road_footprints`) ou à moins
   de `offset_from_edge_m` (3 m) du bord de chaussée, le bord étant l'axe OSM
   le plus proche décalé de `width_m / 2`. Rue étroite si `width_m` < 8 m :
   le tableau `narrow_street_weights` (Terminalia mantaly par défaut) remplace
   les poids d'alignement. Une largeur absente est signalée, jamais supposée.
3. **flood** : point dans `flood_areas` (Mangifera exclu, Mitragyna réservé).
4. **block** : point dans une occupation du sol listée dans
   `block.land_use_labels` (espaces verts et parcelles forestières communes
   `par_10 variable plots forets`) ; avec `other_points_as_block: true`
   (défaut confirmé), tout autre point hors bâtiment (parcelles loties,
   agricoles, parkings, servitudes, équipements) est aussi un arbre d'îlot.
   Le champ `planting_zone` conserve l'étiquette land_use d'origine.

Avant attribution, les points CAO coïncidents (à moins de
`deduplicate_tolerance_m`, 1 cm) sont dédoublonnés : le plus petit `tree_id`
est conservé, les autres figurent dans `*_excluded.csv` avec `duplicate_of`.

Les règles de placement (espacement, jitter, densité) de la spécification ne
s'appliquent pas : les positions CAO ne sont pas déplacées. L'espacement
minimal `0,8 × (r_i + r_j)` est seulement **diagnostiqué**
(`spacing_conflicts`) : la plantation dense est un choix de projet confirmé
et les houppiers se recouvrent (voisin médian à 2,5–4 m).

## Dimensions

Pour chaque arbre : `g` (stade) et un bruit individuel `ε` par dimension,
lois normales tronquées ; `hauteur = base × g × ε_h`, `houppier = base × g × ε_c`,
`tronc = base × ε_t` (le tronc dépend de l'élagage). Contraintes : tronc
≥ 1,5 m ; tronc ≤ 0,5 × hauteur ; alignement : tronc ≥ 4,5 m, la hauteur est
relevée si nécessaire ; arrondi à 0,1 m. Chaque ajustement est tracé
(`height_raised`, `trunk_raised`, `trunk_capped`).

## Sorties

Dans `data/scenarios/<scenario>/exports/umep/<study_id>/`, jamais écrasées :

| Fichier | Contenu |
|---|---|
| `trees_thies_<stade>_seed<graine>.gpkg` | couche `trees_umep`, EPSG:32628 |
| `…_summary.csv` | effectif, moyenne/min/max par essence et contexte |
| `…_histograms.png` | histogrammes hauteur et houppier par essence |
| `…_excluded.csv` | points non attribués et motif |
| `…_adjustments.csv` | arbres dont la taille a été contrainte |
| `…_run.yml` | versions, graine, empreintes SHA-256, paramètres, comptages |

Champs UMEP (sélectionnés dans la boîte de dialogue du Tree Generator) :
`ttype` (2 = feuillu pour toutes les essences, y compris sempervirentes),
`totheight` (hauteur totale, m au-dessus du sol), `trunkheight` (base du
houppier, m au-dessus du sol), `diameter` (diamètre circulaire du houppier, m).

Le Tree Generator exige aussi un DSM bâti + sol et un MNT de même emprise et
même pixel ; la génération CDSM/TDSM reste à faire dans QGIS/UMEP.

## Réserves

- **Faidherbia albida** a une phénologie inversée : il perd ses feuilles en
  saison des pluies et est feuillé en saison sèche. Sa période feuillée doit
  être réglée manuellement dans SOLWEIG (paramètres de feuillaison), sinon
  son ombrage est faux pour la saison simulée.
- Les dimensions sont des **ordres de grandeur issus de la littérature**, à
  valider par des relevés de terrain locaux avant toute conclusion.
- La transmissivité de la végétation n'est pas fixée par cet outil.

## Prétraitement UMEP (`umep_preprocess.sh`)

Étude `scenario_01 / ombrage_arbres`, grille de référence
`dem/mnt_50cm_etude.tif` (0,5 m, EPSG:32628, altitudes en mètres). Chaque
raster produit est contrôlé contre cette grille ; aucune sortie n'est écrasée.

```bash
scripts/umep_trees/umep_preprocess.sh dsm        # DSM bâti + sol (DSM Generator)
scripts/umep_trees/umep_preprocess.sh trees      # CDSM/TDSM saison sèche et saison des pluies
scripts/umep_trees/umep_preprocess.sh walls      # hauteur et orientation des murs (seuil 3 m)
scripts/umep_trees/umep_preprocess.sh landcover  # occupation du sol UMEP (2 saisons)
scripts/umep_trees/umep_preprocess.sh svf        # facteurs de vue du ciel : 2 saisons × 2 transmissivités
```

| Choix confirmé | Valeur |
|---|---|
| Transmissivité | 3 % (référence), 15 % (sensibilité) |
| Phénologie | feuilles toute l'année (jours 1–366) ; saison des pluies : Faidherbia albida retiré du CDSM/TDSM |
| Occupation du sol | bâtiment = 2, emprises de voirie = 1, zones inondables = 7 (eau, saison des pluies seulement), reste = 6 (sol nu) ; pas d'herbe. Priorité : bâtiment > voirie > eau > sol nu |
| Météorologie | ERA5 via UMEP (`supy`), hauteur de diagnostic 2 m ; nécessite `~/.cdsapirc` |

Remarques techniques :

- UMEP transmet la source vectorielle brute à GDAL : une URI
  `fichier.gpkg|layername=…` échoue. Le script passe des GeoPackages
  mono-couche dérivés (`vector/`). Le DSM Generator y ajoute un champ
  `height_asl` (copie dérivée seulement).
- Le Tree Generator alloue deux grilles complètes par arbre : compter
  environ 1 h 30 par variante à 0,5 m pour ~19 000 arbres. Lancer en tâche
  détachée (`setsid nohup … &`) avec un journal dans `logs/`.
- Ne jamais modifier `umep_preprocess.sh` pendant qu'il s'exécute : bash lit
  le script au fil de l'eau et reprendrait à un décalage erroné. Pour les
  enchaînements longs, exécuter une copie figée
  (`ECODEKK_ROOT=<racine> bash logs/umep_preprocess.snapshot.sh …`).

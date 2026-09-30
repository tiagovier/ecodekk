# Ecodekk — règles du projet

## Objet

Cette application R Shiny simule la programmation urbaine et les équilibres
financiers d'une opération de 110 ha entre Thiès et Pout, au Sénégal.

La phase 1 couvre le moteur de programmation et de calcul financier, ainsi que
les vues cartographiques 2D et 3D demandées. Ne pas étendre le SIG à d’autres
usages sans demande explicite.

## Langue et unités

- Tout texte visible dans l'application doit être en français.
- Les identifiants techniques internes restent stables, normalisés et séparés
  des libellés visibles.
- Afficher explicitement les unités : CFA, CFA/m² SDP, m² SDP et unités.
- Les montants de base sont HT. La TVA est calculée séparément, à 20 % par
  défaut. Le poste de frais financiers ne porte pas de TVA dans le bilan de
  référence.

## Architecture

- Garder toute logique métier hors des blocs Shiny `render*()`, `observe()` et
  `observeEvent()`.
- Écrire des fonctions R pures dans `R/` et les tester avec `testthat`.
- Les modules Shiny collectent les entrées, appellent le moteur et affichent les
  résultats. Ils ne contiennent pas les formules financières.
- Utiliser des jointures par identifiants stables, jamais des coordonnées de
  cellules Excel.
- Le classeur principal de validation est `data/ref/bilan aménagement eco cité SAFRU Aout-26-TV (1).xlsx`. Il ne
  doit pas être une dépendance d'exécution de l'application.

## Règles métier confirmées

- Conserver les charges foncières négatives : elles matérialisent la
  péréquation entre produits. Ne jamais les annuler par un facteur nul.
- Le coût de construction effectif d'un produit hérite de sa catégorie, sauf
  lorsqu'un coût dérogatoire est renseigné.
- Modes de charge foncière : `simplified`, `manual` et `promoter_balance`.
  Seuls les deux premiers sont calculés en phase 1. Le troisième doit retourner
  un résultat explicitement non disponible.
- EP et EL ne sont pas cessibles et ne génèrent aucune recette. Leurs coûts de
  construction restent calculés.
- EV est un produit foncier non bâti. Sa recette repose sur la surface de
  terrain renseignée dans chaque ligne de programmation, pas sur la SDP, et son
  coût de construction est nul.
- Le SDP par unité est une hypothèse de produit. La programmation contient les
  quantités et hérite du SDP par unité du produit.
- Les lignes non programmées sont initialisées avec une quantité nulle.
- Les lignes EC, EP et EL peuvent porter une SDP totale issue directement du
  programme lorsque leur diversité ne permet pas un SDP/unité unique.
- Aucun montant de cession ne doit être calé ou compensé par quartier : il doit
  provenir des surfaces et charges foncières du programme.

## Bilan d'aménagement

- Respecter l'ordre et la hiérarchie du tableau du classeur : dépenses,
  recettes, sous-totaux, totaux et résultat d'opération.
- La colonne visible `Assiette` n'est pas requise en phase 1. Conserver toutefois
  la base de calcul de chaque ligne dans le modèle interne.
- Les cessions sont reconstruites par quartier depuis les charges foncières.
- Les frais sur ventes utilisent les cessions TTC et un taux modifiable, puis
  supportent la TVA.
- Les frais financiers et la rémunération SAFRU utilisent des taux modifiables.
- Ne pas ajouter les coûts de construction des produits privés comme nouvelle
  ligne du bilan d'aménagement. Conserver la ligne existante `Construction
  équipements publics` comme hypothèse propre au bilan; sa valeur initiale est
  nulle dans la référence.

## Données et validation

- Ne jamais remplacer silencieusement une hypothèse manquante par une valeur
  plausible. Afficher une validation en français.
- Distinguer une vraie valeur nulle d'une donnée manquante.
- Tester au minimum les coûts hérités et dérogatoires, les trois modes de charge
  foncière, la péréquation négative, le calcul de SDP, les agrégations par
  produit et quartier, la TVA et le bilan d'aménagement.
- Après toute modification du moteur, exécuter les tests avant livraison.

## Données cartographiques

- `data/sig/buildings.gpkg` est la source géométrique la plus récente des bâtiments.
  Les fonctions et niveaux sont transférés depuis `data/sig/thies13.osm` par
  appariement spatial contrôlé à 30 m ; les correspondances faibles ou absentes
  doivent rester signalées. Les axes de voirie restent issus de `thies13.osm`.
- Les bâtiments OSM sans empreinte corrigée à moins de 30 m sont conservés dans
  l’inventaire avec la source `OSM conservé`; ils ne doivent pas disparaître sans
  décision manuelle dans la table de contrôle.
- Les quartiers et occupations du sol sont liés spatialement aux bâtiments. Le
  lien produit n’est initialisé que pour les zones non ambiguës et reste éditable.
- La première SDP cartographique indicative est `emprise au sol × niveaux`. Elle
  doit rester distincte de la SDP du modèle, notamment pour EC, EP et EL.
- Les surfaces au sol et SDP cartographiques sont recalculées depuis les
  géométries au chargement; ne pas utiliser des colonnes calculées persistées
  dans QGIS comme source d'autorité.
- Les éditions de `spatial.gpkg` réalisées dans QGIS sont fiables. Une valeur
  renseignée par QGIS est conservée; les écarts avec l'occupation du sol et les
  niveaux issus d'une hypothèse sont signalés sans alimenter la file de contrôle.
- La file de contrôle est réservée aux problèmes bloquants : produit absent ou
  inconnu, niveau fourni invalide ou géométrie invalide.
- Les niveaux par défaut et l'éventuel produit distinct du RDC commercial sont
  des hypothèses de produit éditables. RM2 et RC1 sont initialisés à deux
  niveaux (R+1); RC2 et RT1 utilisent leur produit commercial au RDC.
- Pour RC1, RC2, RC3, RC4 et RT1, le nombre de logements par niveau est une
  hypothèse de produit. La quantité d'un bâtiment est `niveaux × logements par
  niveau`, sauf pour un RDC affecté à un produit commercial distinct, compté
  comme une unité commerciale. RC4 est initialisé à 5 niveaux et 4 logements
  par niveau.
- La SDP cartographique d'un bâtiment est `emprise géométrique × niveaux`. La
  SDP d'un logement collectif est l'emprise retenue divisée par le nombre de
  logements du niveau. Une variation d'emprise saisie dans le contrôle applique
  une homothétie autour du centre du bâtiment, puis enregistre la géométrie
  modifiée dans le GeoPackage du scénario.
- Dans les hypothèses produit, distinguer la SDP d'une unité de référence
  (logement ou local), la SDP moyenne d'un bâtiment complet calculée depuis les
  bâtiments du scénario et la SDP totale du programme. Les quantités restent
  présentées dans la programmation et la cartographie.
- `data/sig/projet-120526.gpkg`, table `polylines`, reste la source des limites
  parcellaires identifiées par le champ CAO `layer` (`par_*` et `Parcelles *`).
- Les géométries `COMPOUNDCURVE` du GeoPackage doivent être linéarisées. Ses
  coordonnées EPSG:32628 sont stockées en millimètres : appliquer `1 / 1000`
  avant reprojection.
- La hauteur calculée d’un bâtiment OSM est : hauteur du RDC +
  `(nombre de niveaux - 1) × hauteur d’un étage courant`.
- Les hauteurs du RDC et des étages courants, ainsi que le nombre de niveaux
  utilisé lorsque la source est manquante, sont des hypothèses éditables.
- Par décision métier confirmée, exclure des vues les deux bâtiments atypiques
  d’identifiants OSM `-38433` et `-38434`. Conserver leurs géométries et leurs
  attributs dans le fichier source, sans le modifier.
- Tout bâtiment consolidé sans nombre de niveaux utilise l’hypothèse éditable de
  niveau par défaut ; le nombre concerné doit rester signalé dans l’interface.
- Les vues 2D et 3D doivent utiliser les mêmes bâtiments consolidés, axes OSM,
  emprises de voirie, quartiers, occupations du sol, zones inondables, emprises
  du projet et du titre foncier, ainsi que les limites parcellaires. Elles sont
  cadrées sur l’ensemble du lotissement.
- La vue 3D utilise MapLibre sur le fond raster OpenStreetMap, sans clé API, et
  extrude les bâtiments à partir des mêmes données que la vue 2D.
- Ne jamais modifier les fichiers SIG sources lors de la préparation des vues.
- Les fichiers de `data/sig/` sont des références immuables. Chaque scénario est
  un dossier `data/scenarios/<scenario_id>/` contenant `spatial.gpkg`,
  `model.rds`, `scenario.yml` et le dossier `exports/`.
- `spatial.gpkg` ne contient que les couches spatiales consolidées et peut être
  édité directement dans QGIS. Les hypothèses et objets financiers canoniques
  restent dans `model.rds`; les CSV de `exports/` servent à l'inspection et aux
  jointures QGIS par identifiants stables.
- Un bâtiment marqué comme non inclus est retiré de la couche `buildings` du
  scénario, sans être supprimé de la source de référence.
- Les dossiers de scénario complets sont découverts récursivement sous
  `data/scenarios/`. Pour dupliquer un scénario, copier son dossier complet,
  puis actualiser la liste et utiliser `Recharger`.
- Si `spatial.gpkg` est modifié dans QGIS après son chargement, l'application
  autorise encore les sauvegardes du modèle, mais refuse toute réécriture
  spatiale jusqu'au rechargement. Une modification externe de `model.rds`
  bloque de même sa réécriture jusqu'au rechargement.

# Plan — Charte de performance ECODEKK

Statut : plan validé pour exécution ultérieure (1er octobre 2026). Aucune
modification de code n'a encore été faite.

Sources analysées (références, pas des dépendances d'exécution) :

- `data/charte/Charte_Urbaine_ECODEKK rev LJ 30092026.docx`
- `data/charte/Referentiel_ecocite_tableur rev LJ.xlsx` (feuilles `Piliers`,
  `Synthèse Referentiel`, `récap 110 ha`, `Calcul`, `Synthèse Note`,
  `Radar 8 axes`, `Mini_batons_axes`)

## 1. Modèle de la charte

| Niveau | Nb | Source |
|---|---|---|
| Dimensions | 4 | docx : D1 Eau et nature · D2 Compacité et sobriété carbone · D3 Bien-être et équité · D4 Viabilité et adaptation |
| Thématiques | 8 | 1 Trame bleue · 2 Trame verte · 3 Ville compacte · 4 Transition bas carbone · 5 Cadre de vie · 6 Gouvernance · 7 Économie · 8 Résilience (chacune avec un objectif stratégique) |
| Notions | 28 | docx : questions à se poser, recommandations, détail, exemple |
| Indicateurs | 31 | xlsx `récap 110 ha` : KPI, unité, méthode, valeur de référence, cible, outil, sources |

Répartition des indicateurs : TB-1…4 · TV-1…3 · VC-1a, VC-1b, VC-2…4 ·
CO-1, CO-2a, CO-2b, CO-3a, CO-3b, CO-4 · CV-1…4 · GOV-1, GOV-2 · ECO-1…3 ·
RES-1a, RES-1b, RES-2, RES-3.

Notation (feuille `Calcul`) : valeur projet = numérateur ÷ dénominateur, puis
note 1 à 5 par 4 seuils propres à chaque indicateur (1 Très insuffisant … 5
Très performant). Radar = moyenne des notes par axe ; note globale = moyenne
des 31 notes.

### Incohérences du classeur à arbitrer ou corriger

- Valeurs projet fictives (100/100, 4/8, 50/100…) : ne pas les importer,
  afficher « non renseigné ».
- TB-1 cible ≥ 20 % (`récap`, `Calcul`) contre ≥ 80 % (`Synthèse`, charte).
- RES-2 cible −20 % contre −50 %.
- ECO-1 : « % de réduction vs scénario classique » (`récap`) contre
  « ≤ 240 M FCFA/ha » (`Calcul`).
- VC-2 : dénominateur en superficie (`récap`) contre population (`Calcul`).
- CO-4 : deux sous-KPI (ménages à < 200 m, % recyclé), seul le recyclage est
  noté.
- TV-3 : seuils non monotones (`<3,5 → 3` avant `<3 → 4`), note 4
  inatteignable.
- ECO-2 : saisie 20 comparée à 5 % (unité), note toujours 5.
- Code RES-1a dupliqué dans `récap` (le second est RES-1b).
- Surfaces du classeur obsolètes vs SIG (ex. square 11 873 m² contre
  16 859 m² dans `landuse.gpkg`) : les mesures doivent être liées au scénario.

## 2. Cartographie des intrants

Légende : **SIG** = `spatial.gpkg` du scénario · **PRG** = programmation /
produits · **BIL** = bilan d'aménagement · **SIM** = sorties UMEP ·
**HYP** = hypothèse charte éditable · **SAI** = saisie manuelle.

| Code | Valeur | Intrants et source | Statut |
|---|---|---|---|
| TB-1 | Aléa fort valorisé ÷ aléa fort total | Zone d'aléa fort (**SIG**) ; occupation du sol ∩ aléa, usages compatibles (TVB, TVB Hydraulique, Forêt, Agricole, Parc, bassins) (**SIG** + liste **HYP**) | Auto |
| TB-2 | Surface éco-aménagée ÷ surface aménagée | Surfaces d'occupation du sol × coefficient de biotope (**HYP**) ; emprise projet (**SIG**) | Auto |
| TB-3 | Eau couverte localement ÷ besoin | Emprise des toitures (**SIG**) × pluviométrie × rendement (**HYP**) + REUT ; besoin = population × L/hab/j (**HYP**) | Semi-auto |
| TB-4 | EH phytoépuration ÷ EH total | EH total = population ; EH raccordés **SAI** | Manuel |
| TV-1 | Espaces verts publics / hab | Square + Maille + Forêt + Parc (+ Jardins ?) (**SIG**) ÷ population | Auto |
| TV-2 | Canopée à maturité ÷ surface aménagée | Union des houppiers de l'inventaire UMEP (`crown_diameter_m`) (**SIG/SIM**) | Auto |
| TV-3 | Agriculture nourricière / hab | Parcelles agricoles + Jardins familiaux (**SIG**) ÷ population | Auto |
| VC-1a/1b | log/ha, hab/ha hors EV | Logements résidentiels (**PRG**), population ; surface urbanisée = habitat (**SIG**) | Auto (dénominateur à définir) |
| VC-2 | % à < 500 m du panier de services | Intersection des tampons 500 m école, santé, commerce/marché (+ EV, culte) (**SIG**) ; euclidien en v1 | Auto |
| VC-3 | Population à < 500 m d'un arrêt TC | Nouvelle couche éditable `transit_stops` ; population par bâtiment (**SIG + PRG**) | Couche à créer |
| VC-4 | Linéaire modes doux ÷ linéaire voirie | Axes OSM par profil (**SIG**) + indicateur « cheminement sûr et continu » par profil (**HYP**) | Auto |
| CO-1 | Réduction GES globale | **SAI** en v1, module carbone ensuite | Manuel |
| CO-2a | Réduction carbone matériaux | **SAI** en v1 ; plus tard SDP par catégorie (**PRG**) × facteurs CO₂ (**HYP**) | Manuel → semi-auto |
| CO-2b | Réemploi des déblais | Cubatures **SAI** (MNT disponible, calcul hors périmètre) | Manuel |
| CO-3a | kWh réseau éclairage / hab / an | Linéaire de voirie (**SIG**) ÷ interdistance × puissance × heures × part non solaire (**HYP**) ÷ population | Semi-auto |
| CO-3b | PV local ÷ consommation publique | kWc × productible PVGIS (**HYP/SAI**) ; SUEWS ne produit pas de kWh | Manuel |
| CO-4 | (1) ménages < 200 m d'un point de collecte · (2) % recyclé | (1) nouvelle couche `waste_points` (**SIG**) · (2) **SAI/HYP** | Couche + manuel |
| CV-1 | Ombrage des espaces publics piétons | v1 : canopée ∩ espaces publics (**SIG**) ; v2 : ombre SOLWEIG (**SIM**, `manifest.json` absent aujourd'hui) | Auto → simulation |
| CV-2 | Nb de typologies de logement | Produits résidentiels de quantité > 0, par quartier (**PRG**) | Auto |
| CV-3 | Nb d'usages | Produits / occupations du sol → catégories d'usage (**PRG/SIG + HYP**) | Auto |
| CV-4 | Surface exposée au bruit ; équipements sensibles hors zone | Tampons par classe de voie (**HYP**) ∩ emprise ; écoles/santé hors zone (**SIG**) | Auto |
| GOV-1 | Gouvernance participative (2 × Oui/Non) | **SAI** qualitatif | Manuel |
| GOV-2 | Logements sociaux et abordables ÷ total | Nouvel attribut produit `logement_social_abordable` (**PRG**) | Auto après attribut |
| ECO-1 | CAPEX/ha et % vs scénario classique | Dépenses HT du bilan ÷ ha (**BIL** : 13,95 Md ÷ 110 ha ≈ 127 M/ha) ; % vs scénario de référence désigné | Auto |
| ECO-2 | Réduction OPEX | **SAI** en v1 | Manuel |
| ECO-3 | CAPEX éligible vert ÷ CAPEX | Indicateur « éligible financement vert » par ligne du bilan (**BIL + HYP**) | Semi-auto |
| RES-1a | Réserve foncière ÷ total | Attribut d'occupation du sol à créer (**SIG**) ou **SAI** | Donnée à créer |
| RES-1b | Parcelles évolutives ÷ parcelles | Parcelles (**SIG**) × indicateur « évolutif » par produit (**PRG**) | Auto après attribut |
| RES-2 | ENAF consommés / (logement + emploi) vs classique | Surface artificialisée (**SIG**), logements (**PRG**), emplois/m² SDP et référence classique (**HYP**) | Semi-auto |
| RES-3 | Habitat et équipements sensibles hors aléa fort | Bâtiments ∩ aléa fort (**SIG**) | Auto |

Intrant transversal : la **population** alimente TB-3, TB-4, TV-1, TV-3,
VC-1b, VC-3 et CO-3a ; le modèle n'en contient pas aujourd'hui.

## 3. Page « Intrants de la charte » (revue et correction dans QGIS)

> Implémentée (v1) : `R/charte_inputs.R` (moteur), `R/charte_inputs_page.R`
> (interface), menu « Charte de performance › Intrants de la charte »,
> tests `tests/testthat/test-charte-inputs.R`. Les attributs QGIS
> `charte_classes`, `coef_biotope` et `modes_doux` sont lus s'ils existent ;
> l'application ne les écrit pas encore dans `spatial.gpkg`.

But : voir chaque intrant issu de `spatial.gpkg` (occupation du sol,
quartiers, zones inondables, voirie, bâtiments, arbres, parcelles), la valeur
que l'application en déduit, et les entités concernées sur une carte, pour
corriger les couches dans QGIS puis recharger.

### Contenu

- **Tableau des intrants** (DT), une ligne par intrant :
  indicateur(s) concerné(s) · intrant (libellé français) · couche
  `spatial.gpkg` · règle de sélection (ex. `land_use.Layer ∈ {square, Maille,
  Foret, Parc}` ou attribut charte) · nombre d'entités · valeur calculée et
  unité · source (SIG / PRG / BIL / SIM / HYP / SAI) · statut (calculé,
  manquant, à vérifier).
- **Carte Leaflet** liée à la ligne sélectionnée : couche de contexte
  (emprise projet, quartiers) + entités retenues en couleur + entités exclues
  de la même couche en gris ; pour les intrants géométriques dérivés
  (tampons 500 m, intersections avec l'aléa, union des houppiers), afficher
  aussi la géométrie calculée. Infobulle : identifiant stable (`fid` ou
  identifiant métier), attributs utilisés, surface ou longueur recalculée.
- **Liste des entités** de l'intrant sélectionné (identifiant, attributs,
  surface/longueur) pour les retrouver dans QGIS ; export CSV.
- **Contrôles** : entités sans classe charte, géométries invalides,
  chevauchements qui doubleraient une surface, écarts avec les surfaces du
  classeur de référence (information seulement).
- Bouton **Recharger** : relit `spatial.gpkg` après édition dans QGIS
  (réutiliser le mécanisme existant de détection de modification externe).

### Principe de données

- Les règles de classement charte sont portées **dans `spatial.gpkg`** par des
  attributs éditables dans QGIS, plutôt que codées en dur. Exemple sur
  `land_use` : `charte_classe` (espace vert public, agriculture, perméable,
  usage compatible aléa, réserve foncière, habitat, équipement…) et
  `coef_biotope`. Initialisation depuis `Layer`/`zone`/`CU` pour les cas non
  ambigus ; les valeurs saisies dans QGIS sont conservées (règle AGENTS.md).
- Même principe pour la voirie (`modes_doux` par profil) et les équipements
  (`panier_services`).
- Surfaces et longueurs toujours recalculées depuis les géométries
  (EPSG:32628), jamais lues dans des colonnes persistées.
- Nouvelles couches ponctuelles éditables : `transit_stops`, `waste_points`.

### Codage des colonnes charte de `spatial.gpkg` (implémenté)

Les colonnes ont été ajoutées à `data/scenarios/scenario_01/spatial.gpkg`
le 1er octobre 2026, initialisées par la règle par défaut (copie préalable :
`archive/spatial_avant_colonnes_charte_20261001.gpkg`). Elles sont éditables
dans QGIS ; une cellule vide reprend la règle par défaut et est signalée dans
les contrôles. La fonction `write_charte_columns()` ajoute les colonnes
manquantes et ne remplit que les cellules vides.

Couche `land_use` — 1 = l'entité appartient à la classe, 0 = non :

| Colonne | Signification | Indicateurs |
|---|---|---|
| `ch_ev_public` | Espace vert public accessible | TV-1 |
| `ch_agriculture` | Agriculture nourricière (maraîchage, vergers, jardins) | TV-3 |
| `ch_alea_compatible` | Usage compatible avec l'aléa fort (non bâti : parc inondable, agriculture, loisirs, bassins) | TB-1 |
| `ch_habitat` | Surface urbanisée d'habitat (parcelles résidentielles) | VC-1a, VC-1b |
| `ch_ecole` | Équipement scolaire du panier de services | VC-2 |
| `ch_sante` | Équipement de santé du panier de services | VC-2 |
| `ch_commerce` | Commerce ou marché du panier de services | VC-2 |
| `ch_pieton` | Espace public piéton (place, square, mail) | CV-1 |
| `ch_sensible` | Équipement sensible au bruit et à l'aléa (école, santé) | CV-4, RES-3 |
| `ch_reserve` | Réserve foncière pour des besoins futurs | RES-1a |
| `ch_coef_biotope` | Coefficient de biotope de 0 à 1 (1 pleine terre, 0,15–0,5 perméable, 0 imperméable) | TB-2 |

Couche `road_footprints` : `ch_modes_doux` (0/1), cheminement doux sûr et
continu (VC-4, CV-1).

Règle par défaut d'initialisation (par valeur de `Layer`, ou `CU` pour
l'habitat) :

| Layer / CU | Colonnes à 1 | ch_coef_biotope |
|---|---|---|
| square, Maille | ch_ev_public, ch_pieton, ch_alea_compatible | 1 |
| Parc, Foret | ch_ev_public, ch_alea_compatible | 1 |
| TVB Hydraulique, TVBP, Buffer | ch_alea_compatible | 1 |
| Jardins familiaux | ch_ev_public, ch_agriculture, ch_alea_compatible | 1 |
| Parcelles Agricoles | ch_agriculture, ch_alea_compatible | 1 |
| Equipement Sportif | ch_alea_compatible | 0 |
| Equipement Scolaire | ch_ecole, ch_sensible | 0 |
| Equipement de Santé | ch_sante, ch_sensible | 0 |
| Parcelles Tertiaire Commerce, Parcelles Mixte | ch_commerce | 0 |
| CU « Superficie foncière de Habitat et ses annexes » | ch_habitat | 0 |
| Autres (Parking, Voirie, Servitude RN1, Hotel, Administration, Equipement Culturel, Gare et CBD, Station Service) | aucune | 0 |

`ch_modes_doux` = 1 par défaut pour « Rue piétonne partagée » et « Le mail
planté ».

Références fixées : surface de référence = titre foncier (110 ha) ; zone
d'aléa fort = `flood_areas` de types Lit mineur, Lit moyen (à saisir dans
QGIS), Rétention 50 cm et Cuvette de rétention.

### Population (implémentée)

- Hypothèse produit `persons_per_unit` (« Personnes par unité ») dans
  l'onglet Hypothèses. Vide = non renseigné ; 0 pour les produits sans
  habitants (rc_2_com, rt_1_com, ec, ep, el, ev).
- Calcul par ligne de programmation (quartier × produit) : quantité ×
  personnes par unité, puis totaux par produit, par quartier et total.
- Un produit programmé sans ratio rend la population « incomplète » ; il
  n'est jamais remplacé par une valeur plausible.
- Affichage : tableau des produits (colonne « Population (hab) »), tableau
  de bord (« Population totale » et colonne par quartier), intrant
  « Population du programme » de la charte. L'attribut `population` des
  quartiers ne sert plus que de contrôle.

## 4. Architecture

- `data/charte/referentiel_ecodekk.yml` + `R/charte_referentiel.R` :
  dimensions, thématiques, notions (textes), indicateurs (unité, référence,
  cible, seuils, sens : croissant / décroissant / plage). Extraction unique
  depuis xlsx/docx par un script dans `scripts/` ; pas de dépendance
  d'exécution au classeur.
- `R/charte_inputs.R` : `collect_charte_inputs(state, spatial, financial_model,
  umep)` → une ligne par intrant (valeur, source, statut, identifiants des
  entités retenues, géométrie dérivée éventuelle). Fonctions pures.
- `R/charte_scoring.R` : `score_indicator()`, `compute_charte()` → valeur,
  note, moyennes par axe, note globale, taux de complétude.
- `model.rds` : `charte_assumptions` et `charte_manual_inputs` (NA ≠ 0),
  incrément de `schema_version` + migration.
- `exports/charte_indicateurs.csv` et `exports/charte_intrants.csv`.
- Tests `testthat` : seuils, sens de notation, valeurs manquantes, chaque
  intrant SIG sur géométries de test, agrégations par axe.

### Navigation

Nouvel onglet **Accueil** (gestion du scénario déplacée depuis le Tableau de
bord) avec trois entrées, reprises dans trois `navbarMenu` :

1. **Programme et bilan** : Tableau de bord, Hypothèses, Programmation, Bilan,
   Cartographie.
2. **Simulations et analyses** : Confort thermique, Climat urbain, Bilan
   énergétique.
3. **Charte de performance** : Synthèse (radar 8 axes référence / projet /
   cible, note globale, complétude), 8 fiches thématiques, **Intrants de la
   charte** (section 3), Saisies et hypothèses, Méthode et sources.

## 5. Phasage

1. Accueil et regroupement de la navigation (sans modification du moteur).
2. Référentiel + notation, corrections du classeur, tests.
3. Attributs charte dans `spatial.gpkg` + intrants SIG/PRG/BIL + page
   **Intrants de la charte** (revue cartographique, aller-retour QGIS), tests.
4. Hypothèses et saisies dans `model.rds`, fiches thématiques, synthèse,
   exports CSV.
5. Couches arrêts TC et points de collecte, ombre SOLWEIG pour CV-1,
   comparaison au scénario de référence (ECO-1, RES-2, CO-x).

## 6. Décisions en attente

1. ~~Zone d'aléa fort~~ — décidé : zones inondables de `flood_areas` (lit
   mineur, lit moyen, rétention 50 cm, cuvettes de rétention) ; le lit moyen
   reste à saisir dans QGIS. Surface de référence : titre foncier (110 ha).
2. ~~Population~~ — décidé : personnes par unité par produit (valeurs à
   saisir dans les hypothèses).
3. Cibles contradictoires : TB-1 (20 / 80 %), RES-2 (−20 / −50 %), définition
   d'ECO-1.
4. Produits sociaux/abordables (GOV-2) et évolutifs (RES-1b) : candidats
   actuels `rm_1a`, `rm_1b` (catégorie ECO) et `rc_1`.
5. Création des couches `transit_stops`, `waste_points` ; existence d'une
   réserve foncière dans le plan.
6. Scénario de référence « classique » pour les réductions en %.
7. Intégrer les textes des notions (questions, recommandations) dans
   l'application ou seulement les indicateurs.

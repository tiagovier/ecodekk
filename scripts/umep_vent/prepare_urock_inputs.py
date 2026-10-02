#!/usr/bin/env python3
"""Entrées URock dérivées (lecture seule de ombrage_arbres) :
- inputs/buildings.gpkg : bâtiments mono-couche, champ height_m (EPSG:32628) ;
- inputs/vegetation_saison_seche.gpkg / vegetation_saison_pluies.gpkg :
  houppiers circulaires (rayon = diameter / 2), crown_top = totheight,
  crown_base = trunkheight ; saison des pluies sans Faidherbia albida ;
- inputs/template_2m.tif : grille 2 m, même origine que dem/mnt_1m_etude.tif.
"""

from pathlib import Path

import pandas as _pd_compat
# pandas 3 (~/.local, dépendance d'UMEP) et geopandas 0.14 (Ubuntu) : garder le type
# objet pour le texte, que geopandas 0.14 sait écrire en GeoPackage.
_pd_compat.set_option("future.infer_string", False)
import geopandas as gpd
from osgeo import gdal

gdal.UseExceptions()
gdal.SetConfigOption("GDAL_PAM_ENABLED", "NO")
HERE = Path(__file__).resolve().parent
UMEP = HERE.parents[1] / "data/scenarios/scenario_01/exports/umep"
SRC = UMEP / "ombrage_arbres"
OUT = UMEP / "vent_confort/inputs"

OUT.mkdir(parents=True, exist_ok=True)
targets = [OUT / n for n in ("buildings.gpkg", "vegetation_saison_seche.gpkg",
                             "vegetation_saison_pluies.gpkg", "template_2m.tif")]
existing = [t for t in targets if t.exists()]
if existing:
    raise SystemExit(f"Sorties existantes, non écrasées : {existing}")

# URock (base H2) dimensionne les colonnes texte sur la première valeur : seuls
# des attributs numériques sont transmis ; la correspondance des identifiants
# est conservée dans des tables CSV.
buildings = gpd.read_file(SRC / "umep_inputs.gpkg", layer="buildings")[["building_id", "height_m", "geometry"]]
if buildings.crs.to_epsg() != 32628 or buildings["height_m"].isna().any() or (buildings["height_m"] <= 0).any():
    raise SystemExit("Bâtiments : CRS ou hauteurs invalides.")
buildings.insert(0, "uid", range(1, len(buildings) + 1))
buildings[["uid", "building_id"]].to_csv(OUT / "buildings_ids.csv", index=False)
buildings[["uid", "height_m", "geometry"]].to_file(targets[0], layer="buildings", driver="GPKG")

trees = gpd.read_file(SRC / "trees_thies_mature_seed20260930.gpkg")
if trees.crs.to_epsg() != 32628 or trees[["totheight", "trunkheight", "diameter"]].isna().any().any():
    raise SystemExit("Arbres : CRS ou dimensions invalides.")
crowns = gpd.GeoDataFrame({
    "uid": range(1, len(trees) + 1), "tree_id": trees["tree_id"], "species": trees["species"],
    "crown_top": trees["totheight"].astype(float), "crown_base": trees["trunkheight"].astype(float),
}, geometry=trees.geometry.buffer(trees["diameter"] / 2, resolution=8), crs=trees.crs)
crowns[["uid", "tree_id", "species"]].to_csv(OUT / "vegetation_ids.csv", index=False)
numeric = ["uid", "crown_top", "crown_base", "geometry"]
crowns[numeric].to_file(targets[1], layer="vegetation", driver="GPKG")
crowns.loc[crowns["species"] != "Faidherbia albida", numeric].to_file(targets[2], layer="vegetation", driver="GPKG")

gdal.Warp(str(targets[3]), str(SRC / "dem/mnt_1m_etude.tif"), xRes=2, yRes=2,
          outputBounds=(283091.0, 1633166.5, 284403.0, 1634538.5), resampleAlg="average",
          creationOptions=["COMPRESS=DEFLATE"])
print(len(buildings), "bâtiments ;", len(crowns), "houppiers (sèche) ;",
      int((crowns["species"] != "Faidherbia albida").sum()), "houppiers (pluies)")

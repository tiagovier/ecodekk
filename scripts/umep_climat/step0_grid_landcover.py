#!/usr/bin/env python3
"""Étape 0 du climat urbain : grille d'analyse de 100 m et occupations du sol
« énergie » à 1 m (classes UMEP) pour SUEWS et TARGET.

Entrées en lecture seule (étude ombrage_arbres) ; sorties dans
exports/umep/climat_urbain/{grid,landcover}/ ; aucune sortie n'est écrasée.
"""

from __future__ import annotations

import datetime as dt
import hashlib
import math
import sys
import tempfile
from pathlib import Path

import geopandas as gpd
import numpy as np
import pandas as pd
import shapely
import yaml
from osgeo import gdal

gdal.UseExceptions()
gdal.SetConfigOption("GDAL_PAM_ENABLED", "NO")

ROOT = Path(__file__).resolve().parents[2]
UMEP = ROOT / "data/scenarios/scenario_01/exports/umep"
SRC = UMEP / "ombrage_arbres"
OUT = UMEP / "climat_urbain"
DEM = SRC / "dem/mnt_1m_etude.tif"
INPUTS = SRC / "umep_inputs.gpkg"
TREES = SRC / "trees_thies_mature_seed20260930.gpkg"
ORIGIN = (283091.0, 1634538.5)
CELL = 100.0
CRS = 32628
DECIDUOUS = {"Faidherbia albida", "Mitragyna inermis"}
RAINY_EXCLUDED = {"Faidherbia albida"}
# Classes UMEP : 1 revêtu, 2 bâtiment, 3 arbres sempervirents, 4 arbres
# caducifoliés, 6 sol nu, 7 eau. Pas d'herbe (5).
PAVED, BUILDING, EVERGREEN, DECIDUOUS_CLASS, BARE, WATER = 1, 2, 3, 4, 6, 7
NODATA = 255  # aucune classe ne l'utilise


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with open(path, "rb") as handle:
        for block in iter(lambda: handle.read(1 << 20), b""):
            digest.update(block)
    return digest.hexdigest()


def need_absent(*paths: Path) -> None:
    for path in paths:
        if path.exists():
            sys.exit(f"Sortie existante, non écrasée : {path}")


def make_grid(quartiers: gpd.GeoDataFrame) -> gpd.GeoDataFrame:
    area = shapely.union_all(shapely.make_valid(quartiers.geometry.to_numpy()))
    xmin, ymin, xmax, ymax = area.bounds
    x0, y0 = ORIGIN
    col0 = math.floor((xmin - x0) / CELL)
    col1 = math.ceil((xmax - x0) / CELL)
    row0 = math.floor((y0 - ymax) / CELL)
    row1 = math.ceil((y0 - ymin) / CELL)
    records = []
    for row in range(row0, row1):
        for col in range(col0, col1):
            left, top = x0 + col * CELL, y0 - row * CELL
            box = shapely.box(left, top - CELL, left + CELL, top)
            inside = box.intersection(area).area
            if inside <= 0:
                continue
            records.append({"cell_id": f"c_{col:02d}_{row:02d}", "col": col, "row": row,
                            "share_in_quartiers": round(inside / box.area, 4), "geometry": box})
    grid = gpd.GeoDataFrame(records, crs=CRS)
    q = quartiers[["district_label", "geometry"]].copy()
    q["geometry"] = shapely.make_valid(q.geometry.to_numpy())
    overlay = gpd.overlay(grid[["cell_id", "geometry"]], q, how="intersection", keep_geom_type=True)
    overlay["a"] = overlay.area
    majority = overlay.sort_values("a", ascending=False).drop_duplicates("cell_id")
    grid = grid.merge(majority[["cell_id", "district_label"]], on="cell_id", how="left")
    grid = grid.rename(columns={"district_label": "quartier_majoritaire"})
    grid["grid_id"] = np.arange(1, len(grid) + 1, dtype=np.int32)  # identifiant entier pour UMEP
    return grid[["grid_id", "cell_id", "col", "row", "share_in_quartiers", "quartier_majoritaire", "geometry"]]


def burn(target: gdal.Dataset, frame: gpd.GeoDataFrame, value: int | None = None,
         attribute: str | None = None) -> None:
    """Grave une valeur (ou un attribut) dans l'ordre des lignes : la dernière
    entité l'emporte. CRS du raster conservé via un GeoPackage temporaire."""
    if frame.empty:
        return
    columns = ["geometry"] + ([attribute] if attribute else [])
    with tempfile.TemporaryDirectory() as tmp:
        path = Path(tmp) / "burn.gpkg"
        frame[columns].to_file(path, layer="burn", driver="GPKG")
        source = gdal.OpenEx(str(path), gdal.OF_VECTOR)
        if attribute:
            gdal.RasterizeLayer(target, [1], source.GetLayer(0), options=[f"ATTRIBUTE={attribute}"])
        else:
            gdal.RasterizeLayer(target, [1], source.GetLayer(0), burn_values=[value])
        source = None


def landcover(template: gdal.Dataset, layers: dict, trees: gpd.GeoDataFrame | None, rainy: bool,
              out: Path) -> None:
    """Priorité par ordre de gravure : sol nu < eau < revêtu < arbres < bâtiment."""
    drv = gdal.GetDriverByName("GTiff")
    ds = drv.Create(str(out), template.RasterXSize, template.RasterYSize, 1, gdal.GDT_Byte,
                    options=["COMPRESS=DEFLATE", "TILED=YES"])
    ds.SetGeoTransform(template.GetGeoTransform())
    ds.SetProjection(template.GetProjection())
    band = ds.GetRasterBand(1)
    band.SetNoDataValue(NODATA)  # requis par Land Cover Fraction (Grid)
    band.Fill(BARE)
    if rainy:
        burn(ds, layers["flood_areas"], WATER)
    burn(ds, layers["road_footprints"], PAVED)
    if trees is not None:
        discs = trees.copy()
        discs["geometry"] = shapely.buffer(discs.geometry.to_numpy(), discs["diameter"].to_numpy() / 2, quad_segs=8)
        # Chevauchement : le houppier le plus haut (vu du ciel) l'emporte.
        discs["lc_class"] = np.where(discs["species"].isin(DECIDUOUS), DECIDUOUS_CLASS, EVERGREEN).astype(np.int32)
        burn(ds, discs.sort_values(["totheight", "tree_id"]), attribute="lc_class")
    burn(ds, layers["buildings"], BUILDING)
    ds.FlushCache()
    ds = None


def main() -> int:
    grid_dir, lc_dir = OUT / "grid", OUT / "landcover"
    grid_path = grid_dir / "grille_100m.gpkg"
    variants = {
        "saison_seche_arbres": (False, True),
        "saison_pluies_arbres": (True, True),
        "saison_seche_sans_arbres": (False, False),
        "saison_pluies_sans_arbres": (True, False),
    }
    lc_paths = {v: lc_dir / f"lc_energie_{v}.tif" for v in variants}
    need_absent(grid_path, *lc_paths.values())
    grid_dir.mkdir(parents=True, exist_ok=True)
    lc_dir.mkdir(parents=True, exist_ok=True)

    layers = {name: gpd.read_file(INPUTS, layer=name).to_crs(CRS)
              for name in ("buildings", "road_footprints", "flood_areas", "quartiers")}
    for name in ("buildings", "road_footprints", "flood_areas"):
        layers[name]["geometry"] = shapely.make_valid(layers[name].geometry.to_numpy())
        layers[name] = layers[name][["geometry"]]
    grid = make_grid(layers["quartiers"])
    grid.to_file(grid_path, layer="grille_100m", driver="GPKG")

    trees = gpd.read_file(TREES, layer="trees_umep").to_crs(CRS)[["tree_id", "species", "diameter", "totheight", "geometry"]]
    if trees["diameter"].isna().any() or (trees["diameter"] <= 0).any():
        sys.exit("Diamètre de houppier absent ou invalide.")
    template = gdal.Open(str(DEM))
    stats = {}
    for variant, (rainy, with_trees) in variants.items():
        selected = None
        if with_trees:
            selected = trees[~trees["species"].isin(RAINY_EXCLUDED)] if rainy else trees
        landcover(template, layers, selected, rainy, lc_paths[variant])
        check = gdal.Open(str(lc_paths[variant]))
        if (check.RasterXSize, check.RasterYSize) != (template.RasterXSize, template.RasterYSize) or \
                check.GetGeoTransform() != template.GetGeoTransform():
            sys.exit(f"Grille non conforme : {lc_paths[variant]}")
        values, counts = np.unique(check.ReadAsArray(), return_counts=True)
        stats[variant] = {int(v): round(float(c) / float(counts.sum()), 4) for v, c in zip(values, counts)}
        stats[variant]["n_trees"] = int(0 if selected is None else len(selected))

    run = {
        "generated_at": dt.datetime.now(dt.timezone.utc).isoformat(timespec="seconds"),
        "tool": "scripts/umep_climat/step0_grid_landcover.py",
        "versions": {"python": sys.version.split()[0], "gdal": gdal.__version__,
                     "geopandas": gpd.__version__, "shapely": shapely.__version__},
        "inputs": {p.name: {"path": str(p.relative_to(ROOT)), "sha256": sha256(p)}
                   for p in (DEM, INPUTS, TREES)},
        "grid": {"path": str(grid_path.relative_to(ROOT)), "cell_m": CELL, "origin": list(ORIGIN),
                 "crs": f"EPSG:{CRS}", "cells": int(len(grid)),
                 "rule": "cellules de 100 m intersectant l'union des quartiers ; quartier majoritaire par surface"},
        "landcover": {
            "classes": {1: "revêtu (emprises de voirie, pavés autoblocants)", 2: "bâtiment",
                        3: "arbres sempervirents", 4: "arbres caducifoliés (Faidherbia albida, Mitragyna inermis)",
                        6: "sol nu", 7: "eau (zones inondables, saison des pluies)"},
            "priority": "bâtiment > arbres > revêtu > eau > sol nu ; pas d'herbe",
            "nodata": NODATA,
            "trees": "disques de houppier (diamètre attribué) ; chevauchement : houppier le plus haut ; saison des pluies sans Faidherbia albida",
            "variants": {v: str(p.relative_to(ROOT)) for v, p in lc_paths.items()},
            "fractions_domaine": stats,
        },
    }
    (OUT / "step0_run.yml").write_text(yaml.safe_dump(run, allow_unicode=True, sort_keys=False), encoding="utf-8")
    print(yaml.safe_dump({"cells": len(grid), "fractions": stats}, allow_unicode=True, sort_keys=False))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

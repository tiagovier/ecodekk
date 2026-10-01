#!/usr/bin/env python3
"""Préparation de l'étude SUEWS (bilan_energetique) pour scenario_01.

Produit, sans rien écraser :
- inputs/cells.csv : par maille (grid_id) et variante d'occupation du sol,
  fractions de surface, hauteurs (bâtiments, arbres), altitude, population et
  densité ;
- inputs/forcing_2016_2018.pkl : forçage ERA5 50 m (2016-01-01 → 2018-01-31) ;
- inputs/qf_profile.csv : profil horaire de QF par habitant (métabolisme +
  usage domestique, sans climatisation ni trafic).
"""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path

import geopandas as gpd
import numpy as np
import pandas as pd
import yaml
from osgeo import gdal, ogr

gdal.UseExceptions()
gdal.SetConfigOption("GDAL_PAM_ENABLED", "NO")

ROOT = Path(__file__).resolve().parents[2]
UMEP = ROOT / "data/scenarios/scenario_01/exports/umep"
CLIMAT = UMEP / "climat_urbain"
OMBRAGE = UMEP / "ombrage_arbres"
OUT = UMEP / "bilan_energetique"
SAMPLE = Path(__import__("supy").__file__).parent / "sample_data/sample_config.yml"

VARIANTS = ["saison_seche_arbres", "saison_pluies_arbres", "saison_seche_sans_arbres", "saison_pluies_sans_arbres"]
PERSONS_PER_HOUSEHOLD = 6.4          # donnée utilisateur (2026-09-30)
DOMESTIC_W_PER_CAPITA = 30.0         # HYPOTHÈSE à confirmer (voir run.yml)
LC_COLUMNS = {"Paved": "paved", "Buildings": "bldgs", "EvergreenTrees": "evetr",
              "DecidiousTrees": "dectr", "Grass": "grass", "Baresoil": "bsoil", "Water": "water"}


def sha256(path: Path) -> str:
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for block in iter(lambda: f.read(1 << 20), b""):
            h.update(block)
    return h.hexdigest()


def read_array(path: Path) -> tuple[np.ndarray, gdal.Dataset]:
    ds = gdal.Open(str(path))
    return ds.GetRasterBand(1).ReadAsArray(), ds


def grid_raster(grid_path: Path, template: gdal.Dataset) -> np.ndarray:
    mem = gdal.GetDriverByName("MEM").Create("", template.RasterXSize, template.RasterYSize, 1, gdal.GDT_Int32)
    mem.SetGeoTransform(template.GetGeoTransform())
    mem.SetProjection(template.GetProjection())
    src = ogr.Open(str(grid_path))
    gdal.RasterizeLayer(mem, [1], src.GetLayer(0), options=["ATTRIBUTE=grid_id"])
    return mem.GetRasterBand(1).ReadAsArray()


def cell_means(values: np.ndarray, ids: np.ndarray, mask: np.ndarray, n: int) -> np.ndarray:
    sel = mask & (ids > 0)
    total = np.bincount(ids[sel], weights=values[sel], minlength=n + 1)
    count = np.bincount(ids[sel], minlength=n + 1)
    with np.errstate(invalid="ignore", divide="ignore"):
        return np.where(count > 0, total / count, np.nan)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.parse_args()
    inputs = OUT / "inputs"
    target = inputs / "cells.csv"
    if target.exists():
        raise SystemExit(f"Sortie existante, non écrasée : {target}")
    inputs.mkdir(parents=True, exist_ok=True)

    grid = gpd.read_file(CLIMAT / "grid/grille_100m.gpkg").sort_values("grid_id")
    n = int(grid["grid_id"].max())
    dem, dem_ds = read_array(OMBRAGE / "dem/mnt_1m_etude.tif")
    ids = grid_raster(CLIMAT / "grid/grille_100m.gpkg", dem_ds)
    alt = cell_means(dem.astype(float), ids, dem > -9000, n)

    # Hauteurs des bâtiments : morphométrie (zH, pai, fai) de l'étape 0.
    morph = pd.read_csv(CLIMAT / "morphometry/saison_seche/grid__IMPGrid_isotropic.txt", sep=r"\s+")
    morph = morph.rename(columns={"id": "grid_id"})

    # Population : logements du programme × 6,4 personnes, par centroïde de bâtiment.
    units = pd.read_csv(inputs / "logements_batiments.csv")
    pts = gpd.GeoDataFrame(units, geometry=gpd.points_from_xy(units.x, units.y), crs=32628)
    joined = gpd.sjoin(pts, grid[["grid_id", "geometry"]], predicate="within", how="left")
    outside = int(joined["grid_id"].isna().sum())
    logements = joined.groupby("grid_id")["logements"].sum()

    rows = []
    for variant in VARIANTS:
        season = "saison_seche" if variant.startswith("saison_seche") else "saison_pluies"
        lc_frac = pd.read_csv(CLIMAT / f"lc_fractions/{variant}_umep7/lc__LCFG_isotropic.txt", sep=r"\s+")
        lc_frac = lc_frac.rename(columns={"ID": "grid_id", **LC_COLUMNS})
        lc, _ = read_array(CLIMAT / f"landcover/lc_energie_{variant}.tif")
        cdsm, _ = read_array(OMBRAGE / f"rasters_1m/cdsm_{season}.tif")
        eve_h = cell_means(cdsm.astype(float), ids, lc == 3, n)
        dec_h = cell_means(cdsm.astype(float), ids, lc == 4, n)
        frame = grid[["grid_id", "cell_id", "quartier_majoritaire", "share_in_quartiers"]].merge(lc_frac, on="grid_id")
        frame = frame.merge(morph[["grid_id", "pai", "fai", "zH", "zd", "z0"]], on="grid_id", how="left")
        frame["variant"] = variant
        frame["alt_m"] = alt[frame["grid_id"]]
        frame["evetreeh_m"] = eve_h[frame["grid_id"]]
        frame["dectreeh_m"] = dec_h[frame["grid_id"]]
        frame["logements"] = frame["grid_id"].map(logements).fillna(0.0)
        frame["population"] = frame["logements"] * PERSONS_PER_HOUSEHOLD
        frame["popdens_hab_ha"] = frame["population"] / 1.0  # maille de 1 ha
        rows.append(frame)
    cells = pd.concat(rows, ignore_index=True)
    centroids = grid.to_crs(4326).geometry.centroid
    cells["lat"] = cells["grid_id"].map(dict(zip(grid["grid_id"], centroids.y.round(6))))
    cells["lng"] = cells["grid_id"].map(dict(zip(grid["grid_id"], centroids.x.round(6))))
    fraction_sum = cells[list(LC_COLUMNS.values())].sum(axis=1)
    if (abs(fraction_sum - 1) > 0.005).any():
        raise SystemExit("Somme des fractions différente de 1 dans une maille.")
    cells.to_csv(target, index=False)

    # Profil horaire de QF par habitant (W/hab), heure de fin de pas 1..24.
    sample = yaml.safe_load(SAMPLE.read_text())
    heat = sample["sites"][0]["properties"]["anthropogenic_emissions"]
    act = np.array([heat["co2"]["humactivity_24hr"]["working_day"][str(h)] for h in range(1, 25)], float)
    ahp = np.array([heat["heat"]["ahprof_24hr"]["working_day"][str(h)] for h in range(1, 25)], float)
    qmin = heat["co2"]["minqfmetab"]["value"]
    qmax = heat["co2"]["maxqfmetab"]["value"]
    activity = (act - act.min()) / (act.max() - act.min())
    profile = pd.DataFrame({
        "hour": range(1, 25),
        "metabolism_w_per_capita": qmin + (qmax - qmin) * activity,
        "domestic_w_per_capita": DOMESTIC_W_PER_CAPITA * ahp / ahp.mean(),
    })
    profile["total_w_per_capita"] = profile.metabolism_w_per_capita + profile.domestic_w_per_capita
    profile.round(3).to_csv(inputs / "qf_profile.csv", index=False)

    # Forçage ERA5 50 m, 2016-01-01 → 2018-01-31 (période d'essai + analyse).
    met_dir = CLIMAT / "met/era5_50m"
    frames = []
    for year in (2016, 2017, 2018):
        path = next(met_dir.glob(f"*_{year}_data_60.txt"))
        frames.append(pd.read_csv(path, sep=r"\s+"))
    forcing = pd.concat(frames, ignore_index=True)
    stamp = (pd.to_datetime(forcing["iy"].astype(str), format="%Y") + pd.to_timedelta(forcing["id"] - 1, unit="D")
             + pd.to_timedelta(forcing["it"], unit="h") + pd.to_timedelta(forcing["imin"], unit="min"))
    keep = (stamp > pd.Timestamp("2016-01-01 00:00")) & (stamp <= pd.Timestamp("2018-02-01 00:00"))
    forcing = forcing[keep].reset_index(drop=True)
    forcing.to_pickle(inputs / "forcing_2016_2018.pkl")

    summary = {
        "cells": int(grid.shape[0]),
        "logements_total": float(units["logements"].sum()),
        "logements_hors_grille": float(joined.loc[joined["grid_id"].isna(), "logements"].sum()),
        "batiments_hors_grille": outside,
        "population_programme": float(units["logements"].sum() * PERSONS_PER_HOUSEHOLD),
        "mailles_habitees": int((cells.query("variant == 'saison_seche_arbres'")["population"] > 0).sum()),
        "forcing_hours": int(len(forcing)),
        "inputs": {
            "grid": sha256(CLIMAT / "grid/grille_100m.gpkg"),
            "dem": sha256(OMBRAGE / "dem/mnt_1m_etude.tif"),
            "logements": sha256(inputs / "logements_batiments.csv"),
        },
    }
    (inputs / "prepare_summary.json").write_text(json.dumps(summary, ensure_ascii=False, indent=1), encoding="utf-8")
    print(json.dumps(summary, ensure_ascii=False))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

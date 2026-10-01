#!/usr/bin/env python3
"""Produits d'affichage TARGET (îlot de chaleur) pour l'application.

Lit les sorties CSV horaires des 6 runs (target/prepare/<site>/output/csv/) et
écrit dans display_target/ :
- target_cells.csv : par maille de 100 m et par run, Ta et UTCI à 14 h et
  maximum journalier, intensité d'îlot (Ta − Tb_rur) ;
- target_cells_effet_arbres.csv : écarts avec arbres − sans arbres par maille ;
- target_quartiers.csv : mêmes indicateurs pondérés par la surface
  (intersection maille ∩ quartier) ;
- target_hourly_quartiers.csv : profils horaires pondérés par quartier ;
- grille_100m.geojson (EPSG:4326) et manifest.json.
Masque (décision utilisateur du 2026-09-30) : une maille est masquée pour une
journée, dans les deux runs, si l'écart avec − sans arbres dépasse 5 K sur Ta
ou 3 K sur UTCI (à 14 h ou sur le maximum journalier) ; ces valeurs sortent du
domaine de validité de TARGET (effondrement du vent modélisé sous couvert
dense). Les mailles masquées sont exclues des moyennes et profils par quartier.
display_target/ existant n'est pas écrasé.
"""

from __future__ import annotations

import datetime as dt
import hashlib
import json
import sys
from pathlib import Path

import geopandas as gpd
import numpy as np
import pandas as pd
import shapely

ROOT = Path(__file__).resolve().parents[2]
C = ROOT / "data/scenarios/scenario_01/exports/umep/climat_urbain"
SRC = ROOT / "data/scenarios/scenario_01/exports/umep/ombrage_arbres"
OUT = C / "display_target"
DAYS = {
    "chaud_saison_seche": ("2017-04-14", "saison_seche", "Journée chaude de saison sèche"),
    "saison_pluies": ("2017-09-21", "saison_pluies", "Journée de saison des pluies"),
    "frais_saison_seche": ("2018-01-17", "saison_seche", "Journée fraîche de saison sèche"),
}
VEGETATION = {"arbres": "Avec arbres", "sans_arbres": "Sans arbres (référence)"}
PEAK_HOUR = 14
MASK_TA_K = 5.0
MASK_UTCI_K = 3.0
VARIABLES = ["Ta", "UTCI", "Tmrt", "Ws", "Tb_rur"]


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def read_run(day: str, veg: str) -> pd.DataFrame:
    date, season, _ = DAYS[day]
    run = f"{day}_{veg}"
    csv_dir = C / "target/prepare" / f"{season}_{veg}" / "output/csv"
    frames = []
    for hour in range(24):
        path = csv_dir / f"{run}_{date} {hour:02d}_00_00.csv"
        if not path.exists():
            sys.exit(f"Pas horaire absent : {path.name}")
        frame = pd.read_csv(path, sep=";")
        frame["hour"] = hour
        frames.append(frame)
    data = pd.concat(frames, ignore_index=True).rename(columns={"ID": "grid_id"})
    data["grid_id"] = data["grid_id"].astype(int)
    data["day"], data["vegetation"] = day, veg
    return data


def weighted(frame: pd.DataFrame, weights: pd.DataFrame, keys: list[str], values: list[str]) -> pd.DataFrame:
    merged = frame.merge(weights, on="grid_id")
    rows = []
    for key, group in merged.groupby(keys + ["quartier"]):
        w = group["weight_m2"].to_numpy()
        row = dict(zip(keys + ["quartier"], key))
        row["surface_m2"] = round(float(weights.loc[weights["quartier"] == row["quartier"], "weight_m2"].sum()), 0)
        for value in values:
            row[value] = round(float(np.average(group[value], weights=w)), 2)
        rows.append(row)
    return pd.DataFrame(rows)


def main() -> int:
    if OUT.exists() and any(OUT.iterdir()):
        sys.exit(f"Sortie existante, non écrasée : {OUT}")
    OUT.mkdir(parents=True, exist_ok=True)
    grid = gpd.read_file(C / "grid/grille_100m.gpkg", layer="grille_100m")
    quartiers = gpd.read_file(SRC / "umep_inputs.gpkg", layer="quartiers").to_crs(grid.crs)
    quartiers["geometry"] = shapely.make_valid(quartiers.geometry.to_numpy())
    pieces = gpd.overlay(grid[["grid_id", "geometry"]], quartiers[["district_label", "geometry"]],
                         how="intersection", keep_geom_type=True)
    pieces["weight_m2"] = pieces.area
    weights = (pieces.groupby(["grid_id", "district_label"], as_index=False)["weight_m2"].sum()
               .rename(columns={"district_label": "quartier"}))

    hourly = pd.concat([read_run(d, v) for d in DAYS for v in VEGETATION], ignore_index=True)
    ids = set(hourly["grid_id"])
    if ids != set(grid["grid_id"]):
        sys.exit("Les identifiants TARGET ne correspondent pas à la grille.")
    hourly["uhi"] = hourly["Ta"] - hourly["Tb_rur"]

    keys = ["grid_id", "day", "vegetation"]
    peak = hourly[hourly["hour"] == PEAK_HOUR].set_index(keys)
    daily_max = hourly.groupby(keys)[["Ta", "UTCI", "uhi"]].max()
    cells = pd.DataFrame({
        "ta_14h_c": peak["Ta"], "utci_14h_c": peak["UTCI"], "tmrt_14h_c": peak["Tmrt"],
        "uhi_14h_k": peak["uhi"], "ta_max_c": daily_max["Ta"], "utci_max_c": daily_max["UTCI"],
        "uhi_max_k": daily_max["uhi"], "tb_rur_14h_c": peak["Tb_rur"], "ws_14h_m_s": peak["Ws"],
    }).round(2).reset_index()
    cells = cells.merge(grid[["grid_id", "cell_id", "quartier_majoritaire"]], on="grid_id")
    cells.to_csv(OUT / "target_cells.csv", index=False)

    indicators = ["ta_14h_c", "utci_14h_c", "tmrt_14h_c", "uhi_14h_k", "ta_max_c", "utci_max_c", "uhi_max_k"]
    with_trees = cells[cells["vegetation"] == "arbres"].set_index(["grid_id", "day"])[indicators]
    without = cells[cells["vegetation"] == "sans_arbres"].set_index(["grid_id", "day"])[indicators]
    effect = (with_trees - without).round(2).add_prefix("delta_").reset_index()
    effect = effect.merge(grid[["grid_id", "cell_id", "quartier_majoritaire"]], on="grid_id")
    # Fractions de la maille (variante avec arbres de la saison) pour interpréter
    # les écarts extrêmes des mailles presque entièrement boisées.
    fractions = []
    for day, (_, season, _) in DAYS.items():
        lc = pd.read_csv(C / "target/prepare" / f"{season}_arbres/input/LC/lc_target.txt", skipinitialspace=True)
        fractions.append(pd.DataFrame({"grid_id": lc["FID"].astype(int), "day": day,
                                       "fraction_arbres": lc["Veg"].round(3), "fraction_bati": lc["roof"].round(3)}))
    effect = effect.merge(pd.concat(fractions), on=["grid_id", "day"])
    masked = ((effect[["delta_ta_14h_c", "delta_ta_max_c"]].abs() > MASK_TA_K).any(axis=1)
              | (effect[["delta_utci_14h_c", "delta_utci_max_c"]].abs() > MASK_UTCI_K).any(axis=1))
    effect["masque"] = masked
    mask = effect.loc[masked, ["grid_id", "day"]].assign(masque=True)
    cells = cells.merge(mask, on=["grid_id", "day"], how="left")
    cells["masque"] = cells["masque"].fillna(False).astype(bool)
    cells.to_csv(OUT / "target_cells.csv", index=False)
    effect.to_csv(OUT / "target_cells_effet_arbres.csv", index=False)
    hourly = hourly.merge(mask, on=["grid_id", "day"], how="left")
    hourly = hourly[hourly["masque"].isna()].drop(columns="masque")

    quartier_table = weighted(cells[~cells["masque"]], weights, ["day", "vegetation"], indicators)
    quartier_table.to_csv(OUT / "target_quartiers.csv", index=False)
    hourly_q = weighted(hourly.rename(columns={"Ta": "ta_c", "UTCI": "utci_c", "uhi": "uhi_k"}),
                        weights, ["day", "vegetation", "hour"], ["ta_c", "utci_c", "uhi_k"])
    rural = hourly.groupby(["day", "vegetation", "hour"], as_index=False)["Tb_rur"].first()
    hourly_q = hourly_q.merge(rural.rename(columns={"Tb_rur": "tb_rur_c"}), on=["day", "vegetation", "hour"])
    hourly_q.drop(columns="surface_m2").to_csv(OUT / "target_hourly_quartiers.csv", index=False)

    grid_4326 = grid.to_crs(4326)[["grid_id", "cell_id", "quartier_majoritaire", "share_in_quartiers", "geometry"]]
    grid_4326.to_file(OUT / "grille_100m.geojson", driver="GeoJSON", COORDINATE_PRECISION=7)

    manifest = {
        "generated_at": dt.datetime.now(dt.timezone.utc).isoformat(timespec="seconds"),
        "scenario": "scenario_01",
        "study": "climat_urbain",
        "model": "UMEP TARGET (expérimental), target-py 0.2.1",
        "grid": {"file": "grille_100m.geojson", "cells": int(len(grid)), "cell_m": 100, "crs": "EPSG:4326"},
        "peak_hour": PEAK_HOUR,
        "hour_convention": "heure UTC = heure locale ; horodatage du forçage ERA5 (fin de pas)",
        "days": [{"id": d, "date": v[0], "label": v[2]} for d, v in DAYS.items()],
        "vegetation": [{"id": v, "label": l} for v, l in VEGETATION.items()],
        "files": {
            "target_cells.csv": "par maille et run : Ta, UTCI, Tmrt, intensité d'îlot à 14 h et maxima",
            "target_cells_effet_arbres.csv": "par maille et journée : avec arbres − sans arbres",
            "target_quartiers.csv": "par quartier (pondération surface maille ∩ quartier)",
            "target_hourly_quartiers.csv": "profils horaires pondérés par quartier",
        },
        "mask": {
            "rule": f"maille masquée pour une journée si |Δ Ta| > {MASK_TA_K:g} K ou |Δ UTCI| > {MASK_UTCI_K:g} K (avec − sans arbres, à 14 h ou maximum journalier), dans les deux runs",
            "reason": "hors du domaine de validité de TARGET : sous couvert arboré dense, le modèle réduit fortement le vent et produit des écarts non physiques",
            "masked_cell_days": {d: int(masked[effect["day"] == d].sum()) for d in DAYS},
            "decided": "2026-09-30, choix « masquer » de l'utilisateur",
        },
        "units": {"ta": "°C", "utci": "°C", "tmrt": "°C", "uhi": "K (Ta − Tb_rur)", "ws": "m/s"},
        "provenance": {
            "forcing": "ERA5 (CDS, série ponctuelle), Ta/HR diagnostiquées à 2 m, vent à 10 m ; 48 h de mise en route",
            "land_cover": "lc_energie_* (bâtiment, arbres, pavés autoblocants = béton à 100 %, eau, sol nu classé « herbe sèche » par TARGET)",
            "morphometry": "Morphometric Calculator (Grid), DSM bâti 1 m",
            "limits": "TARGET est expérimental et conçu pour des villes australiennes ; comparaison de scénarios plutôt que valeurs absolues.",
            "inputs_sha256": {p.name: sha256(p) for p in sorted((C / "met/target").glob("*.txt"))},
        },
    }
    (OUT / "manifest.json").write_text(json.dumps(manifest, ensure_ascii=False, indent=1), encoding="utf-8")
    print(f"Produits TARGET écrits : {OUT}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

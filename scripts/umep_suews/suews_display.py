#!/usr/bin/env python3
"""Produits d'affichage de l'étude SUEWS (bilan_energetique, scenario_01).

Composition saisonnière : de juin à octobre, résultats du run « saison des
pluies » (fractions avec eau dans les zones inondables, sans Faidherbia) ;
le reste de l'année, run « saison sèche ». SUEWS ne permet pas de faire
varier les fractions de surface dans un même run.

Écrit dans display/ (refuse d'écraser) :
- days_cells.csv, days_quartiers.csv : cycles horaires des 3 journées ;
- monthly_cells.csv, monthly_quartiers.csv : moyennes mensuelles ;
- validation.csv : bilan d'énergie, rapports de Bowen ;
- grille_100m.geojson (EPSG:4326) ; manifest.json.
"""

from __future__ import annotations

import datetime as dt
import json
import os
from pathlib import Path

import pandas as _pd_compat
# pandas 3 (~/.local, dépendance d'UMEP) et geopandas 0.14 (Ubuntu) : garder le type
# objet pour le texte, que geopandas 0.14 sait écrire en GeoPackage.
_pd_compat.set_option("future.infer_string", False)
import geopandas as gpd
import numpy as np
import pandas as pd

ROOT = Path(os.environ.get("ECODEKK_ROOT", Path(__file__).resolve().parents[2]))
UMEP = ROOT / "data/scenarios/scenario_01/exports/umep"
OUT = UMEP / "bilan_energetique"
RAINY_MONTHS = {6, 7, 8, 9, 10}
DAYS = {"chaud_saison_seche": "2017-04-14", "saison_pluies": "2017-09-21", "frais_saison_seche": "2018-01-17"}
# T2 reste dans les sorties brutes mais n'est pas affichée : le diagnostic
# à 2 m de SUEWS n'est pas fiable sous un couvert arboré haut (calibration v2).
VARS = ["QN", "QH", "QE", "QS", "QF", "RH2", "U10", "Kdown", "LAI", "SMD"]
LABELS = {
    "QN": "Rayonnement net Q* (W/m²)", "QH": "Flux de chaleur sensible QH (W/m²)",
    "QE": "Flux de chaleur latente QE (W/m²)", "QS": "Stockage de chaleur ΔQS (W/m²)",
    "QF": "Chaleur anthropique QF (W/m²)",
    "RH2": "Humidité relative à 2 m (%)", "U10": "Vent à 10 m (m/s)",
    "Kdown": "Rayonnement solaire incident (W/m²)", "LAI": "Indice foliaire (m²/m²)",
    "SMD": "Déficit d'humidité du sol (mm)",
}


def load_runs(cells: pd.DataFrame) -> dict[str, pd.DataFrame]:
    """Séries horaires composées par scénario (arbres / sans_arbres)."""
    series = {}
    for scenario in ("arbres", "sans_arbres"):
        frames = []
        for cell_id in cells["cell_id"].unique():
            dry = pd.read_csv(OUT / f"raw/saison_seche_{scenario}/{cell_id}.csv.gz", index_col=0, parse_dates=True)
            wet = pd.read_csv(OUT / f"raw/saison_pluies_{scenario}/{cell_id}.csv.gz", index_col=0, parse_dates=True)
            # Mois de l'heure de début du pas (index = fin de pas horaire).
            month = (dry.index - pd.Timedelta(minutes=30)).month
            rainy = np.isin(month, list(RAINY_MONTHS))
            composed = dry.copy()
            composed.loc[rainy] = wet.reindex(dry.index).loc[rainy]
            composed["cell_id"] = cell_id
            frames.append(composed)
        series[scenario] = pd.concat(frames)
    return series


def quartier_weights(grid: gpd.GeoDataFrame) -> pd.DataFrame:
    quartiers = gpd.read_file(UMEP / "ombrage_arbres/umep_inputs.gpkg", layer="quartiers")[["district_label", "geometry"]]
    inter = gpd.overlay(grid[["cell_id", "geometry"]], quartiers.to_crs(grid.crs), how="intersection")
    inter["weight_m2"] = inter.geometry.area
    return inter[["cell_id", "district_label", "weight_m2"]].rename(columns={"district_label": "quartier"})


def weighted(frame: pd.DataFrame, weights: pd.DataFrame, keys: list[str]) -> pd.DataFrame:
    merged = frame.merge(weights, on="cell_id")
    cols = [v for v in VARS if v in merged.columns]
    merged[cols] = merged[cols].mul(merged["weight_m2"], axis=0)
    sums = merged.groupby(["quartier", *keys])[[*cols, "weight_m2"]].sum()
    out = sums[cols].div(sums["weight_m2"], axis=0)
    return out.round(3).reset_index()


def with_difference(frames: dict[str, pd.DataFrame], keys: list[str]) -> pd.DataFrame:
    a = frames["arbres"].set_index(keys)
    b = frames["sans_arbres"].set_index(keys)
    diff = (a[VARS] - b[VARS]).round(3)
    out = []
    for name, data in (("arbres", a), ("sans_arbres", b), ("difference_arbres_moins_sans", diff)):
        data = data[VARS].copy()
        data["scenario"] = name
        out.append(data.reset_index())
    return pd.concat(out, ignore_index=True)


def main() -> int:
    display = OUT / "display"
    if display.exists() and any(display.iterdir()):
        raise SystemExit(f"Sortie existante, non écrasée : {display}")
    display.mkdir(parents=True, exist_ok=True)
    cells = pd.read_csv(OUT / "inputs/cells.csv")
    grid = gpd.read_file(UMEP / "climat_urbain/grid/grille_100m.gpkg")
    weights = quartier_weights(grid)
    series = load_runs(cells)

    days, monthly = {}, {}
    for scenario, data in series.items():
        start_hour = data.index - pd.Timedelta(minutes=30)
        parts = []
        for day_id, date in DAYS.items():
            sel = data[start_hour.normalize() == pd.Timestamp(date)].copy()
            sel["day"] = day_id
            sel["hour"] = sel.index.hour.where(sel.index.hour > 0, 24)
            parts.append(sel)
        days[scenario] = pd.concat(parts)[["cell_id", "day", "hour", *VARS]]
        month_key = (data.index - pd.Timedelta(minutes=30)).to_period("M").astype(str)
        monthly[scenario] = data.assign(month=month_key).groupby(["cell_id", "month"])[VARS].mean().round(3).reset_index()

    days_cells = with_difference(days, ["cell_id", "day", "hour"])
    monthly_cells = with_difference(monthly, ["cell_id", "month"])
    days_q = with_difference({k: weighted(v, weights, ["day", "hour"]) for k, v in days.items()}, ["quartier", "day", "hour"])
    monthly_q = with_difference({k: weighted(v, weights, ["month"]) for k, v in monthly.items()}, ["quartier", "month"])
    days_cells.to_csv(display / "days_cells.csv", index=False)
    monthly_cells.to_csv(display / "monthly_cells.csv", index=False)
    days_q.to_csv(display / "days_quartiers.csv", index=False)
    monthly_q.to_csv(display / "monthly_quartiers.csv", index=False)

    # Validation : bilan Q* + QF = QH + QE + ΔQS (résidu) et rapports de Bowen.
    rows = []
    for scenario, data in series.items():
        residual = data["QN"] + data["QF"] - data["QH"] - data["QE"] - data["QS"]
        daytime = data["Kdown"] > 200
        for period, months in (("saison_seche", {11, 12, 1, 2, 3, 4, 5}), ("saison_pluies", RAINY_MONTHS)):
            m = np.isin((data.index - pd.Timedelta(minutes=30)).month, list(months)) & daytime
            sub = data[m]
            rows.append({
                "scenario": scenario, "periode": period,
                "residu_moyen_w_m2": round(float(residual[m].mean()), 3),
                "residu_abs_max_w_m2": round(float(residual[m].abs().max()), 3),
                "bowen_median_jour": round(float((sub["QH"] / sub["QE"].where(sub["QE"].abs() > 5)).median()), 2),
                "qe_moyen_jour_w_m2": round(float(sub["QE"].mean()), 1),
                "qh_moyen_jour_w_m2": round(float(sub["QH"].mean()), 1),
                "lai_moyen": round(float(sub["LAI"].mean()), 2),
            })
    validation = pd.DataFrame(rows)
    validation.to_csv(display / "validation.csv", index=False)

    first = cells[cells.variant == "saison_seche_arbres"].set_index("cell_id")
    geo = grid.merge(first[["paved", "bldgs", "evetr", "dectr", "bsoil", "population", "popdens_hab_ha"]],
                     left_on="cell_id", right_index=True).to_crs(4326)
    geo[["cell_id", "grid_id", "quartier_majoritaire", "paved", "bldgs", "evetr", "dectr", "bsoil",
         "population", "popdens_hab_ha", "geometry"]].to_file(display / "grille_100m.geojson", driver="GeoJSON")

    manifest = {
        "generated_at": dt.datetime.now(dt.timezone.utc).isoformat(timespec="seconds"),
        "scenario": "scenario_01", "study": "bilan_energetique",
        "model": "SUEWS (supy 2026.4.3rc1, moteur Rust), pas de temps 300 s, sorties moyennées à l'heure",
        "periode_analyse": ["2017-01-01", "2018-01-31"], "periode_mise_en_route": "2016",
        "days": [{"id": k, "date": v} for k, v in DAYS.items()],
        "scenarios": {"arbres": "occupation du sol avec arbres attribués",
                      "sans_arbres": "arbres remplacés par la surface sous-jacente",
                      "difference_arbres_moins_sans": "avec arbres moins sans arbres"},
        "variables": LABELS,
        "heure": "heure de fin de pas, UTC = heure locale (1 à 24)",
        "composition_saisonniere": "juin–octobre : run saison des pluies ; autres mois : run saison sèche",
        "files": sorted(p.name for p in display.iterdir()),
        "avertissements": [
            "Calibration v2 (2026-10-01) : albédo du sol nu latéritique 0,30 (ordre de grandeur de la littérature, à confirmer) ; arbres alimentés par l'eau profonde (réserve racinaire de 1 000 mm sur 3 m, pleine à l'initialisation, fermeture stomatique reportée à 74 % de cette réserve). Les autres paramètres physiques (coefficients de stockage OHM, albédo des arbres, phénologie) restent ceux de l'exemple SUEWS (Londres, Ward et al. 2016).",
            "La température de l'air à 2 m n'est pas affichée : le diagnostic à 2 m de SUEWS n'est pas fiable sous un couvert arboré haut (hausse non physique observée même quand l'évaporation augmente).",
            "Les arbres (albédo 0,10) absorbent plus de rayonnement que le sol nu clair (0,30) : la chaleur sensible d'une maille boisée peut rester supérieure à celle du sol nu malgré l'évaporation. L'effet rafraîchissant ressenti au sol (ombre) relève de l'onglet Confort thermique.",
            "Chaleur anthropique : métabolisme (75 à 175 W/hab, défauts SUEWS) + usage domestique de 30 W/hab (hypothèse à confirmer), sans climatisation ni trafic.",
            "Population : logements du programme × 6,4 personnes par ménage.",
            "Forçage ERA5 (maille ~31 km) à 50 m : climat régional, pas de mesure locale ; aucune validation par observation.",
            "Résultats à lire surtout en comparaison (avec / sans arbres, entre mailles et quartiers).",
        ],
    }
    (display / "manifest.json").write_text(json.dumps(manifest, ensure_ascii=False, indent=1), encoding="utf-8")
    print(validation.to_string(index=False))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

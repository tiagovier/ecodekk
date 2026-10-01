#!/usr/bin/env python3
"""Produits d'affichage vent (URock) et confort (UTCI/PET) : vent_confort/display/.

- wind_<cas>.png : vitesse du vent à 1,5 m (5 cas), grille 2 m ;
- utci_<jour>_<veg>.png, pet_<jour>_<veg>.png : indices à 14 h, grille SOLWEIG
  (5 m par défaut, --res 1 pour la grille 1 m) ;
- utci_gain_<jour>.png : UTCI sans arbres − UTCI avec arbres (3 %) ;
- indicators.csv : médianes par quartier et contexte ; manifest.json.
PNG en EPSG:3857 avec coins lon/lat (fonctions de scripts/umep_trees/solweig_display.py),
bâtiments transparents. display/ existant n'est jamais écrasé.
"""

from __future__ import annotations

import datetime as dt
import os
import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import yaml
from osgeo import gdal

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))  # copie figée éventuelle de solweig_display.py
sys.path.insert(1, str(HERE.parent / "umep_trees"))
import solweig_display as sd  # noqa: E402  (lecture seule : fonctions utilitaires)

ROOT = Path(os.environ.get("ECODEKK_ROOT", HERE.parents[1]))
UMEP = ROOT / "data/scenarios/scenario_01/exports/umep"
SRC, V = UMEP / "ombrage_arbres", UMEP / "vent_confort"
DAYS = {"chaud_saison_seche": "Journée chaude extrême de saison sèche",
        "chaud_typique": "Journée chaude typique de saison sèche",
        "saison_pluies": "Journée de saison des pluies",
        "frais_saison_seche": "Journée fraîche de saison sèche"}
DOMINANT = {"dominant_saison_seche": "Vent dominant de saison sèche (nov.–mai)",
            "dominant_saison_pluies": "Vent dominant de saison des pluies (juin–oct.)"}
VEGETATION = {"trans3": "Arbres, transmissivité 3 %", "sans_arbres": "Sans arbres (référence)"}
LEGENDS = {
    "wind": {"label": "Vent à 1,5 m (m/s)", "unit": "m/s",
             "stops": [[0, "#f7fbff"], [0.5, "#c6dbef"], [1, "#6baed6"], [2, "#2171b5"], [3, "#6a51a3"], [5, "#3f007d"]]},
    # Échelle officielle des classes de stress UTCI (°C) : 26, 32, 38, 46.
    "utci": {"label": "UTCI à 14 h (°C)", "unit": "°C",
             "stops": [[9, "#2c7bb6"], [26, "#ffffbf"], [32, "#fdae61"], [38, "#f46d43"], [46, "#a50026"], [52, "#67001f"]],
             "classes": [[26, "sans stress thermique"], [32, "stress modéré"], [38, "stress fort"],
                         [46, "stress très fort"], [99, "stress extrême"]]},
    "pet": {"label": "PET à 14 h (°C)", "unit": "°C",
            "stops": [[20, "#2c7bb6"], [29, "#ffffbf"], [35, "#fdae61"], [41, "#d7301f"], [50, "#a50026"], [60, "#67001f"]]},
    "utci_gain": {"label": "Gain des arbres sur l'UTCI à 14 h (K)", "unit": "K",
                  "stops": [[-2, "#b2182b"], [0, "#f7f7f7"], [2, "#c7e9c0"], [4, "#74c476"], [8, "#238b45"], [12, "#00441b"]]},
}


def load(path: Path) -> tuple[np.ndarray, gdal.Dataset]:
    values, ds = sd.read(path)
    values[values <= -999] = np.nan
    return values, ds


def zonal(values, ds, prefix, indicators, key, stats):
    quartier, labels = sd.quartier_grid(SRC / "umep_inputs.gpkg", ds)
    road = sd.rasterize(SRC / "umep_inputs.gpkg", "road_footprints", ds) > 0
    flood = sd.rasterize(SRC / "umep_inputs.gpkg", "flood_areas", ds) > 0
    building = sd.rasterize(SRC / "umep_inputs.gpkg", "buildings", ds) > 0
    context = np.where(road, "Voirie", np.where(flood, "Zones inondables", "Îlots"))
    for qid, label in labels.items():
        for ctx in ("Voirie", "Îlots", "Zones inondables"):
            cell = ~building & (quartier == qid) & (context == ctx) & ~np.isnan(values)
            if cell.any():
                row = indicators.setdefault((key, label, ctx), {"cas": key, "quartier": label, "contexte": ctx})
                for name, function in stats.items():
                    row[f"{prefix}_{name}"] = round(float(function(values[cell])), 2)
    return building


def main() -> int:
    import argparse
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--res", default="5", choices=("1", "5"), help="résolution des indices de confort (m)")
    args = parser.parse_args()
    tc_root = V / ("tc" if args.res == "1" else f"tc_{args.res}m")
    out = V / "display"
    if out.exists() and any(out.iterdir()):
        raise SystemExit(f"Sortie existante, non écrasée : {out}")
    out.mkdir(parents=True, exist_ok=True)
    wind_ref = yaml.safe_load((V / "wind_reference.yml").read_text(encoding="utf-8"))["runs"]
    layers, indicators, coords, missing = [], {}, {}, []

    for case, label in {**DAYS, **DOMINANT}.items():
        path = V / "urock" / case / "z1_5" / f"{case}WS.tif"
        if not path.exists():
            missing.append("vent_confort/" + str(path.relative_to(V)))
            continue
        wind, ds = load(path)
        building = zonal(wind, ds, "vent", indicators, case, {"median_m_s": np.median, "p90_m_s": lambda x: np.percentile(x, 90)})
        name = f"wind_{case}.png"
        coords["wind"] = sd.write_png_3857(sd.colorize(wind, LEGENDS["wind"], building), ds, out / name)
        layers.append({"indicator": "wind", "case": case, "vegetation": None, "file": name,
                       "reference": {k: wind_ref[case][k] for k in ("vitesse_10m_m_s", "direction_deg")}})

    utci = {}
    for day in DAYS:
        for veg in VEGETATION:
            work = tc_root / f"{day}_{veg}"
            for index in ("UTCI", "PET"):
                found = sorted(work.glob(f"{index}_*_1400D.tif")) if work.exists() else []
                if not found:
                    missing.append(f"vent_confort/{tc_root.name}/{day}_{veg}/{index}_*_1400D.tif")
                    continue
                values, ds = load(found[0])
                if index == "UTCI":
                    utci[(day, veg)] = values
                key = f"{day}_{veg}"
                stats = {"median_c": np.median}
                if index == "UTCI":
                    stats["part_stress_tres_fort_pct"] = lambda x: 100 * np.mean(x >= 38)
                building = zonal(values, ds, index.lower(), indicators, key, stats)
                name = f"{index.lower()}_{day}_{veg}.png"
                coords["comfort"] = sd.write_png_3857(sd.colorize(values, LEGENDS[index.lower()], building), ds, out / name)
                layers.append({"indicator": index.lower(), "case": day, "vegetation": veg, "file": name})
        if (day, "trans3") in utci and (day, "sans_arbres") in utci:
            gain = utci[(day, "sans_arbres")] - utci[(day, "trans3")]
            building = zonal(gain, ds, "gain_utci", indicators, f"{day}_trans3", {"moyen_k": np.mean})
            name = f"utci_gain_{day}.png"
            sd.write_png_3857(sd.colorize(gain, LEGENDS["utci_gain"], building), ds, out / name)
            layers.append({"indicator": "utci_gain", "case": day, "vegetation": "trans3", "file": name})

    pd.DataFrame(list(indicators.values())).to_csv(out / "indicators.csv", index=False)
    manifest = {
        "generated_at": dt.datetime.now(dt.timezone.utc).isoformat(timespec="seconds"),
        "scenario": "scenario_01", "study": "vent_confort",
        "coordinates": coords,
        "cases": [{"id": k, "label": v, **({"date": wind_ref[k]["date"]} if k in DAYS else {})}
                  for k, v in {**DAYS, **DOMINANT}.items()],
        "vegetation": [{"id": k, "label": v} for k, v in VEGETATION.items()],
        "legends": LEGENDS, "layers": layers, "missing": missing,
        "source": f"UMEP URock v2023a (2 m, sortie 1,5 m) et Spatial Thermal Comfort (UTCI, PET) sur SOLWEIG {args.res} m",
        "resolution_confort_m": int(args.res),
        "provenance": {
            "vent": "vent_confort/wind_reference.yml (ERA5 10 m à 14 h ; vents dominants 2015–2024, secteur modal)",
            "vent_regrille": "moyenne des pixels URock 2 m vers la grille SOLWEIG" if args.res != "1" else "plus proche voisin 2 m vers 1 m",
            "tmrt": f"ombrage_arbres/solweig_{args.res}m/<journée>_<végétation>/Tmrt_*_1400D.tif",
            "personne": "valeurs par défaut UMEP : 35 ans, 75 kg, 180 cm, 0,9 clo, 80 W, homme, debout",
            "attenuation_vegetation": "1,00 (défaut URock, plantation de mélèzes, Cionco 1978) — non locale",
        },
        "avertissements": [
            "Vent de référence ERA5 (maille d'environ 31 km) : un instant (14 h) par journée, pas un champ moyen.",
            "URock est un modèle diagnostique (Röckle) : il représente les sillages et accélérations, pas la turbulence.",
            "L'atténuation du vent par les arbres utilise la valeur par défaut d'URock, non calibrée pour ces essences.",
            "UTCI et PET pour une personne type (valeurs par défaut UMEP) ; le vent est moyenné sur la maille de confort.",
            "Résultats surtout comparatifs (avec ou sans arbres, d'une journée ou d'un quartier à l'autre).",
        ],
    }
    (out / "manifest.json").write_text(json.dumps(manifest, ensure_ascii=False, indent=1), encoding="utf-8")
    print(f"{len(layers)} couches ; manquants : {len(missing)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

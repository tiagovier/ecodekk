#!/usr/bin/env python3
"""Vents de référence URock depuis la série ERA5 ponctuelle (u10, v10).

- 3 journées d'étude à 14 h UTC (= heure locale), même pas horaire que les
  fichiers Tmrt_*_1400 de SOLWEIG ;
- 2 vents dominants saisonniers 2015–2024 : saison sèche (novembre–mai) et
  saison des pluies (juin–octobre) ; secteur modal sur 16 secteurs, vitesse
  médiane des heures de ce secteur, direction = centre du secteur.
Convention météorologique : direction d'où vient le vent, degrés depuis le
nord, sens horaire. Vitesse à 10 m = sqrt(u10² + v10²).
"""

import argparse
import datetime as dt
from pathlib import Path

import numpy as np
import pandas as pd
import yaml

HERE = Path(__file__).resolve().parent
UMEP = HERE.parents[1] / "data/scenarios/scenario_01/exports/umep"
CSV = UMEP / "ombrage_arbres/met/era5_2015_2024/14.769988N17.009024W-2015-sfc.csv"
DAYS = {"chaud_saison_seche": "2017-04-14", "saison_pluies": "2017-09-21", "frais_saison_seche": "2018-01-17"}
SEASONS = {"dominant_saison_seche": [11, 12, 1, 2, 3, 4, 5], "dominant_saison_pluies": [6, 7, 8, 9, 10]}
SECTORS = 16


def direction_from(u: np.ndarray, v: np.ndarray) -> np.ndarray:
    return (np.degrees(np.arctan2(-u, -v)) + 360.0) % 360.0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--csv", type=Path, default=CSV)
    parser.add_argument("--out", type=Path, default=UMEP / "vent_confort/wind_reference.yml")
    args = parser.parse_args()
    if args.out.exists():
        raise SystemExit(f"Sortie existante, non écrasée : {args.out}")
    era5 = pd.read_csv(args.csv, parse_dates=["valid_time"])
    era5["speed"] = np.hypot(era5["u10"], era5["v10"])
    era5["direction"] = direction_from(era5["u10"].to_numpy(), era5["v10"].to_numpy())
    width = 360.0 / SECTORS
    era5["sector"] = np.floor(((era5["direction"] + width / 2) % 360) / width).astype(int)

    runs = {}
    for case, day in DAYS.items():
        row = era5[era5["valid_time"] == pd.Timestamp(f"{day} 14:00")].iloc[0]
        runs[case] = {"type": "journee", "date": day, "heure_utc": "14:00",
                      "vitesse_10m_m_s": round(float(row["speed"]), 2),
                      "direction_deg": round(float(row["direction"]), 1),
                      "vegetation": "saison_pluies" if case == "saison_pluies" else "saison_seche"}
    for case, months in SEASONS.items():
        subset = era5[era5["valid_time"].dt.month.isin(months)]
        counts = subset["sector"].value_counts()
        modal = int(counts.idxmax())
        in_sector = subset[subset["sector"] == modal]
        runs[case] = {"type": "dominant", "mois": months,
                      "secteur_modal": modal, "frequence_pct": round(float(100 * counts.max() / len(subset)), 1),
                      "vitesse_10m_m_s": round(float(in_sector["speed"].median()), 2),
                      "direction_deg": round(modal * width, 1),
                      "rose_pct": {int(k): round(float(100 * v / len(subset)), 1) for k, v in counts.sort_index().items()},
                      "vegetation": "saison_pluies" if case == "dominant_saison_pluies" else "saison_seche"}
    record = {
        "generated_at": dt.datetime.now(dt.timezone.utc).isoformat(timespec="seconds"),
        "source": str(args.csv.relative_to(HERE.parents[1])),
        "periode": [str(era5["valid_time"].min()), str(era5["valid_time"].max())],
        "convention": "direction d'où vient le vent, degrés depuis le nord, sens horaire ; vitesse à 10 m",
        "secteurs": SECTORS,
        "runs": runs,
    }
    args.out.parent.mkdir(parents=True, exist_ok=True)
    args.out.write_text(yaml.safe_dump(record, allow_unicode=True, sort_keys=False), encoding="utf-8")
    for case, r in runs.items():
        print(case, r["vitesse_10m_m_s"], "m/s", r["direction_deg"], "°", r.get("frequence_pct", ""))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

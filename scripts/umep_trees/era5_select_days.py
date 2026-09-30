#!/usr/bin/env python3
"""Sélection des trois journées types dans la série ERA5 et extraction des
fichiers météo SOLWEIG (format UMEP, pas horaire, heure UTC = heure locale au
Sénégal).

Critères (documentés dans selection.yml, candidats dans candidates.csv) :
- chaud_saison_seche : avril–juin, ciel clair (indice de clarté journalier
  Kt >= 75e centile de la fenêtre), Tmax la plus élevée ;
- saison_pluies : août–septembre, pluie >= 1 mm sur les 72 h précédentes
  (zones inondables en eau), pas de pluie de 6 h à 19 h, Kt le plus élevé ;
- frais_saison_seche : décembre–janvier, ciel clair (Kt >= 75e centile),
  Tmax la plus basse.
"""

from __future__ import annotations

import argparse
from pathlib import Path

import numpy as np
import pandas as pd
import yaml

HERE = Path(__file__).resolve().parent
STUDY = HERE.parents[1] / "data/scenarios/scenario_01/exports/umep/ombrage_arbres"
SOLAR_CONSTANT = 1361.0  # W/m²

CASES = {
    "chaud_saison_seche": {"months": [4, 5, 6], "rule": "clear_hottest"},
    "saison_pluies": {"months": [8, 9], "rule": "wet_sunny"},
    "frais_saison_seche": {"months": [12, 1], "rule": "clear_coolest"},
}
CLEAR_PERCENTILE = 75
WET_ANTECEDENT_MM = 1.0
DAYTIME_HOURS = range(6, 20)


def read_forcing(directory: Path) -> pd.DataFrame:
    files = sorted(directory.glob("*.txt"))
    if not files:
        raise SystemExit(f"Aucun fichier de forçage dans {directory}")
    frames = []
    for path in files:
        frame = pd.read_csv(path, sep=r"\s+")
        frame.columns = [c.lstrip("%") for c in frame.columns]
        frame["source_file"] = path.name
        frames.append(frame)
    met = pd.concat(frames, ignore_index=True)
    # SUEWS/UMEP : horodatage de fin de pas horaire.
    met["time"] = (pd.to_datetime(met["iy"].astype(str), format="%Y")
                   + pd.to_timedelta(met["id"] - 1, unit="D")
                   + pd.to_timedelta(met["it"], unit="h") + pd.to_timedelta(met["imin"], unit="min"))
    met = met.drop_duplicates("time").sort_values("time").reset_index(drop=True)
    for column in ("Tair", "RH", "kdown", "rain", "U", "pres"):
        met.loc[met[column] <= -999, column] = np.nan
    return met


def toa_irradiance(time: pd.Series, lat: float, lon: float) -> np.ndarray:
    """Éclairement extraterrestre horizontal (W/m²) au milieu du pas horaire."""
    mid = time - pd.Timedelta(minutes=30)
    doy = mid.dt.dayofyear.to_numpy()
    hour = (mid.dt.hour + mid.dt.minute / 60).to_numpy()
    gamma = 2 * np.pi * (doy - 1) / 365
    decl = (0.006918 - 0.399912 * np.cos(gamma) + 0.070257 * np.sin(gamma)
            - 0.006758 * np.cos(2 * gamma) + 0.000907 * np.sin(2 * gamma))
    eot = 229.18 * (0.000075 + 0.001868 * np.cos(gamma) - 0.032077 * np.sin(gamma)
                    - 0.014615 * np.cos(2 * gamma) - 0.040849 * np.sin(2 * gamma))
    solar_time = hour + (eot + 4 * lon) / 60
    omega = np.radians(15 * (solar_time - 12))
    phi = np.radians(lat)
    cos_z = np.sin(phi) * np.sin(decl) + np.cos(phi) * np.cos(decl) * np.cos(omega)
    e0 = 1 + 0.033 * np.cos(2 * np.pi * doy / 365)
    return np.maximum(SOLAR_CONSTANT * e0 * cos_z, 0)


def daily_table(met: pd.DataFrame, lat: float, lon: float) -> pd.DataFrame:
    met = met.copy()
    met["toa"] = toa_irradiance(met["time"], lat, lon)
    met["date"] = (met["time"] - pd.Timedelta(minutes=30)).dt.date
    met["hour"] = (met["time"] - pd.Timedelta(minutes=30)).dt.hour
    met["daytime_rain"] = np.where(met["hour"].isin(DAYTIME_HOURS), met["rain"], 0)
    daily = met.groupby("date").agg(
        hours=("Tair", "size"), tmax=("Tair", "max"), tmin=("Tair", "min"), tmean=("Tair", "mean"),
        rh_mean=("RH", "mean"), rh_min=("RH", "min"), wind_mean=("U", "mean"),
        kdown_sum=("kdown", "sum"), toa_sum=("toa", "sum"), rain=("rain", "sum"),
        daytime_rain=("daytime_rain", "sum"),
    )
    daily = daily[daily["hours"] == 24].copy()
    daily["kt"] = daily["kdown_sum"] / daily["toa_sum"]
    daily["kdown_mj"] = daily["kdown_sum"] * 3600 / 1e6
    rain = daily["rain"].reindex(pd.date_range(daily.index.min(), daily.index.max()).date, fill_value=0)
    daily["rain_prev72h"] = rain.shift(1).rolling(3, min_periods=1).sum().reindex(daily.index).fillna(0)
    daily.index = pd.to_datetime(daily.index)
    return daily.round(3)


def candidates_for(daily: pd.DataFrame, case: str) -> tuple[pd.DataFrame, dict]:
    spec = CASES[case]
    window = daily[daily.index.month.isin(spec["months"])]
    if spec["rule"] == "wet_sunny":
        pool = window[(window["rain_prev72h"] >= WET_ANTECEDENT_MM) & (window["daytime_rain"] == 0)]
        ranked = pool.sort_values(["kt", "rh_mean"], ascending=[False, False])
        criteria = {"pluie_72h_min_mm": WET_ANTECEDENT_MM, "pluie_6h_19h_mm": 0, "tri": "Kt décroissant"}
    else:
        threshold = float(np.nanpercentile(window["kt"], CLEAR_PERCENTILE))
        pool = window[window["kt"] >= threshold]
        ascending = spec["rule"] == "clear_coolest"
        ranked = pool.sort_values("tmax", ascending=ascending)
        criteria = {"kt_min": round(threshold, 3), "kt_centile": CLEAR_PERCENTILE,
                    "tri": "Tmax croissante" if ascending else "Tmax décroissante"}
    criteria.update({"mois": spec["months"], "jours_fenetre": int(len(window)), "jours_eligibles": int(len(pool))})
    return ranked, criteria


def extract_day(met_dir: Path, day: pd.Timestamp, out: Path) -> None:
    """Copie les 24 lignes horaires de la journée (fin de pas 01:00 → 24:00)."""
    sources = sorted(met_dir.glob("*.txt"))
    header = sources[0].read_text().splitlines()[0]
    rows = [row for path in sources for row in path.read_text().splitlines()[1:]]
    start = day + pd.Timedelta(hours=1)
    stamps = pd.date_range(start, periods=24, freq="h")
    wanted = {(t.year, t.dayofyear, t.hour) for t in stamps}
    kept = []
    for row in rows:
        fields = row.split()
        key = (int(fields[0]), int(fields[1]), int(fields[2]))
        if key in wanted:
            kept.append(row)
    if len(kept) != 24:
        raise SystemExit(f"Journée {day.date()} incomplète : {len(kept)} heures.")
    out.write_text("\n".join([header, *kept]) + "\n")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--met-dir", type=Path, default=STUDY / "met/era5_2015_2024")
    parser.add_argument("--out", type=Path, default=STUDY / "met/journees")
    parser.add_argument("--lat", type=float, default=14.769988)
    parser.add_argument("--lon", type=float, default=-17.009024)
    parser.add_argument("--date", action="append", default=[], metavar="CAS=AAAA-MM-JJ",
                        help="choix explicite d'une journée parmi les candidats éligibles")
    args = parser.parse_args()
    overrides = dict(item.split("=", 1) for item in args.date)
    unknown = set(overrides) - set(CASES)
    if unknown:
        raise SystemExit(f"Cas inconnus : {sorted(unknown)}")
    if args.out.exists() and any(args.out.iterdir()):
        raise SystemExit(f"Sortie existante, non écrasée : {args.out}")
    args.out.mkdir(parents=True, exist_ok=True)

    met = read_forcing(args.met_dir)
    daily = daily_table(met, args.lat, args.lon)
    daily.to_csv(args.out / "daily_era5.csv", index_label="date")
    selection, all_candidates = {}, []
    for case in CASES:
        ranked, criteria = candidates_for(daily, case)
        if ranked.empty:
            raise SystemExit(f"Aucune journée éligible pour {case}.")
        top = ranked.head(10).assign(case=case, rank=range(1, min(10, len(ranked)) + 1))
        all_candidates.append(top)
        rule_day = ranked.index[0]
        day = pd.Timestamp(overrides[case]) if case in overrides else rule_day
        if day not in ranked.index:
            raise SystemExit(f"{day.date()} n'est pas éligible pour {case} selon les critères.")
        met_file = args.out / f"met_{case}_{day:%Y%m%d}.txt"
        extract_day(args.met_dir, day, met_file)
        row = ranked.loc[day]
        selection[case] = {
            "date": f"{day:%Y-%m-%d}", "doy": int(day.dayofyear), "met_file": met_file.name,
            "criteres": criteria,
            "rang_selon_critere": int(ranked.index.get_loc(day)) + 1,
            "premier_selon_critere": f"{rule_day:%Y-%m-%d}",
            "choix": "utilisateur" if case in overrides else "règle",
            "tmax_c": float(row["tmax"]), "tmin_c": float(row["tmin"]), "hr_moy_pct": float(row["rh_mean"]),
            "kt": float(row["kt"]), "rayonnement_mj_m2": float(row["kdown_mj"]),
            "pluie_jour_mm": float(row["rain"]), "pluie_72h_prec_mm": float(row["rain_prev72h"]),
            "vent_moy_m_s": float(row["wind_mean"]),
        }
    pd.concat(all_candidates).to_csv(args.out / "candidates.csv", index_label="date")
    record = {
        "source": "ERA5 série ponctuelle (CDS) via supy.util.gen_forcing_era5, diagnostic 2 m",
        "site": {"lat": args.lat, "lon": args.lon, "utc_offset_h": 0},
        "periode": [str(daily.index.min().date()), str(daily.index.max().date())],
        "journees": selection,
        "limites": "ERA5 ~31 km : climat régional, pas le microclimat du site ; Kt calculé sur l'éclairement extraterrestre horizontal.",
    }
    with open(args.out / "selection.yml", "w", encoding="utf-8") as handle:
        yaml.safe_dump(record, handle, allow_unicode=True, sort_keys=False)
    print(yaml.safe_dump(selection, allow_unicode=True, sort_keys=False))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

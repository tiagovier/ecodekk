#!/usr/bin/env python3
"""Extraits météo TARGET (format UMEP, en-tête UMEP) : température et humidité
ERA5 diagnostiquées à 2 m, vent à 10 m (hauteurs de référence z_TaRef = 2 m et
z_URef = 10 m de parameters.json TARGET) ; de J-2 00:00 (48 h de mise en
route) à J+1 23:00."""

import datetime as dt
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
C = ROOT / "data/scenarios/scenario_01/exports/umep/climat_urbain"
SRC_2M = ROOT / "data/scenarios/scenario_01/exports/umep/ombrage_arbres/met/era5_2015_2024"
SRC_10M = C / "met/era5_10m"
WIND = 9  # colonne U / Wind
OUT = C / "met/target"
DAYS = {"chaud_saison_seche": "2017-04-14", "saison_pluies": "2017-09-21", "frais_saison_seche": "2018-01-17"}
# Même ordre de 24 colonnes que le format SUEWS écrit par supy ; seuls les noms changent.
UMEP_HEADER = "%iy id it imin Q* QH QE Qs Qf Wind RH Td press rain Kdn snow ldown fcld wuh xsmd lai_hr Kdiff Kdir Wd"
SUPY_HEADER = "iy id it imin qn qh qe qs qf U RH Tair pres rain kdown snow ldown fcld Wuh xsmd lai kdiff kdir wdir"

def read_rows(directory: Path) -> dict:
    rows = {}
    for path in sorted(directory.glob("*.txt")):
        lines = path.read_text().splitlines()
        if lines[0].split() != SUPY_HEADER.split():
            raise SystemExit(f"En-tête inattendu dans {path.name}")
        for line in lines[1:]:
            f = line.split()
            stamp = dt.datetime(int(f[0]), 1, 1) + dt.timedelta(days=int(f[1]) - 1, hours=int(f[2]), minutes=int(f[3]))
            rows[stamp] = f
    return rows


OUT.mkdir(parents=True, exist_ok=True)
rows_2m, rows_10m = read_rows(SRC_2M), read_rows(SRC_10M)
rows = {}
for stamp, fields in rows_2m.items():
    if stamp in rows_10m:
        merged = list(fields)
        merged[WIND] = rows_10m[stamp][WIND]
        rows[stamp] = " ".join(merged)
for case, day in DAYS.items():
    d = dt.datetime.fromisoformat(day)
    start, end = d - dt.timedelta(days=2), d + dt.timedelta(days=1, hours=23)
    out = OUT / f"met_target_{case}_{d:%Y%m%d}.txt"
    if out.exists():
        raise SystemExit(f"Sortie existante, non écrasée : {out}")
    stamps = [start + dt.timedelta(hours=h) for h in range(int((end - start).total_seconds() // 3600) + 1)]
    missing = [s for s in stamps if s not in rows]
    if missing:
        raise SystemExit(f"{case} : {len(missing)} heures absentes du forçage")
    out.write_text("\n".join([UMEP_HEADER] + [rows[s] for s in stamps]) + "\n")
    print(out.name, len(stamps), "heures")

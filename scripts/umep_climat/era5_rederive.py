#!/usr/bin/env python3
"""Re-dérive le forçage UMEP/SUEWS à une autre hauteur depuis le CSV ERA5 déjà
téléchargé (même chaîne que supy.util.gen_forcing_era5, sans nouvel appel CDS)."""

import argparse
from pathlib import Path

import supy.util._era5 as era5

HERE = Path(__file__).resolve().parent
STUDY = HERE.parents[1] / "data/scenarios/scenario_01/exports/umep"
parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
parser.add_argument("--csv", type=Path,
                    default=STUDY / "ombrage_arbres/met/era5_2015_2024/14.769988N17.009024W-2015-sfc.csv")
parser.add_argument("--height", type=float, required=True)
parser.add_argument("--out", type=Path, required=True)
args = parser.parse_args()
if args.out.exists() and any(args.out.glob("*.txt")):
    raise SystemExit(f"Sortie existante, non écrasée : {args.out}")
args.out.mkdir(parents=True, exist_ok=True)
raw = era5.gen_df_diag_era5_csv(str(args.csv), args.height)
lat, lon = raw.attrs["latitude"], raw.attrs["longitude"]
forcing = era5.format_df_forcing(raw)
forcing["latitude"], forcing["longitude"] = lat, lon
forcing = forcing.reset_index().set_index(["latitude", "longitude", "time"])
for name in era5.save_forcing_era5(forcing, args.out):
    print(name)

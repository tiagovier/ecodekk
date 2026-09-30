#!/usr/bin/env python3
"""Téléchargement ERA5 (série ponctuelle CDS) pour l'étude d'ombrage.

Même fonction que l'outil UMEP « Download data (ERA5) » (supy.util.gen_forcing_era5),
au centre de l'emprise d'étude, hauteur de diagnostic 2 m (confort thermique).
La clé CDS est lue par cdsapi depuis ~/.cdsapirc ; elle n'est jamais copiée.
"""

import argparse
import logging
from pathlib import Path

import supy.util as su

HERE = Path(__file__).resolve().parent
DEFAULT_OUT = HERE.parents[1] / "data/scenarios/scenario_01/exports/umep/ombrage_arbres/met/era5_2015_2024"

parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
parser.add_argument("--lat", type=float, default=14.769988)   # centre de dem/mnt_50cm_etude.tif
parser.add_argument("--lon", type=float, default=-17.009024)
parser.add_argument("--start", default="2015-01-01")
parser.add_argument("--end", default="2024-12-31")
parser.add_argument("--height", type=float, default=2.0)
parser.add_argument("--out", type=Path, default=DEFAULT_OUT)
args = parser.parse_args()

args.out.mkdir(parents=True, exist_ok=True)
if any(args.out.glob("*.txt")):
    raise SystemExit(f"Des fichiers de forçage existent déjà dans {args.out} ; rien n'est écrasé.")
files = su.gen_forcing_era5(args.lat, args.lon, args.start, args.end,
                            dir_save=args.out, hgt_agl_diag=args.height, logging_level=logging.INFO)
print("Fichiers écrits :")
for name in files:
    print(" ", name)

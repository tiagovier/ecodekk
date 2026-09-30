#!/usr/bin/env python3
"""UTCI et PET à 14 h (UMEP Spatial Thermal Comfort) depuis SOLWEIG + URock.

Pour chaque journée × {trans3, sans_arbres} :
- dossier de travail vent_confort/tc/<run>/ : copie du Tmrt_*_1400D.tif et du
  metforcing.txt SOLWEIG (lecture seule de ombrage_arbres/solweig_1m/<run>/) ;
- buildings.tif reconstruit avec la règle SOLWEIG (USE_LC_BUILD=false, MNT
  fourni) : 1 si DSM − MNT < 2 m, sinon 0 ;
- vent URock à 1,5 m de la journée ré-échantillonné au plus proche sur la grille
  1 m (même origine ; 2 m → 1 m, sans interpolation) ;
- Spatial TC : UTCI (TC_TYPE=1) et PET (TC_TYPE=0), personne par défaut UMEP.
Les sorties existantes ne sont jamais écrasées.
"""

from __future__ import annotations

import argparse
import os
import datetime as dt
import shutil
import subprocess
from pathlib import Path

import numpy as np
import yaml
from osgeo import gdal

gdal.UseExceptions()
gdal.SetConfigOption("GDAL_PAM_ENABLED", "NO")
HERE = Path(__file__).resolve().parent
ROOT = Path(os.environ.get("ECODEKK_ROOT", HERE.parents[1]))
UMEP = ROOT / "data/scenarios/scenario_01/exports/umep"
SRC = UMEP / "ombrage_arbres"
OUT = UMEP / "vent_confort"
DAYS = ("chaud_saison_seche", "saison_pluies", "frais_saison_seche")
VEGETATION = ("trans3", "sans_arbres")
PERSON = {"AGE": 35, "ACTIVITY": 80, "CLO": 0.9, "WEIGHT": 75, "HEIGHT": 180, "SEX": 0}
INDICES = {"UTCI": 1, "PET": 0}


def build_grid(dsm: Path, dem: Path, out: Path) -> None:
    d = gdal.Open(str(dsm))
    heights = d.ReadAsArray().astype(np.float64) - gdal.Open(str(dem)).ReadAsArray().astype(np.float64)
    grid = np.where(heights < 2.0, 1.0, 0.0).astype(np.float32)
    ds = gdal.GetDriverByName("GTiff").Create(str(out), d.RasterXSize, d.RasterYSize, 1, gdal.GDT_Float32)
    ds.SetGeoTransform(d.GetGeoTransform())
    ds.SetProjection(d.GetProjection())
    ds.GetRasterBand(1).WriteArray(grid)
    ds = None


def resample_wind(wind_2m: Path, template: Path, out: Path) -> None:
    t = gdal.Open(str(template))
    gt = t.GetGeoTransform()
    bounds = (gt[0], gt[3] + gt[5] * t.RasterYSize, gt[0] + gt[1] * t.RasterXSize, gt[3])
    gdal.Warp(str(out), str(wind_2m), xRes=gt[1], yRes=abs(gt[5]), outputBounds=bounds,
              resampleAlg="near", dstSRS=t.GetProjection())


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--only", default="", help="nom de run exact (optionnel)")
    args = parser.parse_args()
    dsm, dem = SRC / "rasters_1m/dsm_bati_sol_1m.tif", SRC / "dem/mnt_1m_etude.tif"
    record = {"generated_at": dt.datetime.now(dt.timezone.utc).isoformat(timespec="seconds"),
              "personne": PERSON, "indices": list(INDICES), "runs": {}}
    for day in DAYS:
        wind = OUT / "urock" / day / "z1_5" / f"{day}WS.tif"
        if not wind.exists():
            raise SystemExit(f"Vent URock absent : {wind}")
        for veg in VEGETATION:
            name = f"{day}_{veg}"
            if args.only and name != args.only:
                continue
            solweig = SRC / "solweig_1m" / name
            tmrt = sorted(solweig.glob("Tmrt_*_1400D.tif"))
            if len(tmrt) != 1 or not (solweig / "metforcing.txt").exists():
                raise SystemExit(f"Sorties SOLWEIG incomplètes : {solweig}")
            work = OUT / "tc" / name
            if work.exists():
                raise SystemExit(f"Sortie existante, non écrasée : {work}")
            work.mkdir(parents=True)
            shutil.copy2(tmrt[0], work / tmrt[0].name)
            shutil.copy2(solweig / "metforcing.txt", work / "metforcing.txt")
            build_grid(dsm, dem, work / "buildings.tif")
            resample_wind(wind, tmrt[0], work / "wind_1m5_1m.tif")
            for index, code in INDICES.items():
                result = work / f"{index}_{tmrt[0].name.replace('Tmrt_', '')}"
                command = ["qgis_process", "run", "umep:Outdoor Thermal Comfort: Spatial Thermal Comfort", "--",
                           f"TMRT_MAP={work / tmrt[0].name}", f"UROCK_MAP={work / 'wind_1m5_1m.tif'}",
                           f"TC_TYPE={code}", *[f"{k}={v}" for k, v in PERSON.items()],
                           "COMFA=false", f"TC_OUT={result}"]
                completed = subprocess.run(command, capture_output=True, text=True)
                if completed.returncode != 0 or not result.exists():
                    raise SystemExit(f"Échec Spatial TC {name} {index} :\n{completed.stdout[-1500:]}\n{completed.stderr[-1500:]}")
                values = gdal.Open(str(result)).ReadAsArray().astype(float)
                values[values <= -999] = np.nan
                print(name, index, "médiane %.1f  min %.1f  max %.1f" % (np.nanmedian(values), np.nanmin(values), np.nanmax(values)))
            record["runs"][name] = {"tmrt": tmrt[0].name, "vent": str(wind.relative_to(UMEP)),
                                    "vent_regrille": "plus proche voisin 2 m → 1 m"}
    (OUT / "tc").mkdir(exist_ok=True)
    stamp = dt.datetime.now().strftime("%Y%m%d%H%M%S")
    (OUT / "tc" / f"run_{stamp}.yml").write_text(yaml.safe_dump(record, allow_unicode=True, sort_keys=False),
                                                 encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

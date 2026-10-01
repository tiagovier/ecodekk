#!/usr/bin/env python3
"""Exécution SUEWS maille par maille (bilan_energetique, scenario_01).

Chaque run (variante d'occupation du sol × maille) part du fichier exemple
de supy (benchmark KCL, Ward et al. 2016) pour tous les paramètres non fixés
par l'étude, puis applique les valeurs propres au site. Sortie : moyennes
horaires des variables utiles, 2017-01-01 → 2018-01-31, dans raw/<variante>/.
Un run déjà présent n'est pas recalculé ni écrasé.

Contournement local : supy appelle une interpolation scipy (« polynomial »,
ordre 0, c.-à-d. maintien de la valeur précédente) que la version installée
de scipy ne permet pas ; elle est remplacée ici, dans ce seul processus, par
`ffill()`, mathématiquement équivalent. Aucun paquet n'est modifié.
"""

from __future__ import annotations

import argparse
import copy
import json
import multiprocessing as mp
import os
import time
import traceback
from pathlib import Path

import numpy as np
import pandas as pd
import yaml

ROOT = Path(os.environ.get("ECODEKK_ROOT", Path(__file__).resolve().parents[2]))
OUT = ROOT / "data/scenarios/scenario_01/exports/umep/bilan_energetique"
FORCING_HEIGHT_M = 50.0
START, END = "2016-01-01", "2018-01-31"
ANALYSIS_START = pd.Timestamp("2017-01-01 00:00")
VARIABLES = ["QN", "QF", "QS", "QH", "QE", "T2", "RH2", "U10", "Kdown", "LAI", "SMD"]
PHYSICS = {
    "netradiationmethod": 1,   # NARP avec L↓ du forçage ERA5
    "emissionsmethod": 0,      # QF fourni dans le forçage (calcul explicite)
    "storageheatmethod": 1,    # OHM (défaut SUEWS)
    "ohmincqf": 0,
    "roughlenmommethod": 2,    # règle empirique hauteurs/fractions (Grimmond & Oke 1999)
    "roughlenheatmethod": 2,   # défaut SUEWS
    "stabilitymethod": 3,      # défaut SUEWS
    "smdmethod": 0,            # modélisé
    "waterusemethod": 1,       # observé = 0 (pas d'arrosage)
    "rslmethod": 2,            # défaut SUEWS
    "faimethod": 1,            # schéma simple (fractions et hauteurs)
    "rsllevel": 1,             # défaut SUEWS
    "gsmodel": 2,              # défaut SUEWS
    "snowuse": 0,
    "stebbsmethod": 0,
}

# Calibration v2 (approuvée par l'utilisateur le 2026-10-01). Avec les défauts
# de l'exemple (Londres), les arbres réchauffaient l'air en saison sèche :
# albédo des arbres (0,10) inférieur à celui du sol nu (0,18) et transpiration
# coupée par le déficit hydrique du sol. Valeurs surchargeables par
# variables d'environnement pour les essais.
CALIBRATION = {
    # Sol nu latéritique sahélien : albédo de l'ordre de 0,30 (ordre de grandeur
    # de la littérature, à confirmer par mesure locale).
    "bsoil_alb": float(os.environ.get("SUEWS_BSOIL_ALB", 0.30)),
    # Accès des arbres à l'eau profonde : réservoir racinaire profond pour
    # evetr/dectr (capacité et profondeur de sol), plein à l'initialisation.
    "tree_soilstorecap_mm": float(os.environ.get("SUEWS_TREE_SOILSTORECAP", 1000.0)),
    "tree_soildepth_mm": float(os.environ.get("SUEWS_TREE_SOILDEPTH", 3000.0)),
    # Fermeture stomatique reportée en proportion : seuil de déficit
    # s1/g_sm = 111 mm pour 150 mm de réserve (74 %) dans l'exemple ; g_sm
    # réduit pour garder 74 % de la réserve profonde avant fermeture.
    "g_sm": float(os.environ.get("SUEWS_G_SM", 5.56 / (0.74 * 1000.0))),
}


def patch_supy() -> None:
    import supy._load as load

    def resample_sum(data_raw_precip, tstep_in, tstep_mod):
        ratio = 1.0 * tstep_mod / tstep_in
        adj = ratio * data_raw_precip.copy().shift(-tstep_in + tstep_mod, freq="s").resample(
            f"{tstep_mod}{load.str_second}").mean().ffill()
        adj.loc[data_raw_precip.index[-1]] = np.nan
        return adj.sort_index().asfreq(f"{tstep_mod}{load.str_second}").fillna(value=0.0)

    load.resample_sum = resample_sum


def sample_config() -> dict:
    import supy

    return yaml.safe_load((Path(supy.__file__).parent / "sample_data/sample_config.yml").read_text())


def set_temperatures(node, t0: float) -> None:
    """Températures initiales = température de l'air ERA5 au premier pas."""
    if isinstance(node, dict):
        for key, value in node.items():
            if key in ("temperature", "tsfc", "tin") and isinstance(value, dict) and "value" in value:
                v = value["value"]
                value["value"] = [t0] * len(v) if isinstance(v, list) else t0
            else:
                set_temperatures(value, t0)
    elif isinstance(node, list):
        for item in node:
            set_temperatures(item, t0)


def build_config(base: dict, cell: pd.Series, defaults: dict, forcing_file: Path, t0: float) -> dict:
    cfg = copy.deepcopy(base)
    cfg["name"] = f"ecodekk_{cell.variant}_{cell.cell_id}"
    cfg["description"] = "Ecodekk scenario_01 — bilan énergétique SUEWS par maille de 100 m"
    control = cfg["model"]["control"]
    control.update({"tstep": 300, "start_time": START, "end_time": END})
    control["forcing_file"] = {"value": str(forcing_file)}
    for key, value in PHYSICS.items():
        cfg["model"]["physics"][key] = {"value": value}
    site = cfg["sites"][0]
    site["name"] = cell.cell_id
    site["gridiv"] = int(cell.grid_id)
    p = site["properties"]
    p["lat"]["value"], p["lng"]["value"] = float(cell.lat), float(cell.lng)
    p["alt"]["value"] = float(round(cell.alt_m, 2))
    p["timezone"]["value"] = 0
    p["surfacearea"]["value"] = 10000.0
    p["z"]["value"] = FORCING_HEIGHT_M
    zh_all = defaults["zH"] if np.isnan(cell.zH) or cell.zH <= 0 else cell.zH
    p["z0m_in"]["value"] = float(round(max(cell.z0, 0.0), 3)) if cell.z0 > 0 else float(round(0.1 * zh_all, 3))
    p["zdm_in"]["value"] = float(round(cell.zd, 3)) if cell.zd > 0 else float(round(0.7 * zh_all, 3))
    lc = p["land_cover"]
    for surface in ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water"):
        lc[surface]["sfr"]["value"] = float(round(cell[surface], 4))
    total = sum(lc[s]["sfr"]["value"] for s in ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water"))
    lc["bsoil"]["sfr"]["value"] = float(round(lc["bsoil"]["sfr"]["value"] + 1.0 - total, 4))
    lc["bldgs"]["bldgh"]["value"] = float(round(zh_all if cell.bldgs <= 0 else cell.zH, 2))
    lc["bldgs"]["faibldg"]["value"] = float(round(cell.fai if cell.fai > 0 else defaults["fai"], 3))
    lc["evetr"]["evetreeh"]["value"] = float(round(defaults["evetreeh_m"] if np.isnan(cell.evetreeh_m) else cell.evetreeh_m, 2))
    lc["dectr"]["dectreeh"]["value"] = float(round(defaults["dectreeh_m"] if np.isnan(cell.dectreeh_m) else cell.dectreeh_m, 2))
    set_temperatures(site["initial_states"], t0)
    apply_calibration(p, site["initial_states"])
    return cfg


def apply_calibration(properties: dict, initial_states: dict) -> None:
    lc = properties["land_cover"]
    lc["bsoil"]["alb"]["value"] = CALIBRATION["bsoil_alb"]
    for surface in ("evetr", "dectr"):
        lc[surface]["soilstorecap"]["value"] = CALIBRATION["tree_soilstorecap_mm"]
        lc[surface]["soildepth"]["value"] = CALIBRATION["tree_soildepth_mm"]
        initial_states[surface]["soilstore"]["value"] = CALIBRATION["tree_soilstorecap_mm"]
    properties["conductance"]["g_sm"]["value"] = CALIBRATION["g_sm"]


def write_forcing(base: pd.DataFrame, popdens_hab_ha: float, profile: pd.DataFrame, path: Path) -> None:
    if path.exists():
        return
    df = base.copy()
    per_capita = profile.set_index("hour")["total_w_per_capita"]
    hour = df["it"].replace(0, 24)
    df["qf"] = (popdens_hab_ha / 10000.0) * hour.map(per_capita).to_numpy()
    df["Wuh"] = 0.0
    tmp = path.with_suffix(".tmp")
    df.to_csv(tmp, sep=" ", index=False, float_format="%.4f")
    tmp.rename(path)


def clean_state(state_json: str) -> str:
    """Remplace les valeurs nulles (NaN) de l'état transmis par 0. Seul cas
    observé : un diagnostic d'humidité du sol des surfaces végétalisées,
    indéfini quand la maille n'a aucune végétation ; le calcul continu porte
    ce NaN sans effet sur les flux (vérifié : écarts identiques au simple
    découpage)."""
    data = json.loads(state_json)

    def fix(node):
        items = node.items() if isinstance(node, dict) else enumerate(node) if isinstance(node, list) else ()
        for key, value in list(items):
            if value is None:
                node[key] = 0.0
            else:
                fix(value)

    fix(data)
    return json.dumps(data)


def run_one(job: dict) -> str:
    patch_supy()
    import supy as sp

    out = Path(job["out"])
    if out.exists():
        return f"déjà fait {out.name}"
    started = time.time()
    try:
        from supy._run_rust import _normalise_grid_id, run_suews_rust, run_suews_rust_with_state

        sim = sp.SUEWSSimulation(job["config"])
        sim.update_forcing(job["forcing"])
        forcing = sim.forcing
        forcing = forcing.df if hasattr(forcing, "df") else forcing
        config = sim.config
        grid = _normalise_grid_id(config.sites[0].gridiv)
        # Enchaînement mensuel avec transmission de l'état (même mécanisme que
        # le découpage interne de supy), en ne gardant que les variables utiles.
        state, parts = None, []
        for _, chunk in forcing.groupby(forcing.index.to_period("M")):
            if state is None:
                result, state = run_suews_rust(config=config, df_forcing=chunk, grid_id=grid)
            else:
                result, state = run_suews_rust_with_state(config=config, df_forcing=chunk, grid_id=grid,
                                                          state_json=clean_state(state))
            parts.append(result["SUEWS"][VARIABLES].droplevel("grid"))
            del result
        res = pd.concat(parts)
        hourly = res.resample("1h", label="right", closed="right").mean()
        hourly = hourly[hourly.index > ANALYSIS_START]
        out.parent.mkdir(parents=True, exist_ok=True)
        tmp = out.with_suffix(".tmp")
        hourly.round(3).to_csv(tmp, compression="gzip")
        tmp.rename(out)
        del sim, res, parts
        return f"ok {out.parent.name}/{out.name} {time.time() - started:.0f}s"
    except Exception:
        return f"ÉCHEC {out.parent.name}/{out.name}\n{traceback.format_exc(limit=3)}"


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--grid-ids", type=int, nargs="*", help="sous-ensemble de mailles (test)")
    parser.add_argument("--variants", nargs="*")
    parser.add_argument("--workers", type=int, default=2)
    parser.add_argument("--tag", default="raw", help="dossier de sortie sous bilan_energetique/")
    args = parser.parse_args()

    inputs = OUT / "inputs"
    cells = pd.read_csv(inputs / "cells.csv")
    profile = pd.read_csv(inputs / "qf_profile.csv")
    base_forcing = pd.read_pickle(inputs / "forcing_2016_2018.pkl")
    t0 = float(base_forcing["Tair"].iloc[0])
    if args.grid_ids:
        cells = cells[cells.grid_id.isin(args.grid_ids)]
    if args.variants:
        cells = cells[cells.variant.isin(args.variants)]
    base = sample_config()
    work = OUT / "work"
    jobs = []
    # Hauteurs de repli pour une surface de fraction nulle (sans effet sur le
    # calcul, mais exigées par la validation) : moyennes de l'étude.
    all_cells = pd.read_csv(inputs / "cells.csv")
    defaults = {
        "zH": float(all_cells.loc[all_cells.zH > 0, "zH"].mean()),
        "fai": float(all_cells.loc[all_cells.fai > 0, "fai"].mean()),
        "evetreeh_m": float(all_cells["evetreeh_m"].mean()),
        "dectreeh_m": float(all_cells["dectreeh_m"].mean()),
    }
    for variant, frame in cells.groupby("variant"):
        for _, cell in frame.iterrows():
            dens = float(cell.popdens_hab_ha)
            forcing = work / "forcing" / ("qf_0.txt" if dens == 0 else f"qf_{int(cell.grid_id):03d}.txt")
            forcing.parent.mkdir(parents=True, exist_ok=True)
            write_forcing(base_forcing, dens, profile, forcing)
            cfg_path = work / "configs" / variant / f"{cell.cell_id}.yml"
            cfg_path.parent.mkdir(parents=True, exist_ok=True)
            cfg_path.write_text(yaml.safe_dump(build_config(base, cell, defaults, forcing, t0), sort_keys=False))
            jobs.append({"config": str(cfg_path), "forcing": str(forcing),
                         "out": str(OUT / args.tag / variant / f"{cell.cell_id}.csv.gz")})
    print(f"{len(jobs)} runs, {args.workers} processus", flush=True)
    ctx = mp.get_context("spawn")
    with ctx.Pool(args.workers, maxtasksperchild=1) as pool:
        for message in pool.imap_unordered(run_one, jobs):
            print(message, flush=True)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

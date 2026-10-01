#!/usr/bin/env python3
"""Écrit bilan_energetique/run.yml : provenance, paramètres retenus et liste
explicite des valeurs par défaut SUEWS utilisées (refuse d'écraser)."""

from __future__ import annotations

import datetime as dt
import hashlib
import json
import os
import sys
from pathlib import Path

import yaml

ROOT = Path(os.environ.get("ECODEKK_ROOT", Path(__file__).resolve().parents[2]))
OUT = ROOT / "data/scenarios/scenario_01/exports/umep/bilan_energetique"
sys.path.insert(0, str(Path(__file__).resolve().parent))
import run_suews  # noqa: E402


def sha(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def value(node):
    return node.get("value", node) if isinstance(node, dict) else node


def main() -> int:
    target = OUT / "run.yml"
    if target.exists():
        raise SystemExit(f"Sortie existante, non écrasée : {target}")
    import supy

    sample_path = Path(supy.__file__).parent / "sample_data/sample_config.yml"
    sample = yaml.safe_load(sample_path.read_text())
    lc = sample["sites"][0]["properties"]["land_cover"]
    defaults = {}
    for surface, props in lc.items():
        entry = {}
        for key in ("alb", "alb_min", "alb_max", "emis", "soildepth", "soilstorecap", "sathydraulicconduct"):
            if key in props:
                entry[key] = value(props[key])
        if "ohm_coef" in props:
            entry["ohm_coef"] = {season: {k: value(v) for k, v in coefs.items()} for season, coefs in props["ohm_coef"].items()}
        if "lai" in props:
            entry["lai"] = {k: value(v) for k, v in props["lai"].items() if k != "laipower"}
        if "maxconductance" in props:
            entry["maxconductance"] = value(props["maxconductance"])
        defaults[surface] = entry
    summary = json.loads((OUT / "inputs/prepare_summary.json").read_text(encoding="utf-8"))
    log_path = OUT / "logs/suews_runs_v2.log"
    log = log_path.read_text(encoding="utf-8", errors="replace") if log_path.exists() else ""
    record = {
        "generated_at": dt.datetime.now(dt.timezone.utc).isoformat(timespec="seconds"),
        "study": "bilan_energetique", "scenario": "scenario_01",
        "versions": {"supy": supy.__version__, "python": sys.version.split()[0]},
        "outil": ["scripts/umep_suews/housing_units.R", "scripts/umep_suews/prepare_suews.py",
                  "scripts/umep_suews/run_suews.py", "scripts/umep_suews/suews_display.py"],
        "modele_de_reference": {
            "fichier": str(sample_path), "sha256": sha(sample_path),
            "description": "exemple supy « benchmark 1a » (KCL, Londres ; Ward et al. 2016) : source de toutes les valeurs par défaut SUEWS",
        },
        "periode": {"mise_en_route": "2016-01-01 → 2016-12-31", "analyse": "2017-01-01 → 2018-01-31",
                    "pas_de_temps_s": 300, "sorties": "moyennes horaires (heure de fin de pas)"},
        "forcage": {"source": "ERA5 série ponctuelle re-dérivée à 50 m (climat_urbain/met/era5_50m)",
                    "hauteur_m": run_suews.FORCING_HEIGHT_M,
                    "qf": "calculé par maille et ajouté au forçage (emissionsmethod 0)", "wuh": 0.0},
        "physique": {k: {"value": v} for k, v in run_suews.PHYSICS.items()},
        "physique_justification": {
            "netradiationmethod": "1 : NARP avec L↓ du forçage (ERA5 fournit L↓) — choix lié aux données",
            "emissionsmethod": "0 : QF explicite (métabolisme + domestique) fourni dans le forçage",
            "roughlenmommethod": "2 : longueurs de rugosité calculées par SUEWS depuis hauteurs et fractions (règle empirique), sans valeur inventée pour le sol nu",
            "waterusemethod": "1 avec Wuh = 0 : aucun arrosage (approuvé)",
            "faimethod": "1 : indice de surface frontale par schéma simple (fractions et hauteurs)",
            "autres": "valeurs de l'exemple SUEWS",
        },
        "site": {
            "surfacearea_m2": 10000.0, "z_m": run_suews.FORCING_HEIGHT_M, "timezone": 0,
            "lat_lng": "centroïde de la maille", "alt": "moyenne du MNT 1 m dans la maille",
            "fractions": "climat_urbain/lc_fractions/<variante>_umep7 (étape 0)",
            "hauteur_batiments": "zH de la morphométrie (étape 0) ; si aucune surface bâtie, moyenne de l'étude (sans effet)",
            "hauteur_arbres": "moyenne du CDSM 1 m sur les pixels sempervirents / caducifoliés de la maille ; si fraction nulle, moyenne de l'étude (sans effet)",
            "revetement": "pavés autoblocants documentés comme surface revêtue ; propriétés = valeurs par défaut SUEWS du revêtement (albédo 0,10 de l'exemple, plutôt asphalte)",
            "temperatures_initiales": "température de l'air ERA5 au premier pas (toutes surfaces et couches)",
            "humidite_initiale_sol": "valeurs de l'exemple SUEWS, absorbées par l'année de mise en route",
        },
        "population": {
            "regle": "logements du programme (fonctions de l'application) × 6,4 personnes par ménage (donnée utilisateur)",
            "logements": summary["logements_total"], "population": summary["population_programme"],
            "mailles_habitees": summary["mailles_habitees"],
            "controle_attribut_quartiers_population": 15650,
            "presence": "résidents présents jour et nuit (pas de navettes)",
        },
        "chaleur_anthropique": {
            "metabolisme": "75 à 175 W/hab (minqfmetab, maxqfmetab : valeurs par défaut SUEWS), profil horaire d'activité par défaut SUEWS ramené de 0 à 1",
            "domestique": "30 W/hab en moyenne journalière, profil ahprof_24hr par défaut SUEWS — HYPOTHÈSE À CONFIRMER : ordre de grandeur de la consommation d'électricité par habitant au Sénégal (~250–300 kWh/an, tous usages) ; pas de climatisation",
            "trafic": 0.0,
            "profil": "inputs/qf_profile.csv",
        },
        "saisons": {
            "methode": "deux runs complets par maille (fractions saison sèche / saison des pluies), composés par mois",
            "mois_saison_pluies": [6, 7, 8, 9, 10],
            "raison": "SUEWS ne permet pas de faire varier les fractions de surface dans un même run",
        },
        "calibration_v2": {
            "date": "2026-10-01",
            "decision": "approuvée par l'utilisateur après constat que les défauts de Londres faisaient réchauffer l'air par les arbres (saison sèche, mailles > 60 % d'arbres : QE −7 W/m², QH +113 W/m², T2 +1,8 K)",
            "version_precedente": "superseded/v1_defauts_londres/",
            "parametres": {
                "bsoil.alb": {
                    "avant": 0.18, "apres": run_suews.CALIBRATION["bsoil_alb"],
                    "justification": "sol nu latéritique sahélien, plus clair que le sol de l'exemple londonien",
                    "source": "ordre de grandeur de la littérature (sols sableux à latéritiques sahéliens, albédo ~0,25–0,35) — À CONFIRMER par mesure ou référence locale ; aucune source vérifiable consultée dans la documentation disponible",
                },
                "evetr.soilstorecap, dectr.soilstorecap": {
                    "avant": 150.0, "apres": run_suews.CALIBRATION["tree_soilstorecap_mm"], "unite": "mm",
                    "justification": "accès des arbres à l'eau profonde (racines profondes de Faidherbia, manguier, Khaya) représenté par une réserve racinaire profonde",
                },
                "evetr.soildepth, dectr.soildepth": {
                    "avant": 350.0, "apres": run_suews.CALIBRATION["tree_soildepth_mm"], "unite": "mm",
                    "justification": "profondeur cohérente avec la réserve racinaire profonde (la capacité ne peut dépasser la profondeur)",
                },
                "initial_states.evetr/dectr.soilstore": {
                    "avant": 120.0, "apres": run_suews.CALIBRATION["tree_soilstorecap_mm"], "unite": "mm",
                    "justification": "réserve profonde pleine à l'initialisation (nappe), puis bilan hydrique modélisé pendant l'année de mise en route",
                },
                "conductance.g_sm": {
                    "avant": 0.05, "apres": round(run_suews.CALIBRATION["g_sm"], 5),
                    "justification": "seuil de fermeture stomatique s1/g_sm reporté proportionnellement : 111 mm pour 150 mm de réserve dans l'exemple (74 %), soit 74 % de la réserve profonde ; s1 inchangé. Paramètre de site, n'affecte que la végétation (pas d'herbe dans l'étude)",
                },
            },
            "mecanisme": "le moins invasif : aucune eau ajoutée (pas d'arrosage), seule la capacité de la réserve accessible aux arbres et la sensibilité stomatique au déficit changent",
            "affichage": "T2 exclue des produits d'affichage (diagnostic à 2 m non fiable sous couvert haut)",
        },
        "valeurs_par_defaut_suews": defaults,
        "contournements": [
            "Interpolation « polynomial » d'ordre 0 de supy (exige scipy ≥ 1.14.1, non installé) remplacée dans le processus par ffill(), équivalent (maintien de la valeur précédente).",
            "Exécution par tranches mensuelles avec transmission de l'état JSON (mécanisme interne de supy, run_suews_rust_with_state) pour limiter la mémoire ; écart maximal avec un calcul continu : 0,16 W/m² (QH, ΔQS), 0,006 K (T2).",
            "Valeur nulle (NaN) de l'état hydrique des surfaces végétalisées remplacée par 0 à chaque transmission, uniquement dans les mailles sans végétation (diagnostic indéfini, sans effet vérifié sur les flux).",
        ],
        "runs": {"total": 512, "ok": log.count("\nok "), "echecs": log.count("ÉCHEC")},
        "entrees": {
            "cells.csv": sha(OUT / "inputs/cells.csv"),
            "qf_profile.csv": sha(OUT / "inputs/qf_profile.csv"),
            "logements_batiments.csv": sha(OUT / "inputs/logements_batiments.csv"),
            "forcing_2016_2018.pkl": sha(OUT / "inputs/forcing_2016_2018.pkl"),
            **summary["inputs"],
        },
    }
    target.write_text(yaml.safe_dump(record, allow_unicode=True, sort_keys=False), encoding="utf-8")
    print(f"écrit {target}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

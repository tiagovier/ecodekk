#!/usr/bin/env bash
# TARGET : 3 journées × {arbres, sans_arbres}, 48 h de mise en route.
# Usage : target_runs.sh [run ...]   (défaut : les 6 runs). Aucun run existant n'est refait.
set -euo pipefail
ROOT="${ECODEKK_ROOT:-$(cd "$(dirname "$0")/../.." && pwd)}"
C="$ROOT/data/scenarios/scenario_01/exports/umep/climat_urbain"
declare -A DAY=( [chaud_saison_seche]=2017-04-14 [saison_pluies]=2017-09-21 [frais_saison_seche]=2018-01-17 )
declare -A SEASON=( [chaud_saison_seche]=saison_seche [saison_pluies]=saison_pluies [frais_saison_seche]=saison_seche )
all=()
for d in chaud_saison_seche saison_pluies frais_saison_seche; do for v in arbres sans_arbres; do all+=("${d}_$v"); done; done
runs=("${@:-${all[@]}}")
for run in "${runs[@]}"; do
  if [[ $run == *_sans_arbres ]]; then veg=sans_arbres; else veg=arbres; fi
  day=${run%_"$veg"}
  [[ -n "${DAY[$day]:-}" ]] || { echo "Run inconnu : $run" >&2; exit 1; }
  site="$C/target/prepare/${SEASON[$day]}_$veg"
  [[ -d "$site" ]] || { echo "Site TARGET absent : $site" >&2; exit 1; }
  ls "$site/output/csv/${run}_"*.csv >/dev/null 2>&1 && { echo "Run existant, non refait : $run" >&2; exit 1; }
  d="${DAY[$day]}"
  start=$(date -d "$d -2 days" +%F); stop=$(date -d "$d +1 day" +%F)
  met=$(ls "$C/met/target/met_target_${day}_"*.txt)
  qgis_process run "umep:Urban Heat Island: TARGET" -- INPUT_FOLDER="$site" INPUT_POLYGONLAYER="$C/grid/grille_100m.gpkg" \
    RUN_NAME="$run" START_DATE="$start" START_DATE_INTEREST="$d" STOP_DATE_INTEREST="$stop" INPUT_MET="$met" \
    MOD_LDOWN=false OUTPUT_CSV=true OUTPUT_UMEP=false 2>/dev/null | grep -E "calculation time|Error" || true
  n=$(ls "$site/output/csv/${run}_"*.csv | wc -l)
  [[ $n -ge 24 ]] && echo "OK $run ($n pas horaires)" || { echo "ÉCHEC $run ($n fichiers)" >&2; exit 1; }
done

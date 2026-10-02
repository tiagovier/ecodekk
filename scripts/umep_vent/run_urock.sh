#!/usr/bin/env bash
# URock 2 m × 2 m, sortie à 1,5 m, profil puissance (0), atténuation par défaut
# URock (1,00, Cionco 1978). Runs strictement séquentiels (/tmp/buildings.fgb
# partagé par URock). Vents : vent_confort/wind_reference.yml. Pas d'écrasement.
set -euo pipefail
ROOT="${ECODEKK_ROOT:?}"
V="$ROOT/data/scenarios/scenario_01/exports/umep/vent_confort"
IN="$V/inputs"
LOG="$V/logs/urock_chain.log"
mkdir -p "$V/logs" "$V/urock"
# URock écrit une base H2 de 1,5 à 2 Go par run dans le dossier temporaire et
# ne la supprime pas : on l'envoie sur /home et on la nettoie après chaque run
# (la partition système s'est remplie le 2026-10-02).
export TMPDIR="${TMPDIR_UROCK:-$V/tmp}"
mkdir -p "$TMPDIR"
cleanup_tmp() { rm -rf "$TMPDIR"/URock_*; }
trap cleanup_tmp EXIT
for case in ${CASES:-chaud_saison_seche saison_pluies frais_saison_seche dominant_saison_seche dominant_saison_pluies}; do
  out="$V/urock/$case"
  [[ -e "$out" ]] && { echo "ÉCHEC $case : sortie existante" >> "$LOG"; exit 1; }
  read -r speed direction veg < <(python3 - "$V/wind_reference.yml" "$case" <<'PY'
import sys, yaml
r = yaml.safe_load(open(sys.argv[1], encoding="utf-8"))["runs"][sys.argv[2]]
print(r["vitesse_10m_m_s"], r["direction_deg"], r["vegetation"])
PY
)
  echo "début $case vitesse=$speed direction=$direction végétation=$veg $(date +%H:%M)" >> "$LOG"
  /usr/bin/time -v qgis_process run "umep:Urban Wind Field: URock" -- \
    BUILDINGS="$IN/buildings.gpkg" HEIGHT_FIELD_BUILD=height_m \
    VEGETATION="$IN/vegetation_$veg.gpkg" VEGETATION_CROWN_TOP_HEIGHT=crown_top \
    VEGETATION_CROWN_BASE_HEIGHT=crown_base INPUT_PROFILE_TYPE=0 INPUT_WIND_HEIGHT=10 \
    INPUT_WIND_SPEED="$speed" INPUT_WIND_DIRECTION="$direction" RASTER_OUTPUT="$IN/template_2m.tif" \
    HORIZONTAL_RESOLUTION=2 VERTICAL_RESOLUTION=2 WIND_HEIGHT=1.5 UROCK_OUTPUT="$out" \
    OUTPUT_FILENAME="$case" SAVE_RASTER=true SAVE_VECTOR=false SAVE_NETCDF=false LOAD_OUTPUT=false \
    > "$V/logs/urock_$case.log" 2>&1 || { echo "ÉCHEC $case $(date +%H:%M)" >> "$LOG"; exit 1; }
  cleanup_tmp
  [[ -s "$out/z1_5/${case}WS.tif" ]] || { echo "ÉCHEC $case : raster absent" >> "$LOG"; exit 1; }
  echo "OK $case $(grep -E 'Elapsed \(wall' "$V/logs/urock_$case.log" | awk '{print $NF}') RSS=$(grep 'Maximum resident' "$V/logs/urock_$case.log" | awk '{print $NF}')kB $(date +%H:%M)" >> "$LOG"
done
echo "URock terminés $(date +%H:%M)" >> "$LOG"

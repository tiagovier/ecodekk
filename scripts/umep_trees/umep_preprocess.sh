#!/usr/bin/env bash
# Prétraitement UMEP de l'étude d'ombrage (scenario_01 / ombrage_arbres).
# Toutes les sorties vont dans exports/umep/<study_id>/ ; aucun fichier de
# data/sig/ ni spatial.gpkg n'est modifié. Une sortie existante n'est jamais
# écrasée. Chaque raster produit est contrôlé contre la grille du MNT.
set -euo pipefail

ROOT="${ECODEKK_ROOT:-$(cd "$(dirname "$0")/../.." && pwd)}"
SCENARIO="${SCENARIO:-scenario_01}"
STUDY="${STUDY:-ombrage_arbres}"
TREES_STEM="${TREES_STEM:-trees_thies_mature_seed20260930}"
STEP="${1:-all}"   # dem | dsm | trees | walls | landcover | svf | solweig | all (hors svf/solweig)
# Résolution : 0.5 (grille d'origine) ou 1 (grille dérivée, même origine ;
# choisie le 2026-09-30 car les SVF anisotropes à 0,5 m dépassent la mémoire).
RES="${RES:-0.5}"

S="$ROOT/data/scenarios/$SCENARIO/exports/umep/$STUDY"
INPUTS="$S/umep_inputs.gpkg"
TREES="$S/$TREES_STEM.gpkg"
SOURCE_DEM="$ROOT/data/sig/MNT_THIES_50cm.tif"
case "$RES" in
  0.5) TAG=""; DEM="$S/dem/mnt_50cm_etude.tif"; DSM_NAME="dsm_bati_sol_50cm.tif" ;;
  1) TAG="_1m"; DEM="$S/dem/mnt_1m_etude.tif"; DSM_NAME="dsm_bati_sol_1m.tif" ;;
  5) TAG="_5m"; DEM="$S/dem/mnt_5m_etude.tif"; DSM_NAME="dsm_bati_sol_5m.tif" ;;
  *) echo "Résolution non prévue : $RES" >&2; exit 2 ;;
esac
SPATIAL_OUT="$S/rasters$TAG"
SVF_OUT="$S/svf$TAG"
SOLWEIG_OUT="$S/solweig$TAG"
mkdir -p "$SPATIAL_OUT"
export GDAL_PAM_ENABLED=NO
# Contournement (2026-10-01) : QGIS 3.44.15 est lié à GDAL 3.8 alors que le pilote
# GRASS de GDAL (libgdal-grass, ubuntugis) vise GDAL 3.11 et bloque le chargement
# des greffons Python. Les pilotes GDAL optionnels ne sont pas chargés pour les
# runs UMEP (GeoTIFF, GeoPackage et CSV sont intégrés à GDAL).
if [[ -z "${GDAL_DRIVER_PATH:-}" ]]; then
  export GDAL_DRIVER_PATH="$S/logs/gdal_no_plugins"
  mkdir -p "$GDAL_DRIVER_PATH"
fi

qp() { qgis_process run "$@" 2> >(grep -v -E "GRASS|numexpr|bottleneck|NUMPY driver|binary incompatibility|NoneType|cad_to_gis|^$|_builtin_import" >&2); }
need_absent() { for f in "$@"; do [[ -e "$f" ]] && { echo "Sortie existante, non écrasée : $f" >&2; exit 1; }; done; return 0; }

# MNT 1 m : moyenne des pixels 0,5 m de la source, même origine que la grille
# 0,5 m (283091 ; 1634538,5) ; hauteur arrondie à 1372 px (+0,5 m au sud).
# MNT 5 m : même origine, emprise arrondie vers l'extérieur à 263 × 275 px
# (vue d'ensemble rapide ; zooms à 1 m sur des secteurs choisis).
step_dem() {
  [[ "$RES" != 0.5 ]] || { echo "Le MNT 0,5 m existe déjà." >&2; exit 2; }
  need_absent "$DEM"
  local te="283091 1633166.5 284403 1634538.5"
  [[ "$RES" == 5 ]] && te="283091 1633163.5 284406 1634538.5"
  gdalwarp -q -of GTiff -r average -tr "$RES" "$RES" -te $te \
    -co COMPRESS=DEFLATE -co TILED=YES -co PREDICTOR=3 "$SOURCE_DEM" "$DEM"
}
[[ "$STEP" == dem ]] && { step_dem; exit 0; }

# Emprise et pixel du MNT : référence de toutes les grilles.
read -r XMIN YMAX XMAX YMIN < <(gdalinfo -json "$DEM" | python3 -c '
import json,sys; c=json.load(sys.stdin)["cornerCoordinates"]
print(c["upperLeft"][0], c["upperLeft"][1], c["lowerRight"][0], c["lowerRight"][1])')
EXTENT="$XMIN,$XMAX,$YMIN,$YMAX [EPSG:32628]"

check_grid() {  # vérifie CRS, emprise et pixel identiques au MNT
  python3 - "$DEM" "$1" <<'EOF'
import json, subprocess, sys
info = lambda p: json.loads(subprocess.check_output(["gdalinfo", "-json", p], env={"GDAL_PAM_ENABLED": "NO", "PATH": "/usr/bin:/bin"}))
a, b = info(sys.argv[1]), info(sys.argv[2])
same = (a["size"] == b["size"] and all(abs(x - y) < 1e-6 for x, y in zip(a["geoTransform"], b["geoTransform"]))
        and "32628" in b["coordinateSystem"]["wkt"].split("ID[")[-1])
print(("OK  " if same else "ÉCHEC ") + sys.argv[2].split("/")[-1], b["size"], b["geoTransform"][0:2], b["geoTransform"][1])
sys.exit(0 if same else 1)
EOF
}

# UMEP transmet la source brute à GDAL : un « fichier|layername=… » échoue.
# Les couches vectorielles sont donc passées en GeoPackages mono-couche dérivés.
single_layer() {  # $1 = couche de umep_inputs.gpkg
  local out="$S/vector/$1.gpkg"
  mkdir -p "$S/vector"
  [[ -e "$out" ]] || ogr2ogr -f GPKG "$out" "$INPUTS" "$1" -nln "$1"
  echo "$out"
}

step_dsm() {
  local out="$SPATIAL_OUT/$DSM_NAME"
  need_absent "$out"
  local buildings; buildings="$(single_layer buildings)"
  qp "umep:Spatial Data: DSM Generator" -- \
    INPUT_DEM="$DEM" INPUT_POLYGONLAYER="$buildings" INPUT_FIELD=height_m \
    USE_OSM=false EXTENT="$EXTENT" PIXEL_RESOLUTION="$RES" OUTPUT_DSM="$out"
  check_grid "$out"
}

step_trees() {  # $1 = saison_seche | saison_pluies (défaut : les deux)
  local dsm="$SPATIAL_OUT/$DSM_NAME"
  local rainy="$S/${TREES_STEM}_saison_pluies.gpkg"
  # Saison des pluies : Faidherbia albida défeuillé (phénologie inversée) retiré.
  [[ -e "$rainy" ]] || ogr2ogr -f GPKG "$rainy" "$TREES" trees_umep -nln trees_umep \
    -where "species <> 'Faidherbia albida'"
  for variant in ${1:-saison_seche saison_pluies}; do
    need_absent "$SPATIAL_OUT"/{cdsm,tdsm}_$variant.tif
    local points="$TREES"
    [[ $variant == saison_pluies ]] && points="$rainy"
    qp "umep:Spatial Data: Tree Generator" -- \
      INPUT_POINTLAYER="$points" TREE_TYPE=ttype TOT_HEIGHT=totheight \
      TRUNK_HEIGHT=trunkheight DIA=diameter INPUT_DSM="$dsm" INPUT_DEM="$DEM" \
      CDSM_GRID_OUT="$SPATIAL_OUT/cdsm_$variant.tif" TDSM_GRID_OUT="$SPATIAL_OUT/tdsm_$variant.tif"
    check_grid "$SPATIAL_OUT/cdsm_$variant.tif"
    check_grid "$SPATIAL_OUT/tdsm_$variant.tif"
  done
}

step_walls() {
  need_absent "$SPATIAL_OUT/wall_height.tif" "$SPATIAL_OUT/wall_aspect.tif"
  qp "umep:Urban Geometry: Wall Height and Aspect" -- \
    INPUT="$SPATIAL_OUT/$DSM_NAME" INPUT_LIMIT=3 \
    OUTPUT_HEIGHT="$SPATIAL_OUT/wall_height.tif" OUTPUT_ASPECT="$SPATIAL_OUT/wall_aspect.tif"
  check_grid "$SPATIAL_OUT/wall_height.tif"
  check_grid "$SPATIAL_OUT/wall_aspect.tif"
}

step_landcover() {
  # Classes UMEP : 1 revêtu, 2 bâtiment, 6 sol nu, 7 eau. Pas d'herbe (choix
  # confirmé). Priorité : bâtiment > voirie > eau (saison des pluies) > sol nu.
  for variant in saison_seche saison_pluies; do
    local out="$SPATIAL_OUT/landcover_$variant.tif"
    need_absent "$out"
    gdal_create -of GTiff -outsize $(gdalinfo -json "$DEM" | python3 -c 'import json,sys;print(*json.load(sys.stdin)["size"])') \
      -a_srs EPSG:32628 -a_ullr "$XMIN" "$YMAX" "$XMAX" "$YMIN" -ot Byte -burn 6 -co COMPRESS=DEFLATE "$out"
    [[ $variant == saison_pluies ]] && gdal_rasterize -q -burn 7 -l flood_areas "$INPUTS" "$out"
    gdal_rasterize -q -burn 1 -l road_footprints "$INPUTS" "$out"
    gdal_rasterize -q -burn 2 -l buildings "$INPUTS" "$out"
    check_grid "$out"
  done
}

step_svf() {  # 4 combinaisons végétation × transmissivité + « sans_arbres » ; $1 = filtre
  local runs=(saison_seche_trans3 saison_seche_trans15 saison_pluies_trans3 saison_pluies_trans15 sans_arbres)
  for run in "${runs[@]}"; do
    [[ -n "${1:-}" && "$run" != "$1" ]] && continue
    local dir="$SVF_OUT/$run"
    need_absent "$dir"
    mkdir -p "$dir"
    local veg=()
    if [[ $run != sans_arbres ]]; then
      local variant="${run%_trans*}" trans="${run##*_trans}"
      veg=(INPUT_CDSM="$SPATIAL_OUT/cdsm_$variant.tif" INPUT_TDSM="$SPATIAL_OUT/tdsm_$variant.tif" TRANS_VEG="$trans")
    fi
    qp "umep:Urban Geometry: Sky View Factor" -- \
      INPUT_DSM="$SPATIAL_OUT/$DSM_NAME" "${veg[@]}" ANISO=true \
      WALL_SCHEME=false OUTPUT_DIR="$dir" OUTPUT_FILE="$dir/svf_total.tif"
    check_grid "$dir/svf_total.tif"
  done
}

# SOLWEIG : 3 journées × 2 transmissivités. Paramètres non fixés par l'étude :
# valeurs par défaut UMEP (albédo/émissivité des murs, corps humain).
step_solweig() {  # $1 = nom exact du run (optionnel)
  local met="$S/met/journees"
  local runs=(
    "chaud_saison_seche:$(ls "$met"/met_chaud_saison_seche_*.txt):saison_seche"
    "chaud_typique:$(ls "$met"/met_chaud_typique_*.txt 2>/dev/null):saison_seche"
    "saison_pluies:$(ls "$met"/met_saison_pluies_*.txt):saison_pluies"
    "frais_saison_seche:$(ls "$met"/met_frais_saison_seche_*.txt):saison_seche"
  )
  for spec in "${runs[@]}"; do
    IFS=: read -r day metfile variant <<<"$spec"
    for veg in trans3 trans15 sans_arbres; do
      local name="${day}_${veg}"
      [[ -n "${1:-}" && "$name" != "$1" ]] && continue
      local svf="$SVF_OUT/${variant}_${veg}" vegetation=()
      if [[ $veg == sans_arbres ]]; then
        svf="$SVF_OUT/sans_arbres"
      else
        vegetation=(INPUT_CDSM="$SPATIAL_OUT/cdsm_$variant.tif" INPUT_TDSM="$SPATIAL_OUT/tdsm_$variant.tif"
                    TRANS_VEG="${veg#trans}" LEAF_START=1 LEAF_END=366 CONIFER_TREES=false)
      fi
      local out="$SOLWEIG_OUT/$name"
      need_absent "$out"
      mkdir -p "$out"
      qp "umep:Outdoor Thermal Comfort: SOLWEIG" -- \
        INPUT_DSM="$SPATIAL_OUT/$DSM_NAME" INPUT_SVF="$svf/svfs.zip" \
        INPUT_HEIGHT="$SPATIAL_OUT/wall_height.tif" INPUT_ASPECT="$SPATIAL_OUT/wall_aspect.tif" \
        "${vegetation[@]}" \
        INPUT_LC="$SPATIAL_OUT/landcover_$variant.tif" USE_LC_BUILD=false INPUT_DEM="$DEM" \
        INPUT_ANISO="$svf/shadowmats.npz" \
        INPUTMET="$metfile" ONLYGLOBAL=true UTC=0 SENSOR_HEIGHT=2 POSTURE=0 CYL=true \
        OUTPUT_TMRT=true OUTPUT_SH=true OUTPUT_KDOWN=false OUTPUT_KUP=false \
        OUTPUT_LDOWN=false OUTPUT_LUP=false OUTPUT_TREEPLANTER=false OUTPUT_DIR="$out"
    done
  done
}

case "$STEP" in
  dem) step_dem ;; dsm) step_dsm ;; trees) step_trees "${2:-}" ;; walls) step_walls ;;
  landcover) step_landcover ;; svf) step_svf "${2:-}" ;; solweig) step_solweig "${2:-}" ;;
  all) step_dsm; step_trees; step_walls; step_landcover ;;
  *) echo "Étape inconnue : $STEP" >&2; exit 2 ;;
esac

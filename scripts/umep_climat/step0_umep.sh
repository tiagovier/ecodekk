#!/usr/bin/env bash
# Étape 0 (UMEP) : morphométrie et fractions d'occupation du sol sur la grille
# de 100 m. Entrées : sorties de step0_grid_landcover.py et rasters 1 m de
# l'étude ombrage_arbres (lecture seule). Aucune sortie n'est écrasée.
set -euo pipefail
ROOT="${ECODEKK_ROOT:-$(cd "$(dirname "$0")/../.." && pwd)}"
C="$ROOT/data/scenarios/scenario_01/exports/umep/climat_urbain"
R="$ROOT/data/scenarios/scenario_01/exports/umep/ombrage_arbres"
GRID="$C/grid/grille_100m.gpkg"
export GDAL_PAM_ENABLED=NO
qp() { qgis_process run "$@" 2> >(grep -v -E "GRASS|numexpr|bottleneck|NUMPY driver|binary incompatibility|NoneType|cad_to_gis|^$|_builtin_import" >&2) \
       | grep -v -E "being calculated|done\.|NoData-value" || true; }
need_absent() { for f in "$@"; do [[ -e "$f" ]] && { echo "Sortie existante, non écrasée : $f" >&2; exit 1; }; done; return 0; }

for season in saison_seche saison_pluies; do
  out="$C/morphometry/$season"; need_absent "$out"; mkdir -p "$out"
  qp "umep:Urban Morphology: Morphometric Calculator (Grid)" -- \
    INPUT_POLYGONLAYER="$GRID" ID_FIELD=grid_id SEARCH_METHOD=0 INPUT_INTERVAL=5 \
    USE_DSM_BUILD=false INPUT_DSM="$R/rasters_1m/dsm_bati_sol_1m.tif" INPUT_DEM="$R/dem/mnt_1m_etude.tif" \
    ROUGH=0 FILE_PREFIX=grid_ IGNORE_NODATA=true ATTR_TABLE=false OUTPUT_DIR="$out" \
    CALC_SS=true INPUT_CDSM="$R/rasters_1m/cdsm_$season.tif"
  [[ -s "$out/grid__IMPGrid_isotropic.txt" ]] || { echo "ÉCHEC morphométrie $season" >&2; exit 1; }
done

for variant in saison_seche_arbres saison_pluies_arbres saison_seche_sans_arbres saison_pluies_sans_arbres; do
  for target in false true; do
    kind=$([[ $target == true ]] && echo target9 || echo umep7)
    out="$C/lc_fractions/${variant}_$kind"; need_absent "$out"; mkdir -p "$out"
    qp "umep:Urban Land Cover: Land Cover Fraction (Grid)" -- \
      INPUT_POLYGONLAYER="$GRID" ID_FIELD=grid_id SEARCH_METHOD=0 INPUT_INTERVAL=5 \
      INPUT_LCGRID="$C/landcover/lc_energie_$variant.tif" FILE_PREFIX=lc_ TARGET_LC=$target \
      IGNORE_NODATA=true ATTR_TABLE=false OUTPUT_DIR="$out"
    ls "$out"/*isotropic*.txt >/dev/null 2>&1 || { echo "ÉCHEC fractions $variant $kind" >&2; exit 1; }
    echo "OK fractions $variant $kind"
  done
done

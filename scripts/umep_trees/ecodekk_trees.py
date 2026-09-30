#!/usr/bin/env python3
"""Attribution des arbres CAO existants pour le Tree Generator UMEP (Thiès).

Les positions de data/sig/arbres.gpkg sont conservées. Chaque point est classé
en contexte « road », « flood » ou « block », puis reçoit une essence et des
dimensions tirées des tableaux de config.yaml avec une variation aléatoire
reproductible (graine CLI). Les sources ne sont jamais modifiées ; les sorties
sont écrites dans data/scenarios/<scenario>/exports/umep/<study_id>/.
"""

from __future__ import annotations

import argparse
import datetime as dt
import hashlib
import sys
from pathlib import Path

import geopandas as gpd
import numpy as np
import pandas as pd
import shapely
import yaml

CONTEXTS = ("road", "flood", "block")
ATYPICAL_OSM_IDS = ("-38433", "-38434")


# --------------------------------------------------------------------------
# Configuration
# --------------------------------------------------------------------------

def load_config(path: Path) -> dict:
    with open(path, encoding="utf-8") as handle:
        config = yaml.safe_load(handle)
    validate_config(config)
    return config


def validate_config(config: dict) -> None:
    species = config["species"]
    tables = {
        "roadside.species_weights": config["roadside"]["species_weights"],
        "roadside.narrow_street_weights": config["roadside"]["narrow_street_weights"],
        "block.species_weights": config["block"]["species_weights"],
        "flood.species_weights": config["flood"]["species_weights"],
    }
    for name, table in tables.items():
        unknown = set(table) - set(species)
        if unknown:
            raise ValueError(f"{name} : essences sans dimensions {sorted(unknown)}")
        weights = np.array(list(table.values()), dtype=float)
        if (weights < 0).any() or weights.sum() <= 0:
            raise ValueError(f"{name} : poids invalides")
    for name, dims in species.items():
        for key in ("height_m", "trunk_m", "crown_m"):
            if not dims.get(key) or dims[key] <= 0:
                raise ValueError(f"{name} : {key} absent ou non positif")
    if config["umep_tree_type"] not in (1, 2):
        raise ValueError("umep_tree_type doit valoir 1 (conifère) ou 2 (feuillu)")


# --------------------------------------------------------------------------
# Tirages aléatoires
# --------------------------------------------------------------------------

def truncated_normal(rng: np.random.Generator, mean: float, sd: float,
                     low: float, high: float, size: int) -> np.ndarray:
    """Loi normale tronquée par rejet ; déterministe pour une graine donnée."""
    out = rng.normal(mean, sd, size)
    bad = (out < low) | (out > high)
    while bad.any():
        out[bad] = rng.normal(mean, sd, int(bad.sum()))
        bad = (out < low) | (out > high)
    return out


def draw_species(u: np.ndarray, table: dict) -> np.ndarray:
    """Choisit une essence par tirage uniforme u ∈ [0, 1) dans un tableau de poids."""
    names = [name for name, weight in table.items() if weight > 0]
    weights = np.array([table[name] for name in names], dtype=float)
    cumulative = np.cumsum(weights / weights.sum())
    index = np.searchsorted(cumulative, u, side="right")
    return np.array(names, dtype=object)[np.minimum(index, len(names) - 1)]


# --------------------------------------------------------------------------
# Contraintes de taille
# --------------------------------------------------------------------------

def apply_constraints(height_m: np.ndarray, trunk_m: np.ndarray,
                      crown_m: np.ndarray, roadside: np.ndarray,
                      constraints: dict) -> pd.DataFrame:
    """Applique les contraintes en décimètres entiers pour éviter les erreurs
    d'arrondi. Retourne hauteur, tronc, houppier (m) et les indicateurs."""
    step = constraints["rounding_m"]
    to_units = lambda x: np.rint(np.asarray(x, dtype=float) / step).astype(np.int64)
    ratio = constraints["max_trunk_ratio"]
    min_trunk = to_units(constraints["min_trunk_m"])
    road_trunk = to_units(constraints["roadside_min_trunk_m"])

    height = to_units(height_m)
    trunk = np.maximum(to_units(trunk_m), min_trunk)
    crown = np.maximum(to_units(crown_m), 1)
    trunk_raised = roadside & (trunk < road_trunk)
    trunk = np.where(roadside, np.maximum(trunk, road_trunk), trunk)

    # Arbre d'alignement : le tronc prime, la hauteur est relevée.
    needed_height = np.ceil(trunk / ratio).astype(np.int64)
    height_raised = roadside & (height < needed_height)
    height = np.where(height_raised, needed_height, height)
    # Autres arbres : le tronc est plafonné à ratio × hauteur.
    capped = np.floor(height * ratio).astype(np.int64)
    trunk_capped = ~roadside & (trunk > capped)
    trunk = np.where(trunk_capped, capped, trunk)
    violates = (trunk < min_trunk) | (trunk > np.floor(height * ratio)) | (trunk >= height)

    return pd.DataFrame({
        "total_height_m": height * step,
        "trunk_height_m": trunk * step,
        "crown_diameter_m": crown * step,
        "trunk_raised": trunk_raised,
        "height_raised": height_raised,
        "trunk_capped": trunk_capped,
        "constraint_violation": violates,
    }).round(6)


def attribute_trees(contexts: np.ndarray, narrow: np.ndarray, config: dict,
                    stage: str, seed: int) -> pd.DataFrame:
    """Tire essence et dimensions. Les tirages sont faits pour toutes les lignes
    (y compris hors contexte) afin qu'un changement de classement d'un arbre ne
    décale pas les tirages des autres."""
    if stage not in config["growth_stages"]:
        raise ValueError(f"Stade de croissance inconnu : {stage}")
    n = len(contexts)
    rng = np.random.default_rng(seed)
    u = rng.random(n)
    g_cfg = config["growth_stages"][stage]
    g = truncated_normal(rng, g_cfg["mean"], g_cfg["sd"], g_cfg["min"], g_cfg["max"], n)
    e_cfg = config["individual_noise"]
    eps = [truncated_normal(rng, e_cfg["mean"], e_cfg["sd"], e_cfg["min"], e_cfg["max"], n)
           for _ in range(3)]

    species = np.full(n, None, dtype=object)
    tables = {
        "road": config["roadside"]["species_weights"],
        "flood": config["flood"]["species_weights"],
        "block": config["block"]["species_weights"],
    }
    for context, table in tables.items():
        mask = (contexts == context) & ~((context == "road") & narrow)
        species[mask] = draw_species(u[mask], table)
    narrow_mask = (contexts == "road") & narrow
    species[narrow_mask] = draw_species(u[narrow_mask], config["roadside"]["narrow_street_weights"])

    assigned = species != None  # noqa: E711
    dims = config["species"]
    base = {
        key: np.array([dims[s][key] if s is not None else np.nan for s in species], dtype=float)
        for key in ("height_m", "trunk_m", "crown_m")
    }
    height = base["height_m"] * g * eps[0]
    crown = base["crown_m"] * g * eps[1]
    trunk = base["trunk_m"] * eps[2]  # le tronc dépend de l'élagage, pas de g

    out = pd.DataFrame({"species": species, "g_factor": np.round(g, 3)})
    sized = apply_constraints(
        np.nan_to_num(height[assigned]), np.nan_to_num(trunk[assigned]),
        np.nan_to_num(crown[assigned]), contexts[assigned] == "road",
        config["constraints"],
    )
    for column in sized.columns:
        values = np.full(n, np.nan if sized[column].dtype.kind == "f" else False,
                         dtype=sized[column].dtype)
        values[assigned] = sized[column].to_numpy()
        out[column] = values
    out.loc[~assigned, "g_factor"] = np.nan
    return out


# --------------------------------------------------------------------------
# Classement spatial
# --------------------------------------------------------------------------

def _union(frame: gpd.GeoDataFrame):
    geoms = shapely.make_valid(frame.geometry.to_numpy())
    return shapely.union_all(geoms) if len(geoms) else shapely.GeometryCollection()


def _containing_label(points: np.ndarray, polygons: gpd.GeoDataFrame) -> np.ndarray:
    """Étiquette land_use du premier polygone contenant chaque point (None sinon)."""
    label = np.full(len(points), None, dtype=object)
    if len(polygons):
        tree_index, zone_index = shapely.STRtree(shapely.make_valid(polygons.geometry.to_numpy())).query(
            points, predicate="intersects")
        first = np.unique(tree_index, return_index=True)[1]
        label[tree_index[first]] = polygons["land_use_label"].to_numpy()[zone_index[first]]
    return label


def duplicate_positions(points: np.ndarray, tolerance_m: float) -> np.ndarray:
    """Pour chaque point, indice du point conservé dont il est le doublon
    (distance <= tolérance), ou -1. Le premier point (ordre des lignes) est
    conservé ; les lignes doivent être triées de façon stable (tree_id)."""
    duplicate_of = np.full(len(points), -1, dtype=np.int64)
    if not len(points):
        return duplicate_of
    left, right = shapely.STRtree(points).query(points, predicate="dwithin", distance=tolerance_m)
    keep = left < right
    order = np.lexsort((left[keep], right[keep]))
    for i, j in zip(left[keep][order], right[keep][order]):
        if duplicate_of[j] == -1 and duplicate_of[i] == -1:
            duplicate_of[j] = i
    return duplicate_of


def classify_trees(trees: gpd.GeoDataFrame, roads: gpd.GeoDataFrame,
                   road_footprints: gpd.GeoDataFrame, flood: gpd.GeoDataFrame,
                   green: gpd.GeoDataFrame, buildings: gpd.GeoDataFrame,
                   offset_m: float, narrow_width_m: float,
                   land_use: gpd.GeoDataFrame | None = None,
                   other_as_block: bool = False) -> pd.DataFrame:
    """Priorité : bâtiment (exclu) > voirie > zone inondable > îlot > hors contexte.
    Avec other_as_block, tout point restant hors bâtiment est un arbre d'îlot
    dont la zone est l'étiquette land_use qui le contient.
    Toutes les couches doivent être dans le même CRS métrique."""
    points = trees.geometry.to_numpy()
    n = len(points)
    inside = lambda geom: shapely.intersects(geom, points) if not geom.is_empty else np.zeros(n, bool)

    in_building = inside(_union(buildings))
    in_footprint = inside(_union(road_footprints))
    in_flood = inside(_union(flood))
    in_green = inside(_union(green))
    green_label = _containing_label(points, green)
    other_label = _containing_label(points, land_use if land_use is not None else green.iloc[0:0])

    width = np.full(n, np.nan)
    edge_distance = np.full(n, np.nan)
    if len(roads):
        tree_index, road_index = roads.sindex.nearest(points, return_all=False)
        axis = roads.geometry.to_numpy()[road_index]
        width[tree_index] = roads["width_m"].to_numpy(dtype=float)[road_index]
        edge_distance[tree_index] = shapely.distance(points[tree_index], axis) - width[tree_index] / 2
    near_edge = np.nan_to_num(edge_distance, nan=np.inf) <= offset_m
    is_road = ~in_building & (in_footprint | near_edge)

    context = np.full(n, None, dtype=object)
    context[is_road] = "road"
    context[(context == None) & ~in_building & in_flood] = "flood"  # noqa: E711
    context[(context == None) & ~in_building & in_green] = "block"  # noqa: E711
    other = (context == None) & ~in_building  # noqa: E711
    if other_as_block:
        context[other] = "block"
    zone = np.full(n, None, dtype=object)
    zone[context == "road"] = "voirie"
    zone[context == "flood"] = "zone inondable"
    zone[context == "block"] = green_label[context == "block"]
    if other_as_block:
        zone[other] = np.where(pd.isna(other_label[other]), "hors occupation du sol", other_label[other])
    reason = np.full(n, None, dtype=object)
    reason[in_building] = "dans un bâtiment"
    reason[(context == None) & ~in_building] = "hors contexte de plantation retenu"  # noqa: E711
    return pd.DataFrame({
        "context": context,
        "planting_zone": zone,
        "exclusion_reason": reason,
        "road_width_m": np.where(is_road, width, np.nan),
        "road_width_missing": is_road & np.isnan(width),
        "narrow_street": is_road & (width < narrow_width_m),
        "edge_distance_m": np.round(edge_distance, 2),
    })


def spacing_conflicts(points: np.ndarray, crown_m: np.ndarray, factor: float) -> np.ndarray:
    """Nombre de voisins à moins de factor × (r_i + r_j) pour chaque arbre."""
    tree = shapely.STRtree(points)
    radius = crown_m / 2
    search = factor * (radius + radius.max())
    left, right = tree.query(shapely.buffer(points, search), predicate="intersects")
    keep = left < right
    left, right = left[keep], right[keep]
    close = shapely.distance(points[left], points[right]) < factor * (radius[left] + radius[right])
    counts = np.zeros(len(points), dtype=int)
    np.add.at(counts, left[close], 1)
    np.add.at(counts, right[close], 1)
    return counts


# --------------------------------------------------------------------------
# Lecture des données
# --------------------------------------------------------------------------

def load_source_trees(path: Path, crs_epsg: int) -> gpd.GeoDataFrame:
    raw = gpd.read_file(path, layer="entities")
    handles = raw["EntityHandle"].astype(str).str.strip()
    if handles.isna().any() or (handles == "").any() or handles.duplicated().any():
        raise ValueError("EntityHandle absent ou dupliqué dans la couche des arbres.")
    # CRS source non défini : les coordonnées CAO sont en EPSG:32628. Z ignoré.
    geometry = shapely.force_2d(raw.geometry.to_numpy())
    trees = gpd.GeoDataFrame({
        "tree_id": "tree_" + handles.str.lower(),
        "source_handle": handles,
        "tree_category": raw["Layer"].astype(str),
    }, geometry=geometry, crs=None).set_crs(crs_epsg, allow_override=True)
    return trees.sort_values("tree_id").reset_index(drop=True)


def load_scenario_layers(spatial_path: Path, crs_epsg: int, green_labels: list) -> dict:
    read = lambda layer: gpd.read_file(spatial_path, layer=layer).to_crs(crs_epsg)
    buildings = read("buildings")
    if "osm_id" in buildings:
        buildings = buildings[~buildings["osm_id"].astype(str).isin(ATYPICAL_OSM_IDS)]
    land_use = read("land_use")
    missing_labels = sorted(set(green_labels) - set(land_use["land_use_label"].dropna()))
    roads = read("roads")
    if "width_m" not in roads:
        raise ValueError("La couche roads n'a pas de champ width_m.")
    return {
        "roads": roads,
        "road_footprints": read("road_footprints"),
        "flood": read("flood_areas"),
        "green": land_use[land_use["land_use_label"].isin(green_labels)],
        "land_use": land_use,
        "buildings": buildings,
        "missing_land_use_labels": missing_labels,
    }


# --------------------------------------------------------------------------
# Sorties
# --------------------------------------------------------------------------

def summary_table(attributed: pd.DataFrame) -> pd.DataFrame:
    metrics = {"total_height_m": "hauteur", "trunk_height_m": "tronc", "crown_diameter_m": "houppier"}
    grouped = attributed.groupby(["species", "context"])
    parts = [grouped.size().rename("count")]
    for column, label in metrics.items():
        stats = grouped[column].agg(["mean", "min", "max"]).round(2)
        stats.columns = [f"{label}_{stat}_m" for stat in stats.columns]
        parts.append(stats)
    return pd.concat(parts, axis=1).reset_index()


def histogram_plot(attributed: pd.DataFrame, path: Path, title: str) -> None:
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    fig, axes = plt.subplots(1, 2, figsize=(13, 5))
    for species, frame in attributed.groupby("species"):
        axes[0].hist(frame["total_height_m"], bins=30, histtype="step", label=species)
        axes[1].hist(frame["crown_diameter_m"], bins=30, histtype="step", label=species)
    axes[0].set_xlabel("Hauteur totale (m)")
    axes[1].set_xlabel("Diamètre du houppier (m)")
    for axis in axes:
        axis.set_ylabel("Nombre d'arbres")
    axes[1].legend(fontsize=8)
    fig.suptitle(title)
    fig.tight_layout()
    fig.savefig(path, dpi=120)
    plt.close(fig)


def file_fingerprint(path: Path) -> dict:
    digest = hashlib.sha256()
    with open(path, "rb") as handle:
        for block in iter(lambda: handle.read(1 << 20), b""):
            digest.update(block)
    stat = path.stat()
    return {"path": str(path), "bytes": stat.st_size, "sha256": digest.hexdigest()}


def safe_output_dir(scenario_dir: Path, study_id: str) -> Path:
    allowed = (scenario_dir / "exports" / "umep").resolve()
    output = (allowed / study_id).resolve()
    if output.parent != allowed:
        raise ValueError("study_id invalide : la sortie doit rester dans exports/umep/.")
    return output


def run(project_root: Path, scenario: str, study_id: str, stage: str, seed: int,
        config_path: Path, trees_source: Path | None = None) -> Path:
    config = load_config(config_path)
    scenario_dir = (project_root / "data" / "scenarios" / scenario).resolve()
    spatial_path = scenario_dir / "spatial.gpkg"
    if not spatial_path.exists():
        raise FileNotFoundError(f"GeoPackage de scénario absent : {spatial_path}")
    trees_source = trees_source or project_root / "data" / "sig" / "arbres.gpkg"
    output_dir = safe_output_dir(scenario_dir, study_id)
    stem = f"trees_thies_{stage}_seed{seed}"
    output_gpkg = output_dir / f"{stem}.gpkg"
    if output_gpkg.exists():
        raise FileExistsError(f"Sortie existante, non écrasée : {output_gpkg}")

    crs = config["crs_epsg"]
    trees = load_source_trees(trees_source, crs)
    layers = load_scenario_layers(spatial_path, crs, config["block"]["land_use_labels"])
    classes = classify_trees(
        trees, layers["roads"], layers["road_footprints"], layers["flood"],
        layers["green"], layers["buildings"],
        config["roadside"]["offset_from_edge_m"], config["roadside"]["narrow_street_width_m"],
        land_use=layers["land_use"], other_as_block=bool(config["block"].get("other_points_as_block", False)),
    )
    duplicate_of = duplicate_positions(trees.geometry.to_numpy(), config["deduplicate_tolerance_m"])
    is_duplicate = duplicate_of >= 0
    classes["duplicate_of"] = np.where(is_duplicate, trees["tree_id"].to_numpy()[np.maximum(duplicate_of, 0)], None)
    classes.loc[is_duplicate, "context"] = None
    classes.loc[is_duplicate, "exclusion_reason"] = "doublon de position"
    sizes = attribute_trees(classes["context"].to_numpy(), classes["narrow_street"].to_numpy(),
                            config, stage, seed)
    table = pd.concat([trees.drop(columns="geometry"), classes, sizes], axis=1)
    violation = table["constraint_violation"].fillna(False).astype(bool)
    table.loc[violation, "exclusion_reason"] = "contrainte de taille non satisfaite"
    keep = table["exclusion_reason"].isna().to_numpy()

    fields = config["umep_fields"]
    attributed = gpd.GeoDataFrame(table[keep].copy(), geometry=trees.geometry[keep].to_numpy(), crs=crs)
    attributed["spacing_conflicts"] = spacing_conflicts(
        attributed.geometry.to_numpy(), attributed["crown_diameter_m"].to_numpy(),
        config["spacing_check_factor"],
    )
    umep = gpd.GeoDataFrame({
        "tree_id": attributed["tree_id"],
        "source_handle": attributed["source_handle"],
        "species": attributed["species"],
        "context": attributed["context"],
        "planting_zone": attributed["planting_zone"],
        fields["tree_type"]: np.int32(config["umep_tree_type"]),
        fields["total_height"]: attributed["total_height_m"],
        fields["trunk_height"]: attributed["trunk_height_m"],
        fields["diameter"]: attributed["crown_diameter_m"],
        "g_factor": attributed["g_factor"],
        "growth_stage": stage,
        "seed": np.int64(seed),
        "road_width_m": attributed["road_width_m"],
        "narrow_street": attributed["narrow_street"],
        "height_raised": attributed["height_raised"].astype(bool),
        "trunk_raised": attributed["trunk_raised"].astype(bool),
        "trunk_capped": attributed["trunk_capped"].astype(bool),
        "spacing_conflicts": attributed["spacing_conflicts"],
    }, geometry=attributed.geometry, crs=crs)

    output_dir.mkdir(parents=True, exist_ok=True)
    umep.to_file(output_gpkg, layer="trees_umep", driver="GPKG")
    summary_table(attributed).to_csv(output_dir / f"{stem}_summary.csv", index=False)
    excluded = table.loc[~keep, ["tree_id", "source_handle", "exclusion_reason", "duplicate_of", "edge_distance_m"]]
    excluded.to_csv(output_dir / f"{stem}_excluded.csv", index=False)
    adjustments = umep.loc[umep["height_raised"] | umep["trunk_raised"] | umep["trunk_capped"],
                           ["tree_id", "species", "context", "height_raised", "trunk_raised", "trunk_capped"]]
    adjustments.to_csv(output_dir / f"{stem}_adjustments.csv", index=False)
    histogram_plot(attributed, output_dir / f"{stem}_histograms.png",
                   f"Arbres UMEP — {scenario}, stade {stage}, graine {seed}")

    counts = {
        "source_points": int(len(table)),
        "attributed": int(keep.sum()),
        "by_context": {c: int((umep["context"] == c).sum()) for c in CONTEXTS},
        "by_planting_zone": {str(k): int(v) for k, v in umep["planting_zone"].value_counts().items()},
        "excluded": {str(k): int(v) for k, v in table.loc[~keep, "exclusion_reason"].value_counts().items()},
        "roadside_width_missing": int(table["road_width_missing"].sum()),
        "narrow_street_trees": int(umep["narrow_street"].sum()),
        "height_raised": int(umep["height_raised"].sum()),
        "trunk_raised": int(umep["trunk_raised"].sum()),
        "trunk_capped": int(umep["trunk_capped"].sum()),
        "trees_with_spacing_conflict": int((umep["spacing_conflicts"] > 0).sum()),
        "missing_land_use_labels": layers["missing_land_use_labels"],
    }
    run_record = {
        "generated_at": dt.datetime.now(dt.timezone.utc).isoformat(timespec="seconds"),
        "tool": "scripts/umep_trees/ecodekk_trees.py",
        "python": sys.version.split()[0],
        "geopandas": gpd.__version__,
        "shapely": shapely.__version__,
        "numpy": np.__version__,
        "scenario": scenario,
        "study_id": study_id,
        "growth_stage": stage,
        "seed": seed,
        "crs": f"EPSG:{crs}",
        "source_crs_note": "CRS de arbres.gpkg non défini ; EPSG:32628 attribué (coordonnées CAO).",
        "inputs": {
            "trees": file_fingerprint(trees_source),
            "scenario_spatial": file_fingerprint(spatial_path),
            "config": file_fingerprint(config_path),
        },
        "config": config,
        "counts": counts,
        "umep": {
            "algorithm": "Pre-Processor > Spatial Data > Tree Generator",
            "fields": fields,
            "tree_type_codes": {0: "suppression", 1: "conifère", 2: "feuillu"},
            "note": "CDSM/TDSM non générés : nécessitent UMEP et un DSM bâti+sol aligné sur le MNT.",
        },
    }
    with open(output_dir / f"{stem}_run.yml", "w", encoding="utf-8") as handle:
        yaml.safe_dump(run_record, handle, allow_unicode=True, sort_keys=False)
    return output_gpkg


def main(argv: list[str] | None = None) -> int:
    here = Path(__file__).resolve().parent
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--project-root", type=Path, default=here.parents[1])
    parser.add_argument("--scenario", required=True)
    parser.add_argument("--study-id", required=True)
    parser.add_argument("--growth-stage", choices=("mature", "young"), default="mature")
    parser.add_argument("--seed", type=int, required=True)
    parser.add_argument("--config", type=Path, default=here / "config.yaml")
    parser.add_argument("--trees-source", type=Path, default=None)
    args = parser.parse_args(argv)
    output = run(args.project_root.resolve(), args.scenario, args.study_id, args.growth_stage,
                 args.seed, args.config.resolve(), args.trees_source)
    print(f"Arbres UMEP écrits : {output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

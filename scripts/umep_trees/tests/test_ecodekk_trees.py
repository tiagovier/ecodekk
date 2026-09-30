import hashlib
import sys
from pathlib import Path

import numpy as np
import pytest

gpd = pytest.importorskip("geopandas")
import shapely  # noqa: E402
import yaml  # noqa: E402

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent))
import ecodekk_trees as et  # noqa: E402

CONFIG_PATH = HERE.parent / "config.yaml"


@pytest.fixture(scope="module")
def config():
    return et.load_config(CONFIG_PATH)


def _contexts(n, rng):
    return rng.choice(np.array(["road", "flood", "block", None], dtype=object), n)


@pytest.mark.parametrize("stage", ["mature", "young"])
def test_constraints_always_respected(config, stage):
    rng = np.random.default_rng(1)
    contexts = _contexts(20000, rng)
    narrow = rng.random(20000) < 0.5
    out = et.attribute_trees(contexts, narrow, config, stage, seed=42)
    ok = out["species"].notna().to_numpy()
    assert (out.loc[~ok, "total_height_m"].isna()).all()
    h, t, c = (out.loc[ok, k].to_numpy() for k in ("total_height_m", "trunk_height_m", "crown_diameter_m"))
    road = contexts[ok] == "road"
    assert not out.loc[ok, "constraint_violation"].any()
    assert (t >= 1.5 - 1e-9).all()
    assert (t <= 0.5 * h + 1e-9).all()
    assert (t < h).all()
    assert (t[road] >= 4.5 - 1e-9).all()
    assert (c > 0).all()
    for values in (h, t, c):
        assert np.allclose(values * 10, np.rint(values * 10))
    g = out.loc[ok, "g_factor"].to_numpy()
    lo, hi = config["growth_stages"][stage]["min"], config["growth_stages"][stage]["max"]
    assert ((g >= lo - 1e-3) & (g <= hi + 1e-3)).all()


def test_seed_reproducibility(config):
    rng = np.random.default_rng(3)
    contexts = _contexts(500, rng)
    narrow = rng.random(500) < 0.3
    a = et.attribute_trees(contexts, narrow, config, "mature", seed=7)
    b = et.attribute_trees(contexts, narrow, config, "mature", seed=7)
    c = et.attribute_trees(contexts, narrow, config, "mature", seed=8)
    assert a.equals(b)
    assert not a.equals(c)


def test_species_zone_rules(config):
    n = 30000
    rng = np.random.default_rng(5)
    contexts = _contexts(n, rng)
    narrow = rng.random(n) < 0.5
    out = et.attribute_trees(contexts, narrow, config, "mature", seed=11)
    species = out["species"].to_numpy()
    assert not np.any((contexts == "flood") & (species == "Mangifera indica"))
    assert not np.any((contexts != "flood") & (species == "Mitragyna inermis"))
    assert set(species[contexts == "flood"]) >= {"Mitragyna inermis"}
    assert set(species[contexts == "block"]) >= {"Mangifera indica"}
    assert set(species[(contexts == "road") & narrow]) == {"Terminalia mantaly"}
    road_species = set(config["roadside"]["species_weights"])
    assert set(species[contexts == "road"]) <= road_species


def test_roadside_height_raised_when_trunk_clearance_needed(config):
    constraints = config["constraints"]
    out = et.apply_constraints(np.array([6.0, 6.0]), np.array([2.0, 4.0]), np.array([5.0, 5.0]),
                               np.array([True, False]), constraints)
    assert out.loc[0, "trunk_height_m"] == pytest.approx(4.5)
    assert out.loc[0, "total_height_m"] == pytest.approx(9.0)
    assert out.loc[0, "height_raised"]
    assert out.loc[1, "trunk_height_m"] == pytest.approx(3.0)
    assert out.loc[1, "trunk_capped"]


def test_spacing_diagnostic():
    points = shapely.points([[0, 0], [5, 0], [100, 0]])
    counts = et.spacing_conflicts(points, np.array([10.0, 10.0, 10.0]), 0.8)
    assert counts.tolist() == [1, 1, 0]  # 5 m < 0.8 × (5 + 5)


def _box(x0, y0, x1, y1):
    return shapely.box(x0, y0, x1, y1)


def _make_project(tmp_path):
    crs = 32628
    x0, y0 = 300000, 1630000
    root = tmp_path / "project"
    (root / "data" / "sig").mkdir(parents=True)
    scenario = root / "data" / "scenarios" / "s1"
    scenario.mkdir(parents=True)
    spatial = scenario / "spatial.gpkg"
    # Route est-ouest de 6 m à y0 ; îlot vert au nord ; zone inondable à l'est.
    layers = {
        "roads": gpd.GeoDataFrame({"width_m": [6.0]}, geometry=[shapely.LineString([(x0, y0), (x0 + 400, y0)])], crs=crs),
        "road_footprints": gpd.GeoDataFrame({"Class": [1]}, geometry=[_box(x0, y0 - 2, x0 + 400, y0 + 2)], crs=crs),
        "flood_areas": gpd.GeoDataFrame({"flood_type": ["Lit mineur"]}, geometry=[_box(x0 + 300, y0 + 20, x0 + 400, y0 + 120)], crs=crs),
        "land_use": gpd.GeoDataFrame({"land_use_label": ["Parc", "par_10 variable plots forets", "Parking"]},
                                     geometry=[_box(x0, y0 + 20, x0 + 100, y0 + 120),
                                               _box(x0 + 100, y0 + 20, x0 + 200, y0 + 120),
                                               _box(x0 + 200, y0 + 20, x0 + 300, y0 + 120)], crs=crs),
        "buildings": gpd.GeoDataFrame({"building_id": ["b1"], "osm_id": ["1"]}, geometry=[_box(x0 + 10, y0 + 30, x0 + 30, y0 + 50)], crs=crs),
    }
    for name, frame in layers.items():
        frame.to_crs(4326).to_file(spatial, layer=name, driver="GPKG")
    coords = [
        (x0 + 50, y0 + 5),     # road : 5 m de l'axe, 2 m du bord
        (x0 + 60, y0 + 15),    # hors contexte : 12 m du bord, hors îlot
        (x0 + 20, y0 + 40),    # dans un bâtiment
        (x0 + 70, y0 + 80),    # parc
        (x0 + 150, y0 + 80),   # parcelle forestière partagée
        (x0 + 250, y0 + 80),   # parking : hors contexte
        (x0 + 350, y0 + 80),   # zone inondable
        (x0 + 70.004, y0 + 80),  # doublon du parc (4 mm)
    ]
    trees = gpd.GeoDataFrame(
        {"EntityHandle": [f"A{i}" for i in range(len(coords))], "Layer": "Végétation"},
        geometry=[shapely.Point(x, y, 0.0) for x, y in coords], crs=None,
    )
    source = root / "data" / "sig" / "arbres.gpkg"
    trees.to_file(source, layer="entities", driver="GPKG")
    return root, spatial, source


def _sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def test_end_to_end_classification_outputs_and_inputs_untouched(tmp_path):
    root, spatial, source = _make_project(tmp_path)
    before = (_sha(spatial), _sha(source))
    out = et.run(root, "s1", "etude", "mature", 5, CONFIG_PATH)
    assert (_sha(spatial), _sha(source)) == before
    assert out == root / "data" / "scenarios" / "s1" / "exports" / "umep" / "etude" / "trees_thies_mature_seed5.gpkg"

    trees = gpd.read_file(out, layer="trees_umep").set_index("source_handle")
    assert trees.crs.to_epsg() == 32628
    assert trees.loc["A0", "context"] == "road"
    assert trees.loc["A3", "context"] == "block" and trees.loc["A3", "planting_zone"] == "Parc"
    assert trees.loc["A4", "context"] == "block"
    assert trees.loc["A4", "planting_zone"] == "par_10 variable plots forets"
    assert trees.loc["A6", "context"] == "flood"
    assert trees.loc["A1", "context"] == "block"
    assert trees.loc["A1", "planting_zone"] == "hors occupation du sol"
    assert trees.loc["A5", "context"] == "block" and trees.loc["A5", "planting_zone"] == "Parking"
    assert {"A2", "A7"}.isdisjoint(trees.index)
    assert (trees["ttype"] == 2).all()
    for field in ("totheight", "trunkheight", "diameter"):
        assert trees[field].notna().all()

    stem = out.with_suffix("")
    excluded = __import__("pandas").read_csv(f"{stem}_excluded.csv").set_index("source_handle")
    assert excluded.loc["A2", "exclusion_reason"] == "dans un bâtiment"
    assert excluded.loc["A7", "exclusion_reason"] == "doublon de position"
    assert excluded.loc["A7", "duplicate_of"] == "tree_a3"
    for suffix in ("_summary.csv", "_histograms.png", "_adjustments.csv", "_run.yml"):
        assert Path(f"{stem}{suffix}").exists()
    run = yaml.safe_load(Path(f"{stem}_run.yml").read_text(encoding="utf-8"))
    assert run["seed"] == 5 and run["counts"]["attributed"] == 6

    with pytest.raises(FileExistsError):
        et.run(root, "s1", "etude", "mature", 5, CONFIG_PATH)
    again = et.run(root, "s1", "etude_bis", "mature", 5, CONFIG_PATH)
    a = gpd.read_file(out, layer="trees_umep")
    b = gpd.read_file(again, layer="trees_umep")
    assert a.drop(columns="geometry").equals(b.drop(columns="geometry"))


def test_output_must_stay_under_exports_umep(tmp_path):
    root, _, _ = _make_project(tmp_path)
    with pytest.raises(ValueError):
        et.run(root, "s1", "../../evil", "mature", 1, CONFIG_PATH)


def test_duplicate_positions_keep_first():
    points = shapely.points([[0, 0], [0.005, 0], [0, 0.002], [10, 0], [10, 0.5]])
    assert et.duplicate_positions(points, 0.01).tolist() == [-1, 0, 0, -1, -1]


def test_other_points_can_stay_excluded(tmp_path):
    root, _, _ = _make_project(tmp_path)
    config = yaml.safe_load(CONFIG_PATH.read_text(encoding="utf-8"))
    config["block"]["other_points_as_block"] = False
    variant = tmp_path / "config.yaml"
    variant.write_text(yaml.safe_dump(config, allow_unicode=True), encoding="utf-8")
    out = et.run(root, "s1", "etude", "mature", 5, variant)
    trees = gpd.read_file(out, layer="trees_umep")
    assert {"A1", "A5"}.isdisjoint(trees["source_handle"])

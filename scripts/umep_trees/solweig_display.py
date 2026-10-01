#!/usr/bin/env python3
"""Produits d'affichage des résultats SOLWEIG pour l'onglet « Confort thermique ».

Lit les sorties brutes (solweig_1m/<run>/) et écrit dans display/ :
- PNG colorés en EPSG:3857 (Tmrt à 14 h, heures d'ombre 10–16 h, gain des
  arbres = Tmrt sans arbres − Tmrt avec arbres), bâtiments transparents ;
- indicators.csv : indicateurs par quartier et contexte ;
- hourly_profiles.csv : Tmrt horaire médiane par classe d'exposition ;
- manifest.json : emprise, légendes, journées et fichiers.
Les sorties brutes ne sont pas modifiées ; display/ existant n'est pas écrasé.
"""

from __future__ import annotations

import argparse
import datetime as dt
import json
import re
from pathlib import Path

import numpy as np
import pandas as pd
import yaml
from osgeo import gdal, ogr, osr

gdal.UseExceptions()
gdal.SetConfigOption("GDAL_PAM_ENABLED", "NO")
HERE = Path(__file__).resolve().parent
STUDY = HERE.parents[1] / "data/scenarios/scenario_01/exports/umep/ombrage_arbres"

DAYS = {
    "chaud_saison_seche": "Journée chaude extrême de saison sèche",
    "chaud_typique": "Journée chaude typique de saison sèche",
    "saison_pluies": "Journée de saison des pluies",
    "frais_saison_seche": "Journée fraîche de saison sèche",
}
DAY_VARIANT = {"chaud_saison_seche": "saison_seche", "chaud_typique": "saison_seche", "saison_pluies": "saison_pluies",
               "frais_saison_seche": "saison_seche"}
VEGETATION = {
    "trans3": "Arbres, transmissivité 3 %",
    "trans15": "Arbres, transmissivité 15 %",
    "sans_arbres": "Sans arbres (référence)",
}
PEAK_HOUR = 14
SHADE_HOURS = range(11, 17)  # pas horaires se terminant de 11 h à 16 h = 10 h–16 h
HOT_THRESHOLD_C = 60.0

LEGENDS = {
    "tmrt": {"label": "Tmrt à 14 h (°C)", "unit": "°C",
             "stops": [[20, "#313695"], [30, "#74add1"], [40, "#fee090"], [50, "#fdae61"],
                       [60, "#f46d43"], [70, "#a50026"], [75, "#67001f"]]},
    "shade": {"label": "Heures d'ombre de 10 h à 16 h", "unit": "h",
              "stops": [[0, "#fff7bc"], [1, "#fee391"], [2, "#c6dbef"], [3, "#9ecae1"],
                        [4, "#6baed6"], [5, "#2171b5"], [6, "#08306b"]]},
    "cooling": {"label": "Gain des arbres sur la Tmrt à 14 h (K)", "unit": "K",
                "stops": [[-5, "#b2182b"], [0, "#f7f7f7"], [5, "#c7e9c0"], [10, "#74c476"],
                          [20, "#238b45"], [30, "#00441b"]]},
}


def read(path: Path) -> tuple[np.ndarray, gdal.Dataset]:
    ds = gdal.Open(str(path))
    array = ds.GetRasterBand(1).ReadAsArray().astype(np.float64)
    nodata = ds.GetRasterBand(1).GetNoDataValue()
    if nodata is not None:
        array[array == nodata] = np.nan
    return array, ds


def hourly_files(run_dir: Path, prefix: str) -> dict[int, Path]:
    """{heure de fin de pas : fichier} pour Tmrt_* ou Shadow_* (24 pas)."""
    files = {}
    for path in sorted(run_dir.glob(f"{prefix}_*_*_????[DN].tif")):
        match = re.search(r"_(\d{4})_(\d{1,3})_(\d{2})(\d{2})[DN]\.tif$", path.name)
        hour = int(match.group(3)) or 24
        files[hour] = path
    if len(files) != 24:
        raise SystemExit(f"{run_dir.name} : {len(files)} pas horaires {prefix} au lieu de 24.")
    return files


def colorize(values: np.ndarray, legend: dict, mask: np.ndarray) -> np.ndarray:
    stops = np.array([s[0] for s in legend["stops"]], dtype=float)
    colors = np.array([[int(c[i:i + 2], 16) for i in (1, 3, 5)] for _, c in legend["stops"]], dtype=float)
    clipped = np.clip(values, stops[0], stops[-1])
    rgba = np.zeros(values.shape + (4,), dtype=np.uint8)
    for channel in range(3):
        rgba[..., channel] = np.interp(clipped, stops, colors[:, channel]).round()
    rgba[..., 3] = np.where(mask | np.isnan(values), 0, 215)
    return rgba


def write_png_3857(rgba: np.ndarray, template: gdal.Dataset, out_png: Path) -> list[list[float]]:
    """Reprojette une image RGBA en EPSG:3857 et retourne les 4 coins lon/lat
    (ordre MapLibre : NO, NE, SE, SO)."""
    mem = gdal.GetDriverByName("MEM").Create("", template.RasterXSize, template.RasterYSize, 4, gdal.GDT_Byte)
    mem.SetGeoTransform(template.GetGeoTransform())
    mem.SetProjection(template.GetProjection())
    for band in range(4):
        mem.GetRasterBand(band + 1).WriteArray(rgba[..., band])
    warped = gdal.Warp("", mem, format="MEM", dstSRS="EPSG:3857", resampleAlg="near",
                       dstAlpha=False, srcNodata=None)
    gdal.Translate(str(out_png), warped, format="PNG")
    gt = warped.GetGeoTransform()
    x0, y0 = gt[0], gt[3]
    x1, y1 = x0 + gt[1] * warped.RasterXSize, y0 + gt[5] * warped.RasterYSize
    to_ll = osr.CoordinateTransformation(_srs(3857), _srs(4326))
    corners = [(x0, y0), (x1, y0), (x1, y1), (x0, y1)]
    return [[round(c, 7) for c in to_ll.TransformPoint(x, y)[:2]] for x, y in corners]


def _srs(epsg: int) -> osr.SpatialReference:
    srs = osr.SpatialReference()
    srs.ImportFromEPSG(epsg)
    srs.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    return srs


def rasterize(gpkg: Path, layer: str, template: gdal.Dataset) -> np.ndarray:
    mem = gdal.GetDriverByName("MEM").Create("", template.RasterXSize, template.RasterYSize, 1, gdal.GDT_Int32)
    mem.SetGeoTransform(template.GetGeoTransform())
    mem.SetProjection(template.GetProjection())
    mem.GetRasterBand(1).Fill(0)
    source = ogr.Open(str(gpkg))
    gdal.RasterizeLayer(mem, [1], source.GetLayerByName(layer), burn_values=[1])
    return mem.GetRasterBand(1).ReadAsArray()


def quartier_grid(gpkg: Path, template: gdal.Dataset) -> tuple[np.ndarray, dict[int, str]]:
    source = ogr.Open(str(gpkg))
    layer = source.GetLayerByName("quartiers")
    mem_ds = ogr.GetDriverByName("Memory").CreateDataSource("q")
    mem_layer = mem_ds.CreateLayer("q", layer.GetSpatialRef(), ogr.wkbMultiPolygon)
    mem_layer.CreateField(ogr.FieldDefn("qid", ogr.OFTInteger))
    labels = {}
    for index, feature in enumerate(layer, start=1):
        labels[index] = feature.GetField("district_label") or feature.GetField("district_code")
        out = ogr.Feature(mem_layer.GetLayerDefn())
        out.SetField("qid", index)
        out.SetGeometry(feature.GetGeometryRef().Clone())
        mem_layer.CreateFeature(out)
    raster = gdal.GetDriverByName("MEM").Create("", template.RasterXSize, template.RasterYSize, 1, gdal.GDT_Int32)
    raster.SetGeoTransform(template.GetGeoTransform())
    raster.SetProjection(template.GetProjection())
    gdal.RasterizeLayer(raster, [1], mem_layer, options=["ATTRIBUTE=qid"])
    return raster.GetRasterBand(1).ReadAsArray(), labels


def met_air_temperature(met_file: Path) -> dict[int, float]:
    frame = pd.read_csv(met_file, sep=r"\s+")
    frame.columns = [c.lstrip("%") for c in frame.columns]
    return {int(h) or 24: float(t) for h, t in zip(frame["it"], frame["Tair"])}


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--study", type=Path, default=STUDY)
    parser.add_argument("--res-tag", default="_1m")
    args = parser.parse_args()
    study, tag = args.study, args.res_tag
    out = study / "display"
    if out.exists() and any(out.iterdir()):
        raise SystemExit(f"Sortie existante, non écrasée : {out}")
    out.mkdir(parents=True, exist_ok=True)
    rasters = study / f"rasters{tag}"
    selection = yaml.safe_load((study / "met/journees/selection.yml").read_text(encoding="utf-8"))

    template_path = rasters / f"dsm_bati_sol{tag or '_50cm'}.tif"
    _, template = read(template_path)
    inputs = study / "umep_inputs.gpkg"
    quartier, quartier_labels = quartier_grid(inputs, template)
    road = rasterize(inputs, "road_footprints", template) > 0
    flood = rasterize(inputs, "flood_areas", template) > 0
    building = rasterize(inputs, "buildings", template) > 0
    context = np.where(road, "Voirie", np.where(flood, "Zones inondables", "Îlots")).astype(object)
    ground = ~building
    canopy = {v: read(rasters / f"cdsm_{v}.tif")[0] > 0 for v in ("saison_seche", "saison_pluies")}

    layers, indicators, profiles, corners = [], [], [], None
    tmrt_peak = {}
    for day, day_label in DAYS.items():
        info = selection["journees"][day]
        tair = met_air_temperature(study / "met/journees" / info["met_file"])
        variant = DAY_VARIANT[day]
        for veg, veg_label in VEGETATION.items():
            run = study / f"solweig{tag}" / f"{day}_{veg}"
            tmrt_files, shadow_files = hourly_files(run, "Tmrt"), hourly_files(run, "Shadow")
            tmrt, ds = read(tmrt_files[PEAK_HOUR])
            if ds.RasterXSize != template.RasterXSize or ds.GetGeoTransform() != template.GetGeoTransform():
                raise SystemExit(f"{run.name} : grille différente du DSM.")
            tmrt_peak[(day, veg)] = tmrt
            shade = sum(1 - read(shadow_files[h])[0] for h in SHADE_HOURS)
            for indicator, values in (("tmrt", tmrt), ("shade", shade)):
                name = f"{indicator}_{day}_{veg}.png"
                corners = write_png_3857(colorize(values, LEGENDS[indicator], building), template, out / name)
                layers.append({"indicator": indicator, "day": day, "vegetation": veg, "file": name})
            for qid, qlabel in quartier_labels.items():
                for ctx in ("Voirie", "Zones inondables", "Îlots"):
                    cell = ground & (quartier == qid) & (context == ctx)
                    if not cell.any():
                        continue
                    t = tmrt[cell]
                    indicators.append({
                        "day": day, "vegetation": veg, "quartier": qlabel, "context": ctx,
                        "area_m2": float(cell.sum() * abs(template.GetGeoTransform()[1] * template.GetGeoTransform()[5])),
                        "tmrt_14h_median_c": round(float(np.nanmedian(t)), 2),
                        "share_above_60c_pct": round(float(np.nanmean(t > HOT_THRESHOLD_C) * 100), 1),
                        "shade_hours_mean": round(float(np.nanmean(shade[cell])), 2),
                    })
            for hour in range(1, 25):
                t = read(tmrt_files[hour])[0]
                sh = read(shadow_files[hour])[0]
                classes = {
                    "Soleil, sans houppier": ground & ~canopy[variant] & (sh > 0.99),
                    "Sous houppier": ground & canopy[variant],
                    "Ombre des bâtiments": ground & ~canopy[variant] & (sh < 0.5),
                }
                for klass, cell in classes.items():
                    profiles.append({
                        "day": day, "vegetation": veg, "hour": hour, "class": klass,
                        "tmrt_median_c": round(float(np.nanmedian(t[cell])), 2) if cell.any() else None,
                        "tair_c": tair.get(hour),
                    })
        for veg in ("trans3", "trans15"):
            gain = tmrt_peak[(day, "sans_arbres")] - tmrt_peak[(day, veg)]
            name = f"cooling_{day}_{veg}.png"
            write_png_3857(colorize(gain, LEGENDS["cooling"], building), template, out / name)
            layers.append({"indicator": "cooling", "day": day, "vegetation": veg, "file": name})
            for qid, qlabel in quartier_labels.items():
                for ctx in ("Voirie", "Zones inondables", "Îlots"):
                    cell = ground & (quartier == qid) & (context == ctx)
                    if cell.any():
                        row = next(r for r in indicators if r["day"] == day and r["vegetation"] == veg
                                   and r["quartier"] == qlabel and r["context"] == ctx)
                        row["cooling_14h_mean_k"] = round(float(np.nanmean(gain[cell])), 2)

    pd.DataFrame(indicators).to_csv(out / "indicators.csv", index=False)
    pd.DataFrame(profiles).to_csv(out / "hourly_profiles.csv", index=False)
    manifest = {
        "generated_at": dt.datetime.now(dt.timezone.utc).isoformat(timespec="seconds"),
        "scenario": study.parents[2].name,
        "study": study.name,
        "resolution_m": abs(template.GetGeoTransform()[1]),
        "peak_hour": PEAK_HOUR,
        "shade_window": "10 h–16 h",
        "hot_threshold_c": HOT_THRESHOLD_C,
        "coordinates": corners,
        "days": [{"id": d, "label": l, "date": selection["journees"][d]["date"]} for d, l in DAYS.items()],
        "vegetation": [{"id": v, "label": l} for v, l in VEGETATION.items()],
        "legends": LEGENDS,
        "layers": layers,
        "source": f"UMEP SOLWEIG v2025a, ERA5 (2 m), EPSG:32628",
    }
    (out / "manifest.json").write_text(json.dumps(manifest, ensure_ascii=False, indent=1), encoding="utf-8")
    print(f"Produits d'affichage écrits : {out} ({len(layers)} couches)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

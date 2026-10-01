#!/usr/bin/env Rscript
# Logements par bâtiment du scénario, calculés avec les fonctions pures de
# l'application (même chaîne que app.R : normalisation, hauteurs, affectation
# par niveau). Sortie : CSV building_id, x, y (EPSG:32628), logements.
suppressMessages({
  library(sf)
})
args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args) >= 1) args[[1]] else "."
out <- if (length(args) >= 2) args[[2]] else stop("Chemin de sortie requis.")
if (file.exists(out)) stop("Sortie existante, non écrasée : ", out)
setwd(root)
for (f in c("initial_data.R", "calculations.R", "spatial_data.R", "presentation.R", "scenario_data.R")) {
  source(file.path("R", f), encoding = "UTF-8")
}
directory <- file.path("data", "scenarios", "scenario_01")
data <- read_scenario_bundle(directory)
model <- data$model
products <- ensure_product_building_assumptions(model$products)
products$is_cessible <- as.logical(products$is_cessible)
buildings <- normalize_scenario_buildings(data$spatial$buildings, products, "scenario_01")
buildings <- calculate_osm_building_heights(
  buildings,
  ground_floor_height_m = model$height_assumptions$ground_floor_height_m,
  upper_floor_height_m = model$height_assumptions$upper_floor_height_m,
  default_levels = model$height_assumptions$default_levels,
  products = products
)
buildings <- buildings[buildings$included_in_simulation, , drop = FALSE]
alloc <- calculate_building_level_allocations(buildings, products, model$building_level_allocations)
non_housing <- c("ec", "ep", "el", "ev")
housing <- !(alloc$product_id %in% non_housing) & !grepl("_com$", alloc$product_id) & !is.na(alloc$product_id)
units <- tapply(alloc$unit_count * housing, alloc$building_id, sum)
centroids <- st_coordinates(st_centroid(st_geometry(st_transform(buildings, 32628))))
result <- data.frame(
  building_id = buildings$building_id,
  product_id = buildings$product_id,
  x = round(centroids[, 1], 2),
  y = round(centroids[, 2], 2),
  logements = as.numeric(units[buildings$building_id]),
  stringsAsFactors = FALSE
)
result$logements[is.na(result$logements)] <- 0
utils::write.csv(result, out, row.names = FALSE)
cat("bâtiments", nrow(result), "logements", sum(result$logements), "\n")
print(tapply(result$logements, result$product_id, sum))

#!/usr/bin/env Rscript
# Enregistre l'empreinte des entrées des études UMEP d'un scénario
# (exports/umep/<étude>/empreinte_entrees.json + empreinte_arbres.csv.gz).
#
# --instantane : reconstruit les entrées réellement utilisées par les études
#   déjà calculées (copie dérivée ombrage_arbres/umep_inputs.gpkg et
#   logements de bilan_energetique), plutôt que l'état actuel du scénario.
# Sans option : état actuel du scénario (à lancer juste après un calcul).
# Usage : Rscript scripts/umep_partage/enregistrer_empreintes.R <scenario_id> [--instantane]
suppressMessages(library(sf))
sf::sf_use_s2(FALSE)
for (f in c("initial_data", "calculations", "spatial_data", "presentation", "scenario_data", "umep_sharing")) {
  source(file.path("R", paste0(f, ".R")), encoding = "UTF-8")
}
args <- commandArgs(trailingOnly = TRUE)
scenario_id <- args[[1]]
snapshot <- "--instantane" %in% args
directory <- file.path("data", "scenarios", scenario_id)
umep <- file.path(directory, "exports", "umep")
data <- read_scenario_bundle(directory)
trees <- resolve_scenario_trees(data$spatial, file.path("data", "sig", "arbres.gpkg"))
inputs <- umep_scenario_fingerprints(data$spatial, data$model, scenario_id, trees)

if (snapshot) {
  path <- file.path(umep, "ombrage_arbres", "umep_inputs.gpkg")
  read <- function(layer) sf::st_read(path, layer = layer, quiet = TRUE)
  spatial <- list(buildings = read("buildings"), roads = read("roads"),
                  road_footprints = read("road_footprints"), flood_areas = read("flood_areas"),
                  quartiers = read("quartiers"))
  snapshot_trees <- read("trees")
  contexts <- umep_tree_contexts(snapshot_trees, spatial)
  config <- digest::digest(file = file.path("scripts", "umep_trees", "config.yaml"), algo = "sha256")
  tree_base <- digest::digest(paste(umep_layer_fingerprint(snapshot_trees, id = "tree_id"), config), algo = "sha256")
  housing <- utils::read.csv(file.path(umep, "bilan_energetique", "inputs", "logements_batiments.csv"))
  inputs <- list(
    fingerprints = c(
      batiments = umep_layer_fingerprint(spatial$buildings, "height_m", id = "building_id"),
      arbres = digest::digest(paste(tree_base, paste(contexts$tree_id, contexts$context, contexts$narrow, collapse = "\n")), algo = "sha256"),
      voirie_emprises = umep_layer_fingerprint(spatial$road_footprints),
      zones_inondables = umep_layer_fingerprint(spatial$flood_areas, "flood_type"),
      quartiers = umep_layer_fingerprint(spatial$quartiers, "district_label"),
      logements = digest::digest(paste(housing$building_id, sprintf("%.2f", housing$logements), collapse = "\n"), algo = "sha256")
    ),
    tree_base = tree_base,
    tree_contexts = contexts
  )
}
for (study in names(umep_study_dependencies)) {
  study_directory <- file.path(umep, study)
  if (!dir.exists(study_directory)) next
  write_umep_study_fingerprint(study_directory, study, scenario_id, inputs)
  cat("empreinte enregistrée :", study, "\n")
}

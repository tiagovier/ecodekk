scenario_bundle_paths <- function(directory) {
  list(
    directory = directory,
    spatial = file.path(directory, "spatial.gpkg"),
    model = file.path(directory, "model.rds"),
    manifest = file.path(directory, "scenario.yml"),
    exports = file.path(directory, "exports")
  )
}

resolve_scenario_program_source <- function(scenario_id, stored_source = NULL) {
  scenario_id <- normalize_scenario_id(scenario_id)
  if (!identical(scenario_id, "base")) return("buildings")

  source <- if (is.null(stored_source)) "manual" else stored_source
  if (length(source) != 1L || !source %in% c("manual", "buildings")) {
    stop("Source de programmation inconnue dans le modèle du scénario.")
  }
  source
}

scenario_model <- function(
  scenario_id,
  reference_building_count,
  sources,
  construction_categories,
  products,
  districts,
  program,
  development_expenses,
  financial_assumptions,
  height_assumptions,
  building_level_allocations,
  program_source = "manual"
) {
  list(
    schema_version = 2L,
    scenario_id = normalize_scenario_id(scenario_id),
    updated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    reference_building_count = as.integer(reference_building_count),
    sources = as.data.frame(sources, stringsAsFactors = FALSE),
    construction_categories = as.data.frame(construction_categories, stringsAsFactors = FALSE),
    products = as.data.frame(products, stringsAsFactors = FALSE),
    districts = as.data.frame(districts, stringsAsFactors = FALSE),
    program = as.data.frame(program, stringsAsFactors = FALSE),
    development_expenses = as.data.frame(development_expenses, stringsAsFactors = FALSE),
    financial_assumptions = financial_assumptions,
    height_assumptions = height_assumptions,
    building_level_allocations = as.data.frame(
      building_level_allocations, stringsAsFactors = FALSE
    ),
    program_source = program_source
  )
}

validate_scenario_model <- function(model) {
  required <- c(
    "schema_version", "scenario_id", "reference_building_count", "sources",
    "construction_categories", "products", "districts", "program",
    "development_expenses", "financial_assumptions", "height_assumptions",
    "building_level_allocations"
  )
  missing <- setdiff(required, names(model))
  if (length(missing)) {
    stop("Objets manquants dans le modèle du scénario : ", paste(missing, collapse = ", "), ".")
  }
  if (!identical(as.integer(model$schema_version), 2L)) {
    stop("Version du modèle de scénario non prise en charge.")
  }
  normalize_scenario_id(model$scenario_id)
  program_source <- if (is.null(model$program_source)) "manual" else model$program_source
  if (length(program_source) != 1L || !program_source %in% c("manual", "buildings")) {
    stop("Source de programmation inconnue dans le modèle du scénario.")
  }
  invisible(TRUE)
}

atomic_replace_file <- function(temporary, target) {
  backup <- paste0(target, ".backup")
  if (file.exists(backup)) unlink(backup)
  if (file.exists(target) && !file.rename(target, backup)) {
    stop("Impossible de préparer le remplacement de ", basename(target), ".")
  }
  if (!file.rename(temporary, target)) {
    if (file.exists(backup)) file.rename(backup, target)
    stop("Impossible d’enregistrer ", basename(target), ".")
  }
  if (file.exists(backup)) unlink(backup)
  invisible(target)
}

write_scenario_spatial_geopackage <- function(
  path, buildings, roads, parcels, context
) {
  derived_building_fields <- c(
    "classification_is_ambiguous", "classification_status",
    "needs_classification", "needs_reclassification", "needs_inclusion",
    "land_use_incongruent", "levels_incongruent", "geometry_incongruent",
    "control_required", "control_action", "has_incongruity",
    "incongruity_flag", "levels_effective", "levels_assumption_source",
    "height_m", "footprint_area_sqm", "estimated_sdp_sqm",
    "ground_floor_product_id", "upper_floor_product_id",
    "model_sdp_per_unit", "sdp_difference_sqm", "id_generated",
    "product_alias_applied", "product_id_original"
  )
  buildings <- buildings[, setdiff(names(buildings), derived_building_fields), drop = FALSE]
  layers <- scenario_spatial_layers(buildings, roads, parcels, context)
  invalid <- names(layers)[!vapply(layers, inherits, logical(1), what = "sf")]
  if (length(invalid)) {
    stop("Couches SIG invalides : ", paste(invalid, collapse = ", "), ".")
  }
  if ("included_in_simulation" %in% names(buildings)) {
    included <- !is.na(buildings$included_in_simulation) &
      as.logical(buildings$included_in_simulation)
    layers$buildings <- buildings[included, , drop = FALSE]
  }
  if (!nrow(layers$buildings)) {
    stop("Le scénario doit conserver au moins un bâtiment inclus.")
  }

  directory <- dirname(path)
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)
  temporary <- tempfile("spatial-", tmpdir = directory, fileext = ".gpkg")
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  for (layer_name in names(layers)) {
    layer <- layers[[layer_name]]
    if (!nrow(layer)) next
    sf::st_write(
      layer, temporary, layer = layer_name, driver = "GPKG",
      append = FALSE, quiet = TRUE
    )
  }
  atomic_replace_file(temporary, path)
}

read_scenario_spatial_geopackage <- function(path) {
  if (!file.exists(path)) stop("Le GeoPackage spatial du scénario est introuvable.")
  expected <- c(
    "buildings", "roads", "parcels", "quartiers", "land_use",
    "road_footprints", "flood_areas", "project_boundary", "title_boundary"
  )
  available <- sf::st_layers(path)$name
  missing <- setdiff(expected, available)
  if (length(missing)) {
    stop("Couches manquantes dans le GeoPackage spatial : ", paste(missing, collapse = ", "), ".")
  }
  layers <- c(expected, intersect("trees", available))
  stats::setNames(
    lapply(layers, function(layer) sf::st_read(path, layer = layer, quiet = TRUE)),
    layers
  )
}

write_rds_atomic <- function(object, path) {
  directory <- dirname(path)
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)
  temporary <- tempfile("model-", tmpdir = directory, fileext = ".rds")
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  saveRDS(object, temporary, version = 3)
  atomic_replace_file(temporary, path)
}

write_csv_atomic <- function(data, path) {
  directory <- dirname(path)
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)
  temporary <- tempfile("export-", tmpdir = directory, fileext = ".csv")
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  utils::write.csv(data, temporary, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  atomic_replace_file(temporary, path)
}

scenario_export_tables <- function(model) {
  validate_scenario_model(model)
  calculations <- run_financial_model(
    model$construction_categories,
    model$products,
    model$districts,
    model$program,
    model$development_expenses,
    model$financial_assumptions
  )
  effective_cost <- get_effective_construction_cost(
    model$products, model$construction_categories
  )
  calculated_products <- model$products
  calculated_products$effective_construction_cost_cfa_sqm <- effective_cost
  calculated_products$effective_land_charge_cfa_sqm <-
    calculate_product_land_charge(model$products, effective_cost)

  balance <- build_development_balance_table(
    calculations$balance, model$financial_assumptions
  )
  balance_metrics <- data.frame(
    metric_id = names(calculations$balance$metrics),
    value = as.numeric(calculations$balance$metrics),
    stringsAsFactors = FALSE
  )

  list(
    construction_categories = model$construction_categories,
    products = model$products,
    program = model$program,
    districts = model$districts,
    development_expenses = model$development_expenses,
    financial_assumptions = scenario_parameter_table(model$financial_assumptions),
    height_assumptions = scenario_parameter_table(model$height_assumptions),
    building_level_allocations = model$building_level_allocations,
    calculated_products = calculated_products,
    calculated_program = calculations$program,
    calculated_product_summary = calculations$products,
    calculated_district_summary = calculations$districts,
    calculated_development_balance = balance,
    calculated_balance_metrics = balance_metrics
  )
}

write_scenario_exports <- function(model, directory) {
  exports <- scenario_export_tables(model)
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)
  for (table_name in names(exports)) {
    write_csv_atomic(
      exports[[table_name]],
      file.path(directory, paste0(table_name, ".csv"))
    )
  }
  invisible(exports)
}

write_scenario_manifest <- function(model, paths, building_count) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Le package R 'yaml' est requis pour écrire le manifeste du scénario.")
  }
  manifest <- list(
    scenario_id = model$scenario_id,
    schema_version = as.integer(model$schema_version),
    updated_at_utc = model$updated_at_utc,
    spatial_file = basename(paths$spatial),
    model_file = basename(paths$model),
    exports_directory = basename(paths$exports),
    building_count = as.integer(building_count),
    reference_building_count = as.integer(model$reference_building_count),
    removed_building_count = as.integer(model$reference_building_count - building_count)
  )
  temporary <- tempfile("manifest-", tmpdir = paths$directory, fileext = ".yml")
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  yaml::write_yaml(manifest, temporary)
  atomic_replace_file(temporary, paths$manifest)
}

write_scenario_model_files <- function(directory, model, building_count) {
  validate_scenario_model(model)
  paths <- scenario_bundle_paths(directory)
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)
  model$updated_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  write_rds_atomic(model, paths$model)
  write_scenario_exports(model, paths$exports)
  write_scenario_manifest(model, paths, building_count)
  invisible(paths)
}

write_scenario_bundle <- function(
  directory, model, buildings, roads, parcels, context
) {
  validate_scenario_model(model)
  paths <- scenario_bundle_paths(directory)
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)
  included <- if ("included_in_simulation" %in% names(buildings)) {
    !is.na(buildings$included_in_simulation) & as.logical(buildings$included_in_simulation)
  } else {
    rep(TRUE, nrow(buildings))
  }
  write_scenario_spatial_geopackage(
    paths$spatial, buildings, roads, parcels, context
  )
  write_scenario_model_files(directory, model, sum(included))
  invisible(paths)
}

create_scenario_snapshot <- function(
  root, scenario_id, model, buildings, roads, parcels, context
) {
  scenario_id <- normalize_scenario_id(scenario_id)
  if (identical(scenario_id, "base")) {
    stop("L’identifiant « base » est réservé au scénario de référence.")
  }
  if (!dir.exists(root) && !dir.create(root, recursive = TRUE)) {
    stop("Impossible de créer le dossier des scénarios.")
  }

  target <- file.path(root, scenario_id)
  if (file.exists(target) || dir.exists(target)) {
    stop("Un scénario portant cet identifiant existe déjà.")
  }

  snapshot <- model
  snapshot$scenario_id <- scenario_id
  snapshot$program_source <- "buildings"
  snapshot$updated_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  validate_scenario_model(snapshot)

  staging <- tempfile(paste0(".", scenario_id, "-"), tmpdir = root)
  on.exit(if (dir.exists(staging)) unlink(staging, recursive = TRUE), add = TRUE)
  write_scenario_bundle(
    staging, snapshot, buildings, roads, parcels, context
  )
  if (!file.rename(staging, target)) {
    stop("Impossible de finaliser le nouveau scénario.")
  }
  normalizePath(target, winslash = "/", mustWork = TRUE)
}

write_scenario_archive <- function(
  zipfile, scenario_id, model, buildings, roads, parcels, context
) {
  if (!requireNamespace("zip", quietly = TRUE)) {
    stop("Le package R ‘zip’ est requis pour télécharger un scénario.")
  }
  scenario_id <- normalize_scenario_id(scenario_id)
  temporary_root <- tempfile("ecodekk-download-")
  if (!dir.create(temporary_root, recursive = TRUE)) {
    stop("Impossible de préparer le téléchargement du scénario.")
  }
  on.exit(unlink(temporary_root, recursive = TRUE), add = TRUE)

  snapshot <- model
  snapshot$scenario_id <- scenario_id
  snapshot$updated_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  validate_scenario_model(snapshot)
  scenario_directory <- file.path(temporary_root, scenario_id)
  write_scenario_bundle(
    scenario_directory, snapshot, buildings, roads, parcels, context
  )
  zip::zipr(
    zipfile = zipfile, files = scenario_id, root = temporary_root
  )
  if (!file.exists(zipfile) || file.info(zipfile)$size <= 0) {
    stop("La création de l’archive du scénario a échoué.")
  }
  invisible(zipfile)
}

read_scenario_bundle <- function(directory) {
  paths <- scenario_bundle_paths(directory)
  required_paths <- paths[c("spatial", "model", "manifest")]
  missing <- names(required_paths)[!vapply(required_paths, file.exists, logical(1))]
  if (length(missing)) {
    stop("Fichiers manquants dans le scénario : ", paste(missing, collapse = ", "), ".")
  }
  model <- readRDS(paths$model)
  if (is.null(model$program_source)) model$program_source <- "manual"
  validate_scenario_model(model)
  list(
    directory = normalizePath(directory, winslash = "/", mustWork = TRUE),
    spatial = read_scenario_spatial_geopackage(paths$spatial),
    model = model,
    manifest = yaml::read_yaml(paths$manifest)
  )
}

discover_scenario_bundles <- function(root) {
  if (!dir.exists(root)) {
    return(data.frame(path = character(), label = character(), stringsAsFactors = FALSE))
  }
  spatial_files <- list.files(
    root, pattern = "^spatial\\.gpkg$", recursive = TRUE,
    full.names = TRUE, ignore.case = TRUE
  )
  directories <- unique(dirname(spatial_files))
  complete <- vapply(directories, function(directory) {
    paths <- scenario_bundle_paths(directory)
    file.exists(paths$model) && file.exists(paths$manifest)
  }, logical(1))
  directories <- directories[complete]
  if (!length(directories)) {
    return(data.frame(path = character(), label = character(), stringsAsFactors = FALSE))
  }
  directories <- normalizePath(directories, winslash = "/", mustWork = TRUE)
  root_path <- normalizePath(root, winslash = "/", mustWork = TRUE)
  data.frame(
    path = directories,
    label = substring(directories, nchar(root_path) + 2L),
    stringsAsFactors = FALSE
  )
}

scenario_component_signature <- function(path) {
  info <- file.info(path)
  if (!nrow(info) || is.na(info$size) || is.na(info$mtime)) return(NA_character_)
  paste(info$size, as.numeric(info$mtime), sep = ":")
}

scenario_bundle_signature <- function(directory) {
  paths <- scenario_bundle_paths(directory)
  files <- c(paths$spatial, paths$model, paths$manifest)
  paste(vapply(files, scenario_component_signature, character(1)), collapse = "|")
}

model_from_legacy_scenario <- function(scenario_data) {
  tables <- scenario_data$tables
  metadata <- tables$scenario_metadata
  reference_count <- if (
    !is.null(metadata) && "reference_building_count" %in% names(metadata)
  ) {
    metadata$reference_building_count[1]
  } else {
    metadata$building_count[1] + metadata$removed_building_count[1]
  }
  scenario_model(
    scenario_id = metadata$scenario_id[1],
    reference_building_count = reference_count,
    sources = tables$scenario_sources,
    construction_categories = tables$construction_categories,
    products = tables$products,
    districts = tables$districts,
    program = tables$program,
    development_expenses = tables$development_expenses,
    financial_assumptions = scenario_parameters_from_table(
      tables$financial_assumptions, initial_financial_assumptions()
    ),
    height_assumptions = scenario_parameters_from_table(
      tables$height_assumptions, initial_height_assumptions()
    ),
    building_level_allocations = tables$building_level_allocations
  )
}

migrate_legacy_scenario <- function(legacy_path, directory) {
  legacy <- read_scenario_geopackage(legacy_path)
  spatial <- legacy$spatial
  context <- list(
    quartiers = spatial$quartiers,
    land_use = spatial$land_use,
    road_footprints = spatial$road_footprints,
    flood_areas = spatial$flood_areas,
    project_boundary = spatial$project_boundary,
    title_boundary = spatial$title_boundary
  )
  model <- model_from_legacy_scenario(legacy)
  write_scenario_bundle(
    directory = directory,
    model = model,
    buildings = spatial$buildings,
    roads = spatial$roads,
    parcels = spatial$parcels,
    context = context
  )
  read_scenario_bundle(directory)
}

# Scénario de référence protégé et scénarios masqués à l'utilisateur.
baseline_scenario_id <- "scenario_01"
hidden_scenario_ids <- c("base")

visible_scenario_inventory <- function(inventory) {
  inventory[!inventory$label %in% hidden_scenario_ids, , drop = FALSE]
}

scenario_label_from_path <- function(directory, root) {
  root_path <- normalizePath(root, winslash = "/", mustWork = TRUE)
  path <- normalizePath(directory, winslash = "/", mustWork = TRUE)
  if (!startsWith(path, paste0(root_path, "/"))) {
    stop("Le dossier n’appartient pas au répertoire des scénarios.")
  }
  substring(path, nchar(root_path) + 2L)
}

# Nouveau scénario à partir de l'état enregistré d'un scénario existant.
# Les résultats d'études (exports/umep) ne sont pas copiés.
duplicate_scenario_bundle <- function(source_directory, root, scenario_id) {
  data <- read_scenario_bundle(source_directory)
  spatial <- data$spatial
  context <- spatial[setdiff(names(spatial), c("buildings", "roads", "parcels"))]
  create_scenario_snapshot(
    root = root,
    scenario_id = scenario_id,
    model = data$model,
    buildings = spatial$buildings,
    roads = spatial$roads,
    parcels = spatial$parcels,
    context = context
  )
}

# Suppression réversible : le dossier est déplacé dans trash_root, jamais effacé.
delete_scenario_bundle <- function(directory, root, trash_root, timestamp = Sys.time()) {
  label <- scenario_label_from_path(directory, root)
  if (identical(label, baseline_scenario_id)) {
    stop("Le scénario de référence ", baseline_scenario_id, " ne peut pas être supprimé.")
  }
  if (label %in% hidden_scenario_ids) {
    stop("Ce scénario est réservé et ne peut pas être supprimé depuis l’application.")
  }
  if (!dir.exists(trash_root) && !dir.create(trash_root, recursive = TRUE)) {
    stop("Impossible de créer le dossier des scénarios supprimés.")
  }
  target <- file.path(
    trash_root,
    paste0(gsub("/", "__", label, fixed = TRUE), "_", format(timestamp, "%Y%m%d_%H%M%S"))
  )
  if (file.exists(target)) stop("Un scénario supprimé porte déjà ce nom.")
  if (!file.rename(directory, target)) stop("Impossible de supprimer le scénario.")
  normalizePath(target, winslash = "/", mustWork = TRUE)
}

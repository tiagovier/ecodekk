test_that("les scénarios de travail utilisent toujours les bâtiments SIG", {
  expect_identical(resolve_scenario_program_source("scenario_01", "manual"), "buildings")
  expect_identical(resolve_scenario_program_source("scenario_02", NULL), "buildings")
  expect_identical(resolve_scenario_program_source("base", "manual"), "manual")
  expect_identical(resolve_scenario_program_source("base", "buildings"), "buildings")
})

test_that("un scénario sépare les couches QGIS du modèle financier", {
  points <- sf::st_sfc(
    sf::st_point(c(-17, 14)),
    sf::st_point(c(-17.001, 14.001)),
    crs = 4326
  )
  buildings <- sf::st_sf(
    building_id = c("included", "removed"),
    product_id = c("rm_2", NA_character_),
    included_in_simulation = c(TRUE, FALSE),
    geometry = points
  )
  one_feature <- buildings[1, "building_id", drop = FALSE]
  context <- list(
    quartiers = one_feature,
    land_use = one_feature,
    road_footprints = one_feature,
    flood_areas = one_feature,
    project_boundary = one_feature,
    title_boundary = one_feature
  )
  directory <- tempfile("scenario-bundle-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)

  model <- scenario_model(
    scenario_id = "test_1",
    reference_building_count = 2,
    sources = data.frame(
      scenario_layer = "buildings",
      reference_source = "reference.gpkg",
      stringsAsFactors = FALSE
    ),
    construction_categories = initial_construction_categories(),
    products = initial_products(),
    districts = initial_districts(),
    program = initial_program(),
    development_expenses = initial_development_expenses(),
    financial_assumptions = initial_financial_assumptions(),
    height_assumptions = initial_height_assumptions(),
    building_level_allocations = data.frame(
      building_id = character(),
      level_number = integer(),
      product_id = character(),
      stringsAsFactors = FALSE
    )
  )
  write_scenario_bundle(
    directory, model, buildings, one_feature, one_feature, context
  )
  restored <- read_scenario_bundle(directory)
  paths <- scenario_bundle_paths(directory)

  expect_equal(
    sort(sf::st_layers(paths$spatial)$name),
    sort(c(
      "buildings", "roads", "parcels", "quartiers", "land_use",
      "road_footprints", "flood_areas", "project_boundary", "title_boundary"
    ))
  )
  expect_equal(nrow(restored$spatial$buildings), 1)
  expect_equal(restored$spatial$buildings$building_id, "included")
  expect_identical(restored$model$products, model$products)
  expect_identical(restored$model$program, model$program)
  expect_identical(restored$model$program_source, "manual")
  expect_true(file.exists(file.path(paths$exports, "products.csv")))
  expect_true(file.exists(file.path(paths$exports, "calculated_program.csv")))
  expect_true(file.exists(file.path(paths$exports, "calculated_development_balance.csv")))

  connection <- DBI::dbConnect(RSQLite::SQLite(), paths$spatial)
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  contents <- DBI::dbGetQuery(
    connection,
    "SELECT table_name, data_type FROM gpkg_contents"
  )
  expect_true(all(contents$data_type == "features"))
  expect_false(any(contents$table_name %in% c("products", "program")))

  snapshot_root <- tempfile("scenario-snapshots-")
  dir.create(snapshot_root)
  on.exit(unlink(snapshot_root, recursive = TRUE), add = TRUE)
  snapshot_path <- create_scenario_snapshot(
    snapshot_root, "scenario_02", model,
    buildings, one_feature, one_feature, context
  )
  snapshot <- read_scenario_bundle(snapshot_path)
  expect_identical(snapshot$model$scenario_id, "scenario_02")
  expect_identical(snapshot$model$program_source, "buildings")
  expect_equal(nrow(snapshot$spatial$buildings), 1)
  expect_error(
    create_scenario_snapshot(
      snapshot_root, "scenario_02", model,
      buildings, one_feature, one_feature, context
    ),
    "existe déjà"
  )

  archive <- tempfile(fileext = ".zip")
  write_scenario_archive(
    archive, "test_1", model,
    buildings, one_feature, one_feature, context
  )
  archive_entries <- zip::zip_list(archive)$filename
  expect_true(all(c(
    "test_1/spatial.gpkg", "test_1/model.rds", "test_1/scenario.yml"
  ) %in% archive_entries))
})

test_that("les signatures du spatial et du modèle sont indépendantes", {
  directory <- tempfile("scenario-signatures-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  paths <- scenario_bundle_paths(directory)
  writeLines("spatial", paths$spatial)
  writeLines("model", paths$model)

  spatial_before <- scenario_component_signature(paths$spatial)
  model_before <- scenario_component_signature(paths$model)
  Sys.sleep(1.1)
  writeLines("model modifié", paths$model)

  expect_identical(scenario_component_signature(paths$spatial), spatial_before)
  expect_false(identical(scenario_component_signature(paths$model), model_before))
})

test_that("la couche optionnelle des arbres est conservée dans le GeoPackage", {
  points <- sf::st_sfc(
    sf::st_point(c(-17, 14)),
    sf::st_point(c(-17.001, 14.001)),
    crs = 4326
  )
  buildings <- sf::st_sf(
    building_id = "b1", product_id = "rm_2", included_in_simulation = TRUE,
    geometry = points[1]
  )
  one_feature <- buildings[, "building_id", drop = FALSE]
  context <- list(
    quartiers = one_feature, land_use = one_feature,
    road_footprints = one_feature, flood_areas = one_feature,
    project_boundary = one_feature, title_boundary = one_feature
  )
  trees <- sf::st_sf(
    tree_id = c("tree_a1", "tree_a2"),
    source_handle = c("A1", "A2"),
    tree_category = "Végétation",
    umep_tree_type = c(2L, NA_integer_),
    total_height_m = c(12.5, NA_real_),
    trunk_height_m = c(3, NA_real_),
    crown_diameter_m = c(9.2, NA_real_),
    shadow_ready = c(TRUE, FALSE),
    geometry = points
  )
  path <- tempfile("spatial-trees-", fileext = ".gpkg")
  on.exit(unlink(path), add = TRUE)

  write_scenario_spatial_geopackage(path, buildings, one_feature, one_feature, context)
  without_trees <- read_scenario_spatial_geopackage(path)
  expect_false("trees" %in% names(without_trees))
  expect_false("trees" %in% sf::st_layers(path)$name)

  context$trees <- trees
  write_scenario_spatial_geopackage(path, buildings, one_feature, one_feature, context)
  restored <- read_scenario_spatial_geopackage(path)$trees
  expect_identical(restored$tree_id, trees$tree_id)
  expect_identical(restored$tree_category, rep("Végétation", 2))
  expect_equal(restored$total_height_m, c(12.5, NA))
  expect_true(is.na(restored$umep_tree_type[2]))
  expect_equal(sf::st_crs(restored)$epsg, 4326L)
  expect_equal(tree_shadow_ready(restored), c(TRUE, FALSE))
})

test_that("un scénario sans arbres reste lisible", {
  layers <- list(buildings = NULL)
  missing_reference <- tempfile(fileext = ".gpkg")
  empty <- resolve_scenario_trees(layers, missing_reference)
  expect_s3_class(empty, "sf")
  expect_equal(nrow(empty), 0)
  expect_true(all(tree_umep_fields %in% names(empty)))

  stored <- empty_project_trees()
  expect_identical(resolve_scenario_trees(list(trees = stored), missing_reference), stored)
})

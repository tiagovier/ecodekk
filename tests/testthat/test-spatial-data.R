test_that("les limites parcellaires du GeoPackage sont préparées pour la carte", {
  parcels <- load_parcel_boundaries(
    file.path("..", "..", "data", "sig", "projet-120526.gpkg")
  )

  expect_s3_class(parcels, "sf")
  expect_equal(nrow(parcels), 324)
  expect_equal(sf::st_crs(parcels)$epsg, 4326)
  expect_equal(sum(parcels$parcel_type == "Parcelles résidentielles"), 289)
  expect_equal(sum(parcels$parcel_type == "Parcelles Tertiaire Commerce"), 23)
  expect_equal(sum(parcels$parcel_type == "Parcelles Mixte"), 11)
  expect_equal(sum(parcels$parcel_type == "Parcelles Agricoles"), 1)
})

test_that("thies13 fournit les mêmes bâtiments et voiries aux vues 2D et 3D", {
  urban <- load_osm_urban_data(
    file.path("..", "..", "data", "sig", "thies13.osm")
  )

  expect_s3_class(urban$buildings, "sf")
  expect_s3_class(urban$roads, "sf")
  expect_s3_class(urban$excluded_buildings, "sf")
  expect_equal(nrow(urban$buildings), 441)
  expect_equal(nrow(urban$excluded_buildings), 2)
  expect_equal(nrow(urban$roads), 121)
  expect_equal(sum(urban$buildings$building_function == "residential"), 320)
  expect_equal(sum(is.na(urban$buildings$levels)), 5)
  expect_setequal(urban$excluded_buildings$osm_id, c("-38433", "-38434"))
  expect_false(any(urban$buildings$osm_id %in% c("-38433", "-38434")))
})

test_that("les empreintes corrigées sont polygonisées et identifiées", {
  corrected <- load_corrected_buildings(
    file.path("..", "..", "data", "sig", "buildings.gpkg")
  )

  expect_s3_class(corrected, "sf")
  expect_equal(nrow(corrected), 413)  # 11 cours intérieures exclues
  expect_equal(sf::st_crs(corrected)$epsg, 4326)
  expect_true(all(sf::st_geometry_type(corrected) == "POLYGON"))
  expect_equal(length(unique(corrected$building_id)), 413)
})

test_that("les attributs OSM sont transférés avec une qualité explicite", {
  urban <- load_osm_urban_data(
    file.path("..", "..", "data", "sig", "thies13.osm")
  )
  corrected <- load_corrected_buildings(
    file.path("..", "..", "data", "sig", "buildings.gpkg")
  )
  consolidated <- consolidate_buildings(corrected, urban$buildings)

  expect_equal(nrow(consolidated), 523)
  expect_equal(sum(consolidated$geometry_source == "Empreinte corrigée"), 413)
  expect_equal(sum(consolidated$geometry_source == "OSM conservé"), 110)
  expect_equal(sum(consolidated$attribute_match_quality %in% c("forte", "moyenne", "faible")), 358)
  expect_equal(sum(consolidated$attribute_match_quality == "non apparié"), 55)
  expect_equal(sum(is.na(consolidated$levels)), 60)
  expect_equal(length(unique(consolidated$building_id)), 523)
  expect_true(all(is.na(consolidated$osm_id[consolidated$attribute_match_quality == "non apparié"])))
  expect_false(any(consolidated$osm_id %in% c("-38433", "-38434"), na.rm = TRUE))
})

test_that("les couches contextuelles du projet sont normalisées", {
  context <- load_project_context_layers(file.path("..", "..", "data", "sig"))

  expect_equal(nrow(context$quartiers), 9)
  expect_equal(nrow(context$land_use), 199)
  expect_equal(nrow(context$trees), 22100)
  expect_equal(nrow(context$road_footprints), 35)
  expect_equal(nrow(context$flood_areas), 24)
  expect_equal(nrow(context$project_boundary), 5)
  expect_equal(nrow(context$title_boundary), 1)
  expect_true(all(vapply(context, function(x) sf::st_crs(x)$epsg == 4326, logical(1))))
  expect_equal(length(unique(context$trees$tree_id)), 22100)
  expect_equal(unique(context$trees$tree_category), "Végétation")
  expect_true(all(is.na(context$trees$umep_tree_type)))
  expect_true(all(is.na(context$trees$total_height_m)))
  expect_true(all(is.na(context$trees$trunk_height_m)))
  expect_true(all(is.na(context$trees$crown_diameter_m)))
  expect_false(any(context$trees$shadow_ready))
})


test_that("les bâtiments sont liés aux quartiers, usages et produits sans masquer les absences", {
  urban <- load_osm_urban_data(
    file.path("..", "..", "data", "sig", "thies13.osm")
  )
  corrected <- load_corrected_buildings(
    file.path("..", "..", "data", "sig", "buildings.gpkg")
  )
  context <- load_project_context_layers(file.path("..", "..", "data", "sig"))
  buildings <- link_buildings_to_context(
    consolidate_buildings(corrected, urban$buildings),
    context$quartiers,
    context$land_use
  )

  expect_equal(sum(!is.na(buildings$district_id)), 522)
  expect_equal(sum(!is.na(buildings$land_use_zone)), 511)
  expect_equal(sum(!is.na(buildings$product_id)), 395)
  expect_equal(sum(buildings$classification_is_ambiguous), 110)
  expect_equal(sum(buildings$control_required), 131)
  expect_equal(sum(buildings$needs_classification), 128)
  expect_equal(sum(buildings$levels_incongruent), 0)
  expect_equal(sum(buildings$geometry_incongruent), 4)
  expect_equal(sum(buildings$classification_status == "Produit à affecter — hors nomenclature"), 18)
  expect_true(all(buildings$product_link_source[is.na(buildings$product_id)] == "non renseigné"))
  expect_true(all(buildings$included_in_simulation))
})

test_that("les corrections de bâtiments sont enregistrées et rechargées", {
  geometry <- sf::st_sfc(sf::st_point(c(-17, 14)), crs = 4326)
  buildings <- sf::st_sf(
    building_id = "test_1",
    building_function = "residential",
    function_label = "Résidentiel",
    levels = 2,
    levels_source = "2",
    district_id = "q1",
    land_use_zone = "RM 1",
    land_use_label_spatial = "RM 1",
    product_id = NA_character_,
    product_link_source = "non renseigné",
    included_in_simulation = TRUE,
    control_edited = FALSE,
    geometry = geometry
  )
  buildings <- add_building_classification_status(buildings)
  edited <- buildings
  edited$product_id <- "rm_1a"
  edited$product_link_source <- "manuel"
  edited$control_edited <- TRUE
  edited <- add_building_classification_status(edited)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)

  write_building_control_overrides(edited, path)
  restored <- apply_building_control_overrides(buildings, read_building_control_overrides(path))

  expect_equal(restored$product_id, "rm_1a")
  expect_false(restored$control_required)
  expect_true(restored$control_edited)
})

test_that("la file de contrôle réserve les écarts GIS non bloquants", {
  buildings <- data.frame(
    product_id = c(NA, "rm_2", "rm_2"),
    product_link_source = c("non renseigné", "manuel", "occupation du sol"),
    land_use_zone = c("RM 1", "RV 1", "RM 2"),
    included_in_simulation = c(TRUE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  result <- add_building_classification_status(buildings)

  expect_equal(result$control_action, c("À classifier", "Contrôle terminé", "Contrôle terminé"))
  expect_equal(result$control_required, c(TRUE, FALSE, FALSE))
  expect_equal(result$has_incongruity, c(FALSE, TRUE, TRUE))
})

test_that("les bâtiments QGIS nouveaux sont normalisés sans perdre leur géométrie", {
  geometry <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(
      -17, 14, -16.9999, 14, -16.9999, 14.0001, -17, 14.0001, -17, 14
    ), ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  buildings <- sf::st_sf(
    building_id = NA_character_,
    product_id = "rc_2",
    levels = NA_real_,
    geometry = geometry
  )
  normalized <- normalize_scenario_buildings(
    buildings, initial_products(), "scenario_01"
  )
  normalized <- add_building_classification_status(normalized, initial_products())
  normalized <- calculate_osm_building_heights(
    normalized, 4, 3, 1, initial_products()
  )

  expect_match(normalized$building_id, "^gis_scenario_01_")
  expect_equal(normalized$product_id, "rc_2_log")
  expect_equal(normalized$levels_effective, 4)
  expect_equal(normalized$levels_assumption_source, "Hypothèse produit")
  expect_false(normalized$control_required)
  expect_true(normalized$has_incongruity)
})

test_that("les hypothèses produit composent les volumes étage par étage", {
  geometry <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(
      -17, 14, -16.9999, 14, -16.9999, 14.0001, -17, 14.0001, -17, 14
    ), ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  buildings <- sf::st_sf(
    building_id = "rc2",
    product_id = "rc_2_log",
    levels = NA_real_,
    control_required = FALSE,
    incongruity_flag = "",
    geometry = geometry
  )
  buildings <- calculate_osm_building_heights(
    buildings, 4, 3, 1, initial_products()
  )
  blocks <- create_building_level_blocks(buildings, initial_products(), 4, 3)

  expect_equal(nrow(blocks), 4)
  expect_equal(blocks$product_id, c("rc_2_com", rep("rc_2_log", 3)))
  expect_equal(blocks$base_height_m, c(0, 4.08, 7.08, 10.08))
  expect_equal(blocks$top_height_m, c(3.92, 6.92, 9.92, 12.92))
  expect_equal(unique(blocks$building_total_units), 22)
  expect_equal(
    unique(blocks$building_total_sdp_sqm),
    round(4 * blocks$estimated_sdp_sqm[1], 1)
  )
  expect_true(all(blocks$commercial_ground_floor))
  expect_equal(
    unique(blocks$commercial_surface_total_sqm),
    round(blocks$estimated_sdp_sqm[1], 1)
  )
})

test_that("les produits RC2 et RT1 sont affectés niveau par niveau", {
  geometry <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(
      -17, 14, -16.9999, 14, -16.9999, 14.0001, -17, 14.0001, -17, 14
    ), ncol = 2, byrow = TRUE))),
    sf::st_polygon(list(matrix(c(
      -17.001, 14, -17.0009, 14, -17.0009, 14.0001, -17.001, 14.0001, -17.001, 14
    ), ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  buildings <- sf::st_sf(
    building_id = c("rc2", "rt1"),
    district_id = c("q2", "q2"),
    levels_effective = c(4, 3),
    product_id = c("rc_2_log", "rt_1_log"),
    geometry = geometry
  )
  allocations <- calculate_building_level_allocations(buildings, initial_products())
  comparison <- calculate_building_surface_comparison(buildings, initial_products())
  program <- calculate_program_from_buildings(
    buildings, initial_products(), initial_program()
  )

  expect_equal(comparison$ground_floor_product_id, c("rc_2_com", "rt_1_com"))
  expect_equal(comparison$upper_floor_product_id, c("rc_2_log", "rt_1_log"))
  expect_equal(allocations$product_id[allocations$building_id == "rc2"], c("rc_2_com", rep("rc_2_log", 3)))
  expect_equal(allocations$product_id[allocations$building_id == "rt1"], c("rt_1_com", rep("rt_1_log", 2)))
  expect_equal(allocations$level_label[allocations$building_id == "rc2"], c("RDC", "R+1", "R+2", "R+3"))
  expect_equal(
    sum(allocations$estimated_sdp_sqm[allocations$building_id == "rc2"]),
    4 * allocations$estimated_sdp_sqm[allocations$building_id == "rc2"][1]
  )
  for (product_id in c("rc_2_com", "rc_2_log", "rt_1_com", "rt_1_log")) {
    expect_equal(
      program$manual_total_sdp[
        program$district_id == "q2" & program$product_id == product_id
      ],
      sum(allocations$estimated_sdp_sqm[
        allocations$product_id == product_id
      ])
    )
  }
})
test_that("RC4 calcule la SDP et les logements depuis l'emprise", {
  geometry <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(
      0, 0, 24, 0, 24, 26, 0, 26, 0, 0
    ), ncol = 2, byrow = TRUE))),
    crs = 32628
  )
  buildings <- sf::st_sf(
    building_id = "rc4_test",
    district_id = "q5",
    product_id = "rc_4",
    levels_effective = 5,
    units_per_level = 4,
    footprint_offset_sqm = 0,
    geometry = geometry
  )

  comparison <- calculate_building_surface_comparison(buildings, initial_products())
  allocations <- calculate_building_level_allocations(buildings, initial_products())
  building_summary <- calculate_product_building_summary(buildings, initial_products())
  program <- calculate_program_from_buildings(
    buildings, initial_products(), initial_program()
  )
  rc4_program <- program[
    program$district_id == "q5" & program$product_id == "rc_4",
  ]

  expect_equal(comparison$geometry_footprint_sqm, 624)
  expect_equal(comparison$effective_footprint_sqm, 624)
  expect_equal(comparison$estimated_sdp_sqm, 3120)
  expect_equal(comparison$calculated_sdp_per_unit_sqm, 156)
  expect_equal(nrow(allocations), 5)
  expect_equal(allocations$unit_count, rep(4, 5))
  expect_equal(allocations$sdp_per_unit_sqm, rep(156, 5))
  expect_equal(rc4_program$quantity, 20)
  expect_equal(rc4_program$manual_total_sdp, 3120)
  rc4_building <- building_summary[building_summary$product_id == "rc_4", ]
  expect_equal(rc4_building$building_count, 1)
  expect_equal(rc4_building$total_sdp_sqm, 3120)
  expect_equal(rc4_building$sdp_per_building_sqm, 3120)

  rc4_financial <- calculate_program_financials(
    rc4_program, initial_products(), initial_construction_categories()
  )
  expect_equal(rc4_financial$total_sdp, 3120)
  expect_equal(
    rc4_financial$land_revenue_ht, 3120 * (500000 / 1.45 - 320000)
  )

  buildings$footprint_offset_sqm <- -92
  adjusted <- calculate_building_surface_comparison(buildings, initial_products())
  expect_equal(adjusted$effective_footprint_sqm, 532)
  expect_equal(adjusted$estimated_sdp_sqm, 2660)
  expect_equal(adjusted$calculated_sdp_per_unit_sqm, 133)
})

test_that("la variation d'emprise modifie réellement la géométrie", {
  geometry <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(
      0, 0, 24, 0, 24, 26, 0, 26, 0, 0
    ), ncol = 2, byrow = TRUE))),
    crs = 32628
  )
  buildings <- sf::st_sf(
    building_id = "rc4_test",
    footprint_offset_sqm = 0,
    geometry = geometry
  )
  centre_before <- sf::st_coordinates(sf::st_centroid(geometry))[1, 1:2]

  adjusted <- apply_building_footprint_adjustment(buildings, 1, -92)
  centre_after <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(adjusted)))[1, 1:2]

  expect_equal(as.numeric(sf::st_area(adjusted)), 532, tolerance = 1e-6)
  expect_equal(adjusted$footprint_offset_sqm, 0)
  expect_equal(centre_after, centre_before, tolerance = 1e-8)
  expect_equal(as.numeric(sf::st_area(buildings)), 624)
})

test_that("une hypothèse produit est propagée à tous ses bâtiments", {
  buildings <- data.frame(
    building_id = c("rc4_a", "rc4_b", "rc3_a"),
    product_id = c("rc_4", "rc_4", "rc_3"),
    levels = c(4, 4, 5),
    levels_source = c("QGIS", "QGIS", "QGIS"),
    units_per_level = c(3, 3, 5),
    stringsAsFactors = FALSE
  )

  result <- apply_product_building_assumptions(buildings, "rc_4", 5, 4)

  expect_equal(result$levels, c(5, 5, 5))
  expect_equal(result$units_per_level, c(4, 4, 5))
  expect_equal(
    result$levels_source,
    c("hypothèse produit", "hypothèse produit", "QGIS")
  )
})


test_that("les surfaces cartographiques sont comparées au programme", {
  urban <- load_osm_urban_data(
    file.path("..", "..", "data", "sig", "thies13.osm")
  )
  corrected <- load_corrected_buildings(
    file.path("..", "..", "data", "sig", "buildings.gpkg")
  )
  context <- load_project_context_layers(file.path("..", "..", "data", "sig"))
  buildings <- link_buildings_to_context(
    consolidate_buildings(corrected, urban$buildings),
    context$quartiers,
    context$land_use
  )
  buildings <- calculate_osm_building_heights(buildings, 4, 3, 1)
  comparison <- calculate_building_surface_comparison(buildings, initial_products())
  program <- calculate_program_sdp(initial_program(), initial_products())
  districts <- calculate_district_surface_comparison(
    buildings,
    initial_products(),
    program,
    initial_districts()
  )

  expect_equal(round(sum(comparison$footprint_area_sqm), 1), 143712.3)
  expect_equal(round(sum(comparison$estimated_sdp_sqm), 1), 407533.9)
  expect_equal(districts$model_sdp_sqm[districts$district_id == "q1"], 40856)
  expect_equal(districts$building_count[districts$district_id == "q6"], 1)
})


test_that("les hauteurs utilisent le RDC, les étages et le niveau par défaut", {
  urban <- load_osm_urban_data(
    file.path("..", "..", "data", "sig", "thies13.osm")
  )
  buildings <- calculate_osm_building_heights(
    urban$buildings,
    ground_floor_height_m = 4,
    upper_floor_height_m = 3,
    default_levels = 2
  )

  expect_true(all(buildings$height_m[which(buildings$levels == 1)] == 4))
  expect_true(all(buildings$height_m[which(buildings$levels == 4)] == 13))
  expect_true(all(buildings$levels_effective[is.na(buildings$levels)] == 2))
  expect_true(all(buildings$height_m[is.na(buildings$levels)] == 7))
  expect_equal(max(buildings$height_m), 25)
})

test_that("les bâtiments, voiries et parcelles sont sérialisables pour les cartes", {
  urban <- load_osm_urban_data(
    file.path("..", "..", "data", "sig", "thies13.osm")
  )
  buildings <- calculate_osm_building_heights(urban$buildings, 4, 3, 1)
  buildings_geojson <- jsonlite::fromJSON(sf_to_geojson(buildings), simplifyVector = FALSE)
  roads_geojson <- jsonlite::fromJSON(sf_to_geojson(urban$roads), simplifyVector = FALSE)

  expect_equal(buildings_geojson$type, "FeatureCollection")
  expect_length(buildings_geojson$features, 441)
  expect_equal(roads_geojson$type, "FeatureCollection")
  expect_length(roads_geojson$features, 121)
})

test_that("les hypothèses initiales de hauteur sont explicites", {
  assumptions <- initial_height_assumptions()

  expect_equal(assumptions$ground_floor_height_m, 4)
  expect_equal(assumptions$upper_floor_height_m, 3)
  expect_equal(assumptions$default_levels, 1)
})
test_that("un scénario GeoPackage consolide les couches et retire les bâtiments exclus", {
  points <- sf::st_sfc(
    sf::st_point(c(-17, 14)),
    sf::st_point(c(-17.001, 14.001)),
    crs = 4326
  )
  buildings <- sf::st_sf(
    building_id = c("included", "removed"),
    product_id = c("rm_2", NA_character_),
    product_link_source = c("manuel", "non renseigné"),
    land_use_zone = c("RM 2", NA_character_),
    included_in_simulation = c(TRUE, FALSE),
    geometry = points
  )
  buildings <- add_building_classification_status(buildings)
  one_feature <- buildings[1, c("building_id")]
  context <- list(
    quartiers = one_feature,
    land_use = one_feature,
    road_footprints = one_feature,
    flood_areas = one_feature,
    project_boundary = one_feature,
    title_boundary = one_feature
  )
  path <- tempfile(fileext = ".gpkg")
  on.exit(unlink(path), add = TRUE)

  write_scenario_geopackage(
    path = path,
    scenario_id = "test_1",
    buildings = buildings,
    roads = one_feature,
    parcels = one_feature,
    context = context,
    tables = list(
      products = data.frame(product_id = "rm_2", stringsAsFactors = FALSE),
      financial_assumptions = scenario_parameter_table(list(vat_rate = 0.2))
    ),
    sources = data.frame(
      scenario_layer = "buildings",
      reference_source = "reference.gpkg",
      stringsAsFactors = FALSE
    )
  )
  scenario <- read_scenario_geopackage(path)

  expect_true(file.exists(path))
  expect_equal(nrow(scenario$spatial$buildings), 1)
  expect_equal(scenario$spatial$buildings$building_id, "included")
  expect_false("removed" %in% scenario$spatial$buildings$building_id)
  expect_equal(scenario$tables$scenario_metadata$scenario_id, "test_1")
  expect_equal(scenario$tables$scenario_metadata$removed_building_count, 1)
  expect_equal(scenario$tables$products$product_id, "rm_2")
  expect_equal(
    scenario_parameters_from_table(
      scenario$tables$financial_assumptions,
      list(vat_rate = 0)
    )$vat_rate,
    0.2
  )
})

test_that("les bâtiments héritent du quartier spatial et PHARD compte 53 RM1", {
  scenario <- read_scenario_bundle(file.path("..", "..", "data", "scenarios", "scenario_01"))
  buildings <- assign_building_districts(
    scenario$spatial$buildings,
    scenario$spatial$quartiers
  )
  phard <- buildings[buildings$district_id == "q1", , drop = FALSE]
  rm1 <- phard[phard$product_id %in% c("rm_1a", "rm_1b"), , drop = FALSE]
  counts <- table(rm1$product_id)
  expect_equal(nrow(phard), 97)
  expect_equal(nrow(rm1), 53)
  expect_equal(as.integer(counts[c("rm_1a", "rm_1b")]), c(17L, 36L))
})

test_that("les identifiants de scénario sont sûrs pour les chemins", {
  expect_equal(normalize_scenario_id("base_2026-09"), "base_2026-09")
  expect_error(normalize_scenario_id("../base"), "identifiant du scénario")
})

test_that("la préparation UMEP des arbres exige les quatre paramètres", {
  trees <- sf::st_sf(
    umep_tree_type = c(2L, 2L, 3L, 1L, NA_integer_),
    total_height_m = c(12, 12, 12, 5, 12),
    trunk_height_m = c(3, 12, 3, 1, 3),
    crown_diameter_m = c(8, 8, 8, 4, 8),
    geometry = sf::st_sfc(lapply(1:5, function(i) sf::st_point(c(i, i))), crs = 32628)
  )
  expect_identical(tree_shadow_ready(trees), c(TRUE, FALSE, FALSE, TRUE, FALSE))
  expect_identical(tree_shadow_ready(trees[, "umep_tree_type"]), rep(FALSE, 5))
})

test_that("la charge utile 3D des arbres ne contient que les coordonnées", {
  trees <- sf::st_sf(
    tree_id = c("tree_1", "tree_2"),
    geometry = sf::st_sfc(
      sf::st_point(c(-17.0129000055, 14.7742781733)),
      sf::st_point(c(-17.01, 14.77)),
      crs = 4326
    )
  )
  payload <- jsonlite::fromJSON(tree_display_payload_json(trees))
  expect_identical(sort(names(payload)), c("lat", "lon"))
  expect_equal(payload$lon, c(-17.0129, -17.01))
  expect_equal(payload$lat, c(14.7742782, 14.77))
  expect_identical(
    jsonlite::fromJSON(tree_display_payload_json(empty_project_trees()))$lon,
    list()
  )
})

test_that("les arbres UMEP attribués sont trouvés et colorés par essence", {
  scenario <- tempfile("scenario-umep-")
  study <- file.path(scenario, "exports", "umep", "etude")
  dir.create(file.path(study, "superseded"), recursive = TRUE)
  on.exit(unlink(scenario, recursive = TRUE), add = TRUE)
  expect_null(find_umep_tree_layer(scenario))

  trees <- sf::st_sf(
    tree_id = c("tree_1", "tree_2", "tree_3"),
    species = c("Mangifera indica", "Khaya senegalensis", "Mangifera indica"),
    context = c("block", "road", "block"),
    geometry = sf::st_sfc(
      sf::st_point(c(283500, 1633500)), sf::st_point(c(283510, 1633500)),
      sf::st_point(c(283520, 1633500)), crs = 32628
    )
  )
  old <- file.path(study, "superseded", "trees_thies_mature_seed1.gpkg")
  current <- file.path(study, "trees_thies_mature_seed2.gpkg")
  sf::st_write(trees, old, layer = "trees_umep", quiet = TRUE)
  sf::st_write(trees, current, layer = "trees_umep", quiet = TRUE)
  expect_identical(normalizePath(find_umep_tree_layer(scenario)), normalizePath(current))

  loaded <- load_umep_trees(current)
  expect_equal(sf::st_crs(loaded)$epsg, 4326L)
  payload <- jsonlite::fromJSON(tree_display_payload_json(loaded))
  expect_identical(payload$species_levels, c("Khaya senegalensis", "Mangifera indica"))
  expect_identical(payload$species, c(1L, 0L, 1L))
  expect_identical(payload$species_colors, unname(tree_species_colors[payload$species_levels]))

  legend <- tree_species_legend(loaded)
  expect_identical(legend$species, c("Mangifera indica", "Khaya senegalensis"))
  expect_identical(legend$count, c(2L, 1L))
  expect_null(tree_species_legend(empty_project_trees()))
  expect_match(umep_tree_source_label(current), "« etude »")

  incomplete <- trees
  incomplete$species[2] <- NA
  bad <- file.path(study, "trees_thies_bad.gpkg")
  sf::st_write(incomplete, bad, layer = "trees_umep", quiet = TRUE)
  expect_error(load_umep_trees(bad), "sans essence")
})

test_that("les cours intérieures issues de la polygonisation ne sont pas des bâtiments", {
  outer <- sf::st_polygon(list(
    rbind(c(0, 0), c(40, 0), c(40, 40), c(0, 40), c(0, 0)),
    rbind(c(15, 15), c(25, 15), c(25, 25), c(15, 25), c(15, 15))
  ))
  court <- sf::st_polygon(list(rbind(c(15, 15), c(25, 15), c(25, 25), c(15, 25), c(15, 15))))
  other <- sf::st_polygon(list(rbind(c(100, 0), c(110, 0), c(110, 10), c(100, 10), c(100, 0))))
  buildings <- sf::st_sf(
    building_id = c("bat_a_1", "bat_a_2", "bat_b_1", "bat_b_2"),
    source_handle = c("A", "A", "B", "B"),
    product_id = "ec",
    included_in_simulation = TRUE,
    control_edited = c(FALSE, FALSE, FALSE, FALSE),
    geometry = sf::st_sfc(outer, court, other, sf::st_polygon(list(rbind(c(120, 0), c(130, 0), c(130, 10), c(120, 10), c(120, 0))))),
    crs = 32628
  )
  expect_identical(identify_courtyard_parts(buildings), c(FALSE, TRUE, FALSE, FALSE))
  # Une décision manuelle de contrôle prime sur l'exclusion automatique.
  buildings$control_edited[2] <- TRUE
  normalized <- normalize_scenario_buildings(sf::st_transform(buildings, 4326), initial_products(), "test")
  expect_true(normalized$included_in_simulation[normalized$building_id == "bat_a_2"])
  buildings$control_edited[2] <- FALSE
  normalized <- normalize_scenario_buildings(sf::st_transform(buildings, 4326), initial_products(), "test")
  expect_false(normalized$included_in_simulation[normalized$building_id == "bat_a_2"])
  expect_true(all(normalized$included_in_simulation[normalized$building_id != "bat_a_2"]))
})

test_that("l'aléa fort est ramené à un seul polygone à ses limites extérieures", {
  square <- function(x0, y0, size) {
    sf::st_polygon(list(rbind(c(x0, y0), c(x0 + size, y0), c(x0 + size, y0 + size), c(x0, y0 + size), c(x0, y0))))
  }
  with_hole <- sf::st_polygon(list(
    rbind(c(0, 0), c(100, 0), c(100, 100), c(0, 100), c(0, 0)),
    rbind(c(40, 40), c(60, 40), c(60, 60), c(40, 60), c(40, 40))
  ))
  layer <- sf::st_sf(
    DN = c(1001, 1002, 1001, 1003),
    geometry = sf::st_sfc(with_hole, square(100, 0, 50), square(300, 300, 5), square(500, 0, 20), crs = 32628)
  )
  geometry <- alea_fort_geometry(layer)
  expect_equal(as.numeric(sf::st_area(geometry)), 100 * 100 + 50 * 50 + 20 * 20)
  expect_length(geometry, 1)
  outline <- alea_fort_outline(layer)
  expect_equal(sf::st_crs(outline)$epsg, 4326L)
  expect_true(all(sf::st_geometry_type(outline) == "MULTILINESTRING"))
  expect_length(alea_fort_geometry(NULL), 0)
  expect_equal(nrow(alea_fort_outline(layer[0, ])), 0)
})

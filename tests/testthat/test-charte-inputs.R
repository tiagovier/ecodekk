charte_box <- function(xmin, ymin, xmax, ymax) {
  sf::st_polygon(list(rbind(
    c(xmin, ymin), c(xmax, ymin), c(xmax, ymax), c(xmin, ymax), c(xmin, ymin)
  )))
}

charte_sf <- function(attributes, geometries) {
  attributes$fid <- seq_len(nrow(attributes))
  sf::st_sf(attributes, geometry = sf::st_sfc(geometries, crs = 32628))
}

# Opération fictive de 1 000 m × 1 000 m en EPSG:32628.
make_charte_layers <- function() {
  ring <- sf::st_linestring(rbind(c(0, 0), c(1000, 0), c(1000, 1000), c(0, 1000), c(0, 0)))
  land_use <- charte_sf(
    data.frame(
      Layer = c(
        "square", "Parcelles Agricoles", "Equipement Scolaire", "Equipement de Santé",
        "Parcelles Tertiaire Commerce", "par_02 15 x 20", "Parking", "Parking", "TVBP"
      ),
      zone = NA_character_,
      CU = c(rep(NA, 5), "Superficie foncière de Habitat et ses annexes", NA, NA, NA),
      ch_reserve = c(NA, NA, NA, NA, NA, NA, 1L, NA, NA),
      ch_alea_compatible = c(NA, NA, NA, NA, NA, NA, NA, NA, 0L),
      ch_coef_biotope = c(NA, NA, NA, NA, NA, NA, 0.5, NA, NA),
      stringsAsFactors = FALSE
    ),
    list(
      charte_box(0, 0, 100, 100),
      charte_box(100, 0, 300, 100),
      charte_box(400, 400, 450, 450),
      charte_box(450, 400, 500, 450),
      charte_box(400, 450, 450, 500),
      charte_box(600, 600, 800, 800),
      charte_box(900, 900, 1000, 1000),
      charte_box(950, 950, 1050, 1050),
      charte_box(0, 900, 100, 1000)
    )
  )
  quartiers <- charte_sf(
    data.frame(code = c("SC1", "1"), population = c(0, 1000)),
    list(charte_box(0, 0, 300, 200), charte_box(300, 200, 1000, 1000))
  )
  road_footprints <- charte_sf(
    data.frame(
      Class = c(5, 0, 1), Descr = c("Rue piétonne partagée", "RN", "Boulevard urbain"),
      ch_modes_doux = c(NA, NA, 1L)
    ),
    list(
      charte_box(500, 0, 520, 1000),
      charte_box(0, 300, 1000, 320),
      charte_box(0, 700, 1000, 720)
    )
  )
  roads <- charte_sf(
    data.frame(highway = c("residential", "trunk", "primary", "service")),
    list(
      sf::st_linestring(rbind(c(510, 0), c(510, 1000))),
      sf::st_linestring(rbind(c(0, 310), c(1000, 310))),
      sf::st_linestring(rbind(c(0, 710), c(1000, 710))),
      sf::st_linestring(rbind(c(900, 0), c(900, 200)))
    )
  )
  buildings <- charte_sf(
    data.frame(
      building_id = c("b1", "b2", "b3", "b4"),
      product_id = c("rv_1", "rc_2_com", "rm_2", "ep")
    ),
    list(
      charte_box(10, 150, 20, 160),
      charte_box(30, 150, 40, 160),
      charte_box(610, 610, 620, 620),
      charte_box(410, 410, 420, 420)
    )
  )
  list(
    buildings = buildings,
    roads = roads,
    parcels = NULL,
    quartiers = quartiers,
    land_use = land_use,
    road_footprints = road_footprints,
    flood_areas = charte_sf(
      data.frame(flood_type = c("Lit mineur", "Cuvette de rétention", "Zone hors aléa")),
      list(charte_box(0, 0, 150, 200), charte_box(150, 0, 300, 200), charte_box(700, 0, 800, 100))
    ),
    project_boundary = NULL,
    title_boundary = charte_sf(data.frame(name = "titre"), list(ring))
  )
}

charte_value <- function(result, input_id) {
  result$inputs$value[result$inputs$input_id == input_id]
}

test_that("le classement lit les colonnes ch_* et applique la règle par défaut aux cellules vides", {
  land_use <- classify_charte_land_use(make_charte_layers()$land_use)

  expect_equal(
    land_use$charte_classes_resolved[[1]],
    "espace_vert_public;usage_compatible_alea;espace_public_pieton"
  )
  expect_equal(land_use$coef_biotope_resolved[[1]], 1)
  expect_equal(land_use$charte_rule_source[[1]], "Règle par défaut")
  expect_equal(land_use$charte_classes_resolved[[6]], "habitat")
  expect_equal(land_use$charte_classes_resolved[[7]], "reserve_fonciere")
  expect_equal(land_use$coef_biotope_resolved[[7]], 0.5)
  expect_equal(land_use$charte_rule_source[[7]], "spatial.gpkg incomplet")
  expect_equal(land_use$charte_classes_resolved[[8]], "")
  # TVBP : ch_alea_compatible = 0 saisi, coefficient par défaut conservé.
  expect_equal(land_use$charte_classes_resolved[[9]], "")
  expect_equal(land_use$coef_biotope_resolved[[9]], 1)
  expect_match(charte_land_use_code_label(land_use)[[1]], "ch_ev_public, ch_alea_compatible, ch_pieton")
})

test_that("les colonnes vides et les entités sans classe sont signalées", {
  layers <- make_charte_layers()
  land_use <- classify_charte_land_use(layers$land_use)
  controls <- charte_spatial_controls(layers, land_use)
  empty <- controls[grepl("^Colonnes charte vides", controls$control), ]
  expect_equal(nrow(empty), nrow(layers$land_use))
  unclassified <- controls[grepl("^Occupation du sol sans classe", controls$control), ]
  expect_equal(unclassified$fid, 8L)
})

test_that("write_charte_columns crée les colonnes et ne remplit que les cellules vides", {
  layers <- make_charte_layers()
  path <- tempfile(fileext = ".gpkg")
  on.exit(unlink(path), add = TRUE)
  for (name in c("land_use", "road_footprints")) {
    layer <- layers[[name]]
    layer$fid <- NULL
    sf::st_write(sf::st_transform(layer, 4326), path, layer = name, quiet = TRUE)
  }

  summary <- write_charte_columns(path)
  expect_true(all(c(charte_land_use_columns$column, charte_coef_column) %in%
                    summary$column[summary$table == "land_use"]))
  expect_false(summary$created[summary$column == "ch_reserve"])
  expect_true(summary$created[summary$column == "ch_ev_public"])

  read <- read_charte_spatial_layers(path)
  expect_equal(read$land_use$fid, seq_len(nrow(layers$land_use)))
  expect_equal(read$land_use$ch_ev_public[[1]], 1)
  expect_equal(read$land_use$ch_reserve[[7]], 1)
  expect_equal(read$land_use$ch_coef_biotope[[7]], 0.5)
  expect_equal(read$land_use$ch_alea_compatible[[9]], 0)
  expect_equal(read$land_use$ch_reserve[[1]], 0)
  expect_equal(read$road_footprints$ch_modes_doux, c(1, 0, 1))
  expect_false(anyNA(sf::st_drop_geometry(read$land_use)[charte_land_use_columns$column]))

  again <- write_charte_columns(path)
  expect_equal(sum(again$filled), 0)
  expect_false(any(again$created))
})

test_that("la surface de référence est polygonisée depuis le contour du titre foncier", {
  result <- compute_charte_spatial_inputs(make_charte_layers())
  expect_equal(charte_value(result, "surface_reference"), 1e6)
})

test_that("les intrants de trame bleue croisent l'aléa fort et les usages compatibles", {
  result <- compute_charte_spatial_inputs(make_charte_layers())

  expect_equal(charte_value(result, "alea_fort"), 300 * 200)
  expect_equal(result$features$alea_fort$fid, 1:2)
  # square (100 × 100) et parcelles agricoles (200 × 100) dans SC1.
  expect_equal(charte_value(result, "alea_fort_valorise"), 30000)
  preview <- result$indicators
  expect_equal(preview$value[preview$code == "TB-1"], 50)
})

test_that("un type de zone inondable absent, comme le lit moyen, est signalé", {
  layers <- make_charte_layers()
  result <- compute_charte_spatial_inputs(layers)
  row <- result$inputs[result$inputs$input_id == "alea_fort", ]
  expect_equal(row$status, "Incomplet")
  expect_match(row$note, "Lit moyen")
  expect_match(row$note, "Rétention 50 cm")

  layers$flood_areas <- rbind(
    layers$flood_areas,
    charte_sf(
      data.frame(flood_type = c("Lit moyen", "Rétention 50 cm")),
      list(charte_box(0, 200, 300, 250), charte_box(300, 0, 350, 50))
    )[, names(layers$flood_areas)]
  )
  layers$flood_areas$fid <- seq_len(nrow(layers$flood_areas))
  complete <- compute_charte_spatial_inputs(layers)
  row <- complete$inputs[complete$inputs$input_id == "alea_fort", ]
  expect_equal(row$status, "Calculé")
  expect_equal(row$value, 300 * 200 + 300 * 50 + 50 * 50)
})

test_that("les intrants sont typés spatiaux ou attributaires", {
  result <- compute_charte_spatial_inputs(make_charte_layers())
  kinds <- stats::setNames(result$inputs$kind, result$inputs$input_id)
  expect_equal(unname(kinds[c("population", "batiments_habitat")]), c("attribut", "attribut"))
  expect_true(all(kinds[setdiff(names(kinds), c("population", "batiments_habitat"))] == "spatial"))
})

test_that("les surfaces sont découpées par la référence et pondérées par le biotope", {
  result <- compute_charte_spatial_inputs(make_charte_layers())

  # square 10 000 + agriculture 20 000 + réserve 0,5 × 10 000 + TVBP 10 000
  # (ch_alea_compatible = 0 saisi, coefficient par défaut conservé).
  expect_equal(charte_value(result, "surface_ecoamenagee"), 10000 + 20000 + 5000 + 10000)
  expect_equal(charte_value(result, "espaces_verts_publics"), 10000)
  expect_equal(charte_value(result, "agriculture"), 20000)
  expect_equal(charte_value(result, "surface_habitat"), 40000)
  expect_equal(charte_value(result, "reserve_fonciere"), 10000)
  expect_equal(charte_value(result, "population"), 1000)
})

test_that("la canopée est l'union des houppiers, sans double compte", {
  trees <- sf::st_sf(
    diameter = c(10, 10, 4, NA),
    geometry = sf::st_sfc(
      sf::st_point(c(50, 50)), sf::st_point(c(50, 50)), sf::st_point(c(700, 50)),
      sf::st_point(c(800, 50)),
      crs = 32628
    )
  )
  canopy <- charte_canopy_geometry(trees, quad_segments = 64L)
  expect_equal(charte_area(canopy), pi * 25 + pi * 4, tolerance = 1e-3)

  result <- compute_charte_spatial_inputs(make_charte_layers(), canopy)
  expect_equal(charte_value(result, "canopee"), charte_area(canopy), tolerance = 1e-6)
  # Le houppier centré sur le square est entièrement dans l'espace public piéton.
  expect_equal(charte_value(result, "espaces_publics_ombrages"), pi * 25, tolerance = 1e-3)
})

test_that("les cheminements doux suivent les emprises et l'attribut QGIS modes_doux", {
  result <- compute_charte_spatial_inputs(make_charte_layers())

  expect_equal(charte_value(result, "lineaire_voirie"), 1000 + 1000 + 1000 + 200)
  # Rue piétonne (1 000 m) + boulevard déclaré modes_doux dans QGIS (1 000 m)
  # + traversée de la rue piétonne par la RN (20 m dans l'emprise).
  expect_equal(charte_value(result, "lineaire_modes_doux"), 2020, tolerance = 1e-6)
  footprints <- result$road_footprints
  expect_equal(footprints$charte_rule_source, c("Règle par défaut", "Règle par défaut", "spatial.gpkg"))
})

test_that("le panier de services exige les trois catégories à moins de 500 m", {
  layers <- make_charte_layers()
  result <- compute_charte_spatial_inputs(layers)
  basket <- charte_value(result, "panier_services")
  expect_gt(basket, 0)
  expect_lte(basket, 1e6)

  layers$land_use <- layers$land_use[layers$land_use$Layer != "Equipement de Santé", ]
  without_health <- compute_charte_spatial_inputs(layers)
  expect_equal(charte_value(without_health, "panier_services"), 0)
})

test_that("l'exposition du bâti distingue habitat, commerce et équipements", {
  result <- compute_charte_spatial_inputs(make_charte_layers())

  expect_equal(charte_value(result, "batiments_habitat"), 2)
  expect_equal(charte_value(result, "batiments_habitat_alea"), 1)
  preview <- result$indicators
  expect_equal(preview$value[preview$code == "RES-3"], 50)
  ids <- result$features$batiments_habitat_alea
  expect_equal(ids$fid, 1L)
})

test_that("la zone de bruit applique les tampons par profil de voie", {
  parameters <- charte_default_parameters()
  parameters$noise_buffer_m <- c("RN" = 10)
  result <- compute_charte_spatial_inputs(make_charte_layers(), parameters = parameters)
  expect_equal(charte_value(result, "zone_bruit"), 1000 * 40, tolerance = 1e-3)
})

test_that("les chevauchements d'occupation du sol sont signalés", {
  layers <- make_charte_layers()
  result <- compute_charte_spatial_inputs(layers)
  overlaps <- result$controls[result$controls$control == "Chevauchement d'occupations du sol", ]
  expect_equal(nrow(overlaps), 1L)
  expect_equal(overlaps$fid, 7L)
})

test_that("la lecture du GeoPackage conserve les fid et projette en EPSG:32628", {
  layers <- make_charte_layers()
  path <- tempfile(fileext = ".gpkg")
  on.exit(unlink(path), add = TRUE)
  for (name in names(layers)) {
    layer <- layers[[name]]
    if (is.null(layer)) next
    layer$fid <- NULL
    sf::st_write(sf::st_transform(layer, 4326), path, layer = name, quiet = TRUE)
  }
  read <- read_charte_spatial_layers(path)
  expect_null(read$project_boundary)
  expect_equal(read$flood_areas$fid, 1:3)
  expect_equal(sf::st_crs(read$land_use)$epsg, 32628L)
  expect_equal(read$land_use$fid, seq_len(nrow(layers$land_use)))
  table <- charte_input_feature_table(
    compute_charte_spatial_inputs(read), read, "espaces_verts_publics"
  )
  expect_equal(table$fid, 1L)
  expect_equal(table$area_sqm, 10000, tolerance = 1)
})

test_that("la population de la charte provient du programme, l'attribut des quartiers servant de contrôle", {
  population <- list(total = 1234, complete = FALSE, missing_products = "rc_1")
  result <- compute_charte_spatial_inputs(make_charte_layers(), population = population)
  row <- result$inputs[result$inputs$input_id == "population", ]

  expect_equal(row$value, 1234)
  expect_equal(row$kind, "programme")
  expect_equal(row$status, "Incomplet")
  expect_match(row$note, "rc_1")
  expect_match(row$note, "1 000 hab")
  preview <- result$indicators
  expect_equal(preview$value[preview$code == "TV-1"], 10000 / 1234)
})

test_that("l'empreinte d'une couche ignore l'ordre des entités et le bruit sous le centimètre", {
  layer <- sf::st_sf(id = c("a", "b"), h = c(3, 6), geometry = sf::st_sfc(
    sf::st_point(c(283500, 1634000)), sf::st_point(c(283510, 1634000)), crs = 32628))
  shifted <- layer[2:1, ]
  sf::st_geometry(shifted) <- sf::st_geometry(shifted) + c(0.001, 0)
  sf::st_crs(shifted) <- 32628
  expect_identical(umep_layer_fingerprint(layer, "h", id = "id"), umep_layer_fingerprint(shifted, "h", id = "id"))
  moved <- layer
  sf::st_geometry(moved) <- sf::st_geometry(moved) + c(0.5, 0)
  sf::st_crs(moved) <- 32628
  expect_false(identical(umep_layer_fingerprint(layer, "h", id = "id"), umep_layer_fingerprint(moved, "h", id = "id")))
  layer$h[1] <- 4
  expect_false(identical(umep_layer_fingerprint(layer, "h", id = "id"), umep_layer_fingerprint(shifted, "h", id = "id")))
})

test_that("la catégorie de plantation suit les règles de l'attribution des arbres", {
  trees <- sf::st_sf(tree_id = c("t1", "t2", "t3", "t4", "t5"), geometry = sf::st_sfc(
    sf::st_point(c(0, 4)), sf::st_point(c(0, 30)), sf::st_point(c(50, 50)),
    sf::st_point(c(100, 100)), sf::st_point(c(0, 60)), crs = 32628))
  square <- function(x0, y0, size) sf::st_polygon(list(rbind(c(x0, y0), c(x0 + size, y0), c(x0 + size, y0 + size), c(x0, y0 + size), c(x0, y0))))
  spatial <- list(
    roads = sf::st_sf(width_m = 5, geometry = sf::st_sfc(sf::st_linestring(rbind(c(-50, 0), c(50, 0))), crs = 32628)),
    road_footprints = sf::st_sf(geometry = sf::st_sfc(square(-60, -1, 1), crs = 32628)),
    flood_areas = sf::st_sf(flood_type = "Lit mineur", geometry = sf::st_sfc(square(40, 40, 20), crs = 32628)),
    buildings = sf::st_sf(building_id = "b", geometry = sf::st_sfc(square(90, 90, 20), crs = 32628))
  )
  contexts <- umep_tree_contexts(trees, spatial)
  expect_identical(contexts$context, c("road", "block", "flood", "exclu", "block"))
  expect_identical(contexts$narrow, c(TRUE, FALSE, FALSE, FALSE, FALSE))
})

test_that("une étude est réutilisée si ses entrées sont identiques, avec une tolérance sur les arbres", {
  root <- tempfile("partage-")
  study_dir <- file.path(root, "s1", "exports", "umep", "ombrage_arbres")
  dir.create(study_dir, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  contexts <- data.frame(tree_id = sprintf("t%03d", 1:100), context = "block", narrow = FALSE, stringsAsFactors = FALSE)
  base <- c(batiments = "B", arbres = "A", voirie_emprises = "V", zones_inondables = "Z", quartiers = "Q", logements = "L")
  inputs <- list(fingerprints = base, tree_base = "T", tree_contexts = contexts)
  write_umep_study_fingerprint(study_dir, "ombrage_arbres", "s1", inputs)
  expect_error(write_umep_study_fingerprint(study_dir, "ombrage_arbres", "s1", inputs), "non écrasée")
  records <- umep_study_records(root)

  same <- resolve_umep_study("ombrage_arbres", "s2", inputs, records)
  expect_identical(normalizePath(same$directory), normalizePath(study_dir))
  expect_match(umep_sharing_message(same, "s2"), "réutilisés")
  expect_null(umep_sharing_message(resolve_umep_study("ombrage_arbres", "s1", inputs, records), "s1"))

  few <- inputs
  few$fingerprints["arbres"] <- "A2"
  few$tree_contexts$context[1:3] <- "road"
  shared <- resolve_umep_study("ombrage_arbres", "s2", few, records)
  expect_false(is.null(shared$directory))
  expect_equal(shared$tree_change_share, 0.03)
  expect_match(umep_sharing_message(shared, "s2"), "3,0 % des arbres")

  many <- few
  many$tree_contexts$context[1:6] <- "road"
  blocked <- resolve_umep_study("ombrage_arbres", "s2", many, records)
  expect_null(blocked$directory)
  expect_identical(blocked$changed, "arbres")
  expect_match(umep_sharing_message(blocked, "s2"), "Nouvelle simulation nécessaire.*au-delà de la tolérance")

  moved <- few
  moved$tree_base <- "T2"
  expect_null(resolve_umep_study("ombrage_arbres", "s2", moved, records)$directory)

  buildings <- inputs
  buildings$fingerprints["batiments"] <- "B2"
  stale <- resolve_umep_study("ombrage_arbres", "s1", buildings, records)
  expect_null(stale$directory)
  expect_identical(normalizePath(stale$stale_directory), normalizePath(study_dir))
  expect_match(umep_sharing_message(stale, "s1"), "Résultats périmés")
  other <- resolve_umep_study("ombrage_arbres", "s2", buildings, records)
  expect_null(other$stale_directory)
  expect_null(umep_resolved_directory(other))
  expect_null(resolve_umep_study("climat_urbain", "s2", inputs, records)$directory)
})

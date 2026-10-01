make_target_display <- function(root) {
  directory <- file.path(root, "exports", "umep", "climat", "display_target")
  dir.create(directory, recursive = TRUE)
  grid <- sf::st_sf(
    grid_id = 1:2, cell_id = c("c_00_00", "c_01_00"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(-17.01, 14.77), c(-17.009, 14.77), c(-17.009, 14.771), c(-17.01, 14.771), c(-17.01, 14.77)))),
      sf::st_polygon(list(rbind(c(-17.009, 14.77), c(-17.008, 14.77), c(-17.008, 14.771), c(-17.009, 14.771), c(-17.009, 14.77)))),
      crs = 4326
    )
  )
  sf::st_write(grid, file.path(directory, "grille.geojson"), quiet = TRUE)
  cells <- expand.grid(grid_id = 1:2, day = "chaud", vegetation = c("arbres", "sans_arbres"), stringsAsFactors = FALSE)
  cells$ta_14h_c <- c(40, 30, 41, 41); cells$utci_14h_c <- 43; cells$uhi_14h_k <- 2; cells$ta_max_c <- 41
  cells$masque <- c(FALSE, TRUE, FALSE, TRUE)
  utils::write.csv(cells, file.path(directory, "target_cells.csv"), row.names = FALSE)
  effect <- data.frame(grid_id = 1:2, day = "chaud", delta_ta_14h_c = c(-1, -11), delta_utci_14h_c = 0,
                       delta_uhi_14h_k = 0, delta_ta_max_c = 0, masque = c(FALSE, TRUE))
  utils::write.csv(effect, file.path(directory, "target_cells_effet_arbres.csv"), row.names = FALSE)
  quartiers <- data.frame(day = "chaud", vegetation = c("arbres", "sans_arbres"), quartier = "Quartier 1",
                          surface_m2 = 100, ta_14h_c = c(40, 41), utci_14h_c = 43, uhi_14h_k = c(2, 3), ta_max_c = 41)
  utils::write.csv(quartiers, file.path(directory, "target_quartiers.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  hourly <- data.frame(day = "chaud", vegetation = c("sans_arbres", "arbres", "arbres"), hour = c(0, 1, 0),
                       quartier = "Quartier 1", ta_c = 30, utci_c = 30, uhi_k = 1, tb_rur_c = 29)
  utils::write.csv(hourly, file.path(directory, "target_hourly_quartiers.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  manifest <- list(grid = list(file = "grille.geojson"), days = data.frame(id = "chaud", date = "2017-04-14", label = "Chaud"),
                   mask = list(rule = "règle test"))
  jsonlite::write_json(manifest, file.path(directory, "manifest.json"), auto_unbox = TRUE)
  directory
}

test_that("les résultats TARGET masquent les mailles hors validité", {
  root <- tempfile("target-display-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  expect_null(find_target_display_directory(root))
  directory <- make_target_display(root)
  expect_identical(normalizePath(find_target_display_directory(root)), normalizePath(directory))
  display <- read_target_display(directory)

  values <- target_cell_values(display, "ta_14h", "chaud", "arbres")
  expect_equal(values$value, c(40, NA))
  effect <- target_cell_values(display, "ta_14h", "chaud", "effet")
  expect_equal(effect$value, c(-1, NA))
  expect_equal(unname(target_masked_summary(display, "chaud")), c(1, 2))

  geojson <- jsonlite::fromJSON(target_grid_geojson(display, values, target_scale("ta_14h", "arbres"), "°C"))
  props <- geojson$features$properties
  expect_true(is.na(props$color[props$grid_id == 2]))
  expect_match(props$label[props$grid_id == 2], "masquée")
  expect_match(props$label[props$grid_id == 1], "40,0 °C")

  expect_equal(unname(target_quartier_values(display, "uhi_14h", "chaud", "effet")), -1)
  expect_equal(unname(target_quartier_values(display, "ta_14h", "chaud", "sans_arbres")), 41)
  expect_identical(target_hourly_profile(display, "chaud", "Quartier 1")$hour, c(0L, 1L, 0L))
  expect_error(target_indicator("autre"), "inconnu")
})

test_that("les couleurs et textes TARGET sont cohérents", {
  scale <- target_color_scales$effect
  expect_identical(target_colors(c(-3, 3, NA), scale), c("#1A9850", "#D73027", NA))
  expect_match(target_gradient(scale), "^linear-gradient")
  for (indicator in target_indicators$id) {
    expect_true(nchar(target_indicator_guidance(indicator, "arbres")) > 40)
  }
  expect_match(target_indicator_guidance("ta_14h", "effet"), "effet des arbres")
})

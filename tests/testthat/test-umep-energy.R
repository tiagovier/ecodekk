make_energy_display <- function(root) {
  directory <- file.path(root, "exports", "umep", "bilan_energetique", "display")
  dir.create(directory, recursive = TRUE)
  grid <- sf::st_sf(
    grid_id = 1:2, cell_id = c("c_00_00", "c_01_00"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(-17.01, 14.77), c(-17.009, 14.77), c(-17.009, 14.771), c(-17.01, 14.771), c(-17.01, 14.77)))),
      sf::st_polygon(list(rbind(c(-17.009, 14.77), c(-17.008, 14.77), c(-17.008, 14.771), c(-17.009, 14.771), c(-17.009, 14.77)))),
      crs = 4326
    )
  )
  sf::st_write(grid, file.path(directory, "grille_100m.geojson"), quiet = TRUE)
  cells <- expand.grid(cell_id = c("c_00_00", "c_01_00"), day = "chaud", hour = c(13, 14),
                       scenario = c("arbres", "sans_arbres"), stringsAsFactors = FALSE)
  cells$QN <- 500; cells$QH <- c(300, 250, 310, 260, 320, 280, 330, 290); cells$QE <- 20
  cells$QS <- 100; cells$QF <- 1; cells$T2 <- 40
  utils::write.csv(cells, file.path(directory, "days_cells.csv"), row.names = FALSE)
  quartiers <- data.frame(quartier = "Quartier 1", day = "chaud", hour = c(2, 1), QN = 1, QH = 1, QE = 1,
                          QS = 1, QF = 1, T2 = 30, scenario = "arbres")
  utils::write.csv(quartiers, file.path(directory, "days_quartiers.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  monthly <- data.frame(quartier = "Quartier 1", month = c("2017-02", "2017-01", "2017-01"),
                        QN = 1, QH = c(80, 70, 75), QE = 5, QS = 1, QF = 1, T2 = 25,
                        scenario = c("arbres", "arbres", "sans_arbres"))
  utils::write.csv(monthly, file.path(directory, "monthly_quartiers.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  utils::write.csv(data.frame(scenario = "arbres", residu_moyen_w_m2 = 0), file.path(directory, "validation.csv"), row.names = FALSE)
  jsonlite::write_json(list(days = data.frame(id = "chaud", date = "2017-04-14"), avertissements = c("Limite A", "Limite B")),
                       file.path(directory, "manifest.json"), auto_unbox = TRUE)
  directory
}

test_that("les résultats SUEWS sont lus et cartographiés à 14 h", {
  root <- tempfile("energy-display-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  expect_null(find_energy_display_directory(root))
  directory <- make_energy_display(root)
  display <- read_energy_display(find_energy_display_directory(root))

  values <- energy_cell_values(display, "QH", "chaud", "arbres")
  expect_equal(values$value, c(310, 260))
  geojson <- jsonlite::fromJSON(energy_grid_geojson(display, values, energy_scale("QH", "arbres"), "W/m²"))
  expect_match(geojson$features$properties$label[1], "310,0 W/m²")
  expect_error(energy_scale("T2", "arbres"), "inconnue")
  expect_identical(energy_scale("QE", "difference_arbres_moins_sans"), energy_effect_scales$flux)

  expect_identical(energy_daily_cycle(display, "Quartier 1", "chaud", "arbres")$hour, c(1L, 2L))
  monthly <- energy_monthly_series(display, "Quartier 1", "QH")
  expect_identical(monthly$month, c("2017-01", "2017-02", "2017-01"))
  expect_equal(monthly$value, c(70, 80, 75))
  expect_error(energy_variable("XX"), "inconnue")
})

test_that("chaque variable SUEWS a un texte d'interprétation", {
  for (variable in energy_variables$id) expect_true(nchar(energy_variable_guidance(variable)) > 40)
  root <- tempfile("energy-display-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  guide <- as.character(energy_reading_guide(read_energy_display(make_energy_display(root))))
  expect_match(guide, "Limite B")
})

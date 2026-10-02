make_wind_display <- function(root) {
  directory <- file.path(root, "exports", "umep", "vent_confort", "display")
  dir.create(directory, recursive = TRUE)
  layers <- data.frame(
    indicator = c("wind", "wind", "utci", "utci", "utci_gain"),
    case = c("chaud", "dominant", "chaud", "chaud", "chaud"),
    vegetation = c(NA, NA, "trans3", "sans_arbres", NA),
    file = c("wind_chaud.png", "wind_dominant.png", "utci_chaud_trans3.png", "utci_chaud_sans.png", "gain_chaud.png"),
    stringsAsFactors = FALSE
  )
  layers$reference <- data.frame(vitesse_10m_m_s = c(4.94, 4.91, NA, NA, NA), direction_deg = c(62.6, 0, NA, NA, NA))
  for (file in layers$file) writeBin(as.raw(1:4), file.path(directory, file))
  manifest <- list(
    cases = data.frame(id = c("chaud", "dominant"), label = c("Journée chaude", "Vent dominant"), date = c("2017-04-14", NA)),
    vegetation = data.frame(id = c("trans3", "sans_arbres"), label = c("Arbres 3 %", "Sans arbres")),
    legends = list(utci = list(label = "UTCI", unit = "°C", stops = list(list(26, "#ffffbf"), list(46, "#d73027")))),
    layers = layers,
    coordinates = list(wind = list(c(1, 2), c(3, 2), c(3, 1), c(1, 1)), comfort = list(c(5, 6), c(7, 6), c(7, 5), c(5, 5))),
    avertissements = c("Limite vent")
  )
  jsonlite::write_json(manifest, file.path(directory, "manifest.json"), auto_unbox = TRUE, na = "null")
  indicators <- data.frame(cas = "chaud", quartier = "Quartier 1", contexte = c("Îlots", "Voirie"),
                           vent_median_m_s = c(1.2, 1.8), vent_p90_m_s = 2, utci_median_c = c(40, 42),
                           utci_part_stress_tres_fort_pct = 50, pet_median_c = NA, gain_utci_moyen_k = 1.5)
  utils::write.csv(indicators, file.path(directory, "indicators.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  directory
}

test_that("les résultats vent et confort ressenti sont lus et sélectionnés", {
  root <- tempfile("wind-display-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  expect_null(find_wind_display_directory(root))
  display <- read_wind_display(make_wind_display(root))

  expect_identical(unname(wind_case_choices(display, "wind")), c("chaud", "dominant"))
  expect_identical(unname(wind_case_choices(display, "utci")), "chaud")
  expect_match(names(wind_case_choices(display, "wind"))[1], "14/04/2017")
  expect_identical(names(wind_case_choices(display, "wind"))[2], "Vent dominant")

  layer <- wind_layer(display, "wind", "chaud", "trans3")
  expect_identical(layer$file, "wind_chaud.png")
  expect_equal(unname(layer$coordinates[1, ]), c(1, 2))
  expect_match(wind_reference_text(layer), "4,9 m/s, venant de l'est-nord-est (63°)", fixed = TRUE)
  expect_identical(wind_layer(display, "utci", "chaud", "sans_arbres")$file, "utci_chaud_sans.png")
  expect_equal(unname(wind_layer(display, "utci", "chaud", "trans3")$coordinates[1, ]), c(5, 6))
  expect_null(wind_layer(display, "utci", "dominant", "trans3"))
  expect_null(wind_reference_text(wind_layer(display, "utci_gain", "chaud")))

  table <- wind_indicator_table(display, "chaud")
  expect_identical(table$Contexte, c("Voirie", "Îlots"))
  expect_false("PET médiane (°C)" %in% names(table))
  expect_identical(wind_direction_label(c(0, 62.6, 292.5, 359)), c("du nord", "de l'est-nord-est", "de l'ouest-nord-ouest", "du nord"))
})

test_that("chaque indicateur vent/confort a un texte d'interprétation", {
  for (indicator in names(wind_indicator_labels)) expect_true(nchar(wind_indicator_guidance(indicator)) > 60)
  expect_error(wind_indicator_guidance("autre"), "inconnu")
})

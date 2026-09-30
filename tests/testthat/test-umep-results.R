make_umep_display <- function(root) {
  directory <- file.path(root, "exports", "umep", "etude", "display")
  dir.create(directory, recursive = TRUE)
  layers <- expand.grid(
    indicator = c("tmrt", "shade"), day = c("chaud", "pluies"),
    vegetation = c("trans3", "sans_arbres"), stringsAsFactors = FALSE
  )
  layers <- rbind(layers, data.frame(indicator = "cooling", day = c("chaud", "pluies"), vegetation = "trans3"))
  layers$file <- paste0(layers$indicator, "_", layers$day, "_", layers$vegetation, ".png")
  for (file in layers$file) writeBin(as.raw(1:4), file.path(directory, file))
  manifest <- list(
    coordinates = list(c(-17.01, 14.77), c(-17.0, 14.77), c(-17.0, 14.76), c(-17.01, 14.76)),
    days = data.frame(id = c("chaud", "pluies"), label = c("Journée chaude", "Journée pluvieuse"),
                      date = c("2017-04-14", "2017-09-21")),
    vegetation = data.frame(id = c("trans3", "sans_arbres"), label = c("Arbres 3 %", "Sans arbres")),
    legends = list(tmrt = list(label = "Tmrt", unit = "°C", stops = list(list(20, "#313695"), list(40, "#fee090"), list(60, "#a50026")))),
    layers = layers
  )
  jsonlite::write_json(manifest, file.path(directory, "manifest.json"), auto_unbox = TRUE)
  indicators <- data.frame(
    day = "chaud", vegetation = "trans3",
    quartier = c("Quartier 1", "Quartier 1", "Quartier 2"),
    context = c("Zones inondables", "Voirie", "Îlots"),
    area_m2 = c(100, 200, 300), tmrt_14h_median_c = c(50, 60, 45),
    share_above_60c_pct = c(10, 50, 0), shade_hours_mean = c(3, 1, 5),
    cooling_14h_mean_k = c(8, 4, 12)
  )
  utils::write.csv(indicators, file.path(directory, "indicators.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  profiles <- data.frame(
    day = "chaud", vegetation = "trans3", hour = c(2, 1, 1),
    class = c("Sous houppier", "Sous houppier", "Soleil, sans houppier"),
    tmrt_median_c = c(30, 25, 28), tair_c = c(24, 23, 23)
  )
  utils::write.csv(profiles, file.path(directory, "hourly_profiles.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  directory
}

test_that("les résultats SOLWEIG préparés sont trouvés et lus", {
  root <- tempfile("umep-display-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  expect_null(find_umep_display_directory(root))
  directory <- make_umep_display(root)
  expect_identical(normalizePath(find_umep_display_directory(root)), normalizePath(directory))

  display <- read_umep_display(directory)
  expect_identical(unname(umep_day_choices(display)), c("chaud", "pluies"))
  expect_match(names(umep_day_choices(display))[1], "14/04/2017")
  expect_identical(umep_layer_file(display, "tmrt", "chaud", "sans_arbres"), "tmrt_chaud_sans_arbres.png")
  expect_true(is.na(umep_layer_file(display, "cooling", "chaud", "sans_arbres")))

  legend <- umep_legend(display, "tmrt")
  expect_equal(c(legend$min, legend$max), c(20, 60))
  expect_match(legend$gradient, "#313695 0%.*#fee090 50%.*#a50026 100%")
})

test_that("les indicateurs SOLWEIG sont mis en forme par quartier et contexte", {
  root <- tempfile("umep-display-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  display <- read_umep_display(make_umep_display(root))

  table <- umep_indicator_table(display, "chaud", "trans3")
  expect_identical(table$Contexte, c("Voirie", "Zones inondables", "Îlots"))
  expect_equal(table$`Gain des arbres (K)`, c(4, 8, 12))
  expect_equal(nrow(umep_indicator_table(display, "pluies", "trans3")), 0)

  values <- umep_indicator_matrix(display, "tmrt", "chaud", "trans3")
  expect_identical(dimnames(values), list(c("Voirie", "Îlots", "Zones inondables"), c("Quartier 1", "Quartier 2")))
  expect_equal(values["Voirie", "Quartier 1"], 60)
  expect_true(is.na(values["Îlots", "Quartier 1"]))
  expect_null(umep_indicator_matrix(display, "tmrt", "pluies", "trans3"))
  expect_error(umep_indicator_column("autre"), "inconnu")

  profile <- umep_profile_data(display, "chaud", "trans3")
  expect_identical(profile$hour, c(1L, 1L, 2L))
})

test_that("des images SOLWEIG manquantes sont signalées", {
  root <- tempfile("umep-display-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  directory <- make_umep_display(root)
  unlink(file.path(directory, "tmrt_chaud_trans3.png"))
  expect_error(read_umep_display(directory), "manquantes")
})

test_that("chaque indicateur SOLWEIG a un texte d'interprétation", {
  for (indicator in names(umep_indicator_labels)) {
    expect_true(nchar(umep_indicator_guidance(indicator)) > 100)
  }
  expect_error(umep_indicator_guidance("autre"), "inconnu")
  guide <- as.character(umep_thermal_reading_guide())
  expect_match(guide, "Comment lire ces résultats")
  expect_match(guide, "transmissivité")
})

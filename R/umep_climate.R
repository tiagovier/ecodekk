# Résultats TARGET (îlot de chaleur, maille de 100 m) préparés par
# scripts/umep_climat/target_display.py. Lecture seule ; les mailles masquées
# (hors domaine de validité de TARGET) ne sont ni colorées ni moyennées.

target_indicators <- data.frame(
  id = c("ta_14h", "utci_14h", "uhi_14h", "ta_max"),
  label = c(
    "Température de l'air à 14 h",
    "UTCI à 14 h",
    "Îlot de chaleur à 14 h",
    "Température de l'air maximale"
  ),
  column = c("ta_14h_c", "utci_14h_c", "uhi_14h_k", "ta_max_c"),
  unit = c("°C", "°C", "K", "°C"),
  stringsAsFactors = FALSE
)

# Échelles fixes, identiques pour toutes les journées afin de rester comparables.
target_color_scales <- list(
  ta_14h = list(stops = c(20, 25, 30, 35, 40, 45), colors = c("#313695", "#74add1", "#e0f3f8", "#fee090", "#f46d43", "#a50026")),
  ta_max = list(stops = c(20, 25, 30, 35, 40, 45), colors = c("#313695", "#74add1", "#e0f3f8", "#fee090", "#f46d43", "#a50026")),
  utci_14h = list(stops = c(9, 26, 32, 38, 46, 50), colors = c("#74add1", "#ffffbf", "#fee090", "#f46d43", "#d73027", "#a50026")),
  uhi_14h = list(stops = c(-2, 0, 2, 4, 6), colors = c("#4575b4", "#f7f7f7", "#fdae61", "#d73027", "#a50026")),
  effect = list(stops = c(-3, -1.5, 0, 1.5, 3), colors = c("#1a9850", "#a6d96a", "#f7f7f7", "#fdae61", "#d73027"))
)

find_target_display_directory <- function(scenario_directory) {
  root <- file.path(scenario_directory, "exports", "umep")
  if (!dir.exists(root)) return(NULL)
  manifests <- list.files(root, pattern = "^manifest\\.json$", recursive = TRUE, full.names = TRUE)
  manifests <- manifests[basename(dirname(manifests)) == "display_target"]
  manifests <- manifests[!grepl("/superseded/", manifests, fixed = TRUE)]
  if (!length(manifests)) return(NULL)
  dirname(manifests[order(file.mtime(manifests), manifests, decreasing = TRUE)][[1]])
}

read_target_display <- function(directory) {
  manifest <- jsonlite::fromJSON(file.path(directory, "manifest.json"), simplifyVector = TRUE)
  required <- c("target_cells.csv", "target_cells_effet_arbres.csv", "target_quartiers.csv",
                "target_hourly_quartiers.csv", manifest$grid$file)
  absent <- required[!file.exists(file.path(directory, required))]
  if (length(absent)) stop("Fichiers TARGET manquants : ", paste(absent, collapse = ", "), ".")
  read <- function(file) utils::read.csv(file.path(directory, file), stringsAsFactors = FALSE, encoding = "UTF-8")
  cells <- read("target_cells.csv")
  if (!"masque" %in% names(cells)) stop("Les mailles TARGET ne portent pas le masque de validité.")
  cells$masque <- as.logical(cells$masque)
  effect <- read("target_cells_effet_arbres.csv")
  effect$masque <- as.logical(effect$masque)
  list(
    directory = directory,
    manifest = manifest,
    days = as.data.frame(manifest$days, stringsAsFactors = FALSE),
    cells = cells,
    effect = effect,
    quartiers = read("target_quartiers.csv"),
    hourly = read("target_hourly_quartiers.csv"),
    grid = sf::st_read(file.path(directory, manifest$grid$file), quiet = TRUE)
  )
}

target_vegetation_choices <- c(
  "Avec arbres" = "arbres",
  "Sans arbres (référence)" = "sans_arbres",
  "Effet des arbres (avec − sans)" = "effet"
)

target_indicator_choices <- function() {
  stats::setNames(target_indicators$id, target_indicators$label)
}

target_indicator <- function(indicator) {
  row <- target_indicators[target_indicators$id == indicator, , drop = FALSE]
  if (!nrow(row)) stop("Indicateur TARGET inconnu : ", indicator)
  as.list(row)
}

target_scale <- function(indicator, vegetation) {
  if (identical(vegetation, "effet")) target_color_scales$effect else target_color_scales[[indicator]]
}

target_colors <- function(values, scale) {
  out <- rep(NA_character_, length(values))
  valid <- !is.na(values)
  if (!any(valid)) return(out)
  clipped <- pmin(pmax(values[valid], min(scale$stops)), max(scale$stops))
  rgb <- grDevices::col2rgb(scale$colors)
  channel <- function(i) stats::approx(scale$stops, rgb[i, ], xout = clipped)$y
  out[valid] <- grDevices::rgb(channel(1), channel(2), channel(3), maxColorValue = 255)
  out
}

target_gradient <- function(scale) {
  positions <- round(100 * (scale$stops - min(scale$stops)) / diff(range(scale$stops)))
  paste0("linear-gradient(to right,", paste(paste0(scale$colors, " ", positions, "%"), collapse = ","), ")")
}

# Valeur par maille ; NA pour une maille masquée.
target_cell_values <- function(display, indicator, day, vegetation) {
  info <- target_indicator(indicator)
  if (identical(vegetation, "effet")) {
    data <- display$effect[display$effect$day == day, , drop = FALSE]
    value <- data[[paste0("delta_", info$column)]]
  } else {
    data <- display$cells[display$cells$day == day & display$cells$vegetation == vegetation, , drop = FALSE]
    value <- data[[info$column]]
  }
  value[data$masque] <- NA_real_
  data.frame(grid_id = data$grid_id, value = value, masque = data$masque)
}

target_grid_geojson <- function(display, values, scale, unit) {
  grid <- display$grid
  matched <- values[match(grid$grid_id, values$grid_id), , drop = FALSE]
  grid$color <- target_colors(matched$value, scale)
  grid$masque <- matched$masque %in% TRUE
  grid$label <- ifelse(
    grid$masque,
    "Maille masquée (hors domaine de validité de TARGET)",
    paste0(format_number_fr(matched$value, 1), " ", unit)
  )
  sf_to_geojson(grid[, c("grid_id", "cell_id", "color", "masque", "label")])
}

# Valeur par quartier (mailles masquées exclues à la préparation).
target_quartier_values <- function(display, indicator, day, vegetation) {
  column <- target_indicator(indicator)$column
  pick <- function(veg) {
    data <- display$quartiers[display$quartiers$day == day & display$quartiers$vegetation == veg, , drop = FALSE]
    stats::setNames(data[[column]], data$quartier)
  }
  if (!identical(vegetation, "effet")) return(pick(vegetation))
  with_trees <- pick("arbres")
  without <- pick("sans_arbres")
  common <- intersect(names(with_trees), names(without))
  round(with_trees[common] - without[common], 2)
}

target_hourly_profile <- function(display, day, quartier) {
  data <- display$hourly[display$hourly$day == day & display$hourly$quartier == quartier, , drop = FALSE]
  data[order(data$vegetation, data$hour), , drop = FALSE]
}

target_masked_summary <- function(display, day) {
  data <- display$effect[display$effect$day == day, , drop = FALSE]
  c(masked = sum(data$masque), total = nrow(data))
}

target_indicator_guidance <- function(indicator, vegetation) {
  text <- switch(
    indicator,
    ta_14h = "Température de l'air à hauteur de piéton, moyennée sur la maille, à 14 h.",
    ta_max = "Température de l'air la plus élevée de la journée dans la maille.",
    utci_14h = paste(
      "L'UTCI est une température ressentie qui combine air, humidité, vent et rayonnement.",
      "Repères officiels : 26 à 32 °C stress modéré, 32 à 38 °C fort, 38 à 46 °C très fort, au-delà extrême."
    ),
    uhi_14h = paste(
      "Intensité de l'îlot de chaleur : écart entre la température de l'air de la maille et la référence rurale",
      "calculée par TARGET. Une valeur positive signale une maille plus chaude que la campagne environnante."
    ),
    stop("Indicateur TARGET inconnu : ", indicator)
  )
  if (identical(vegetation, "effet")) {
    text <- paste(text, "En mode « effet des arbres », la carte montre l'écart avec − sans arbres : en vert, les arbres rafraîchissent la maille.")
  }
  text
}

target_reading_guide <- function(display) {
  mask <- display$manifest$mask
  htmltools::tags$details(
    class = "umep-guide",
    htmltools::tags$summary("Comment lire ces résultats"),
    htmltools::tags$h4("Ce qui est simulé"),
    htmltools::tags$p(
      "Le modèle TARGET (UMEP) estime, pour chaque maille de 100 m, la température de l'air dans la rue, l'UTCI",
      "et l'intensité d'îlot de chaleur, à partir de la part de bâti, d'arbres, de pavés, d'eau et de sol nu,",
      "de la hauteur des bâtiments et de la météorologie ERA5, avec 48 h de mise en route avant chaque journée."
    ),
    htmltools::tags$h4("Avec, sans arbres et effet des arbres"),
    htmltools::tags$p(
      "La référence « sans arbres » remplace les houppiers par la surface située dessous, mêmes bâtiments et même météo.",
      "Le mode « effet des arbres » affiche la différence : une valeur négative (verte) est un rafraîchissement."
    ),
    htmltools::tags$h4("Mailles masquées"),
    htmltools::tags$p(
      "Sous un couvert arboré presque continu, TARGET réduit fortement le vent et produit des écarts non physiques.",
      paste0("Règle appliquée : ", mask$rule, ". Ces mailles restent vides sur la carte et sont exclues des moyennes par quartier.")
    ),
    htmltools::tags$h4("Limites"),
    htmltools::tags$ul(
      htmltools::tags$li("TARGET est un modèle expérimental conçu pour des villes australiennes : les écarts entre scénarios sont plus fiables que les valeurs absolues."),
      htmltools::tags$li("La maille de 100 m moyenne les contrastes ; la carte de confort thermique (1 m) montre le détail rue par rue."),
      htmltools::tags$li("TARGET traite le sol nu comme de l'herbe sèche et regroupe toutes les essences en une seule classe d'arbres."),
      htmltools::tags$li("La météorologie ERA5 représente le climat régional ; aucune mesure de terrain ne valide encore ces valeurs.")
    )
  )
}

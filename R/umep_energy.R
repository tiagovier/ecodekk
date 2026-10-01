# Résultats SUEWS (bilan d'énergie, maille de 100 m) préparés par
# scripts/umep_suews/suews_display.py. Lecture seule.

# La température à 2 m de SUEWS n'est pas affichée : ce diagnostic n'est pas
# fiable sous un couvert arboré haut (voir le manifeste de l'étude).
energy_variables <- data.frame(
  id = c("QN", "QH", "QE", "QS", "QF"),
  label = c(
    "Rayonnement net Q*", "Chaleur sensible QH", "Chaleur latente QE (évaporation)",
    "Stockage de chaleur ΔQS", "Chaleur anthropique QF"
  ),
  unit = c("W/m²", "W/m²", "W/m²", "W/m²", "W/m²"),
  color = c("#d7301f", "#fc8d59", "#2b8cbe", "#8c510a", "#6a51a3"),
  stringsAsFactors = FALSE
)

energy_scenario_choices <- c(
  "Avec arbres" = "arbres",
  "Sans arbres (référence)" = "sans_arbres",
  "Effet des arbres (avec − sans)" = "difference_arbres_moins_sans"
)

# Échelles fixes à 14 h, communes aux trois journées, calées sur les valeurs
# observées (2e–98e centiles des mailles).
energy_color_scales <- list(
  QN = list(stops = c(450, 500, 550, 600, 650, 700), colors = c("#ffffcc", "#fed976", "#feb24c", "#fd8d3c", "#e31a1c", "#800026")),
  QH = list(stops = c(200, 275, 350, 425, 500), colors = c("#ffffcc", "#fed976", "#fd8d3c", "#e31a1c", "#800026")),
  QE = list(stops = c(0, 50, 100, 175, 250), colors = c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b")),
  QS = list(stops = c(100, 150, 200, 250, 300), colors = c("#f7f4f9", "#d4b9da", "#c994c7", "#dd1c77", "#67001f")),
  QF = list(stops = c(0, 2, 4, 6, 8), colors = c("#fcfbfd", "#dadaeb", "#9e9ac8", "#6a51a3", "#3f007d"))
)
energy_effect_scales <- list(
  flux = list(stops = c(-150, -75, 0, 75, 150), colors = c("#2166ac", "#92c5de", "#f7f7f7", "#f4a582", "#b2182b"))
)

find_energy_display_directory <- function(scenario_directory, study = "bilan_energetique") {
  directory <- file.path(scenario_directory, "exports", "umep", study, "display")
  if (file.exists(file.path(directory, "manifest.json"))) directory else NULL
}

read_energy_display <- function(directory) {
  manifest <- jsonlite::fromJSON(file.path(directory, "manifest.json"), simplifyVector = TRUE)
  files <- c("days_cells.csv", "days_quartiers.csv", "monthly_quartiers.csv", "validation.csv", "grille_100m.geojson")
  absent <- files[!file.exists(file.path(directory, files))]
  if (length(absent)) stop("Fichiers SUEWS manquants : ", paste(absent, collapse = ", "), ".")
  read <- function(file) utils::read.csv(file.path(directory, file), stringsAsFactors = FALSE, encoding = "UTF-8")
  list(
    directory = directory,
    manifest = manifest,
    days = as.data.frame(manifest$days, stringsAsFactors = FALSE),
    cells = read("days_cells.csv"),
    quartiers = read("days_quartiers.csv"),
    monthly = read("monthly_quartiers.csv"),
    validation = read("validation.csv"),
    grid = sf::st_read(file.path(directory, "grille_100m.geojson"), quiet = TRUE)
  )
}

energy_variable <- function(variable) {
  row <- energy_variables[energy_variables$id == variable, , drop = FALSE]
  if (!nrow(row)) stop("Variable SUEWS inconnue : ", variable)
  as.list(row)
}

energy_scale <- function(variable, scenario) {
  energy_variable(variable)
  if (identical(scenario, "difference_arbres_moins_sans")) energy_effect_scales$flux else energy_color_scales[[variable]]
}

energy_cell_values <- function(display, variable, day, scenario, hour = 14L) {
  energy_variable(variable)
  data <- display$cells[
    display$cells$day == day & display$cells$scenario == scenario & display$cells$hour == hour, , drop = FALSE
  ]
  data.frame(cell_id = data$cell_id, value = data[[variable]], stringsAsFactors = FALSE)
}

energy_grid_geojson <- function(display, values, scale, unit) {
  grid <- display$grid
  matched <- values$value[match(grid$cell_id, values$cell_id)]
  grid$color <- target_colors(matched, scale)
  grid$masque <- is.na(matched)
  grid$label <- ifelse(is.na(matched), "Valeur absente", paste0(format_number_fr(matched, 1), " ", unit))
  sf_to_geojson(grid[, c("grid_id", "cell_id", "color", "masque", "label")])
}

energy_daily_cycle <- function(display, quartier, day, scenario) {
  data <- display$quartiers[
    display$quartiers$quartier == quartier & display$quartiers$day == day &
      display$quartiers$scenario == scenario, , drop = FALSE
  ]
  data[order(data$hour), , drop = FALSE]
}

energy_monthly_series <- function(display, quartier, variable) {
  energy_variable(variable)
  data <- display$monthly[
    display$monthly$quartier == quartier & display$monthly$scenario %in% c("arbres", "sans_arbres"), , drop = FALSE
  ]
  data <- data[order(data$scenario, data$month), c("month", "scenario", variable), drop = FALSE]
  names(data)[3] <- "value"
  data
}

energy_variable_guidance <- function(variable) {
  switch(
    variable,
    QN = "Rayonnement net : énergie radiative disponible en surface (soleil absorbé moins rayonnement émis). C'est le « budget » que se partagent QH, QE et ΔQS.",
    QH = "Chaleur sensible : part de l'énergie qui réchauffe directement l'air. Plus elle est élevée, plus l'air de la maille chauffe.",
    QE = "Chaleur latente : énergie consommée par l'évaporation et la transpiration des arbres. Elle rafraîchit sans élever la température de l'air ; elle dépend de l'eau disponible.",
    QS = "Stockage : chaleur accumulée le jour dans les bâtiments, les pavés et le sol, puis restituée la nuit (valeurs négatives), ce qui entretient la chaleur nocturne.",
    QF = "Chaleur anthropique : chaleur dégagée par les habitants et les usages domestiques (sans climatisation ni trafic dans cette étude).",
    stop("Variable SUEWS inconnue : ", variable)
  )
}

energy_reading_guide <- function(display) {
  warnings <- display$manifest$avertissements
  htmltools::tags$details(
    class = "umep-guide",
    htmltools::tags$summary("Comment lire ces résultats"),
    htmltools::tags$h4("Ce qui est simulé"),
    htmltools::tags$p(
      "Le modèle SUEWS (UMEP) calcule, maille par maille de 100 m et toutes les 5 minutes de 2016 à janvier 2018,",
      "comment l'énergie reçue du soleil se répartit : réchauffement de l'air (QH), évaporation (QE) et stockage",
      "dans les matériaux (ΔQS), avec la chaleur dégagée par les habitants (QF). Q* + QF ≈ QH + QE + ΔQS."
    ),
    htmltools::tags$h4("Comment interpréter"),
    htmltools::tags$ul(
      htmltools::tags$li("Sur sol nu sec, presque toute l'énergie part en chaleur sensible (QH) : c'est elle qui réchauffe l'air."),
      htmltools::tags$li("Les arbres transpirent toute l'année grâce à l'eau profonde : ils convertissent une partie de l'énergie en évaporation (QE), surtout dans les mailles très boisées."),
      htmltools::tags$li("Un stockage élevé le jour annonce une restitution de chaleur la nuit : c'est le mécanisme de l'îlot de chaleur nocturne."),
      htmltools::tags$li("En mode « effet des arbres », une valeur négative de QH est un gain ; une valeur positive de QE signale plus d'évaporation.")
    ),
    htmltools::tags$h4("Pourquoi les arbres n'abaissent pas ici la chaleur sensible"),
    htmltools::tags$p(
      "À l'échelle d'une maille de 100 m, SUEWS trouve que les arbres ajoutent un peu de chaleur sensible (environ +10 à +55 W/m² l'après-midi),",
      "malgré une évaporation nettement plus forte. La raison est leur couleur : un houppier sombre (albédo 0,10 dans le modèle) absorbe",
      "bien plus de soleil que le sol latéritique clair qu'il recouvre (albédo 0,30), soit 80 à 100 W/m² de plus, ce qui dépasse le gain",
      "d'évaporation. Cet effet est en partie réel au-dessus d'un sol très clair, et probablement exagéré par l'albédo des arbres retenu,",
      "faible pour des feuillus tropicaux (souvent 0,15 à 0,20)."
    ),
    htmltools::tags$p(
      "Ce bilan ne mesure pas le confort du piéton : sous les arbres, l'ombre réduit la température moyenne radiante de 15 à 20 °C",
      "(onglet Confort thermique). Les deux résultats sont compatibles : l'ombre protège les personnes, tandis que la maille absorbe plus d'énergie."
    ),
    htmltools::tags$h4("Limites"),
    htmltools::tags$ul(lapply(warnings, htmltools::tags$li))
  )
}

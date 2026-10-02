# Résultats URock (vent à 1,5 m) et confort ressenti (UTCI, PET) préparés par
# scripts/umep_vent/vent_display.py. Lecture seule.

wind_indicator_labels <- c(
  wind = "Vent à 1,5 m",
  utci = "UTCI à 14 h (température ressentie)",
  pet = "PET à 14 h (température physiologique)",
  utci_gain = "Gain d'UTCI apporté par les arbres"
)

find_wind_display_directory <- function(scenario_directory, study = "vent_confort") {
  directory <- file.path(scenario_directory, "exports", "umep", study, "display")
  if (file.exists(file.path(directory, "manifest.json"))) directory else NULL
}

read_wind_display <- function(directory) {
  manifest <- jsonlite::fromJSON(file.path(directory, "manifest.json"), simplifyVector = TRUE)
  required <- c("cases", "legends", "layers", "coordinates")
  missing <- setdiff(required, names(manifest))
  if (length(missing)) stop("Manifeste vent/confort incomplet : ", paste(missing, collapse = ", "), ".")
  layers <- as.data.frame(manifest$layers[c("indicator", "case", "vegetation", "file")], stringsAsFactors = FALSE)
  layers$vegetation[is.na(layers$vegetation)] <- ""
  absent <- layers$file[!file.exists(file.path(directory, layers$file))]
  if (length(absent)) stop("Images vent/confort manquantes : ", paste(head(absent, 3), collapse = ", "), ".")
  reference <- manifest$layers$reference
  list(
    directory = directory,
    manifest = manifest,
    cases = as.data.frame(manifest$cases, stringsAsFactors = FALSE),
    vegetation = as.data.frame(manifest$vegetation, stringsAsFactors = FALSE),
    layers = layers,
    reference = if (is.data.frame(reference)) reference else NULL,
    indicators = utils::read.csv(file.path(directory, "indicators.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")
  )
}

# Cas disponibles pour un indicateur : seules les combinaisons produites.
wind_case_choices <- function(display, indicator) {
  available <- unique(display$layers$case[display$layers$indicator == indicator])
  cases <- display$cases[display$cases$id %in% available, , drop = FALSE]
  labels <- ifelse(
    is.na(cases$date) | !nzchar(cases$date),
    cases$label,
    paste0(cases$label, " (", format(as.Date(cases$date), "%d/%m/%Y"), ")")
  )
  stats::setNames(cases$id, labels)
}

wind_needs_vegetation <- function(indicator) indicator %in% c("utci", "pet")

wind_layer <- function(display, indicator, case, vegetation = "") {
  if (!wind_needs_vegetation(indicator)) vegetation <- ""
  match <- display$layers$indicator == indicator & display$layers$case == case &
    display$layers$vegetation == vegetation
  if (!any(match)) return(NULL)
  index <- which(match)[1]
  coordinates_key <- if (identical(indicator, "wind")) "wind" else "comfort"
  reference <- if (!is.null(display$reference)) display$reference[index, , drop = FALSE] else NULL
  list(
    file = display$layers$file[index],
    coordinates = display$manifest$coordinates[[coordinates_key]],
    reference = reference
  )
}

wind_reference_text <- function(layer) {
  reference <- layer$reference
  if (is.null(reference) || !nrow(reference) || is.na(reference$vitesse_10m_m_s)) return(NULL)
  paste0(
    "Vent de référence ERA5 à 10 m : ", format_number_fr(reference$vitesse_10m_m_s, 1),
    " m/s, venant ", wind_direction_label(reference$direction_deg),
    " (", format_number_fr(reference$direction_deg, 0), "°)."
  )
}

# Secteur de provenance avec son article (« du nord », « de l'est »).
wind_direction_label <- function(degrees) {
  sectors <- c("du nord", "du nord-nord-est", "du nord-est", "de l'est-nord-est", "de l'est",
               "de l'est-sud-est", "du sud-est", "du sud-sud-est", "du sud", "du sud-sud-ouest",
               "du sud-ouest", "de l'ouest-sud-ouest", "de l'ouest", "de l'ouest-nord-ouest",
               "du nord-ouest", "du nord-nord-ouest")
  sectors[(round((degrees %% 360) / 22.5) %% 16) + 1]
}

wind_indicator_table <- function(display, case) {
  data <- display$indicators[display$indicators$cas == case, , drop = FALSE]
  if (!nrow(data)) return(data.frame())
  data$contexte <- factor(data$contexte, levels = umep_context_levels)
  data <- data[order(data$quartier, data$contexte), , drop = FALSE]
  out <- data.frame(
    Quartier = data$quartier,
    Contexte = as.character(data$contexte),
    `Vent médian à 1,5 m (m/s)` = data$vent_median_m_s,
    `Vent 90e centile (m/s)` = data$vent_p90_m_s,
    `UTCI médiane (°C)` = data$utci_median_c,
    `Part en stress très fort (%)` = data$utci_part_stress_tres_fort_pct,
    `PET médiane (°C)` = data$pet_median_c,
    `Gain d'UTCI (K)` = data$gain_utci_moyen_k,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  keep <- vapply(out, function(column) !all(is.na(column)), logical(1))
  out[, keep, drop = FALSE]
}

wind_indicator_guidance <- function(indicator) {
  switch(
    indicator,
    wind = paste(
      "Vitesse du vent à hauteur de piéton calculée par URock à partir du vent ERA5 de la journée.",
      "Les bâtiments et les arbres créent des sillages (zones abritées, en clair) et des accélérations dans les rues alignées sur le vent (en foncé).",
      "Un vent modéré rafraîchit par temps chaud ; les couloirs de ventilation se lisent comme des bandes continues."
    ),
    utci = paste(
      "L'UTCI est une température ressentie qui combine l'air, l'humidité, le vent et le rayonnement (Tmrt).",
      "Repères officiels : 26 à 32 °C stress modéré, 32 à 38 °C fort, 38 à 46 °C très fort, au-delà de 46 °C extrême."
    ),
    pet = paste(
      "La PET (température physiologique équivalente) est la température d'une pièce où le corps serait dans le même état thermique.",
      "Au-delà de 41 °C, le stress thermique est extrême pour la plupart des échelles."
    ),
    utci_gain = paste(
      "Écart d'UTCI à 14 h entre la référence sans arbres et la simulation avec arbres.",
      "Une valeur positive est le confort gagné grâce aux arbres (ombre surtout) ; elle peut être réduite là où les arbres freinent le vent."
    ),
    stop("Indicateur vent/confort inconnu : ", indicator)
  )
}

wind_reading_guide <- function(display) {
  warnings <- display$manifest$avertissements
  htmltools::tags$details(
    class = "umep-guide",
    htmltools::tags$summary("Comment lire ces résultats"),
    htmltools::tags$h4("Ce qui est simulé"),
    htmltools::tags$p(
      "URock (UMEP) calcule le champ de vent autour des bâtiments et des arbres à partir d'un vent de référence ERA5,",
      "sur une grille de 2 m, à 1,5 m du sol. L'UTCI et la PET combinent ensuite ce vent avec la température moyenne",
      "radiante de SOLWEIG (onglet Confort thermique), la température et l'humidité de l'air : ce sont des températures ressenties."
    ),
    htmltools::tags$h4("Journées et vents dominants"),
    htmltools::tags$p(
      "Pour chaque journée, le vent est celui de 14 h. Les deux vents dominants (saison sèche et saison des pluies, secteur",
      "le plus fréquent de 2015 à 2024) servent à lire les couloirs de ventilation indépendamment d'une journée particulière ;",
      "ils n'ont pas d'UTCI associé."
    ),
    htmltools::tags$h4("Comment interpréter"),
    htmltools::tags$ul(
      htmltools::tags$li("Par forte chaleur, l'ombre réduit beaucoup plus la température ressentie que le vent : l'essentiel du gain d'UTCI vient des arbres qui ombragent."),
      htmltools::tags$li("Les arbres freinent aussi le vent ; là où le vent rafraîchissait, ce freinage réduit un peu leur gain."),
      htmltools::tags$li("Les écarts d'UTCI entre quartiers et entre scénarios sont plus fiables que les valeurs absolues.")
    ),
    htmltools::tags$h4("Limites"),
    htmltools::tags$ul(
      htmltools::tags$li(paste(
        "Directions arrondies : pour le 17/01/2018 et le 11/05/2022, URock échoue avec la direction exacte d'ERA5",
        "(28,6° et 322,7°, défaut du modèle reproduit sur un extrait). Les calculs utilisent 30° et 325°, un écart",
        "inférieur à l'incertitude d'une direction horaire ERA5."
      )),
      lapply(warnings, htmltools::tags$li)
    )
  )
}

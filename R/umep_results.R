# Résultats SOLWEIG (étude UMEP) préparés pour l'affichage par
# scripts/umep_trees/solweig_display.py. Lecture seule : l'application ne
# recalcule ni ne réécrit aucun raster.

umep_indicator_labels <- c(
  tmrt = "Température moyenne radiante à 14 h",
  shade = "Heures d'ombre de 10 h à 16 h",
  cooling = "Gain des arbres sur la Tmrt à 14 h"
)

umep_context_levels <- c("Voirie", "Îlots", "Zones inondables")

umep_profile_colors <- c(
  "Soleil, sans houppier" = "#d7301f",
  "Ombre des bâtiments" = "#6a51a3",
  "Sous houppier" = "#238b45"
)

# Dossier display/ de l'étude SOLWEIG (exports/umep/<study>/display/). Chaque
# étude a son propre dossier : ne jamais prendre le display/ d'une autre étude.
find_umep_display_directory <- function(scenario_directory, study = "ombrage_arbres") {
  directory <- file.path(scenario_directory, "exports", "umep", study, "display")
  if (file.exists(file.path(directory, "manifest.json"))) directory else NULL
}

read_umep_display <- function(directory) {
  manifest_path <- file.path(directory, "manifest.json")
  if (!file.exists(manifest_path)) stop("Le manifeste des résultats SOLWEIG est introuvable.")
  manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = TRUE)
  required <- c("coordinates", "days", "vegetation", "legends", "layers")
  missing <- setdiff(required, names(manifest))
  if (length(missing)) {
    stop("Manifeste SOLWEIG incomplet : ", paste(missing, collapse = ", "), ".")
  }
  layers <- as.data.frame(manifest$layers, stringsAsFactors = FALSE)
  absent <- layers$file[!file.exists(file.path(directory, layers$file))]
  if (length(absent)) {
    stop("Images SOLWEIG manquantes : ", paste(head(absent, 3), collapse = ", "), ".")
  }
  indicators <- utils::read.csv(file.path(directory, "indicators.csv"), stringsAsFactors = FALSE,
                                encoding = "UTF-8")
  profiles <- utils::read.csv(file.path(directory, "hourly_profiles.csv"), stringsAsFactors = FALSE,
                              encoding = "UTF-8")
  list(
    directory = directory,
    manifest = manifest,
    layers = layers,
    days = as.data.frame(manifest$days, stringsAsFactors = FALSE),
    vegetation = as.data.frame(manifest$vegetation, stringsAsFactors = FALSE),
    indicators = indicators,
    profiles = profiles
  )
}

umep_day_choices <- function(display) {
  stats::setNames(
    display$days$id,
    paste0(display$days$label, " (", format(as.Date(display$days$date), "%d/%m/%Y"), ")")
  )
}

umep_vegetation_choices <- function(display) {
  stats::setNames(display$vegetation$id, display$vegetation$label)
}

# Fichier image d'une combinaison ; NA si elle n'existe pas (le gain des arbres
# n'a pas de sens pour la référence sans arbres).
umep_layer_file <- function(display, indicator, day, vegetation) {
  match <- display$layers$indicator == indicator &
    display$layers$day == day & display$layers$vegetation == vegetation
  if (!any(match)) return(NA_character_)
  display$layers$file[which(match)[1]]
}

umep_legend <- function(display, indicator) {
  legend <- display$manifest$legends[[indicator]]
  stops <- legend$stops
  if (is.list(stops)) stops <- do.call(rbind, stops)
  values <- as.numeric(stops[, 1])
  colors <- as.character(stops[, 2])
  positions <- round(100 * (values - min(values)) / diff(range(values)))
  list(
    label = legend$label,
    unit = legend$unit,
    min = min(values),
    max = max(values),
    ticks = values,
    gradient = paste0(
      "linear-gradient(to right,",
      paste(paste0(colors, " ", positions, "%"), collapse = ","), ")"
    )
  )
}

umep_indicator_column <- function(indicator) {
  switch(
    indicator,
    tmrt = "tmrt_14h_median_c",
    shade = "shade_hours_mean",
    cooling = "cooling_14h_mean_k",
    stop("Indicateur inconnu : ", indicator)
  )
}

# Tableau par quartier et contexte pour une journée et une végétation.
umep_indicator_table <- function(display, day, vegetation) {
  data <- display$indicators[
    display$indicators$day == day & display$indicators$vegetation == vegetation, , drop = FALSE
  ]
  if (!nrow(data)) return(data.frame())
  data$context <- factor(data$context, levels = umep_context_levels)
  data <- data[order(data$quartier, data$context), , drop = FALSE]
  cooling <- if ("cooling_14h_mean_k" %in% names(data)) data$cooling_14h_mean_k else NA_real_
  out <- data.frame(
    Quartier = data$quartier,
    Contexte = as.character(data$context),
    `Surface (m²)` = round(data$area_m2),
    `Tmrt médiane à 14 h (°C)` = data$tmrt_14h_median_c,
    `Part > 60 °C (%)` = data$share_above_60c_pct,
    `Ombre 10 h–16 h (h)` = data$shade_hours_mean,
    `Gain des arbres (K)` = cooling,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

# Matrice contexte × quartier de l'indicateur, pour un diagramme groupé.
umep_indicator_matrix <- function(display, indicator, day, vegetation) {
  column <- umep_indicator_column(indicator)
  data <- display$indicators[
    display$indicators$day == day & display$indicators$vegetation == vegetation, , drop = FALSE
  ]
  if (!nrow(data) || !column %in% names(data) || all(is.na(data[[column]]))) return(NULL)
  quartiers <- sort(unique(data$quartier))
  contexts <- umep_context_levels[umep_context_levels %in% data$context]
  values <- matrix(NA_real_, length(contexts), length(quartiers), dimnames = list(contexts, quartiers))
  values[cbind(match(data$context, contexts), match(data$quartier, quartiers))] <- data[[column]]
  values
}

umep_profile_data <- function(display, day, vegetation) {
  data <- display$profiles[
    display$profiles$day == day & display$profiles$vegetation == vegetation, , drop = FALSE
  ]
  data[order(data$class, data$hour), , drop = FALSE]
}

# Textes d'aide à l'interprétation (affichés dans l'onglet « Confort thermique »).
umep_indicator_guidance <- function(indicator) {
  switch(
    indicator,
    tmrt = paste(
      "La température moyenne radiante (Tmrt) résume le rayonnement solaire et thermique reçu par un piéton :",
      "soleil direct, ciel, sol et murs. Ce n'est pas la température de l'air. En plein soleil de saison chaude,",
      "elle dépasse souvent la température de l'air de 20 à 30 °C ; à l'ombre, elle s'en rapproche.",
      "Plus la couleur est rouge, plus la charge thermique ressentie est forte."
    ),
    shade = paste(
      "Nombre d'heures pendant lesquelles chaque point est à l'ombre entre 10 h et 16 h, les heures les plus chaudes (0 à 6 h).",
      "L'ombre des houppiers est comptée en tenant compte de la lumière qu'ils laissent passer (transmissivité).",
      "Une valeur élevée signale un lieu protégé pour les déplacements et les activités extérieures."
    ),
    cooling = paste(
      "Écart de Tmrt à 14 h entre la référence sans arbres et la simulation avec arbres, mêmes bâtiments et même météo.",
      "Une valeur positive est le refroidissement apporté par les arbres ; elle est la plus forte sous les houppiers",
      "et nulle loin des arbres. Des valeurs légèrement négatives peuvent apparaître près des arbres,",
      "qui renvoient une partie du rayonnement thermique."
    ),
    stop("Indicateur inconnu : ", indicator)
  )
}

umep_thermal_reading_guide <- function() {
  htmltools::tags$details(
    class = "umep-guide",
    htmltools::tags$summary("Comment lire ces résultats"),
    htmltools::tags$h4("Ce qui est simulé"),
    htmltools::tags$p(
      "Le modèle SOLWEIG (UMEP) calcule, heure par heure et pour chaque mètre carré de sol hors bâtiments,",
      "le rayonnement reçu par un piéton debout, à partir des bâtiments, des arbres attribués du scénario,",
      "de l'occupation du sol et de la météorologie ERA5 des journées retenues."
    ),
    htmltools::tags$h4("Les quatre journées"),
    htmltools::tags$ul(
      htmltools::tags$li(htmltools::tags$strong("Journée chaude extrême (14/04/2017)"), " : 42,2 °C de maximum, parmi les 2 % des jours d'avril les plus chauds de 2015 à 2024. Elle montre le pire cas : vague de chaleur, soleil presque au zénith, air très sec."),
      htmltools::tags$li(htmltools::tags$strong("Journée chaude typique (11/05/2022)"), " : 36,7 °C de maximum, proche du 90e centile des jours d'avril à juin. Elle représente une journée chaude ordinaire de saison sèche, à privilégier pour les comparaisons courantes."),
      htmltools::tags$li(htmltools::tags$strong("Journée de saison des pluies (21/09/2017)"), " : moins chaude mais humide ; Faidherbia albida, sans feuilles à cette saison, est retiré et les zones inondables sont en eau."),
      htmltools::tags$li(htmltools::tags$strong("Journée fraîche de saison sèche (17/01/2018)"), " : référence, soleil plus bas et harmattan ; Faidherbia albida est en feuilles.")
    ),
    htmltools::tags$h4("Fiabilité des données météorologiques"),
    htmltools::tags$p(
      "Les journées proviennent de la réanalyse ERA5 (maille d'environ 31 km), qui n'est pas une mesure sur le site.",
      "Elle a été comparée aux stations météorologiques (NOAA GHCN-Daily) : la climatologie d'avril d'ERA5",
      "(maximum moyen 33,4 °C, 90e centile 38,6 °C, record 44,5 °C) est très proche de celle de l'ancienne station de Thiès",
      "(1940-1983 : 33,0 / 39,1 / 43,5 °C). Le 14/04/2017, la station de Diourbel a relevé 42,4 °C pour 42,2 °C dans ERA5 :",
      "la vague de chaleur est réelle. Les stations actuelles les plus proches encadrent Thiès : Diourbel, à l'intérieur, est",
      "nettement plus chaude ; Dakar-Yoff, sur le littoral, nettement plus fraîche. Aucune station actuelle n'existe à Thiès."
    ),
    htmltools::tags$p(
      "La température moyenne radiante en plein soleil (environ 69 °C à 14 h lors de la journée extrême) dépasse la",
      "température de l'air de 20 à 30 °C, un écart courant par ciel clair en climat chaud et sec. Elle peut être un peu",
      "surestimée si la journée était poussiéreuse (harmattan), ce qu'ERA5 ne représente qu'en moyenne. Des mesures au",
      "thermomètre à globe sur le site permettraient de la confirmer."
    ),
    htmltools::tags$h4("Végétation et transmissivité"),
    htmltools::tags$p(
      "La transmissivité est la part de la lumière qui traverse les houppiers. 3 % correspond à des houppiers denses",
      "(caïlcédrat, figuier, manguier) et sert de cas de référence ; 15 % teste des houppiers plus clairs",
      "(acacias, Mitragyna, Faidherbia). Si une conclusion tient pour les deux valeurs, elle est robuste ;",
      "sinon, la densité des houppiers compte autant que l'implantation des arbres.",
      "La référence « sans arbres » garde les mêmes bâtiments et la même météo, sans aucune végétation."
    ),
    htmltools::tags$h4("Graphiques"),
    htmltools::tags$ul(
      htmltools::tags$li("Le diagramme compare les quartiers par contexte (voirie, îlots, zones inondables) pour l'indicateur choisi. Les valeurs de Tmrt sont des médianes, qui ne sont pas influencées par quelques pixels extrêmes."),
      htmltools::tags$li("Le tableau donne aussi la part de surface au-dessus de 60 °C de Tmrt à 14 h, un repère indicatif de charge radiative très forte."),
      htmltools::tags$li("Le profil journalier montre l'écart entre plein soleil (rouge), ombre des bâtiments (violet) et ombre des arbres (vert). La courbe pointillée verte donne, aux mêmes emplacements, la Tmrt sans arbres : l'écart entre les deux courbes vertes est l'effet des arbres heure par heure.")
    ),
    htmltools::tags$h4("Limites"),
    htmltools::tags$ul(
      htmltools::tags$li("La Tmrt n'est pas un indice de confort : elle ne tient compte ni du vent ni de l'humidité. Les indices UTCI et PET seront ajoutés avec l'étude du vent."),
      htmltools::tags$li("La météorologie ERA5 représente le climat régional (maille d'environ 31 km), pas un microclimat mesuré sur le site ; voir ci-dessus sa comparaison aux stations."),
      htmltools::tags$li("Les dimensions et essences des arbres sont des ordres de grandeur issus de la littérature, à valider par des relevés de terrain."),
      htmltools::tags$li("Les propriétés des matériaux sont les valeurs par défaut d'UMEP ; les pavés autoblocants prévus peuvent réfléchir davantage que le revêtement par défaut."),
      htmltools::tags$li("La carte couvre tout le site avec des pixels de 5 m : un houppier occupe 1 à 3 pixels. Des zooms à 1 m sont prévus sur des secteurs choisis."),
      htmltools::tags$li("Les résultats sont surtout comparatifs (avec ou sans arbres, d'une journée ou d'un quartier à l'autre) ; aucune mesure de terrain ne les valide encore.")
    )
  )
}

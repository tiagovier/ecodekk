# Partage des résultats d'études UMEP entre scénarios.
#
# Chaque étude enregistre l'empreinte des entrées dont elle dépend
# (exports/umep/<étude>/empreinte_entrees.json). Un scénario réutilise les
# résultats d'une étude calculée pour un autre scénario si, et seulement si,
# toutes ces entrées sont identiques ; sinon l'étude est signalée « à
# recalculer » avec la liste des entrées modifiées. Les empreintes portent sur
# les géométries (arrondies au centimètre en EPSG:32628) et les seuls attributs
# utilisés par les études : les colonnes de classement de la charte, par
# exemple, ne déclenchent pas de nouvelle simulation.

umep_component_labels <- c(
  batiments = "bâtiments (emprises et hauteurs)",
  arbres = "arbres (positions, catégorie de plantation et paramètres d'attribution)",
  voirie_emprises = "emprises de voirie",
  zones_inondables = "zones inondables",
  quartiers = "quartiers",
  logements = "logements par bâtiment"
)

# Entrées réellement utilisées par chaque étude. L'occupation du sol n'entre
# dans aucun résultat numérique (seulement l'étiquette de zone des arbres) et
# les axes de voirie n'agissent que via la catégorie de plantation des arbres,
# portée par la composante « arbres » : les modifier sans changer la
# catégorie d'aucun arbre ne déclenche pas de nouvelle simulation.
umep_study_dependencies <- list(
  ombrage_arbres = c("batiments", "arbres", "voirie_emprises", "zones_inondables", "quartiers"),
  climat_urbain = c("batiments", "arbres", "voirie_emprises", "zones_inondables", "quartiers"),
  vent_confort = c("batiments", "arbres", "voirie_emprises", "zones_inondables", "quartiers"),
  bilan_energetique = c("batiments", "arbres", "voirie_emprises", "zones_inondables", "quartiers", "logements")
)

# Catégorie de plantation de chaque arbre, selon les règles de
# scripts/umep_trees (config.yaml) : exclu dans un bâtiment, « road » dans une
# emprise de voirie ou à moins de offset m du bord de chaussée (axe ± width/2),
# « flood » dans une zone inondable, sinon « block » ; rue étroite si width <
# narrow_width. L'essence et les dimensions tirées en dépendent seules (tirages
# reproductibles par tree_id).
umep_tree_contexts <- function(trees, spatial, offset_m = 3, narrow_width_m = 8) {
  trees <- sf::st_transform(trees, 32628)
  trees <- trees[order(trees$tree_id), , drop = FALSE]
  union_of <- function(layer) {
    if (is.null(layer) || !nrow(layer)) return(sf::st_sfc(crs = 32628))
    sf::st_union(sf::st_make_valid(sf::st_geometry(sf::st_transform(layer, 32628))))
  }
  inside <- function(geometry) {
    if (!length(geometry)) return(rep(FALSE, nrow(trees)))
    lengths(sf::st_intersects(trees, geometry)) > 0
  }
  buildings <- spatial$buildings
  if (!is.null(buildings) && "osm_id" %in% names(buildings)) {
    buildings <- buildings[!as.character(buildings$osm_id) %in% c("-38433", "-38434"), , drop = FALSE]
  }
  in_building <- inside(union_of(buildings))
  in_footprint <- inside(union_of(spatial$road_footprints))
  in_flood <- inside(union_of(spatial$flood_areas))
  width <- rep(NA_real_, nrow(trees))
  edge <- rep(NA_real_, nrow(trees))
  roads <- spatial$roads
  if (!is.null(roads) && nrow(roads)) {
    roads <- sf::st_transform(roads, 32628)
    nearest <- sf::st_nearest_feature(trees, roads)
    width <- suppressWarnings(as.numeric(roads$width_m))[nearest]
    edge <- as.numeric(sf::st_distance(trees, roads[nearest, ], by_element = TRUE)) - width / 2
  }
  road <- !in_building & (in_footprint | (!is.na(edge) & edge <= offset_m))
  context <- ifelse(in_building, "exclu", ifelse(road, "road", ifelse(in_flood, "flood", "block")))
  data.frame(
    tree_id = trees$tree_id,
    context = context,
    narrow = road & !is.na(width) & width < narrow_width_m,
    stringsAsFactors = FALSE
  )
}

umep_fingerprint_file <- "empreinte_entrees.json"
umep_tree_context_file <- "empreinte_arbres.csv.gz"

# Part maximale d'arbres changeant de catégorie de plantation (ou de statut de
# rue étroite) pour réutiliser les résultats d'une étude (décision
# utilisateur du 2026-10-02) ; positions et paramètres d'attribution restent
# exigés identiques.
umep_tree_tolerance <- 0.05

# Empreinte canonique d'une couche : une ligne par entité (identifiant,
# attributs retenus, coordonnées au centimètre), lignes triées.
umep_layer_fingerprint <- function(layer, attributes = character(), id = NULL) {
  if (is.null(layer) || !nrow(layer)) return(digest::digest("vide", algo = "sha256"))
  layer <- sf::st_transform(layer, 32628)
  if (all(sf::st_geometry_type(layer) == "POINT")) {
    xy <- sf::st_coordinates(layer)
    geometry <- sprintf("%.2f %.2f", xy[, "X"], xy[, "Y"])
  } else geometry <- vapply(sf::st_geometry(layer), function(shape) {
    if (sf::st_is_empty(shape)) return("")
    xy <- sf::st_coordinates(shape)
    paste(sprintf("%.2f %.2f", xy[, "X"], xy[, "Y"]), collapse = ",")
  }, character(1))
  values <- sf::st_drop_geometry(layer)
  fields <- vapply(seq_len(nrow(layer)), function(index) {
    parts <- vapply(attributes, function(name) {
      value <- values[[name]][[index]]
      if (is.numeric(value)) sprintf("%.2f", value) else as.character(value)
    }, character(1))
    paste(c(if (!is.null(id)) as.character(values[[id]][[index]]), parts), collapse = "|")
  }, character(1))
  digest::digest(paste(sort(paste(fields, geometry, sep = "#")), collapse = "\n"), algo = "sha256")
}

# Bâtiments simulés : même chaîne que la préparation UMEP (normalisation,
# hauteurs, bâtiments inclus, bâtiments atypiques écartés).
umep_simulated_buildings <- function(spatial, model, scenario_id) {
  products <- ensure_product_building_assumptions(model$products)
  products$is_cessible <- as.logical(products$is_cessible)
  buildings <- normalize_scenario_buildings(spatial$buildings, products, scenario_id)
  buildings <- calculate_osm_building_heights(
    buildings,
    ground_floor_height_m = model$height_assumptions$ground_floor_height_m,
    upper_floor_height_m = model$height_assumptions$upper_floor_height_m,
    default_levels = model$height_assumptions$default_levels,
    products = products
  )
  buildings <- buildings[buildings$included_in_simulation, , drop = FALSE]
  if ("osm_id" %in% names(buildings)) {
    buildings <- buildings[!as.character(buildings$osm_id) %in% c("-38433", "-38434"), , drop = FALSE]
  }
  list(buildings = buildings, products = products)
}

umep_housing_units <- function(buildings, products, allocations) {
  alloc <- calculate_building_level_allocations(buildings, products, allocations)
  housing <- !(alloc$product_id %in% c("ec", "ep", "el", "ev")) &
    !grepl("_com$", alloc$product_id) & !is.na(alloc$product_id)
  units <- tapply(alloc$unit_count * housing, alloc$building_id, sum)
  result <- as.numeric(units[buildings$building_id])
  result[is.na(result)] <- 0
  stats::setNames(result, buildings$building_id)
}

# Empreintes de toutes les composantes d'entrée d'un scénario.
# Entrées d'un scénario : empreintes strictes par composante, plus les
# catégories de plantation de chaque arbre et l'empreinte de base des arbres
# (positions et paramètres d'attribution) pour la tolérance.
umep_scenario_fingerprints <- function(spatial, model, scenario_id, trees,
                                       tree_config = file.path("scripts", "umep_trees", "config.yaml")) {
  simulated <- umep_simulated_buildings(spatial, model, scenario_id)
  buildings <- simulated$buildings
  units <- umep_housing_units(buildings, simulated$products, model$building_level_allocations)
  contexts <- umep_tree_contexts(trees, spatial)
  config <- if (file.exists(tree_config)) digest::digest(file = tree_config, algo = "sha256") else "absent"
  tree_base <- digest::digest(paste(umep_layer_fingerprint(trees, id = "tree_id"), config), algo = "sha256")
  fingerprints <- c(
    batiments = umep_layer_fingerprint(buildings, "height_m", id = "building_id"),
    arbres = digest::digest(paste(
      tree_base, paste(contexts$tree_id, contexts$context, contexts$narrow, collapse = "\n")
    ), algo = "sha256"),
    voirie_emprises = umep_layer_fingerprint(spatial$road_footprints),
    zones_inondables = umep_layer_fingerprint(spatial$flood_areas, "flood_type"),
    quartiers = umep_layer_fingerprint(spatial$quartiers, "district_label"),
    logements = digest::digest(paste(names(units), sprintf("%.2f", units), collapse = "\n"), algo = "sha256")
  )
  list(fingerprints = fingerprints, tree_base = tree_base, tree_contexts = contexts)
}

write_umep_study_fingerprint <- function(study_directory, study, scenario_id, inputs) {
  path <- file.path(study_directory, umep_fingerprint_file)
  if (file.exists(path)) stop("Empreinte existante, non écrasée : ", path)
  components <- umep_study_dependencies[[study]]
  if (is.null(components)) stop("Étude inconnue : ", study)
  record <- list(
    study = study,
    computed_for = scenario_id,
    recorded_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    components = as.list(inputs$fingerprints[components]),
    tree_base = inputs$tree_base
  )
  jsonlite::write_json(record, path, auto_unbox = TRUE, pretty = TRUE)
  connection <- gzfile(file.path(study_directory, umep_tree_context_file), "w")
  on.exit(close(connection), add = TRUE)
  utils::write.csv(inputs$tree_contexts, connection, row.names = FALSE)
  invisible(path)
}

# Toutes les études enregistrées sous data/scenarios/*/exports/umep/.
umep_study_records <- function(scenario_root) {
  paths <- Sys.glob(file.path(scenario_root, "*", "exports", "umep", "*", umep_fingerprint_file))
  lapply(paths, function(path) {
    record <- jsonlite::fromJSON(path, simplifyVector = TRUE)
    record$directory <- dirname(path)
    record
  })
}

# Part des arbres dont la catégorie de plantation diffère de l'étude.
umep_tree_change_share <- function(record, inputs) {
  if (!identical(record$tree_base, inputs$tree_base)) return(1)
  path <- file.path(record$directory, umep_tree_context_file)
  if (!file.exists(path)) return(1)
  stored <- utils::read.csv(path, stringsAsFactors = FALSE)
  current <- inputs$tree_contexts
  matched <- match(current$tree_id, stored$tree_id)
  if (anyNA(matched) || nrow(stored) != nrow(current)) return(1)
  mean(stored$context[matched] != current$context | as.logical(stored$narrow[matched]) != current$narrow)
}

# Résultats utilisables par un scénario pour une étude : ceux du scénario
# lui-même, ou d'un autre scénario aux entrées identiques (arbres : tolérance
# `umep_tree_tolerance`). Sinon, pas de répertoire et les composantes
# modifiées par rapport à l'étude la plus proche.
resolve_umep_study <- function(study, scenario_id, inputs, records, tolerance = umep_tree_tolerance) {
  if (is.null(inputs)) return(list(directory = NULL, source = NULL, changed = NULL, tree_change_share = NA_real_))
  components <- umep_study_dependencies[[study]]
  candidates <- Filter(function(record) identical(record$study, study), records)
  empty <- list(directory = NULL, source = NULL, changed = NULL, tree_change_share = NA_real_)
  if (!length(candidates)) return(empty)
  own <- vapply(candidates, function(record) identical(record$computed_for, scenario_id), logical(1))
  candidates <- c(candidates[own], candidates[!own])
  assessed <- lapply(candidates, function(record) {
    stored <- unlist(record$components)[components]
    changed <- components[is.na(stored) | stored != inputs$fingerprints[components]]
    share <- 0
    if ("arbres" %in% changed) {
      share <- umep_tree_change_share(record, inputs)
      if (share < tolerance) changed <- setdiff(changed, "arbres")
    }
    list(changed = changed, share = share)
  })
  ok <- which(vapply(assessed, function(item) !length(item$changed), logical(1)))
  if (length(ok)) {
    record <- candidates[[ok[1]]]
    return(list(directory = record$directory, source = record$computed_for, changed = character(),
                tree_change_share = assessed[[ok[1]]]$share))
  }
  # Étude propre au scénario mais périmée : ses résultats restent affichables,
  # signalés comme à recalculer ; on n'emprunte jamais ceux d'un autre scénario.
  nearest <- if (any(own)) 1L else which.min(vapply(assessed, function(item) length(item$changed), integer(1)))
  list(directory = NULL, source = candidates[[nearest]]$computed_for,
       changed = assessed[[nearest]]$changed, tree_change_share = assessed[[nearest]]$share,
       stale_directory = if (any(own)) candidates[[1]]$directory else NULL)
}

# Message affiché dans les onglets selon la résolution.
umep_sharing_message <- function(resolution, scenario_id) {
  share <- resolution$tree_change_share
  tree_note <- if (!is.na(share) && share > 0 && share < 1) {
    paste0(" ", format_number_fr(100 * share, 1), " % des arbres changent de catégorie de plantation")
  } else ""
  if (!is.null(resolution$directory)) {
    if (identical(resolution$source, scenario_id) && !nzchar(tree_note)) return(NULL)
    if (identical(resolution$source, scenario_id)) {
      return(paste0("Résultats de ce scénario ; depuis le calcul,", tree_note,
                    " (sous la tolérance de ", format_number_fr(100 * umep_tree_tolerance, 0), " %)."))
    }
    return(paste0(
      "Résultats calculés pour le scénario ", resolution$source, " et réutilisés : les entrées de cette étude sont identiques",
      if (nzchar(tree_note)) paste0(", sauf", tree_note, " (sous la tolérance de ",
                                    format_number_fr(100 * umep_tree_tolerance, 0), " %)") else "",
      "."
    ))
  }
  if (is.null(resolution$source)) return(NULL)
  labels <- umep_component_labels[resolution$changed]
  if ("arbres" %in% resolution$changed && nzchar(tree_note)) {
    labels[resolution$changed == "arbres"] <- paste0("arbres :", tree_note, ", au-delà de la tolérance")
  }
  if (identical(resolution$source, scenario_id)) {
    return(paste0(
      "Résultats périmés, nouvelle simulation nécessaire : entrées modifiées depuis le calcul (",
      paste(labels, collapse = ", "), "). Les résultats affichés correspondent à l'état précédent du scénario."
    ))
  }
  paste0(
    "Nouvelle simulation nécessaire pour le scénario ", scenario_id, " : entrées modifiées par rapport au scénario ",
    resolution$source, " (", paste(labels, collapse = ", "), ")."
  )
}

# Dossier de résultats à afficher pour une résolution (y compris les
# résultats propres périmés), ou NULL.
umep_resolved_directory <- function(resolution) {
  resolution$directory %||% resolution$stale_directory
}

umep_study_display_directory <- function(resolution, subdirectory = "display") {
  directory <- umep_resolved_directory(resolution)
  if (is.null(directory)) return(NULL)
  display <- file.path(directory, subdirectory)
  if (file.exists(file.path(display, "manifest.json"))) display else NULL
}

umep_study_tree_layer <- function(resolution) {
  directory <- umep_resolved_directory(resolution)
  if (is.null(directory)) return(NULL)
  files <- list.files(directory, pattern = "^trees_thies_.+\\.gpkg$", full.names = TRUE)
  files <- files[!grepl("_saison_pluies\\.gpkg$", files)]
  if (!length(files)) return(NULL)
  files[order(file.mtime(files), decreasing = TRUE)][[1]]
}

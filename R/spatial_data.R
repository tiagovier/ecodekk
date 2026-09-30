load_parcel_boundaries <- function(gpkg_path) {
  if (!file.exists(gpkg_path)) {
    stop("Le GeoPackage des parcelles est introuvable.")
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Le package R 'sf' est requis pour charger les parcelles.")
  }

  linearized_file <- tempfile(fileext = ".geojson")
  on.exit(unlink(linearized_file), add = TRUE)
  parcel_query <- paste(
    "SELECT fid, layer, geom FROM polylines",
    "WHERE layer GLOB 'par_*' OR layer GLOB 'Parcelles *'"
  )

  sf::gdal_utils(
    util = "vectortranslate",
    source = gpkg_path,
    destination = linearized_file,
    options = c(
      "-f", "GeoJSON", "-dialect", "SQLite", "-sql", parcel_query,
      "-nlt", "CONVERT_TO_LINEAR", "-dim", "XY"
    ),
    quiet = TRUE
  )

  parcels <- sf::st_read(linearized_file, quiet = TRUE)
  if (nrow(parcels) == 0) {
    stop("Aucune limite parcellaire n'a été trouvée dans le GeoPackage.")
  }

  geometry <- sf::st_geometry(parcels) / 1000
  sf::st_crs(geometry) <- 32628
  parcels <- sf::st_transform(sf::st_set_geometry(parcels, geometry), 4326)
  parcels$parcel_type <- ifelse(
    grepl("^par_", parcels$layer),
    "Parcelles résidentielles",
    parcels$layer
  )
  parcels
}

osm_way_tags <- function(way) {
  tags <- xml2::xml_find_all(way, "./tag")
  stats::setNames(xml2::xml_attr(tags, "v"), xml2::xml_attr(tags, "k"))
}

osm_tag <- function(tags, key, default = NA_character_) {
  if (key %in% names(tags)) unname(tags[[key]]) else default
}

building_function_label <- function(building_function) {
  labels <- c(
    residential = "Résidentiel",
    apartments = "Immeuble collectif",
    office = "Bureaux",
    mixed_use = "Usage mixte",
    mosque = "Mosquée",
    school = "École",
    clinic = "Clinique",
    civi = "Équipement public",
    civic = "Équipement public",
    hospital = "Hôpital",
    hotel = "Hôtel",
    retail = "Commerce",
    transportation = "Transport"
  )
  translated <- unname(labels[building_function])
  translated[is.na(translated)] <- building_function[is.na(translated)]
  translated
}

road_class_label <- function(highway) {
  labels <- c(
    trunk = "Route structurante",
    primary = "Voie primaire",
    secondary = "Voie secondaire",
    tertiary = "Voie tertiaire",
    residential = "Voie résidentielle",
    service = "Voie de service",
    pedestrian = "Voie piétonne"
  )
  translated <- unname(labels[highway])
  translated[is.na(translated)] <- highway[is.na(translated)]
  translated
}

load_osm_urban_data <- function(osm_path) {
  if (!file.exists(osm_path)) {
    stop("Le fichier OSM urbain est introuvable.")
  }
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("Le package R 'xml2' est requis pour lire le fichier OSM.")
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Le package R 'sf' est requis pour préparer les données OSM.")
  }

  document <- xml2::read_xml(osm_path)
  nodes <- xml2::xml_find_all(document, ".//node")
  node_ids <- xml2::xml_attr(nodes, "id")
  node_longitudes <- as.numeric(xml2::xml_attr(nodes, "lon"))
  node_latitudes <- as.numeric(xml2::xml_attr(nodes, "lat"))
  ways <- xml2::xml_find_all(document, ".//way")

  building_records <- list()
  building_geometries <- list()
  road_records <- list()
  road_geometries <- list()

  for (way in ways) {
    tags <- osm_way_tags(way)
    is_building <- "building" %in% names(tags)
    is_road <- "highway" %in% names(tags)
    if (!is_building && !is_road) next

    references <- xml2::xml_attr(xml2::xml_find_all(way, "./nd"), "ref")
    node_positions <- match(references, node_ids)
    if (length(node_positions) < 2 || anyNA(node_positions)) next
    coordinates <- cbind(
      node_longitudes[node_positions],
      node_latitudes[node_positions]
    )
    way_id <- xml2::xml_attr(way, "id")

    if (is_building && nrow(coordinates) >= 4) {
      if (!all(coordinates[1, ] == coordinates[nrow(coordinates), ])) {
        coordinates <- rbind(coordinates, coordinates[1, ])
      }
      building_geometries[[length(building_geometries) + 1L]] <- sf::st_polygon(list(coordinates))
      raw_levels <- osm_tag(tags, "levels")
      building_records[[length(building_records) + 1L]] <- data.frame(
        osm_id = way_id,
        building_function = osm_tag(tags, "building", "non renseigné"),
        amenity = osm_tag(tags, "amenity"),
        office = osm_tag(tags, "office"),
        shop = osm_tag(tags, "shop"),
        name = osm_tag(tags, "name"),
        levels_source = raw_levels,
        levels = suppressWarnings(as.numeric(raw_levels)),
        stringsAsFactors = FALSE
      )
    }

    if (is_road && nrow(coordinates) >= 2) {
      road_geometries[[length(road_geometries) + 1L]] <- sf::st_linestring(coordinates)
      road_records[[length(road_records) + 1L]] <- data.frame(
        osm_id = way_id,
        highway = osm_tag(tags, "highway", "non renseigné"),
        width_m = suppressWarnings(as.numeric(osm_tag(tags, "width"))),
        lanes = suppressWarnings(as.numeric(osm_tag(tags, "lanes"))),
        maxspeed_kmh = suppressWarnings(as.numeric(osm_tag(tags, "maxspeed"))),
        surface = osm_tag(tags, "surface"),
        name = osm_tag(tags, "name"),
        stringsAsFactors = FALSE
      )
    }
  }

  buildings <- sf::st_sf(
    do.call(rbind, building_records),
    geometry = sf::st_sfc(building_geometries, crs = 4326)
  )
  roads <- sf::st_sf(
    do.call(rbind, road_records),
    geometry = sf::st_sfc(road_geometries, crs = 4326)
  )
  buildings$function_label <- building_function_label(buildings$building_function)
  roads$road_label <- road_class_label(roads$highway)
  excluded_ids <- c("-38433", "-38434")
  excluded_buildings <- buildings[buildings$osm_id %in% excluded_ids, , drop = FALSE]
  buildings <- buildings[!buildings$osm_id %in% excluded_ids, , drop = FALSE]

  list(
    buildings = buildings,
    roads = roads,
    excluded_buildings = excluded_buildings
  )
}


read_project_spatial_layer <- function(path, layer = NULL, assumed_crs = NULL) {
  if (!file.exists(path)) {
    stop(paste0("La couche SIG est introuvable : ", basename(path), "."))
  }

  features <- if (is.null(layer)) {
    sf::st_read(path, quiet = TRUE)
  } else {
    sf::st_read(path, layer = layer, quiet = TRUE)
  }
  if (is.na(sf::st_crs(features)) || (!is.null(assumed_crs) && is.na(sf::st_crs(features)$epsg))) {
    if (is.null(assumed_crs)) {
      stop(paste0("Le système de coordonnées est absent : ", basename(path), "."))
    }
    suppressWarnings(sf::st_crs(features) <- assumed_crs)
  }

  sf::st_transform(sf::st_zm(features, drop = TRUE, what = "ZM"), 4326)
}

load_corrected_buildings <- function(gpkg_path) {
  raw <- read_project_spatial_layer(
    gpkg_path,
    layer = "entities",
    assumed_crs = 32628
  )
  raw <- sf::st_transform(raw, 32628)
  raw <- raw[raw$Layer != "VOIRIE-PIETONNE", , drop = FALSE]

  records <- list()
  geometries <- list()
  output_index <- 0L
  for (source_index in seq_len(nrow(raw))) {
    polygonized <- suppressWarnings(
      sf::st_collection_extract(
        sf::st_polygonize(sf::st_geometry(raw[source_index, ])),
        "POLYGON"
      )
    )
    if (length(polygonized) == 0) next

    for (part_index in seq_along(polygonized)) {
      output_index <- output_index + 1L
      records[[output_index]] <- data.frame(
        building_id = paste0(
          "bat_", tolower(raw$EntityHandle[source_index]), "_", part_index
        ),
        source_layer = raw$Layer[source_index],
        source_handle = raw$EntityHandle[source_index],
        source_part = part_index,
        stringsAsFactors = FALSE
      )
      geometries[[output_index]] <- polygonized[[part_index]]
    }
  }

  buildings <- sf::st_sf(
    do.call(rbind, records),
    geometry = sf::st_sfc(geometries, crs = 32628)
  )
  sf::st_transform(buildings, 4326)
}

consolidate_buildings <- function(
  corrected_buildings,
  osm_buildings,
  maximum_match_distance_m = 30,
  preserve_unrepresented_osm = TRUE
) {
  if (!inherits(corrected_buildings, "sf") || !inherits(osm_buildings, "sf")) {
    stop("Les bâtiments corrigés et OSM doivent être des objets sf.")
  }
  if (is.na(maximum_match_distance_m) || maximum_match_distance_m <= 0) {
    stop("La distance maximale d’appariement doit être strictement positive.")
  }

  corrected_metric <- sf::st_transform(corrected_buildings, 32628)
  osm_metric <- sf::st_transform(osm_buildings, 32628)
  corrected_points <- suppressWarnings(sf::st_point_on_surface(corrected_metric))
  osm_points <- suppressWarnings(sf::st_point_on_surface(osm_metric))
  nearest <- sf::st_nearest_feature(corrected_points, osm_points)
  distance_m <- as.numeric(sf::st_distance(
    corrected_points,
    osm_points[nearest, ],
    by_element = TRUE
  ))
  matched <- distance_m <= maximum_match_distance_m

  attribute_names <- c(
    "osm_id", "building_function", "amenity", "office", "shop", "name",
    "levels_source", "levels"
  )
  attributes <- sf::st_drop_geometry(osm_buildings[nearest, attribute_names])
  for (field in attribute_names) attributes[[field]][!matched] <- NA
  attributes$building_function[!matched] <- "non renseigné"

  result <- cbind(corrected_buildings, attributes)
  result$function_label <- building_function_label(result$building_function)
  result$attribute_match_distance_m <- round(distance_m, 1)
  result$attribute_match_quality <- ifelse(
    !matched,
    "non apparié",
    ifelse(
      distance_m <= 5,
      "forte",
      ifelse(distance_m <= 15, "moyenne", "faible")
    )
  )
  result$geometry_source <- "Empreinte corrigée"

  if (isTRUE(preserve_unrepresented_osm)) {
    represented <- lengths(sf::st_is_within_distance(
      osm_points,
      corrected_points,
      dist = maximum_match_distance_m
    )) > 0
    osm_only <- osm_buildings[!represented, , drop = FALSE]
    if (nrow(osm_only) > 0) {
      osm_only$building_id <- paste0("osm_", gsub("^-", "", osm_only$osm_id))
      osm_only$source_layer <- "OSM"
      osm_only$source_handle <- NA_character_
      osm_only$source_part <- NA_integer_
      osm_only$attribute_match_distance_m <- 0
      osm_only$attribute_match_quality <- "source OSM"
      osm_only$geometry_source <- "OSM conservé"
      osm_only <- osm_only[, names(result)]
      result <- rbind(result, osm_only)
    }
  }

  rownames(result) <- NULL
  result
}


load_project_context_layers <- function(data_directory) {
  quartiers <- read_project_spatial_layer(
    file.path(data_directory, "quartiers.gpkg"), "quartiers_synt"
  )
  quartiers$district_code <- as.character(quartiers$code)
  quartiers$district_label <- ifelse(
    is.na(quartiers$descr) | !nzchar(quartiers$descr),
    paste("Quartier", quartiers$district_code),
    quartiers$descr
  )

  land_use <- read_project_spatial_layer(
    file.path(data_directory, "landuse.gpkg"), "projet270626_ep"
  )
  land_use$land_use_label <- ifelse(
    is.na(land_use$Layer) | !nzchar(land_use$Layer),
    land_use$zone,
    land_use$Layer
  )

  road_footprints <- read_project_spatial_layer(
    file.path(data_directory, "emprises_voirie.gpkg"), "emprise_voirie_copy"
  )
  road_footprints$road_footprint_label <- ifelse(
    is.na(road_footprints$Descr) | !nzchar(road_footprints$Descr),
    "Emprise de voirie",
    road_footprints$Descr
  )

  prepare_flood_layer <- function(path, layer, flood_type) {
    source <- read_project_spatial_layer(path, layer)
    basin <- if ("BV" %in% names(source)) as.character(source$BV) else NA_character_
    surface <- if ("Surface" %in% names(source)) {
      as.numeric(source$Surface)
    } else if ("area" %in% names(source)) {
      as.numeric(source$area)
    } else {
      NA_real_
    }
    sf::st_sf(
      flood_type = flood_type,
      basin = basin,
      surface_sqm = surface,
      geometry = sf::st_geometry(source)
    )
  }
  flood_areas <- rbind(
    prepare_flood_layer(
      file.path(data_directory, "inond_litmineur.gpkg"),
      "lit_mineur",
      "Lit mineur"
    ),
    prepare_flood_layer(
      file.path(data_directory, "bassins_ret50.gpkg"),
      "ret_50cm",
      "Rétention 50 cm"
    ),
    prepare_flood_layer(
      file.path(data_directory, "bassins_cuvettes_ret50.gpkg"),
      "emprise_cuvettes",
      "Cuvette de rétention"
    )
  )

  list(
    quartiers = quartiers,
    land_use = land_use,
    road_footprints = road_footprints,
    flood_areas = flood_areas,
    project_boundary = read_project_spatial_layer(
      file.path(data_directory, "emprise_projet.gpkg"), "emprise_projet"
    ),
    title_boundary = read_project_spatial_layer(
      file.path(data_directory, "emprise_titrefoncier.gpkg"), "emprise_tf"
    )
  )
}


land_use_product_id <- function(zone) {
  mapping <- c(
    "RM 2" = "rm_2",
    "RC 1" = "rc_1",
    "RV 1" = "rv_1",
    "RV 2" = "rv_2",
    "RV 3" = "rv_3",
    "RT 1" = "rt_1_log",
    "EC" = "ec",
    "EP" = "ep",
    "EL" = "el",
    "EV" = "ev"
  )
  result <- unname(mapping[as.character(zone)])
  result[is.na(result)] <- NA_character_
  result
}

ensure_product_building_assumptions <- function(products) {
  defaults <- initial_products()[, c(
    "product_id", "default_building_levels", "ground_floor_commercial",
    "ground_floor_product_id", "uses_units_per_level", "units_per_level"
  )]
  index <- match(products$product_id, defaults$product_id)
  if (!"default_building_levels" %in% names(products)) {
    products$default_building_levels <- defaults$default_building_levels[index]
  }
  if (!"ground_floor_commercial" %in% names(products)) {
    products$ground_floor_commercial <- defaults$ground_floor_commercial[index]
  }
  if (!"ground_floor_product_id" %in% names(products)) {
    products$ground_floor_product_id <- defaults$ground_floor_product_id[index]
  }
  if (!"uses_units_per_level" %in% names(products)) {
    products$uses_units_per_level <- defaults$uses_units_per_level[index]
  }
  if (!"units_per_level" %in% names(products)) {
    products$units_per_level <- defaults$units_per_level[index]
  }
  products$default_building_levels <- suppressWarnings(
    as.numeric(products$default_building_levels)
  )
  products$uses_units_per_level <- as.logical(products$uses_units_per_level)
  products$uses_units_per_level[is.na(products$uses_units_per_level)] <- FALSE
  products$units_per_level <- suppressWarnings(as.numeric(products$units_per_level))
  invalid_units <- is.na(products$units_per_level) | products$units_per_level < 1 |
    abs(products$units_per_level - round(products$units_per_level)) > 1e-8
  if (any(invalid_units)) stop("Les unités par niveau doivent être des entiers strictement positifs.")
  products$units_per_level <- as.integer(round(products$units_per_level))
  products$ground_floor_commercial <- as.logical(products$ground_floor_commercial)
  products$ground_floor_commercial[is.na(products$ground_floor_commercial)] <- FALSE
  products$ground_floor_product_id <- as.character(products$ground_floor_product_id)
  products$ground_floor_product_id[
    is.na(products$ground_floor_product_id) |
      !nzchar(products$ground_floor_product_id)
  ] <- NA_character_
  products
}

normalize_scenario_buildings <- function(buildings, products, scenario_id) {
  if (!inherits(buildings, "sf")) stop("La couche des bâtiments doit être spatiale.")
  products <- ensure_product_building_assumptions(products)
  row_count <- nrow(buildings)
  character_defaults <- c(
    building_id = NA_character_, building_function = "non renseigné",
    function_label = NA_character_, product_id = NA_character_,
    product_link_source = NA_character_, land_use_zone = NA_character_,
    district_id = NA_character_, geometry_source = "QGIS",
    attribute_match_quality = "édition QGIS"
  )
  for (field in names(character_defaults)) {
    if (!field %in% names(buildings)) {
      buildings[[field]] <- rep(unname(character_defaults[field]), row_count)
    }
    buildings[[field]] <- as.character(buildings[[field]])
  }
  if (!"levels" %in% names(buildings)) buildings$levels <- NA_real_
  buildings$levels <- suppressWarnings(as.numeric(buildings$levels))
  if (!"units_per_level" %in% names(buildings)) buildings$units_per_level <- NA_real_
  buildings$units_per_level <- suppressWarnings(as.numeric(buildings$units_per_level))
  product_units <- products$units_per_level[
    match(buildings$product_id, products$product_id)
  ]
  missing_units <- is.na(buildings$units_per_level) | buildings$units_per_level <= 0
  buildings$units_per_level[missing_units] <- product_units[missing_units]
  if (!"footprint_offset_sqm" %in% names(buildings)) {
    buildings$footprint_offset_sqm <- 0
  }
  buildings$footprint_offset_sqm <- suppressWarnings(as.numeric(buildings$footprint_offset_sqm))
  buildings$footprint_offset_sqm[is.na(buildings$footprint_offset_sqm)] <- 0
  if (!"levels_source" %in% names(buildings)) buildings$levels_source <- NA_character_
  if (!"included_in_simulation" %in% names(buildings)) {
    buildings$included_in_simulation <- TRUE
  }
  buildings$included_in_simulation <- as.logical(buildings$included_in_simulation)
  buildings$included_in_simulation[is.na(buildings$included_in_simulation)] <- TRUE
  if (!"control_edited" %in% names(buildings)) buildings$control_edited <- FALSE
  buildings$control_edited <- as.logical(buildings$control_edited)
  buildings$control_edited[is.na(buildings$control_edited)] <- FALSE

  original_product <- buildings$product_id
  aliases <- c(rc_2 = "rc_2_log")
  alias_index <- match(buildings$product_id, names(aliases))
  alias_applied <- !is.na(alias_index)
  buildings$product_id[alias_applied] <- unname(aliases[alias_index[alias_applied]])
  buildings$product_alias_applied <- alias_applied
  buildings$product_id_original <- ifelse(alias_applied, original_product, NA_character_)
  product_units <- products$units_per_level[
    match(buildings$product_id, products$product_id)
  ]
  missing_units <- is.na(buildings$units_per_level) | buildings$units_per_level <= 0
  buildings$units_per_level[missing_units] <- product_units[missing_units]

  assigned <- !is.na(buildings$product_id) & nzchar(buildings$product_id)
  missing_source <- is.na(buildings$product_link_source) |
    !nzchar(buildings$product_link_source)
  buildings$product_link_source[assigned & missing_source] <- "QGIS"
  buildings$product_link_source[!assigned & missing_source] <- "non renseigné"

  missing_function <- is.na(buildings$building_function) |
    !nzchar(buildings$building_function)
  buildings$building_function[missing_function] <- "non renseigné"
  product_label <- products$product_label[match(buildings$product_id, products$product_id)]
  missing_label <- is.na(buildings$function_label) | !nzchar(buildings$function_label)
  buildings$function_label[missing_label] <- ifelse(
    is.na(product_label[missing_label]),
    building_function_label(buildings$building_function[missing_label]),
    product_label[missing_label]
  )
  buildings$function_label[
    is.na(buildings$function_label) | !nzchar(buildings$function_label)
  ] <- "Fonction non renseignée"

  building_id <- trimws(buildings$building_id)
  regenerate <- is.na(building_id) | !nzchar(building_id) | duplicated(building_id)
  existing <- unique(building_id[!regenerate])
  prefix <- paste0("gis_", normalize_scenario_id(scenario_id), "_")
  next_number <- 1L
  for (index in which(regenerate)) {
    repeat {
      candidate <- paste0(prefix, sprintf("%05d", next_number))
      next_number <- next_number + 1L
      if (!candidate %in% existing) break
    }
    building_id[index] <- candidate
    existing <- c(existing, candidate)
  }
  buildings$building_id <- building_id
  buildings$id_generated <- regenerate
  buildings
}

largest_overlap_index <- function(features, zones, preferred = NULL) {
  feature_points <- suppressWarnings(sf::st_point_on_surface(
    sf::st_transform(features, 32628)
  ))
  zone_metric <- sf::st_transform(zones, 32628)
  candidates <- sf::st_intersects(feature_points, zone_metric)
  zone_areas <- as.numeric(sf::st_area(zone_metric))
  selected <- rep(NA_integer_, nrow(features))

  for (feature_index in seq_len(nrow(features))) {
    zone_indices <- candidates[[feature_index]]
    if (length(zone_indices) == 0) next
    if (!is.null(preferred)) {
      preferred_indices <- zone_indices[preferred[zone_indices]]
      if (length(preferred_indices) > 0) zone_indices <- preferred_indices
    }
    selected[feature_index] <- zone_indices[which.min(zone_areas[zone_indices])]
  }
  selected
}

assign_building_districts <- function(buildings, quartiers) {
  required <- c("district_code", "district_label")
  missing <- setdiff(required, names(quartiers))
  if (length(missing)) {
    stop("Colonnes manquantes dans les quartiers : ", paste(missing, collapse = ", "), ".")
  }
  district_index <- largest_overlap_index(buildings, quartiers)
  matched <- !is.na(district_index)
  district_mapping <- c(
    "1" = "q1", "2" = "q2", "3" = "q3", "4" = "q4",
    "5" = "q5", "6" = "q6", "A" = "qa"
  )
  buildings$district_code[matched] <- quartiers$district_code[district_index[matched]]
  buildings$district_id[matched] <- unname(
    district_mapping[as.character(buildings$district_code[matched])]
  )
  buildings$district_label_spatial[matched] <-
    quartiers$district_label[district_index[matched]]
  buildings
}

add_building_classification_status <- function(buildings, products = NULL) {
  required <- c(
    "product_id", "product_link_source", "land_use_zone",
    "included_in_simulation"
  )
  missing <- setdiff(required, names(buildings))
  if (length(missing)) {
    stop("Colonnes manquantes pour le statut de classification : ", paste(missing, collapse = ", "), ".")
  }
  if (!"levels" %in% names(buildings)) buildings$levels <- NA_real_
  buildings$levels <- suppressWarnings(as.numeric(buildings$levels))
  ambiguous_zones <- c("RM 1", "RC 2, 3 et 4", "RC 3 et 4")
  assigned <- !is.na(buildings$product_id) & nzchar(buildings$product_id)
  known_product <- assigned
  if (!is.null(products)) {
    known_product <- assigned & buildings$product_id %in% products$product_id
  }
  ambiguous <- !assigned & !is.na(buildings$land_use_zone) & buildings$land_use_zone %in% ambiguous_zones
  buildings$classification_is_ambiguous <- ambiguous
  buildings$classification_status <- ifelse(
    assigned & buildings$product_link_source == "manuel",
    "Affecté manuellement",
    ifelse(
      assigned,
      "Affecté automatiquement",
      ifelse(ambiguous, "Usage ambigu — produit à choisir", "Produit à affecter — hors nomenclature")
    )
  )

  expected_product <- land_use_product_id(buildings$land_use_zone)
  buildings$needs_classification <- !known_product
  buildings$land_use_incongruent <- known_product & !is.na(expected_product) &
    buildings$product_id != expected_product
  buildings$needs_reclassification <- FALSE
  buildings$needs_inclusion <- !buildings$included_in_simulation
  buildings$levels_incongruent <- !is.na(buildings$levels) &
    (buildings$levels <= 0 | abs(buildings$levels - round(buildings$levels)) > 1e-8)
  buildings$geometry_incongruent <- if (inherits(buildings, "sf")) {
    sf::st_is_empty(buildings) | !sf::st_is_valid(buildings)
  } else {
    rep(FALSE, nrow(buildings))
  }
  buildings$control_required <- buildings$needs_classification |
    buildings$levels_incongruent | buildings$geometry_incongruent
  buildings$has_incongruity <- buildings$land_use_incongruent |
    buildings$needs_inclusion |
    if ("id_generated" %in% names(buildings)) buildings$id_generated else FALSE |
    if ("product_alias_applied" %in% names(buildings)) buildings$product_alias_applied else FALSE
  buildings$control_action <- vapply(seq_len(nrow(buildings)), function(index) {
    actions <- character()
    if (buildings$needs_classification[index]) actions <- c(actions, "À classifier")
    if (buildings$levels_incongruent[index]) actions <- c(actions, "Niveaux invalides")
    if (buildings$geometry_incongruent[index]) actions <- c(actions, "Géométrie invalide")
    if (!length(actions)) "Contrôle terminé" else paste(actions, collapse = " / ")
  }, character(1))
  buildings$incongruity_flag <- vapply(seq_len(nrow(buildings)), function(index) {
    flags <- character()
    if (buildings$land_use_incongruent[index]) {
      flags <- c(flags, "Produit différent de l’occupation du sol")
    }
    if (buildings$needs_inclusion[index]) flags <- c(flags, "Exclu dans QGIS")
    if ("id_generated" %in% names(buildings) && buildings$id_generated[index]) {
      flags <- c(flags, "Identifiant généré au chargement")
    }
    if ("product_alias_applied" %in% names(buildings) && buildings$product_alias_applied[index]) {
      flags <- c(flags, "Code produit normalisé")
    }
    if (!length(flags)) "" else paste(flags, collapse = " ; ")
  }, character(1))
  buildings
}

read_building_control_overrides <- function(path) {
  if (!file.exists(path)) return(data.frame())
  utils::read.csv(
    path, stringsAsFactors = FALSE, check.names = FALSE,
    na.strings = c("", "NA")
  )
}

apply_building_control_overrides <- function(buildings, overrides) {
  if (is.null(overrides) || nrow(overrides) == 0) return(buildings)
  required <- c("building_id", "building_function", "levels", "district_id",
                "land_use_zone", "product_id", "included_in_simulation")
  missing <- setdiff(required, names(overrides))
  if (length(missing)) {
    stop("Colonnes manquantes dans les corrections de bâtiments : ", paste(missing, collapse = ", "), ".")
  }
  override_index <- match(buildings$building_id, overrides$building_id)
  matched <- !is.na(override_index)
  if (!any(matched)) return(buildings)
  source_rows <- override_index[matched]
  for (field in c("building_function", "levels", "district_id", "land_use_zone",
                  "product_id", "included_in_simulation")) {
    buildings[[field]][matched] <- overrides[[field]][source_rows]
  }
  buildings$function_label[matched] <- building_function_label(buildings$building_function[matched])
  buildings$levels_source[matched] <- ifelse(
    is.na(buildings$levels[matched]), NA_character_, as.character(buildings$levels[matched])
  )
  buildings$land_use_label_spatial[matched] <- buildings$land_use_zone[matched]
  buildings$product_link_source[matched] <- ifelse(
    is.na(buildings$product_id[matched]), "non renseigné", "manuel"
  )
  buildings$control_edited[matched] <- TRUE
  add_building_classification_status(buildings)
}

write_building_control_overrides <- function(buildings, path) {
  fields <- c("building_id", "building_function", "levels", "district_id",
              "land_use_zone", "product_id", "included_in_simulation")
  edited <- !is.na(buildings$control_edited) & buildings$control_edited
  output <- sf::st_drop_geometry(buildings[edited, fields, drop = FALSE])
  directory <- dirname(path)
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)
  temporary <- tempfile("building-control-", tmpdir = directory, fileext = ".csv")
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  utils::write.csv(output, temporary, row.names = FALSE, na = "")
  if (!file.rename(temporary, path)) {
    stop("Impossible d’enregistrer les corrections de bâtiments.")
  }
  invisible(path)
}

normalize_scenario_id <- function(scenario_id) {
  scenario_id <- trimws(as.character(scenario_id)[1])
  if (is.na(scenario_id) || !nzchar(scenario_id)) scenario_id <- "base"
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9_-]*$", scenario_id)) {
    stop(
      "L’identifiant du scénario doit commencer par une lettre ou un chiffre et ne contenir que des lettres, chiffres, tirets ou tirets bas."
    )
  }
  scenario_id
}

scenario_parameter_table <- function(parameters) {
  data.frame(
    parameter_id = names(parameters),
    value = as.numeric(unlist(parameters, use.names = FALSE)),
    stringsAsFactors = FALSE
  )
}

scenario_parameters_from_table <- function(table, defaults) {
  if (is.null(table) || !nrow(table)) return(defaults)
  required <- c("parameter_id", "value")
  if (!all(required %in% names(table))) {
    stop("La table de paramètres du scénario est incomplète.")
  }
  result <- defaults
  matched <- intersect(names(result), as.character(table$parameter_id))
  for (parameter_id in matched) {
    value <- table$value[match(parameter_id, table$parameter_id)]
    if (!is.na(value)) result[[parameter_id]] <- as.numeric(value)
  }
  result
}

scenario_spatial_layers <- function(buildings, roads, parcels, context) {
  list(
    buildings = buildings,
    roads = roads,
    parcels = parcels,
    quartiers = context$quartiers,
    land_use = context$land_use,
    road_footprints = context$road_footprints,
    flood_areas = context$flood_areas,
    project_boundary = context$project_boundary,
    title_boundary = context$title_boundary
  )
}

write_scenario_geopackage <- function(
  path,
  scenario_id,
  buildings,
  roads,
  parcels,
  context,
  tables = list(),
  sources = NULL,
  reference_building_count = NULL
) {
  if (!requireNamespace("sf", quietly = TRUE) ||
      !requireNamespace("DBI", quietly = TRUE) ||
      !requireNamespace("RSQLite", quietly = TRUE)) {
    stop("Les packages 'sf', 'DBI' et 'RSQLite' sont requis pour enregistrer un scénario.")
  }
  scenario_id <- normalize_scenario_id(scenario_id)
  if (!inherits(buildings, "sf")) stop("La couche des bâtiments doit être un objet sf.")
  input_building_count <- nrow(buildings)
  if (is.null(reference_building_count)) reference_building_count <- input_building_count
  if (reference_building_count < input_building_count) {
    stop("Le nombre de bâtiments de référence ne peut pas être inférieur au scénario.")
  }
  if ("included_in_simulation" %in% names(buildings)) {
    included <- !is.na(buildings$included_in_simulation) &
      as.logical(buildings$included_in_simulation)
    buildings <- buildings[included, , drop = FALSE]
  }
  if (nrow(buildings) == 0) {
    stop("Le scénario doit conserver au moins un bâtiment inclus.")
  }

  layers <- scenario_spatial_layers(buildings, roads, parcels, context)
  invalid_layers <- names(layers)[!vapply(layers, inherits, logical(1), what = "sf")]
  if (length(invalid_layers)) {
    stop("Couches SIG invalides : ", paste(invalid_layers, collapse = ", "), ".")
  }

  directory <- dirname(path)
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)
  temporary <- tempfile("scenario-", tmpdir = directory, fileext = ".gpkg")
  backup <- paste0(path, ".backup")
  on.exit({
    if (file.exists(temporary)) unlink(temporary)
    if (file.exists(backup) && !file.exists(path)) file.rename(backup, path)
  }, add = TRUE)

  for (layer_name in names(layers)) {
    layer <- layers[[layer_name]]
    if (!nrow(layer)) next
    sf::st_write(
      layer,
      temporary,
      layer = layer_name,
      driver = "GPKG",
      append = FALSE,
      quiet = TRUE
    )
  }

  metadata <- data.frame(
    scenario_id = scenario_id,
    schema_version = 1L,
    generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    building_count = nrow(buildings),
    reference_building_count = reference_building_count,
    removed_building_count = reference_building_count - nrow(buildings),
    stringsAsFactors = FALSE
  )
  attribute_tables <- c(
    list(scenario_metadata = metadata),
    if (is.null(sources)) list() else list(scenario_sources = sources),
    tables
  )
  connection <- DBI::dbConnect(RSQLite::SQLite(), temporary)
  connection_open <- TRUE
  on.exit({
    if (connection_open) try(DBI::dbDisconnect(connection), silent = TRUE)
  }, add = TRUE)
  for (table_name in names(attribute_tables)) {
    table <- attribute_tables[[table_name]]
    if (is.null(table)) next
    table <- as.data.frame(table, stringsAsFactors = FALSE)
    DBI::dbWriteTable(connection, table_name, table, overwrite = TRUE)
    DBI::dbExecute(
      connection,
      "DELETE FROM gpkg_contents WHERE table_name = ?",
      params = list(table_name)
    )
    DBI::dbExecute(
      connection,
      paste(
        "INSERT INTO gpkg_contents",
        "(table_name, data_type, identifier, description, last_change, srs_id)",
        "VALUES (?, 'attributes', ?, '', strftime('%Y-%m-%dT%H:%M:%fZ','now'), NULL)"
      ),
      params = list(table_name, table_name)
    )
  }
  DBI::dbDisconnect(connection)
  connection_open <- FALSE

  if (file.exists(backup)) unlink(backup)
  if (file.exists(path) && !file.rename(path, backup)) {
    stop("Impossible de préparer le remplacement du GeoPackage du scénario.")
  }
  if (!file.rename(temporary, path)) {
    if (file.exists(backup)) file.rename(backup, path)
    stop("Impossible d’enregistrer le GeoPackage du scénario.")
  }
  if (file.exists(backup)) unlink(backup)
  invisible(path)
}

read_scenario_geopackage <- function(path) {
  if (!file.exists(path)) stop("Le GeoPackage du scénario est introuvable.")
  expected_layers <- c(
    "buildings", "roads", "parcels", "quartiers", "land_use",
    "road_footprints", "flood_areas", "project_boundary", "title_boundary"
  )
  available_layers <- sf::st_layers(path)$name
  missing_layers <- setdiff(expected_layers, available_layers)
  if (length(missing_layers)) {
    stop("Couches manquantes dans le scénario : ", paste(missing_layers, collapse = ", "), ".")
  }
  spatial <- stats::setNames(
    lapply(expected_layers, function(layer) sf::st_read(path, layer = layer, quiet = TRUE)),
    expected_layers
  )
  connection <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  user_tables <- setdiff(
    DBI::dbListTables(connection),
    c(
      expected_layers,
      "gpkg_contents", "gpkg_extensions", "gpkg_geometry_columns",
      "gpkg_ogr_contents", "gpkg_spatial_ref_sys", "gpkg_tile_matrix",
      "gpkg_tile_matrix_set", "sqlite_sequence"
    )
  )
  user_tables <- user_tables[!grepl("^rtree_", user_tables)]
  tables <- stats::setNames(
    lapply(user_tables, function(table) DBI::dbReadTable(connection, table)),
    user_tables
  )
  list(spatial = spatial, tables = tables)
}


link_buildings_to_context <- function(buildings, quartiers, land_use) {
  district_index <- largest_overlap_index(buildings, quartiers)
  preferred_land_use <- !is.na(land_use$zone) & land_use$zone != "TVB"
  land_use_index <- largest_overlap_index(
    buildings,
    land_use,
    preferred = preferred_land_use
  )

  district_code <- rep(NA_character_, nrow(buildings))
  district_label <- rep(NA_character_, nrow(buildings))
  district_code[!is.na(district_index)] <- quartiers$district_code[district_index[!is.na(district_index)]]
  district_label[!is.na(district_index)] <- quartiers$district_label[district_index[!is.na(district_index)]]
  district_mapping <- c(
    "1" = "q1", "2" = "q2", "3" = "q3", "4" = "q4",
    "5" = "q5", "6" = "q6", "A" = "qa"
  )

  land_use_zone <- rep(NA_character_, nrow(buildings))
  land_use_label <- rep(NA_character_, nrow(buildings))
  land_use_zone[!is.na(land_use_index)] <- as.character(land_use$zone[land_use_index[!is.na(land_use_index)]])
  land_use_label[!is.na(land_use_index)] <- land_use$land_use_label[land_use_index[!is.na(land_use_index)]]

  buildings$district_code <- district_code
  buildings$district_id <- unname(district_mapping[district_code])
  buildings$district_label_spatial <- district_label
  buildings$land_use_zone <- land_use_zone
  buildings$land_use_label_spatial <- land_use_label
  buildings$product_id <- land_use_product_id(land_use_zone)
  buildings$product_link_source <- ifelse(
    is.na(buildings$product_id),
    "non renseigné",
    "occupation du sol"
  )
  buildings$included_in_simulation <- TRUE
  buildings$control_edited <- FALSE
  add_building_classification_status(buildings)
}

calculate_building_footprints <- function(buildings) {
  if (!inherits(buildings, "sf")) stop("La couche des bâtiments doit être spatiale.")
  geometry_area <- as.numeric(sf::st_area(sf::st_transform(buildings, 32628)))
  offset <- if ("footprint_offset_sqm" %in% names(buildings)) {
    suppressWarnings(as.numeric(buildings$footprint_offset_sqm))
  } else {
    rep(0, nrow(buildings))
  }
  offset[is.na(offset)] <- 0
  effective <- geometry_area + offset
  if (any(!is.finite(effective) | effective <= 0)) {
    stop("L’ajustement d’emprise doit conserver une emprise retenue strictement positive.")
  }
  data.frame(
    geometry_footprint_sqm = round(geometry_area, 1),
    footprint_offset_sqm = round(offset, 1),
    effective_footprint_sqm = round(effective, 1)
  )
}

apply_building_footprint_adjustment <- function(buildings, row_index, adjustment_sqm) {
  if (!inherits(buildings, "sf")) stop("La couche des bâtiments doit être spatiale.")
  if (length(row_index) != 1L || is.na(row_index) || row_index < 1L ||
      row_index > nrow(buildings)) {
    stop("Le bâtiment à ajuster est introuvable.")
  }
  adjustment_sqm <- suppressWarnings(as.numeric(adjustment_sqm))
  if (length(adjustment_sqm) != 1L || is.na(adjustment_sqm) ||
      !is.finite(adjustment_sqm)) {
    stop("L’ajustement d’emprise doit être un nombre fini en m².")
  }

  original_crs <- sf::st_crs(buildings)
  metric <- sf::st_transform(buildings, 32628)
  geometry <- sf::st_geometry(metric[row_index, ])
  current_area <- as.numeric(sf::st_area(geometry))
  target_area <- current_area + adjustment_sqm
  if (!is.finite(target_area) || target_area <= 0) {
    stop("L’ajustement d’emprise doit conserver une emprise strictement positive.")
  }

  centre <- sf::st_coordinates(sf::st_centroid(geometry))[1, 1:2]
  scale_factor <- sqrt(target_area / current_area)
  adjusted_geometry <- (geometry - centre) * scale_factor + centre
  if (!all(sf::st_is_valid(adjusted_geometry))) {
    adjusted_geometry <- sf::st_make_valid(adjusted_geometry)
  }
  sf::st_geometry(metric)[row_index] <- adjusted_geometry
  if (!"footprint_offset_sqm" %in% names(metric)) {
    metric$footprint_offset_sqm <- 0
  }
  metric$footprint_offset_sqm[row_index] <- 0

  result <- sf::st_transform(metric, original_crs)
  rownames(result) <- rownames(buildings)
  result
}

apply_product_building_assumptions <- function(
  buildings, product_id, levels, units_per_level
) {
  levels <- suppressWarnings(as.numeric(levels))
  units_per_level <- suppressWarnings(as.numeric(units_per_level))
  if (length(levels) != 1L || is.na(levels) || levels < 1 ||
      abs(levels - round(levels)) > 1e-8) {
    stop("Le nombre de niveaux doit être un entier strictement positif.")
  }
  if (length(units_per_level) != 1L || is.na(units_per_level) ||
      units_per_level < 1 || abs(units_per_level - round(units_per_level)) > 1e-8) {
    stop("Le nombre d’unités par niveau doit être un entier strictement positif.")
  }
  if (!"levels" %in% names(buildings)) buildings$levels <- NA_real_
  if (!"levels_source" %in% names(buildings)) buildings$levels_source <- NA_character_
  if (!"units_per_level" %in% names(buildings)) buildings$units_per_level <- NA_real_
  matched <- !is.na(buildings$product_id) & buildings$product_id == product_id
  buildings$levels[matched] <- as.integer(round(levels))
  buildings$levels_source[matched] <- "hypothèse produit"
  buildings$units_per_level[matched] <- as.integer(round(units_per_level))
  buildings
}

calculate_building_surface_comparison <- function(buildings, products) {
  products <- ensure_product_building_assumptions(products)
  result <- buildings
  footprints <- calculate_building_footprints(result)
  result$geometry_footprint_sqm <- footprints$geometry_footprint_sqm
  result$footprint_offset_sqm <- footprints$footprint_offset_sqm
  result$effective_footprint_sqm <- footprints$effective_footprint_sqm
  result$footprint_area_sqm <- result$effective_footprint_sqm
  result$estimated_sdp_sqm <- round(
    result$effective_footprint_sqm * result$levels_effective,
    1
  )
  product_index <- match(result$product_id, products$product_id)
  commercial <- products$ground_floor_commercial[product_index]
  commercial[is.na(commercial)] <- FALSE
  commercial_product <- products$ground_floor_product_id[product_index]
  result$ground_floor_product_id <- ifelse(
    commercial & !is.na(commercial_product), commercial_product, result$product_id
  )
  result$upper_floor_product_id <- result$product_id
  result$upper_floor_product_id[result$levels_effective <= 1] <- NA_character_
  uses_units <- products$uses_units_per_level[product_index]
  uses_units[is.na(uses_units)] <- FALSE
  product_units <- products$units_per_level[product_index]
  building_units <- if ("units_per_level" %in% names(result)) {
    suppressWarnings(as.numeric(result$units_per_level))
  } else {
    rep(NA_real_, nrow(result))
  }
  replace_units <- is.na(building_units) | building_units <= 0
  building_units[replace_units] <- product_units[replace_units]
  result$units_per_level_effective <- ifelse(uses_units, building_units, 1)
  result$calculated_sdp_per_unit_sqm <- ifelse(
    uses_units, round(result$effective_footprint_sqm / building_units, 1),
    round(result$estimated_sdp_sqm, 1)
  )
  result$model_sdp_per_unit <- products$sdp_per_unit[product_index]
  result$sdp_difference_sqm <- round(
    result$calculated_sdp_per_unit_sqm - result$model_sdp_per_unit, 1
  )
  result
}

calculate_building_level_allocations <- function(buildings, products, overrides = NULL) {
  products <- ensure_product_building_assumptions(products)
  required <- c("building_id", "levels_effective", "product_id")
  missing <- setdiff(required, names(buildings))
  if (length(missing)) {
    stop("Colonnes manquantes pour l’affectation par niveau : ", paste(missing, collapse = ", "), ".")
  }
  levels <- buildings$levels_effective
  if (anyNA(levels) || any(levels <= 0) || any(abs(levels - round(levels)) > 1e-8)) {
    stop("Le nombre de niveaux doit être un entier strictement positif.")
  }

  footprints <- calculate_building_footprints(buildings)
  footprint <- footprints$effective_footprint_sqm
  rows <- lapply(seq_len(nrow(buildings)), function(index) {
    level_number <- seq.int(0L, as.integer(round(levels[index])) - 1L)
    base_product <- buildings$product_id[index]
    product_id <- rep(base_product, length(level_number))
    source <- rep("produit du bâtiment", length(level_number))

    product_index <- match(base_product, products$product_id)
    commercial <- !is.na(product_index) &&
      isTRUE(products$ground_floor_commercial[product_index])
    commercial_product <- if (!is.na(product_index)) {
      products$ground_floor_product_id[product_index]
    } else {
      NA_character_
    }
    if (commercial && !is.na(commercial_product)) {
      product_id[level_number == 0L] <- commercial_product
      source[] <- "hypothèse produit"
    }

    data.frame(
      building_id = buildings$building_id[index],
      level_number = level_number,
      level_label = ifelse(level_number == 0L, "RDC", paste0("R+", level_number)),
      use_type = ifelse(grepl("_com$", product_id), "Commerce / services", "Résidentiel / autre"),
      product_id = product_id,
      geometry_footprint_sqm = rep(footprints$geometry_footprint_sqm[index], length(level_number)),
      footprint_offset_sqm = rep(footprints$footprint_offset_sqm[index], length(level_number)),
      effective_footprint_sqm = rep(footprint[index], length(level_number)),
      estimated_sdp_sqm = round(rep(footprint[index], length(level_number)), 1),
      assignment_source = source,
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)

  if (!is.null(overrides) && nrow(overrides)) {
    key <- paste(result$building_id, result$level_number, sep = "__")
    override_key <- paste(overrides$building_id, overrides$level_number, sep = "__")
    override_index <- match(key, override_key)
    changed <- !is.na(override_index)
    result$product_id[changed] <- overrides$product_id[override_index[changed]]
    result$assignment_source[changed] <- "manuel"
    result$use_type[changed] <- ifelse(
      grepl("_com$", result$product_id[changed]),
      "Commerce / services",
      "Résidentiel / autre"
    )
  }

  allocation_product_index <- match(result$product_id, products$product_id)
  uses_units <- products$uses_units_per_level[allocation_product_index]
  uses_units[is.na(uses_units)] <- FALSE
  product_units <- products$units_per_level[allocation_product_index]
  building_index <- match(result$building_id, buildings$building_id)
  building_units <- if ("units_per_level" %in% names(buildings)) {
    suppressWarnings(as.numeric(buildings$units_per_level[building_index]))
  } else {
    rep(NA_real_, nrow(result))
  }
  base_product <- buildings$product_id[building_index]
  same_as_building_product <- result$product_id == base_product
  effective_units <- product_units
  effective_units[same_as_building_product & !is.na(building_units)] <-
    building_units[same_as_building_product & !is.na(building_units)]
  valid_units <- !is.na(effective_units) & effective_units >= 1 &
    abs(effective_units - round(effective_units)) <= 1e-8
  if (any(uses_units & !valid_units)) {
    stop("Le nombre d’unités par niveau doit être un entier strictement positif.")
  }
  result$unit_count <- 0
  result$unit_count[uses_units] <- round(effective_units[uses_units])
  noncollective_key <- paste(result$building_id, result$product_id, sep = "__")
  first_noncollective <- !uses_units & !duplicated(noncollective_key)
  result$unit_count[first_noncollective] <- 1
  result$sdp_per_unit_sqm <- ifelse(
    result$unit_count > 0,
    round(result$estimated_sdp_sqm / result$unit_count, 1),
    NA_real_
  )

  result$product_label <- products$product_label[match(result$product_id, products$product_id)]
  result
}

calculate_product_building_summary <- function(buildings, products, overrides = NULL) {
  products <- ensure_product_building_assumptions(products)
  allocations <- calculate_building_level_allocations(buildings, products, overrides)
  allocations <- allocations[
    !is.na(allocations$product_id) & nzchar(allocations$product_id),
    ,
    drop = FALSE
  ]

  result <- data.frame(
    product_id = products$product_id,
    building_count = 0L,
    total_sdp_sqm = 0,
    sdp_per_building_sqm = NA_real_,
    stringsAsFactors = FALSE
  )
  if (!nrow(allocations)) return(result)

  split_rows <- split(allocations, allocations$product_id)
  summaries <- do.call(rbind, lapply(names(split_rows), function(product_id) {
    rows <- split_rows[[product_id]]
    building_count <- length(unique(rows$building_id))
    total_sdp <- sum(rows$estimated_sdp_sqm, na.rm = TRUE)
    data.frame(
      product_id = product_id,
      building_count = building_count,
      total_sdp_sqm = total_sdp,
      sdp_per_building_sqm = if (building_count > 0) total_sdp / building_count else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
  index <- match(result$product_id, summaries$product_id)
  matched <- !is.na(index)
  result[matched, names(summaries)[-1]] <- summaries[index[matched], names(summaries)[-1]]
  result
}

calculate_district_surface_comparison <- function(
  buildings,
  products,
  model_program,
  districts
) {
  comparison <- calculate_building_surface_comparison(buildings, products)
  comparison <- comparison[comparison$included_in_simulation, , drop = FALSE]
  attributes <- sf::st_drop_geometry(comparison)
  attributes <- attributes[!is.na(attributes$district_id), , drop = FALSE]

  map_summary <- if (nrow(attributes) > 0) {
    stats::aggregate(
      cbind(footprint_area_sqm, estimated_sdp_sqm) ~ district_id,
      data = attributes,
      FUN = sum
    )
  } else {
    data.frame(
      district_id = character(),
      footprint_area_sqm = numeric(),
      estimated_sdp_sqm = numeric()
    )
  }
  building_counts <- table(attributes$district_id)
  map_summary$building_count <- as.integer(building_counts[map_summary$district_id])

  model_summary <- stats::aggregate(
    total_sdp ~ district_id,
    data = model_program,
    FUN = sum
  )
  names(model_summary)[names(model_summary) == "total_sdp"] <- "model_sdp_sqm"

  result <- merge(districts, map_summary, by = "district_id", all.x = TRUE, sort = FALSE)
  result <- merge(result, model_summary, by = "district_id", all.x = TRUE, sort = FALSE)
  numeric_fields <- c("footprint_area_sqm", "estimated_sdp_sqm", "building_count", "model_sdp_sqm")
  for (field in numeric_fields) result[[field]][is.na(result[[field]])] <- 0
  result$sdp_difference_sqm <- result$estimated_sdp_sqm - result$model_sdp_sqm
  result
}

calculate_cartographic_kpis <- function(buildings, products) {
  comparison <- calculate_building_surface_comparison(buildings, products)
  included <- comparison$included_in_simulation
  included[is.na(included)] <- FALSE
  data.frame(
    building_count = sum(included),
    footprint_area_sqm = sum(comparison$footprint_area_sqm[included], na.rm = TRUE),
    estimated_sdp_sqm = sum(comparison$estimated_sdp_sqm[included], na.rm = TRUE),
    flagged_building_count = sum(
      comparison$has_incongruity[included] | comparison$control_required[included],
      na.rm = TRUE
    ),
    stringsAsFactors = FALSE
  )
}

calculate_osm_building_heights <- function(
  buildings,
  ground_floor_height_m = 4,
  upper_floor_height_m = 3,
  default_levels = 1,
  products = NULL
) {
  assumptions <- c(ground_floor_height_m, upper_floor_height_m, default_levels)
  if (anyNA(assumptions) || any(assumptions <= 0)) {
    stop("Les hypothèses de hauteur et de niveaux doivent être strictement positives.")
  }

  levels <- suppressWarnings(as.numeric(buildings$levels))
  valid_source <- !is.na(levels) & levels > 0 & abs(levels - round(levels)) <= 1e-8
  product_defaults <- rep(NA_real_, nrow(buildings))
  if (!is.null(products)) {
    products <- ensure_product_building_assumptions(products)
    product_defaults <- products$default_building_levels[
      match(buildings$product_id, products$product_id)
    ]
  }
  product_default_valid <- !is.na(product_defaults) & product_defaults > 0
  levels[!valid_source & product_default_valid] <- product_defaults[
    !valid_source & product_default_valid
  ]
  levels[!valid_source & !product_default_valid] <- default_levels

  buildings$levels_effective <- as.integer(round(levels))
  buildings$levels_assumption_source <- ifelse(
    valid_source, "QGIS / OSM",
    ifelse(product_default_valid, "Hypothèse produit", "Hypothèse générale")
  )
  buildings$height_m <- ground_floor_height_m + pmax(levels - 1, 0) * upper_floor_height_m
  buildings
}

create_building_level_blocks <- function(
  buildings, products, ground_floor_height_m = 4, upper_floor_height_m = 3,
  overrides = NULL, gap_m = 0.08
) {
  allocations <- calculate_building_level_allocations(buildings, products, overrides)
  building_index <- match(allocations$building_id, buildings$building_id)
  result <- sf::st_sf(
    allocations,
    control_required = buildings$control_required[building_index],
    incongruity_flag = buildings$incongruity_flag[building_index],
    geometry = sf::st_geometry(buildings)[building_index]
  )
  total_units <- stats::ave(
    result$unit_count, result$building_id,
    FUN = function(value) sum(value, na.rm = TRUE)
  )
  total_sdp <- stats::ave(
    result$estimated_sdp_sqm, result$building_id,
    FUN = function(value) sum(value, na.rm = TRUE)
  )
  commercial_rdc_surface <- ifelse(
    result$level_number == 0L & result$use_type == "Commerce / services",
    result$estimated_sdp_sqm,
    0
  )
  result$building_total_units <- as.numeric(total_units)
  result$building_total_sdp_sqm <- round(as.numeric(total_sdp), 1)
  result$commercial_ground_floor <- stats::ave(
    commercial_rdc_surface > 0, result$building_id, FUN = any
  )
  result$commercial_surface_total_sqm <- round(stats::ave(
    commercial_rdc_surface, result$building_id, FUN = sum
  ), 1)
  base <- ifelse(
    result$level_number == 0L,
    0,
    ground_floor_height_m + (result$level_number - 1L) * upper_floor_height_m
  )
  height <- ifelse(
    result$level_number == 0L,
    ground_floor_height_m,
    ground_floor_height_m + result$level_number * upper_floor_height_m
  )
  result$base_height_m <- round(base + ifelse(result$level_number == 0L, 0, gap_m), 2)
  result$top_height_m <- round(pmax(result$base_height_m, height - gap_m), 2)
  result
}

calculate_program_from_buildings <- function(
  buildings, products, template_program, overrides = NULL
) {
  products <- ensure_product_building_assumptions(products)
  blocks <- calculate_building_level_allocations(buildings, products, overrides)
  valid <- !is.na(blocks$product_id) & nzchar(blocks$product_id) &
    !is.na(buildings$district_id[match(blocks$building_id, buildings$building_id)])
  blocks <- blocks[valid, , drop = FALSE]
  blocks$district_id <- buildings$district_id[
    match(blocks$building_id, buildings$building_id)
  ]

  summary_rows <- if (nrow(blocks)) {
    keys <- interaction(blocks$district_id, blocks$product_id, drop = TRUE)
    do.call(rbind, lapply(split(blocks, keys), function(rows) {
      data.frame(
        district_id = rows$district_id[1],
        product_id = rows$product_id[1],
        map_total_sdp = sum(rows$estimated_sdp_sqm, na.rm = TRUE),
        map_quantity = sum(rows$unit_count, na.rm = TRUE),
        map_building_count = length(unique(rows$building_id)),
        stringsAsFactors = FALSE
      )
    }))
  } else {
    data.frame(
      district_id = character(), product_id = character(),
      map_total_sdp = numeric(), map_quantity = numeric(), map_building_count = integer()
    )
  }

  observed_key <- paste(summary_rows$district_id, summary_rows$product_id, sep = "__")
  template_key <- paste(template_program$district_id, template_program$product_id, sep = "__")
  missing_key <- setdiff(observed_key, template_key)
  if (length(missing_key)) {
    index <- match(missing_key, observed_key)
    additions <- data.frame(
      program_line_id = missing_key,
      district_id = summary_rows$district_id[index],
      product_id = summary_rows$product_id[index],
      quantity = 0,
      manual_total_sdp = NA_real_,
      manual_total_land_area = NA_real_,
      stringsAsFactors = FALSE
    )
    template_program <- rbind(template_program, additions[, names(template_program), drop = FALSE])
  }

  result <- template_program
  result_key <- paste(result$district_id, result$product_id, sep = "__")
  summary_index <- match(result_key, observed_key)
  product_index <- match(result$product_id, products$product_id)
  spatial_product <- products$area_basis[product_index] == "sdp"
  spatial_product[is.na(spatial_product)] <- FALSE
  mapped_sdp <- summary_rows$map_total_sdp[summary_index]
  mapped_sdp[is.na(mapped_sdp)] <- 0
  mapped_quantity <- summary_rows$map_quantity[summary_index]
  mapped_quantity[is.na(mapped_quantity)] <- 0
  result$manual_total_sdp[spatial_product] <- mapped_sdp[spatial_product]
  result$quantity[spatial_product] <- mapped_quantity[spatial_product]
  rownames(result) <- NULL
  result
}

sf_to_geojson <- function(features) {
  if (!inherits(features, "sf")) {
    stop("Les données doivent être fournies sous forme d'objet sf.")
  }
  output_file <- tempfile(fileext = ".geojson")
  on.exit(unlink(output_file), add = TRUE)
  sf::st_write(features, output_file, driver = "GeoJSON", quiet = TRUE)
  paste(readLines(output_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

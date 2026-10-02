# Intrants spatiaux de la charte de performance.
#
# Les intrants sont mesurés sur un instantané en lecture seule de spatial.gpkg
# (identifiants fid conservés pour les retrouver dans QGIS). Les surfaces et
# longueurs sont toujours recalculées depuis les géométries en EPSG:32628.
# Le classement charte est porté par des colonnes de spatial.gpkg, éditables
# dans QGIS : sur land_use, une colonne 0/1 par classe (`ch_*`) et
# `ch_coef_biotope` ; sur road_footprints, `ch_modes_doux`. Une colonne absente
# ou une valeur vide prend la règle par défaut.

charte_metric_crs <- 32628L

charte_spatial_layer_names <- c(
  "buildings", "roads", "parcels", "quartiers", "land_use",
  "road_footprints", "flood_areas", "project_boundary", "title_boundary",
  "alea_fort"
)

charte_class_labels <- c(
  espace_vert_public = "Espace vert public",
  agriculture = "Agriculture nourricière",
  usage_compatible_alea = "Usage compatible en aléa fort",
  habitat = "Habitat (surface urbanisée)",
  service_ecole = "Panier de services : école",
  service_sante = "Panier de services : santé",
  service_commerce = "Panier de services : commerce / marché",
  espace_public_pieton = "Espace public piéton",
  equipement_sensible = "Équipement sensible (école, santé)",
  reserve_fonciere = "Réserve foncière"
)

# Codage des colonnes charte de land_use : une colonne 0/1 par classe.
charte_land_use_columns <- data.frame(
  column = c(
    "ch_ev_public", "ch_agriculture", "ch_alea_compatible", "ch_habitat",
    "ch_ecole", "ch_sante", "ch_commerce", "ch_pieton", "ch_sensible", "ch_reserve"
  ),
  class_id = c(
    "espace_vert_public", "agriculture", "usage_compatible_alea", "habitat",
    "service_ecole", "service_sante", "service_commerce", "espace_public_pieton",
    "equipement_sensible", "reserve_fonciere"
  ),
  meaning = c(
    "Espace vert public accessible",
    "Agriculture nourricière (maraîchage, vergers, jardins)",
    "Usage compatible avec l'aléa fort (non bâti : parc inondable, agriculture, loisirs, bassins)",
    "Surface urbanisée d'habitat (parcelles résidentielles)",
    "Équipement scolaire du panier de services",
    "Équipement de santé du panier de services",
    "Commerce ou marché du panier de services",
    "Espace public piéton (place, square, mail)",
    "Équipement sensible au bruit et à l'aléa (école, santé)",
    "Réserve foncière pour des besoins futurs"
  ),
  used_by = c(
    "TV-1", "TV-3", "TB-1", "VC-1a, VC-1b", "VC-2", "VC-2", "VC-2", "CV-1",
    "CV-4, RES-3", "RES-1a"
  ),
  stringsAsFactors = FALSE
)

charte_coef_column <- "ch_coef_biotope"
charte_soft_mobility_column <- "ch_modes_doux"

charte_reference_area_labels <- c(
  title_boundary = "Titre foncier",
  project_boundary = "Emprise du projet",
  quartiers = "Union des quartiers"
)

# Types de zones inondables composant l'aléa fort. Un type absent de
# flood_areas est signalé, sans être remplacé par une autre couche.
charte_hazard_flood_types <- c(
  "Lit mineur", "Lit moyen", "Rétention 50 cm", "Cuvette de rétention"
)

# Hypothèses par défaut, éditables ultérieurement dans le modèle du scénario.
charte_default_parameters <- function() {
  list(
    reference_area = "title_boundary",
    hazard_flood_types = charte_hazard_flood_types,
    service_distance_m = 500,
    canopy_quad_segments = 6L,
    noise_buffer_m = c(
      "RN" = 100,
      "Boulevard urbain" = 50,
      "Boucle primaire" = 30
    )
  )
}

# Règles par défaut du classement de l'occupation du sol. Une entité reçoit
# l'union des classes des règles qui la concernent et le coefficient de
# biotope le plus élevé.
charte_default_land_use_rules <- function() {
  habitat_cu <- "Superficie foncière de Habitat et ses annexes"
  rules <- data.frame(
    match_field = c(
      rep("Layer", 14), "CU"
    ),
    match_value = c(
      "square", "Maille", "Parc", "Foret", "TVB Hydraulique", "TVBP", "Buffer",
      "Jardins familiaux", "Parcelles Agricoles", "Equipement Sportif",
      "Equipement Scolaire", "Equipement de Santé",
      "Parcelles Tertiaire Commerce", "Parcelles Mixte",
      habitat_cu
    ),
    classes = c(
      "espace_vert_public;espace_public_pieton;usage_compatible_alea",
      "espace_vert_public;espace_public_pieton;usage_compatible_alea",
      "espace_vert_public;usage_compatible_alea",
      "espace_vert_public;usage_compatible_alea",
      "usage_compatible_alea",
      "usage_compatible_alea",
      "usage_compatible_alea",
      "espace_vert_public;agriculture;usage_compatible_alea",
      "agriculture;usage_compatible_alea",
      "usage_compatible_alea",
      "service_ecole;equipement_sensible",
      "service_sante;equipement_sensible",
      "service_commerce",
      "service_commerce",
      "habitat"
    ),
    coef_biotope = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0),
    stringsAsFactors = FALSE
  )
  rules
}

# Profils de voirie offrant des cheminements doux sûrs et continus.
charte_default_soft_mobility_profiles <- function() {
  c("Rue piétonne partagée", "Le mail planté")
}

charte_soft_highway_types <- c(
  "pedestrian", "footway", "cycleway", "path", "living_street", "steps"
)

read_charte_spatial_layers <- function(path) {
  if (!file.exists(path)) stop("Le GeoPackage spatial du scénario est introuvable.")
  available <- sf::st_layers(path)$name
  layers <- lapply(charte_spatial_layer_names, function(layer) {
    if (!layer %in% available) return(NULL)
    data <- sf::st_read(path, layer = layer, quiet = TRUE, fid_column_name = "fid")
    data$fid <- as.integer(data$fid)
    sf::st_transform(data, charte_metric_crs)
  })
  names(layers) <- charte_spatial_layer_names
  layers
}

# Houppiers à maturité : union des disques de diamètre `diameter`, sans
# double compte des recouvrements.
charte_canopy_geometry <- function(trees, quad_segments = 6L) {
  empty <- sf::st_sfc(crs = charte_metric_crs)
  if (is.null(trees) || !nrow(trees)) return(empty)
  diameter_field <- intersect(c("diameter", "crown_diameter_m"), names(trees))
  if (!length(diameter_field)) return(empty)
  trees <- sf::st_transform(trees, charte_metric_crs)
  diameter <- suppressWarnings(as.numeric(sf::st_drop_geometry(trees)[[diameter_field[[1]]]]))
  keep <- is.finite(diameter) & diameter > 0
  if (!any(keep)) return(empty)
  discs <- sf::st_buffer(
    sf::st_geometry(trees)[keep], diameter[keep] / 2, nQuadSegs = quad_segments
  )
  sf::st_sfc(sf::st_union(discs), crs = charte_metric_crs)
}

charte_split_classes <- function(value) {
  if (is.na(value)) return(character())
  parts <- trimws(strsplit(value, "[;,]")[[1]])
  parts[nzchar(parts)]
}

charte_number_column <- function(data, field) {
  if (!field %in% names(data)) return(rep(NA_real_, nrow(data)))
  suppressWarnings(as.numeric(sf::st_drop_geometry(data)[[field]]))
}

# Valeurs par défaut des colonnes charte de land_use (0/1 et coefficient).
charte_default_land_use_values <- function(land_use, rules = charte_default_land_use_rules()) {
  attributes <- sf::st_drop_geometry(land_use)
  values <- as.data.frame(
    matrix(0L, nrow(land_use), nrow(charte_land_use_columns),
           dimnames = list(NULL, charte_land_use_columns$column))
  )
  coef <- rep(0, nrow(land_use))
  matched <- rep(FALSE, nrow(land_use))
  for (index in seq_len(nrow(rules))) {
    field <- rules$match_field[[index]]
    if (!field %in% names(attributes)) next
    hit <- !is.na(attributes[[field]]) & attributes[[field]] == rules$match_value[[index]]
    if (!any(hit)) next
    matched[hit] <- TRUE
    for (class_id in charte_split_classes(rules$classes[[index]])) {
      column <- charte_land_use_columns$column[charte_land_use_columns$class_id == class_id]
      values[[column]][hit] <- 1L
    }
    coef[hit] <- pmax(coef[hit], rules$coef_biotope[[index]])
  }
  values[[charte_coef_column]] <- coef
  list(values = values, matched = matched)
}

# Classement charte de chaque entité : la valeur des colonnes ch_* de
# spatial.gpkg, ou la règle par défaut lorsque la cellule est vide.
classify_charte_land_use <- function(land_use, rules = charte_default_land_use_rules()) {
  defaults <- charte_default_land_use_values(land_use, rules)
  columns <- c(charte_land_use_columns$column, charte_coef_column)
  empty <- rep(0L, nrow(land_use))
  for (column in columns) {
    stored <- charte_number_column(land_use, column)
    empty <- empty + is.na(stored)
    land_use[[paste0(column, "_resolved")]] <- ifelse(
      is.na(stored), defaults$values[[column]], stored
    )
  }
  flags <- sf::st_drop_geometry(land_use)[paste0(charte_land_use_columns$column, "_resolved")]
  land_use$charte_classes_resolved <- apply(flags == 1, 1, function(row) {
    paste(charte_land_use_columns$class_id[row], collapse = ";")
  })
  if (!nrow(land_use)) land_use$charte_classes_resolved <- character()
  land_use$coef_biotope_resolved <- land_use[[paste0(charte_coef_column, "_resolved")]]
  land_use$charte_empty_values <- empty
  land_use$charte_default_matched <- defaults$matched
  land_use$charte_rule_source <- ifelse(
    empty == 0L, "spatial.gpkg",
    ifelse(empty == length(columns), "Règle par défaut", "spatial.gpkg incomplet")
  )
  land_use$area_sqm <- as.numeric(sf::st_area(land_use))
  land_use
}

charte_class_column <- function(class_id) {
  charte_land_use_columns$column[charte_land_use_columns$class_id == class_id]
}

charte_has_class <- function(land_use, class_id) {
  land_use[[paste0(charte_class_column(class_id), "_resolved")]] == 1
}

# Libellé court des colonnes à 1 d'une entité, pour la carte et les listes.
charte_land_use_code_label <- function(land_use) {
  flags <- sf::st_drop_geometry(land_use)[paste0(charte_land_use_columns$column, "_resolved")]
  codes <- apply(flags == 1, 1, function(row) {
    if (!any(row)) "aucune classe" else paste(charte_land_use_columns$column[row], collapse = ", ")
  })
  paste0(codes, " ; ", charte_coef_column, " = ", format_number_fr(land_use$coef_biotope_resolved, 2))
}

# Ajoute les colonnes charte à spatial.gpkg (land_use, road_footprints) et
# remplit uniquement les cellules vides avec la règle par défaut. Les autres
# colonnes, les fid et les géométries ne sont pas modifiés.
write_charte_columns <- function(path, rules = charte_default_land_use_rules(),
                                 profiles = charte_default_soft_mobility_profiles()) {
  layers <- read_charte_spatial_layers(path)
  defaults <- charte_default_land_use_values(layers$land_use, rules)$values
  footprints <- layers$road_footprints
  soft <- as.integer(!is.na(footprints$Descr) & footprints$Descr %in% profiles)
  plan <- list(
    land_use = list(fid = layers$land_use$fid, values = defaults),
    road_footprints = list(
      fid = footprints$fid,
      values = stats::setNames(data.frame(soft), charte_soft_mobility_column)
    )
  )
  # Les écritures passent par GDAL (ogrinfo -sql) : les déclencheurs de l'index
  # spatial du GeoPackage appellent des fonctions SQL fournies par GDAL.
  run_sql <- function(sql) {
    output <- suppressWarnings(system2(
      "ogrinfo", c("-q", shQuote(path), "-sql", shQuote(sql)), stdout = TRUE, stderr = TRUE
    ))
    status <- attr(output, "status")
    if ((!is.null(status) && status != 0) || any(grepl("ERROR", output))) {
      stop("Écriture impossible dans spatial.gpkg : ", paste(output, collapse = " "))
    }
  }
  quote <- function(name) paste0('"', gsub('"', '""', name), '"')
  connection <- DBI::dbConnect(RSQLite::SQLite(), path, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  summary <- list()
  for (table in names(plan)) {
    info <- DBI::dbGetQuery(connection, paste0("PRAGMA table_info(", quote(table), ")"))
    fid_column <- info$name[info$pk == 1]
    if (length(fid_column) != 1) stop("Identifiant fid introuvable dans la table ", table, ".")
    for (column in names(plan[[table]]$values)) {
      created <- !column %in% info$name
      if (created) {
        type <- if (column == charte_coef_column) "REAL" else "INTEGER"
        run_sql(paste0("ALTER TABLE ", quote(table), " ADD COLUMN ", quote(column), " ", type))
      }
      filled <- DBI::dbGetQuery(connection, paste0(
        "SELECT count(*) AS n FROM ", quote(table), " WHERE ", quote(column), " IS NULL"
      ))$n
      if (filled > 0) {
        values <- plan[[table]]$values[[column]]
        cases <- paste0("WHEN ", plan[[table]]$fid, " THEN ", format(values, scientific = FALSE, trim = TRUE),
                        collapse = " ")
        run_sql(paste0(
          "UPDATE ", quote(table), " SET ", quote(column), " = CASE ", quote(fid_column), " ",
          cases, " END WHERE ", quote(column), " IS NULL"
        ))
      }
      summary[[length(summary) + 1L]] <- data.frame(
        table = table, column = column, created = created, filled = filled,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, summary)
}

charte_polygonize_lines <- function(layer) {
  if (is.null(layer) || !nrow(layer)) return(sf::st_sfc(crs = charte_metric_crs))
  geometry <- sf::st_geometry(layer)
  types <- unique(as.character(sf::st_geometry_type(geometry)))
  if (all(types %in% c("POLYGON", "MULTIPOLYGON"))) {
    return(sf::st_sfc(sf::st_union(sf::st_make_valid(geometry)), crs = charte_metric_crs))
  }
  polygons <- sf::st_collection_extract(
    sf::st_polygonize(sf::st_union(sf::st_cast(geometry, "MULTILINESTRING"))),
    "POLYGON"
  )
  if (!length(polygons)) return(sf::st_sfc(crs = charte_metric_crs))
  sf::st_sfc(sf::st_union(polygons), crs = charte_metric_crs)
}

charte_union <- function(geometry) {
  if (!length(geometry)) return(sf::st_sfc(crs = charte_metric_crs))
  sf::st_sfc(sf::st_union(sf::st_make_valid(geometry)), crs = charte_metric_crs)
}

charte_intersection <- function(x, y) {
  if (!length(x) || !length(y)) return(sf::st_sfc(crs = charte_metric_crs))
  result <- suppressWarnings(sf::st_intersection(x, y))
  if (!length(result)) return(sf::st_sfc(crs = charte_metric_crs))
  charte_union(charte_polygons_only(result))
}

charte_extract <- function(geometry, type = c("POLYGON", "LINESTRING")) {
  type <- match.arg(type)
  types <- as.character(sf::st_geometry_type(geometry))
  if (all(types %in% c(type, paste0("MULTI", type)))) return(geometry)
  suppressWarnings(sf::st_collection_extract(geometry, type))
}

charte_polygons_only <- function(geometry) charte_extract(geometry, "POLYGON")

charte_area <- function(geometry) {
  if (!length(geometry)) return(0)
  sum(as.numeric(sf::st_area(geometry)))
}

# Surface de chaque entité découpée par la surface de référence.
charte_clipped_areas <- function(layer, reference) {
  if (!nrow(layer)) return(numeric())
  if (!length(reference)) return(rep(0, nrow(layer)))
  geometry <- sf::st_make_valid(sf::st_geometry(layer))
  inside <- lengths(sf::st_intersects(geometry, reference)) > 0
  areas <- rep(0, nrow(layer))
  if (any(inside)) {
    candidates <- sf::st_sf(row = which(inside), geometry = geometry[inside])
    parts <- suppressWarnings(sf::st_intersection(candidates, reference))
    clipped <- tapply(as.numeric(sf::st_area(parts)), parts$row, sum)
    areas[as.integer(names(clipped))] <- clipped
  }
  areas
}

charte_reference_geometry <- function(layers, reference_area) {
  switch(
    reference_area,
    title_boundary = charte_polygonize_lines(layers$title_boundary),
    project_boundary = charte_polygonize_lines(layers$project_boundary),
    quartiers = charte_polygonize_lines(layers$quartiers),
    stop("Surface de référence inconnue : ", reference_area, ".")
  )
}

# Zone d'aléa fort : la couche `alea_fort` du scénario quand elle existe
# (zone réellement inondée, ramenée à ses limites extérieures), sinon les
# types d'inondation de flood_areas.
charte_hazard_features <- function(layers, flood_types = charte_hazard_flood_types) {
  alea <- layers$alea_fort
  if (!is.null(alea) && nrow(alea)) {
    return(list(
      layer = "alea_fort", data = alea, missing_types = character(),
      geometry = alea_fort_geometry(alea, crs = charte_metric_crs)
    ))
  }
  flood_areas <- layers$flood_areas
  if (is.null(flood_areas)) {
    return(list(layer = "flood_areas", data = NULL, missing_types = flood_types,
                geometry = sf::st_sfc(crs = charte_metric_crs)))
  }
  type <- as.character(flood_areas$flood_type)
  keep <- !is.na(type) & type %in% flood_types
  data <- flood_areas[keep, ]
  list(
    layer = "flood_areas",
    data = data,
    missing_types = setdiff(flood_types, unique(type[keep])),
    geometry = if (nrow(data)) charte_union(sf::st_geometry(data)) else sf::st_sfc(crs = charte_metric_crs)
  )
}

# Les bâtiments résidentiels portent un produit RM, RC, RV ou RT hors
# produit commercial du RDC.
charte_residential_building <- function(buildings) {
  product <- as.character(buildings$product_id)
  !is.na(product) & grepl("^r[mcvt]_", product) & !grepl("_com$", product)
}

charte_soft_road_footprints <- function(road_footprints,
                                        profiles = charte_default_soft_mobility_profiles()) {
  description <- as.character(road_footprints$Descr)
  default <- !is.na(description) & description %in% profiles
  stored <- charte_number_column(road_footprints, charte_soft_mobility_column)
  road_footprints$modes_doux_resolved <- ifelse(is.na(stored), default, stored == 1)
  road_footprints$charte_rule_source <- ifelse(is.na(stored), "Règle par défaut", "spatial.gpkg")
  road_footprints
}

charte_input_row <- function(input_id, label, indicators, layer, rule, n_features,
                             value, unit, status = "Calculé", note = "",
                             kind = "spatial") {
  data.frame(
    input_id = input_id, label = label, kind = kind, indicators = indicators, layer = layer,
    rule = rule, n_features = as.integer(n_features), value = as.numeric(value),
    unit = unit, status = status, note = note, stringsAsFactors = FALSE
  )
}

charte_feature_ids <- function(layer_name, layer, keep = rep(TRUE, nrow(layer))) {
  if (is.null(layer) || !nrow(layer) || !any(keep)) {
    return(data.frame(layer = character(), fid = integer()))
  }
  data.frame(layer = layer_name, fid = as.integer(layer$fid[keep]), stringsAsFactors = FALSE)
}

# Calcule l'ensemble des intrants spatiaux. `layers` provient de
# read_charte_spatial_layers() ; `canopy` de charte_canopy_geometry().
compute_charte_spatial_inputs <- function(layers,
                                          canopy = sf::st_sfc(crs = charte_metric_crs),
                                          population = NULL,
                                          parameters = charte_default_parameters(),
                                          land_use_rules = charte_default_land_use_rules()) {
  inputs <- list()
  features <- list()
  geometries <- list()
  add <- function(row, ids = NULL, geometry = NULL) {
    inputs[[row$input_id]] <<- row
    features[[row$input_id]] <<- if (is.null(ids)) charte_feature_ids("", NULL) else ids
    geometries[[row$input_id]] <<- geometry
  }

  reference_label <- charte_reference_area_labels[[parameters$reference_area]]
  reference <- charte_reference_geometry(layers, parameters$reference_area)
  reference_area <- charte_area(reference)
  reference_layer <- layers[[parameters$reference_area]]
  add(
    charte_input_row(
      "surface_reference", "Surface de référence de l'opération",
      "TB-2, TV-2, VC-2, CV-4, RES-1a", parameters$reference_area,
      paste0(reference_label, " (contour polygonisé)"),
      if (is.null(reference_layer)) 0 else nrow(reference_layer),
      reference_area, "m²",
      if (reference_area > 0) "Calculé" else "Couche absente ou non fermée"
    ),
    charte_feature_ids(parameters$reference_area, reference_layer),
    reference
  )

  land_use <- layers$land_use
  if (is.null(land_use)) stop("La couche land_use est absente de spatial.gpkg.")
  land_use <- classify_charte_land_use(sf::st_make_valid(land_use), land_use_rules)
  land_use$area_in_reference_sqm <- charte_clipped_areas(land_use, reference)

  land_use_input <- function(input_id, label, indicators, class_id, note = "") {
    keep <- charte_has_class(land_use, class_id)
    geometry <- charte_intersection(charte_union(sf::st_geometry(land_use)[keep]), reference)
    add(
      charte_input_row(
        input_id, label, indicators, "land_use",
        paste0(charte_class_column(class_id), " = 1"),
        sum(keep), charte_area(geometry), "m²",
        if (any(keep)) "Calculé" else "Aucune entité classée", note
      ),
      charte_feature_ids("land_use", land_use, keep),
      geometry
    )
    geometry
  }

  # Trame bleue : aléa fort et usages compatibles.
  hazard <- charte_hazard_features(layers, parameters$hazard_flood_types)
  hazard_geometry <- hazard$geometry
  add(
    charte_input_row(
      "alea_fort", "Zone d'aléa fort", "TB-1, RES-3", hazard$layer,
      if (identical(hazard$layer, "alea_fort")) {
        paste0(
          "Couche alea_fort fusionnée en un seul polygone : limites extérieures, trous comblés, ",
          "parcelles isolées < ", alea_fort_minimum_part_sqm, " m² écartées"
        )
      } else {
        paste0("flood_type ∈ {", paste(parameters$hazard_flood_types, collapse = ", "), "} (couche alea_fort absente)")
      },
      if (is.null(hazard$data)) 0 else nrow(hazard$data),
      charte_area(hazard_geometry), "m²",
      if (!length(hazard_geometry)) "Aucune entité" else if (length(hazard$missing_types)) {
        "Incomplet"
      } else {
        "Calculé"
      },
      if (length(hazard$missing_types)) {
        paste0(
          "Type(s) absent(s) de flood_areas : ", paste(hazard$missing_types, collapse = ", "),
          ". Les ajouter dans QGIS (attribut flood_type) puis relire spatial.gpkg."
        )
      } else {
        ""
      }
    ),
    charte_feature_ids(hazard$layer, hazard$data),
    hazard_geometry
  )
  compatible <- charte_has_class(land_use, "usage_compatible_alea")
  hazard_valorised <- charte_intersection(
    charte_union(sf::st_geometry(land_use)[compatible]), hazard_geometry
  )
  add(
    charte_input_row(
      "alea_fort_valorise", "Aléa fort en usage compatible", "TB-1", "land_use",
      "ch_alea_compatible = 1, découpé par la zone d'aléa fort",
      sum(compatible), charte_area(hazard_valorised), "m²"
    ),
    charte_feature_ids("land_use", land_use, compatible),
    hazard_valorised
  )
  hazard_not_compatible <- charte_intersection(
    charte_union(sf::st_geometry(land_use)[!compatible]), hazard_geometry
  )
  touching <- !compatible & lengths(sf::st_intersects(land_use, hazard_geometry)) > 0
  add(
    charte_input_row(
      "alea_fort_non_compatible", "Aléa fort en usage non compatible", "TB-1, RES-3", "land_use",
      "ch_alea_compatible = 0, découpé par la zone d'aléa fort",
      sum(touching), charte_area(hazard_not_compatible), "m²"
    ),
    charte_feature_ids("land_use", land_use, touching),
    hazard_not_compatible
  )

  biotope <- land_use$coef_biotope_resolved > 0
  add(
    charte_input_row(
      "surface_ecoamenagee", "Surface éco-aménagée pondérée", "TB-2", "land_use",
      "Σ surface dans le titre foncier × ch_coef_biotope (entités où ch_coef_biotope > 0)",
      sum(biotope),
      sum(land_use$area_in_reference_sqm * land_use$coef_biotope_resolved), "m²",
      note = "Les parcelles bâties ont un coefficient nul par défaut (pleine terre non renseignée)."
    ),
    charte_feature_ids("land_use", land_use, biotope),
    charte_intersection(charte_union(sf::st_geometry(land_use)[biotope]), reference)
  )

  # Trame verte.
  land_use_input(
    "espaces_verts_publics", "Espaces verts publics", "TV-1", "espace_vert_public"
  )
  land_use_input("agriculture", "Agriculture nourricière", "TV-3", "agriculture")
  canopy_reference <- charte_intersection(canopy, reference)
  add(
    charte_input_row(
      "canopee", "Canopée à maturité", "TV-2, CV-1", "arbres UMEP",
      "Union des houppiers (diamètre à maturité) dans la référence",
      NA_integer_, charte_area(canopy_reference), "m²",
      if (length(canopy)) "Calculé" else "Inventaire d'arbres absent",
      "Inventaire d'arbres de l'étude UMEP la plus récente du scénario."
    ),
    NULL,
    canopy_reference
  )

  # Ville compacte : habitat, population, panier de services, voirie.
  land_use_input(
    "surface_habitat", "Surface urbanisée d'habitat", "VC-1a, VC-1b", "habitat"
  )
  quartiers <- layers$quartiers
  quartier_population <- if (is.null(quartiers)) NA_real_ else {
    sum(suppressWarnings(as.numeric(quartiers$population)), na.rm = TRUE)
  }
  control_note <- paste0(
    "Contrôle : attribut population des quartiers = ",
    format_number_fr(quartier_population, 0), " hab."
  )
  if (is.null(population)) {
    add(
      charte_input_row(
        "population", "Population des quartiers", "TV-1, TV-3, VC-1b", "quartiers",
        "Σ attribut population", if (is.null(quartiers)) 0 else nrow(quartiers),
        quartier_population, "hab", "À vérifier",
        "Population du programme non disponible : attribut des quartiers utilisé.",
        kind = "attribut"
      ),
      charte_feature_ids("quartiers", quartiers),
      NULL
    )
  } else {
    add(
      charte_input_row(
        "population", "Population du programme", "TV-1, TV-3, VC-1b", "programme",
        "Σ quantité × personnes par unité (hypothèses produits)",
        NA_integer_, population$total, "hab",
        if (population$complete) "Calculé" else "Incomplet",
        if (population$complete) control_note else paste0(
          "Personnes par unité non renseignées pour : ",
          paste(population$missing_products, collapse = ", "), ". ", control_note
        ),
        kind = "programme"
      ),
      NULL,
      NULL
    )
  }

  distance <- parameters$service_distance_m
  service_classes <- c("service_ecole", "service_sante", "service_commerce")
  basket <- reference
  basket_ids <- charte_feature_ids("", NULL)
  for (class_id in service_classes) {
    keep <- charte_has_class(land_use, class_id)
    basket_ids <- rbind(basket_ids, charte_feature_ids("land_use", land_use, keep))
    buffer <- if (any(keep)) {
      charte_union(sf::st_buffer(sf::st_geometry(land_use)[keep], distance))
    } else {
      sf::st_sfc(crs = charte_metric_crs)
    }
    basket <- charte_intersection(basket, buffer)
  }
  add(
    charte_input_row(
      "panier_services", paste0("Surface à moins de ", distance, " m du panier de services"),
      "VC-2", "land_use",
      "Intersection des tampons école, santé et commerce, dans la référence",
      nrow(basket_ids), charte_area(basket), "m²",
      note = "Distance euclidienne depuis les limites des emprises d'équipement."
    ),
    basket_ids,
    basket
  )

  roads <- layers$roads
  road_footprints <- charte_soft_road_footprints(layers$road_footprints)
  axes <- if (is.null(roads) || !length(reference)) {
    sf::st_sfc(crs = charte_metric_crs)
  } else {
    suppressWarnings(sf::st_intersection(sf::st_geometry(roads), reference))
  }
  total_length <- if (length(axes)) sum(as.numeric(sf::st_length(axes))) else 0
  add(
    charte_input_row(
      "lineaire_voirie", "Linéaire de voirie (axes OSM)", "VC-4, CO-3a", "roads",
      "Axes OSM dans la référence", if (is.null(roads)) 0 else nrow(roads),
      total_length, "m"
    ),
    charte_feature_ids("roads", roads),
    if (length(axes)) sf::st_sfc(sf::st_union(axes), crs = charte_metric_crs) else NULL
  )
  soft_footprints <- road_footprints$modes_doux_resolved
  soft_area <- charte_union(sf::st_geometry(road_footprints)[soft_footprints])
  soft_highway <- if (is.null(roads)) logical() else {
    !is.na(roads$highway) & roads$highway %in% charte_soft_highway_types
  }
  soft_axes <- sf::st_sfc(crs = charte_metric_crs)
  if (length(axes)) {
    in_footprint <- if (length(soft_area)) {
      suppressWarnings(sf::st_intersection(axes, soft_area))
    } else {
      sf::st_sfc(crs = charte_metric_crs)
    }
    by_type <- if (any(soft_highway)) {
      suppressWarnings(sf::st_intersection(sf::st_geometry(roads)[soft_highway], reference))
    } else {
      sf::st_sfc(crs = charte_metric_crs)
    }
    pieces <- c(in_footprint, by_type)
    pieces <- pieces[as.character(sf::st_geometry_type(pieces)) %in%
                       c("LINESTRING", "MULTILINESTRING", "GEOMETRYCOLLECTION")]
    if (length(pieces)) {
      lines <- charte_extract(pieces, "LINESTRING")
      if (length(lines)) soft_axes <- sf::st_sfc(sf::st_union(lines), crs = charte_metric_crs)
    }
  }
  add(
    charte_input_row(
      "lineaire_modes_doux", "Linéaire de cheminements doux", "VC-4", "road_footprints, roads",
      paste0(
        "Axes OSM dans les emprises « modes doux » (",
        paste(charte_default_soft_mobility_profiles(), collapse = ", "),
        " par défaut) ou de type piéton/cyclable"
      ),
      sum(soft_footprints) + sum(soft_highway),
      if (length(soft_axes)) sum(as.numeric(sf::st_length(soft_axes))) else 0, "m",
      "À vérifier",
      "Colonne ch_modes_doux (0/1) de road_footprints, éditable dans QGIS."
    ),
    rbind(
      charte_feature_ids("road_footprints", road_footprints, soft_footprints),
      charte_feature_ids("roads", roads, soft_highway)
    ),
    soft_axes
  )

  # Cadre de vie : espaces publics piétons, ombrage, bruit.
  pedestrian_land_use <- charte_has_class(land_use, "espace_public_pieton")
  pedestrian <- charte_intersection(
    charte_union(c(
      sf::st_geometry(land_use)[pedestrian_land_use],
      sf::st_geometry(road_footprints)[soft_footprints]
    )),
    reference
  )
  add(
    charte_input_row(
      "espaces_publics_pietons", "Espaces publics piétons", "CV-1",
      "land_use, road_footprints",
      "ch_pieton = 1 (land_use) + ch_modes_doux = 1 (road_footprints)",
      sum(pedestrian_land_use) + sum(soft_footprints), charte_area(pedestrian), "m²"
    ),
    rbind(
      charte_feature_ids("land_use", land_use, pedestrian_land_use),
      charte_feature_ids("road_footprints", road_footprints, soft_footprints)
    ),
    pedestrian
  )
  shaded <- charte_intersection(canopy, pedestrian)
  add(
    charte_input_row(
      "espaces_publics_ombrages", "Espaces publics piétons sous canopée", "CV-1",
      "arbres UMEP", "Canopée à maturité ∩ espaces publics piétons",
      NA_integer_, charte_area(shaded), "m²",
      note = "Ombre portée du bâti non comptée (simulation SOLWEIG à intégrer)."
    ),
    NULL,
    shaded
  )

  noise_buffers <- parameters$noise_buffer_m
  description <- as.character(road_footprints$Descr)
  noisy <- !is.na(description) & description %in% names(noise_buffers)
  noise_zone <- if (any(noisy)) {
    charte_intersection(
      charte_union(sf::st_buffer(
        sf::st_geometry(road_footprints)[noisy],
        unname(noise_buffers[description[noisy]])
      )),
      reference
    )
  } else {
    sf::st_sfc(crs = charte_metric_crs)
  }
  add(
    charte_input_row(
      "zone_bruit", "Zone exposée au bruit routier", "CV-4", "road_footprints",
      paste0(
        "Tampons : ", paste0(names(noise_buffers), " ", noise_buffers, " m", collapse = ", ")
      ),
      sum(noisy), charte_area(noise_zone), "m²", "À vérifier",
      "Tampons forfaitaires par profil de voie, sans modélisation acoustique."
    ),
    charte_feature_ids("road_footprints", road_footprints, noisy),
    noise_zone
  )
  sensitive <- charte_has_class(land_use, "equipement_sensible")
  sensitive_noise <- sensitive & lengths(sf::st_intersects(land_use, noise_zone)) > 0
  add(
    charte_input_row(
      "equipements_sensibles_bruit", "Équipements sensibles en zone de bruit", "CV-4",
      "land_use", "ch_sensible = 1, touchant la zone de bruit",
      sum(sensitive_noise), sum(sensitive_noise), "entités",
      note = paste0(sum(sensitive), " équipements sensibles au total.")
    ),
    charte_feature_ids("land_use", land_use, sensitive_noise),
    NULL
  )

  # Ressource en eau : toitures.
  buildings <- layers$buildings
  roof_area <- if (is.null(buildings)) 0 else sum(as.numeric(sf::st_area(sf::st_make_valid(buildings))))
  add(
    charte_input_row(
      "toitures", "Emprise des toitures", "TB-3", "buildings",
      "Σ emprise des bâtiments du scénario", if (is.null(buildings)) 0 else nrow(buildings),
      roof_area, "m²"
    ),
    charte_feature_ids("buildings", buildings),
    NULL
  )

  # Résilience : réserve foncière, exposition du bâti à l'aléa fort.
  land_use_input(
    "reserve_fonciere", "Réserve foncière", "RES-1a", "reserve_fonciere",
    "Aucune réserve par défaut : mettre ch_reserve = 1 dans QGIS sur les entités concernées."
  )
  residential <- if (is.null(buildings)) logical() else charte_residential_building(buildings)
  exposed <- if (is.null(buildings) || !length(hazard_geometry)) {
    rep(FALSE, length(residential))
  } else {
    residential & lengths(sf::st_intersects(sf::st_make_valid(buildings), hazard_geometry)) > 0
  }
  add(
    charte_input_row(
      "batiments_habitat", "Bâtiments d'habitation", "RES-3", "buildings",
      "Produits RM, RC, RV, RT (hors RDC commercial)", sum(residential),
      sum(residential), "bâtiments", kind = "attribut"
    ),
    charte_feature_ids("buildings", buildings, residential),
    NULL
  )
  add(
    charte_input_row(
      "batiments_habitat_alea", "Bâtiments d'habitation en aléa fort", "RES-3", "buildings",
      "Bâtiments d'habitation touchant la zone d'aléa fort", sum(exposed),
      sum(exposed), "bâtiments"
    ),
    charte_feature_ids("buildings", buildings, exposed),
    NULL
  )
  sensitive_hazard <- sensitive & lengths(sf::st_intersects(land_use, hazard_geometry)) > 0
  add(
    charte_input_row(
      "equipements_sensibles_alea", "Équipements sensibles en aléa fort", "RES-3",
      "land_use", "ch_sensible = 1, touchant la zone d'aléa fort",
      sum(sensitive_hazard), sum(sensitive_hazard), "entités"
    ),
    charte_feature_ids("land_use", land_use, sensitive_hazard),
    NULL
  )

  input_table <- do.call(rbind, unname(inputs))
  rownames(input_table) <- NULL
  list(
    inputs = input_table,
    features = features,
    geometries = geometries,
    land_use = land_use,
    road_footprints = road_footprints,
    reference = reference,
    indicators = charte_indicator_preview(input_table),
    controls = charte_spatial_controls(layers, land_use)
  )
}

# Aperçu des indicateurs déductibles des seuls intrants spatiaux, sans
# notation (le barème sera porté par le référentiel).
charte_indicator_preview <- function(inputs) {
  value <- stats::setNames(inputs$value, inputs$input_id)
  ratio <- function(numerator, denominator, scale = 1) {
    if (is.na(value[[denominator]]) || value[[denominator]] <= 0) return(NA_real_)
    value[[numerator]] / value[[denominator]] * scale
  }
  rows <- list(
    c("TB-1", "Aléa fort valorisé en usages compatibles", "alea_fort_valorise", "alea_fort", "%", "≥ 20 % (référentiel) / ≥ 80 % (charte)"),
    c("TB-2", "Surface perméable / éco-aménagée", "surface_ecoamenagee", "surface_reference", "%", "≥ 30 %"),
    c("TV-1", "Espaces verts publics par habitant", "espaces_verts_publics", "population", "m²/hab", "≥ 1 m²/hab"),
    c("TV-2", "Couverture de canopée à maturité", "canopee", "surface_reference", "%", "≥ 20 %"),
    c("TV-3", "Agriculture nourricière par habitant", "agriculture", "population", "m²/hab", "≥ 3 m²/hab"),
    c("VC-1b", "Densité d'habitants nette", "population", "surface_habitat", "hab/ha", "320–480 hab/ha"),
    c("VC-2", "Surface à moins de 500 m du panier de services", "panier_services", "surface_reference", "%", "≥ 80 %"),
    c("VC-4", "Part du linéaire en cheminements doux", "lineaire_modes_doux", "lineaire_voirie", "%", "≥ 80 %"),
    c("CV-1", "Ombrage des espaces publics piétons", "espaces_publics_ombrages", "espaces_publics_pietons", "%", "≥ 50 % (idéal ≥ 75 %)"),
    c("CV-4", "Surface exposée au bruit", "zone_bruit", "surface_reference", "%", "≤ 10 %"),
    c("RES-1a", "Réserve foncière", "reserve_fonciere", "surface_reference", "%", "3–5 %")
  )
  scale_for <- function(unit) switch(unit, "%" = 100, "hab/ha" = 1e4, 1)
  preview <- do.call(rbind, lapply(rows, function(row) {
    data.frame(
      code = row[[1]], label = row[[2]], numerator = row[[3]], denominator = row[[4]],
      value = ratio(row[[3]], row[[4]], scale_for(row[[5]])), unit = row[[5]],
      target = row[[6]], stringsAsFactors = FALSE
    )
  }))
  exposed <- value[["batiments_habitat_alea"]]
  total <- value[["batiments_habitat"]]
  rbind(preview, data.frame(
    code = "RES-3", label = "Bâtiments d'habitation hors aléa fort",
    numerator = "batiments_habitat_alea", denominator = "batiments_habitat",
    value = if (is.na(total) || total <= 0) NA_real_ else (total - exposed) / total * 100,
    unit = "%", target = "100 %", stringsAsFactors = FALSE
  ))
}

# Contrôles d'information : ils ne bloquent aucun calcul.
charte_spatial_controls <- function(layers, land_use) {
  rows <- list()
  for (name in names(layers)) {
    layer <- layers[[name]]
    if (is.null(layer) || !nrow(layer)) next
    invalid <- !sf::st_is_valid(layer)
    invalid[is.na(invalid)] <- TRUE
    if (any(invalid)) {
      rows[[length(rows) + 1L]] <- data.frame(
        control = "Géométrie invalide", layer = name, fid = as.integer(layer$fid[invalid]),
        detail = "Corrigée à la volée pour les calculs.", stringsAsFactors = FALSE
      )
    }
  }
  empty <- land_use$charte_empty_values > 0
  if (any(empty)) {
    rows[[length(rows) + 1L]] <- data.frame(
      control = "Colonnes charte vides (règle par défaut appliquée)", layer = "land_use",
      fid = as.integer(land_use$fid[empty]),
      detail = paste0("Layer : ", land_use$Layer[empty], " — ", land_use$charte_empty_values[empty], " valeur(s) vide(s)"),
      stringsAsFactors = FALSE
    )
  }
  unclassified <- land_use$charte_classes_resolved == "" & land_use$coef_biotope_resolved == 0
  if (any(unclassified)) {
    rows[[length(rows) + 1L]] <- data.frame(
      control = "Occupation du sol sans classe charte (toutes les colonnes ch_* à 0)",
      layer = "land_use", fid = as.integer(land_use$fid[unclassified]),
      detail = paste0("Layer : ", land_use$Layer[unclassified]), stringsAsFactors = FALSE
    )
  }
  overlaps <- sf::st_overlaps(land_use)
  for (index in seq_along(overlaps)) {
    others <- overlaps[[index]][overlaps[[index]] > index]
    for (other in others) {
      shared <- charte_area(suppressWarnings(sf::st_intersection(
        sf::st_geometry(land_use)[index], sf::st_geometry(land_use)[other]
      )))
      if (shared < 1) next
      rows[[length(rows) + 1L]] <- data.frame(
        control = "Chevauchement d'occupations du sol", layer = "land_use",
        fid = as.integer(land_use$fid[index]),
        detail = paste0(
          "Avec fid ", land_use$fid[other], " : ", format(round(shared), big.mark = " "), " m²"
        ),
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) {
    return(data.frame(control = character(), layer = character(), fid = integer(), detail = character()))
  }
  do.call(rbind, rows)
}

# Synthèse du classement de l'occupation du sol par valeur de `Layer`.
charte_land_use_rule_summary <- function(land_use) {
  attributes <- sf::st_drop_geometry(land_use)
  key <- paste(attributes$Layer, attributes$charte_classes_resolved,
               attributes$coef_biotope_resolved, attributes$charte_rule_source, sep = "\r")
  groups <- split(seq_len(nrow(attributes)), key)
  summary <- do.call(rbind, lapply(groups, function(index) {
    first <- index[[1]]
    data.frame(
      Layer = attributes$Layer[[first]],
      classes = paste(
        vapply(charte_split_classes(attributes$charte_classes_resolved[[first]]),
               charte_class_column, character(1)),
        collapse = ", "
      ),
      coef_biotope = attributes$coef_biotope_resolved[[first]],
      source = attributes$charte_rule_source[[first]],
      n_features = length(index),
      area_sqm = sum(attributes$area_sqm[index]),
      stringsAsFactors = FALSE
    )
  }))
  rownames(summary) <- NULL
  summary[order(summary$Layer, summary$source), ]
}

# Liste exportable des entités d'un intrant, avec leurs attributs utiles.
charte_input_feature_table <- function(result, layers, input_id) {
  ids <- result$features[[input_id]]
  if (is.null(ids) || !nrow(ids)) {
    return(data.frame(layer = character(), fid = integer(), description = character(),
                      area_sqm = numeric(), length_m = numeric()))
  }
  rows <- lapply(split(ids, ids$layer), function(part) {
    name <- part$layer[[1]]
    layer <- if (name == "land_use") result$land_use else if (name == "road_footprints") {
      result$road_footprints
    } else {
      layers[[name]]
    }
    layer <- layer[match(part$fid, layer$fid), ]
    attributes <- sf::st_drop_geometry(layer)
    description <- switch(
      name,
      land_use = paste0(attributes$Layer, " — ", charte_land_use_code_label(layer)),
      road_footprints = paste0(
        attributes$Descr, " — ", charte_soft_mobility_column, " = ",
        as.integer(attributes$modes_doux_resolved)
      ),
      roads = paste0(attributes$highway, ifelse(is.na(attributes$name), "", paste0(" — ", attributes$name))),
      buildings = paste0(attributes$building_id, " — ", attributes$product_id),
      quartiers = paste0(attributes$code, " — population ", attributes$population),
      flood_areas = paste0(attributes$flood_type, " — ", attributes$basin),
      rep("", nrow(attributes))
    )
    types <- as.character(sf::st_geometry_type(layer))
    polygonal <- types %in% c("POLYGON", "MULTIPOLYGON")
    data.frame(
      layer = name, fid = part$fid, description = description,
      area_sqm = ifelse(polygonal, as.numeric(sf::st_area(layer)), NA_real_),
      length_m = ifelse(polygonal, NA_real_, as.numeric(sf::st_length(layer))),
      stringsAsFactors = FALSE
    )
  })
  table <- do.call(rbind, unname(rows))
  rownames(table) <- NULL
  table
}

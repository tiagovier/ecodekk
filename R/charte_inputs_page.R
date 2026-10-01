# Page « Intrants de la charte » : interface et branchement serveur. Les
# calculs sont faits par R/charte_inputs.R ; cette page lit un instantané de
# spatial.gpkg en lecture seule et n'écrit jamais dans le GeoPackage.

charte_layer_styles <- list(
  land_use = list(label = "Occupation du sol", color = "#2e7d32"),
  road_footprints = list(label = "Emprises de voirie", color = "#6a3d9a"),
  roads = list(label = "Axes de voirie OSM", color = "#1f78b4"),
  buildings = list(label = "Bâtiments", color = "#c0392b"),
  quartiers = list(label = "Quartiers", color = "#e67e22"),
  flood_areas = list(label = "Zones inondables", color = "#0277bd"),
  title_boundary = list(label = "Titre foncier", color = "#222222"),
  project_boundary = list(label = "Emprise du projet", color = "#222222")
)

charte_derived_color <- "#ff7f00"

charte_format_value <- function(value, unit) {
  digits <- ifelse(unit %in% c("m²", "m", "hab", "entités", "bâtiments"), 0, 1)
  vapply(seq_along(value), function(index) {
    format_number_fr(value[[index]], digits[[index]])
  }, character(1))
}

charte_inputs_tab <- function() {
  tabPanel(
    "Intrants de la charte",
    fluidPage(
      h2("Intrants spatiaux de la charte de performance"),
      p(
        class = "help-text",
        "Chaque ligne est un intrant mesuré sur le spatial.gpkg du scénario. Sélectionner une ligne pour voir sur la carte les entités retenues (en couleur), les autres entités des mêmes couches (en gris) et la géométrie calculée (en orange). Les identifiants fid permettent de retrouver les entités dans QGIS."
      ),
      p(
        class = "help-text",
        "Pour corriger un classement dans QGIS : attribut texte charte_classes (classes séparées par « ; », ou « aucune ») et attribut numérique coef_biotope sur land_use ; attribut modes_doux (0/1) sur road_footprints. Enregistrer dans QGIS puis utiliser « Relire spatial.gpkg »."
      ),
      fluidRow(
        column(
          3,
          selectInput(
            "charte_reference_area", "Surface de référence",
            choices = charte_reference_area_choices
          )
        ),
        column(
          4,
          selectInput(
            "charte_hazard_source", "Zone d'aléa fort",
            choices = charte_hazard_choices
          )
        ),
        column(
          5,
          div(
            style = "margin-top: 25px;",
            actionButton("charte_reload", "Relire spatial.gpkg"),
            downloadButton("charte_download_inputs", "Intrants (CSV)")
          )
        )
      ),
      uiOutput("charte_status"),
      tabsetPanel(
        id = "charte_inputs_view",
        tabPanel(
          "Intrants et carte",
          DTOutput("charte_inputs_table"),
          h3(textOutput("charte_selected_title")),
          uiOutput("charte_selected_note"),
          leafletOutput("charte_inputs_map", height = "560px"),
          h3("Entités de l'intrant sélectionné"),
          p(class = "help-text", "Cliquer sur une ligne pour centrer la carte sur l'entité."),
          downloadButton("charte_download_features", "Entités (CSV)"),
          DTOutput("charte_features_table")
        ),
        tabPanel(
          "Aperçu des indicateurs",
          p(
            class = "help-text",
            "Ratios déduits des seuls intrants spatiaux, sans notation. Les cibles affichées sont celles du référentiel ; les intrants marqués « À vérifier » restent provisoires."
          ),
          DTOutput("charte_indicators_table")
        ),
        tabPanel(
          "Classement de l'occupation du sol",
          p(
            class = "help-text",
            "Classes charte et coefficient de biotope retenus pour chaque valeur de Layer, avec leur origine : règle par défaut, valeur saisie dans QGIS ou entité non classée."
          ),
          DTOutput("charte_land_use_rules_table")
        ),
        tabPanel(
          "Contrôles",
          p(
            class = "help-text",
            "Signalements d'information : ils n'empêchent aucun calcul."
          ),
          DTOutput("charte_controls_table")
        )
      )
    )
  )
}

charte_inputs_server <- function(input, output, session, scenario) {
  reload_count <- reactiveVal(0L)
  observeEvent(input$charte_reload, reload_count(reload_count() + 1L))

  snapshot <- reactive({
    req(scenario$path)
    scenario$loaded_spatial_signature
    reload_count()
    paths <- scenario_bundle_paths(scenario$path)
    withProgress(message = "Lecture des couches de la charte", value = 0.2, {
      layers <- read_charte_spatial_layers(paths$spatial)
      incProgress(0.3, detail = "Canopée des arbres UMEP")
      tree_path <- find_umep_tree_layer(scenario$path)
      trees <- if (is.null(tree_path)) NULL else load_umep_trees(tree_path)
      canopy <- charte_canopy_geometry(trees)
      list(
        layers = layers,
        canopy = canopy,
        tree_path = tree_path,
        signature = scenario_component_signature(paths$spatial),
        read_at = Sys.time()
      )
    })
  })

  result <- reactive({
    data <- snapshot()
    parameters <- charte_default_parameters()
    parameters$reference_area <- input$charte_reference_area
    parameters$hazard_source <- input$charte_hazard_source
    withProgress(message = "Calcul des intrants de la charte", value = 0.5, {
      compute_charte_spatial_inputs(data$layers, data$canopy, parameters)
    })
  })

  output$charte_status <- renderUI({
    data <- snapshot()
    stale <- !identical(data$signature, scenario$loaded_spatial_signature)
    tagList(
      p(
        class = "help-text",
        paste0(
          "spatial.gpkg lu le ", format(data$read_at, "%d/%m/%Y à %H:%M:%S"),
          ". Arbres : ",
          if (is.null(data$tree_path)) "aucun inventaire UMEP, canopée non calculée." else
            umep_tree_source_label(data$tree_path)
        )
      ),
      if (stale) {
        div(
          class = "alert alert-warning",
          "spatial.gpkg a été modifié depuis le chargement du scénario. Cette page utilise la version relue ; utiliser « Recharger » dans le tableau de bord pour mettre à jour les autres onglets."
        )
      }
    )
  })

  inputs_display <- reactive({
    inputs <- result()$inputs
    data.frame(
      Intrant = inputs$label,
      Indicateurs = inputs$indicators,
      Couche = inputs$layer,
      `Règle de sélection` = inputs$rule,
      Entités = ifelse(is.na(inputs$n_features), "", as.character(inputs$n_features)),
      Valeur = charte_format_value(inputs$value, inputs$unit),
      Unité = inputs$unit,
      ha = ifelse(inputs$unit == "m²", format_number_fr(inputs$value / 1e4, 2), ""),
      Statut = inputs$status,
      check.names = FALSE
    )
  })

  output$charte_inputs_table <- renderDT({
    datatable(
      inputs_display(),
      rownames = FALSE,
      selection = list(mode = "single", selected = 1L),
      options = list(dom = "t", paging = FALSE, scrollX = TRUE)
    )
  })

  selected_input <- reactive({
    inputs <- result()$inputs
    row <- input$charte_inputs_table_rows_selected
    if (!length(row)) row <- 1L
    inputs[row[[1]], ]
  })

  output$charte_selected_title <- renderText(selected_input()$label)

  output$charte_selected_note <- renderUI({
    row <- selected_input()
    if (!nzchar(row$note)) return(NULL)
    p(class = "help-text", row$note)
  })

  feature_table <- reactive({
    charte_input_feature_table(result(), snapshot()$layers, selected_input()$input_id)
  })

  layer_with_rules <- function(name) {
    current <- result()
    if (name == "land_use") return(current$land_use)
    if (name == "road_footprints") return(current$road_footprints)
    snapshot()$layers[[name]]
  }

  output$charte_inputs_map <- renderLeaflet({
    current <- result()
    row <- selected_input()
    ids <- current$features[[row$input_id]]
    derived <- current$geometries[[row$input_id]]
    reference <- sf::st_transform(current$reference, 4326)

    map <- leaflet() |>
      addTiles(
        urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
        attribution = "© contributeurs OpenStreetMap"
      )
    if (length(reference)) {
      map <- map |>
        addPolylines(
          data = sf::st_cast(reference, "MULTILINESTRING"),
          color = "#222222", weight = 2, dashArray = "6 4",
          group = "Surface de référence"
        )
    }
    groups <- "Surface de référence"
    for (name in unique(ids$layer)) {
      layer <- layer_with_rules(name)
      if (is.null(layer) || !nrow(layer)) next
      style <- charte_layer_styles[[name]]
      if (is.null(style)) style <- list(label = name, color = "#333333")
      layer <- sf::st_transform(sf::st_make_valid(layer), 4326)
      selected <- layer$fid %in% ids$fid[ids$layer == name]
      label <- paste0(style$label, " — fid ", layer$fid)
      if (name == "land_use") {
        label <- paste0(label, " — ", layer$Layer, " — ", layer$charte_rule_source)
      } else if (name == "road_footprints") {
        label <- paste0(label, " — ", layer$Descr)
      } else if (name == "roads") {
        label <- paste0(label, " — ", layer$highway)
      } else if (name == "buildings") {
        label <- paste0(label, " — ", layer$product_id)
      } else if (name == "quartiers") {
        label <- paste0(label, " — ", layer$code)
      }
      linear <- all(as.character(sf::st_geometry_type(layer)) %in% c("LINESTRING", "MULTILINESTRING"))
      excluded_group <- paste0(style$label, " (autres)")
      if (linear) {
        if (any(!selected)) {
          map <- map |>
            addPolylines(
              data = layer[!selected, ], color = "#9e9e9e", weight = 1.5,
              label = label[!selected], group = excluded_group
            )
        }
        if (any(selected)) {
          map <- map |>
            addPolylines(
              data = layer[selected, ], color = style$color, weight = 3.5,
              label = label[selected], group = style$label
            )
        }
      } else {
        if (any(!selected)) {
          map <- map |>
            addPolygons(
              data = layer[!selected, ], color = "#9e9e9e", weight = 0.8,
              fillColor = "#bdbdbd", fillOpacity = 0.2,
              label = label[!selected], group = excluded_group
            )
        }
        if (any(selected)) {
          map <- map |>
            addPolygons(
              data = layer[selected, ], color = style$color, weight = 1.5,
              fillColor = style$color, fillOpacity = 0.45,
              label = label[selected], group = style$label
            )
        }
      }
      groups <- c(groups, style$label, excluded_group)
    }
    if (!is.null(derived) && length(derived)) {
      derived <- sf::st_transform(derived, 4326)
      types <- as.character(sf::st_geometry_type(derived))
      if (all(types %in% c("LINESTRING", "MULTILINESTRING"))) {
        map <- map |>
          addPolylines(data = derived, color = charte_derived_color, weight = 4,
                       group = "Géométrie calculée")
      } else {
        map <- map |>
          addPolygons(data = derived, color = charte_derived_color, weight = 2,
                      fillColor = charte_derived_color, fillOpacity = 0.25,
                      group = "Géométrie calculée")
      }
      groups <- c(groups, "Géométrie calculée")
    }
    bounds <- sf::st_bbox(if (length(reference)) reference else sf::st_transform(current$land_use, 4326))
    map |>
      addLayersControl(overlayGroups = groups, options = layersControlOptions(collapsed = TRUE)) |>
      fitBounds(bounds[["xmin"]], bounds[["ymin"]], bounds[["xmax"]], bounds[["ymax"]])
  })

  output$charte_features_table <- renderDT({
    table <- feature_table()
    datatable(
      data.frame(
        Couche = table$layer,
        fid = table$fid,
        Description = table$description,
        `Surface (m²)` = format_number_fr(table$area_sqm, 0),
        `Longueur (m)` = format_number_fr(table$length_m, 0),
        check.names = FALSE
      ),
      rownames = FALSE,
      selection = "single",
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  observeEvent(input$charte_features_table_rows_selected, {
    table <- feature_table()
    row <- input$charte_features_table_rows_selected
    if (!length(row)) return()
    layer <- layer_with_rules(table$layer[[row]])
    feature <- layer[layer$fid == table$fid[[row]], ]
    if (!nrow(feature)) return()
    bounds <- sf::st_bbox(sf::st_transform(feature, 4326))
    leafletProxy("charte_inputs_map") |>
      clearGroup("Entité sélectionnée") |>
      addPolygons(
        data = sf::st_transform(sf::st_buffer(feature, 3), 4326),
        color = "#ffff00", weight = 4, fill = FALSE, group = "Entité sélectionnée"
      ) |>
      fitBounds(bounds[["xmin"]], bounds[["ymin"]], bounds[["xmax"]], bounds[["ymax"]])
  })

  output$charte_indicators_table <- renderDT({
    preview <- result()$indicators
    labels <- stats::setNames(result()$inputs$label, result()$inputs$input_id)
    datatable(
      data.frame(
        Code = preview$code,
        Indicateur = preview$label,
        Numérateur = unname(labels[preview$numerator]),
        Dénominateur = unname(labels[preview$denominator]),
        Valeur = format_number_fr(preview$value, 1),
        Unité = preview$unit,
        Cible = preview$target,
        check.names = FALSE
      ),
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, scrollX = TRUE)
    )
  })

  output$charte_land_use_rules_table <- renderDT({
    summary <- charte_land_use_rule_summary(result()$land_use)
    datatable(
      data.frame(
        Layer = summary$Layer,
        `Classes charte` = summary$classes,
        `Coef. biotope` = format_number_fr(summary$coef_biotope, 2),
        Origine = summary$source,
        Entités = summary$n_features,
        `Surface (m²)` = format_number_fr(summary$area_sqm, 0),
        check.names = FALSE
      ),
      rownames = FALSE,
      options = list(dom = "ft", paging = FALSE, scrollX = TRUE)
    )
  })

  output$charte_controls_table <- renderDT({
    controls <- result()$controls
    datatable(
      data.frame(
        Contrôle = controls$control, Couche = controls$layer,
        fid = controls$fid, Détail = controls$detail,
        check.names = FALSE
      ),
      rownames = FALSE,
      options = list(pageLength = 25, scrollX = TRUE)
    )
  })

  output$charte_download_inputs <- downloadHandler(
    filename = function() paste0("charte_intrants_", scenario$id, ".csv"),
    content = function(file) {
      utils::write.csv(result()$inputs, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    }
  )

  output$charte_download_features <- downloadHandler(
    filename = function() paste0("charte_entites_", selected_input()$input_id, ".csv"),
    content = function(file) {
      utils::write.csv(feature_table(), file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    }
  )

  invisible(list(result = result, selected_input = selected_input, feature_table = feature_table))
}

library(shiny)
library(DT)
library(leaflet)

source(file.path("R", "initial_data.R"), encoding = "UTF-8")
source(file.path("R", "calculations.R"), encoding = "UTF-8")
source(file.path("R", "presentation.R"), encoding = "UTF-8")
source(file.path("R", "spatial_data.R"), encoding = "UTF-8")
source(file.path("R", "scenario_data.R"), encoding = "UTF-8")
source(file.path("R", "umep_results.R"), encoding = "UTF-8")
source(file.path("R", "umep_climate.R"), encoding = "UTF-8")
source(file.path("R", "umep_energy.R"), encoding = "UTF-8")

addResourcePath("branding", normalizePath("img", winslash = "/", mustWork = TRUE))

scenario_root <- file.path("data", "scenarios")
default_scenario_id <- normalize_scenario_id(Sys.getenv("ECODEKK_SCENARIO_ID", baseline_scenario_id))
scenario_trash_root <- file.path("data", "scenarios_supprimes")
default_scenario_directory <- file.path(scenario_root, default_scenario_id)
legacy_building_overrides_path <- file.path("data", "building_control_overrides.csv")

empty_level_allocations <- function() {
  data.frame(
    building_id = character(),
    level_number = integer(),
    product_id = character(),
    stringsAsFactors = FALSE
  )
}

initialize_scenario_from_references <- function(directory, scenario_id) {
  parcels <- load_parcel_boundaries(file.path("data", "sig", "projet-120526.gpkg"))
  osm <- load_osm_urban_data(file.path("data", "sig", "thies13.osm"))
  corrected <- load_corrected_buildings(file.path("data", "sig", "buildings.gpkg"))
  context <- load_project_context_layers(file.path("data", "sig"))
  buildings <- link_buildings_to_context(
    consolidate_buildings(corrected, osm$buildings),
    context$quartiers,
    context$land_use
  )
  legacy_overrides <- if (identical(scenario_id, "base")) {
    read_building_control_overrides(legacy_building_overrides_path)
  } else {
    data.frame()
  }
  buildings <- apply_building_control_overrides(buildings, legacy_overrides)
  sources <- data.frame(
    scenario_layer = c(
      "buildings", "roads", "parcels", "quartiers", "land_use",
      "trees", "road_footprints", "flood_areas", "project_boundary", "title_boundary"
    ),
    reference_source = c(
      "data/sig/buildings.gpkg + data/sig/thies13.osm",
      "data/sig/thies13.osm",
      "data/sig/projet-120526.gpkg",
      "data/sig/quartiers.gpkg",
      "data/sig/landuse.gpkg",
      "data/sig/arbres.gpkg",
      "data/sig/emprises_voirie.gpkg",
      "data/sig/inond_litmineur.gpkg + bassins_ret50.gpkg + bassins_cuvettes_ret50.gpkg",
      "data/sig/emprise_projet.gpkg",
      "data/sig/emprise_titrefoncier.gpkg"
    ),
    stringsAsFactors = FALSE
  )
  model <- scenario_model(
    scenario_id = scenario_id,
    reference_building_count = nrow(buildings),
    sources = sources,
    construction_categories = initial_construction_categories(),
    products = initial_products(),
    districts = initial_districts(),
    program = initial_program(),
    development_expenses = initial_development_expenses(),
    financial_assumptions = initial_financial_assumptions(),
    height_assumptions = initial_height_assumptions(),
    building_level_allocations = empty_level_allocations()
  )
  write_scenario_bundle(
    directory, model, buildings, osm$roads, parcels, context
  )
  read_scenario_bundle(directory)
}

scenario_id_from_directory <- function(directory) {
  normalize_scenario_id(basename(normalizePath(directory, winslash = "/", mustWork = FALSE)))
}

scenario_choice_values <- function(inventory) {
  stats::setNames(inventory$path, inventory$label)
}

initial_scenario_inventory <- discover_scenario_bundles(scenario_root)
initial_scenario_choices <- scenario_choice_values(visible_scenario_inventory(initial_scenario_inventory))
if (!length(initial_scenario_choices)) {
  initial_scenario_choices <- stats::setNames(
    default_scenario_directory, default_scenario_id
  )
}
default_scenario_selection <- if (
  normalizePath(default_scenario_directory, winslash = "/", mustWork = FALSE) %in%
    unname(initial_scenario_choices)
) {
  normalizePath(default_scenario_directory, winslash = "/", mustWork = FALSE)
} else {
  unname(initial_scenario_choices)[1]
}

show_building_control <- tolower(Sys.getenv("ECODEKK_SHOW_BUILDING_CONTROL", "true")) %in%
  c("true", "1", "yes", "oui")

building_control_tab <- if (show_building_control) {
  tabPanel(
    "Contrôle bâtiments",
    fluidPage(
      h2("Contrôle des bâtiments"),
      p(
        class = "help-text",
        "Utiliser le filtre pour afficher les bâtiments à contrôler, les signalements de cohérence ou l’ensemble des bâtiments du scénario."
      ),
      selectInput(
        "building_review_filter", "Bâtiments affichés",
        choices = c(
          "À contrôler" = "control", "Signalés" = "flagged",
          "Tous les bâtiments" = "all"
        ),
        selected = "control"
      ),
      uiOutput("building_control_status"),
      fluidRow(
        column(
          7,
          h3("Bâtiments affichés"),
          p(class = "help-text", "Double-cliquer sur une valeur éditable. La variation d’emprise, positive ou négative, redimensionne réellement le polygone autour de son centre et l’enregistre dans le GeoPackage. Les corrections sont enregistrées automatiquement."),
          DTOutput("building_review_table")
        ),
        column(
          5,
          h3("Localisation"),
          p(class = "help-text", "Rouge : problème bloquant ; orange : signalement de cohérence ; gris clair : bâtiment validé."),
          uiOutput("building_control_map_ui")
        )
      ),
      h3("Affectation par niveau du bâtiment sélectionné"),
      p(
        class = "help-text",
        "Pour RC2 et RT1, le RDC est commercial ou de services et les niveaux supérieurs sont résidentiels."
      ),
      DTOutput("building_level_table")
    )
  )
} else {
  NULL
}

thermal_comfort_tab <- tabPanel(
  "Confort thermique",
  fluidPage(
    h2("Confort thermique et ombrage"),
    uiOutput("thermal_status"),
    umep_thermal_reading_guide(),
    fluidRow(
      column(
        3,
        selectInput("thermal_day", "Journée", choices = NULL),
        selectInput("thermal_vegetation", "Végétation", choices = NULL),
        radioButtons(
          "thermal_indicator", "Indicateur",
          choices = stats::setNames(names(umep_indicator_labels), umep_indicator_labels)
        ),
        uiOutput("thermal_legend"),
        uiOutput("thermal_guidance"),
        uiOutput("thermal_notes")
      ),
      column(9, uiOutput("thermal_map"))
    ),
    h3("Indicateurs par quartier"),
    plotOutput("thermal_bar", height = "340px"),
    p(class = "help-text",
      "Chaque barre est la valeur de l’indicateur choisi pour un contexte du quartier : voirie (emprises de voirie), îlots, zones inondables. Comparez les quartiers entre eux, puis changez la végétation pour voir ce que les arbres modifient."),
    DTOutput("thermal_table"),
    h3("Profil journalier"),
    plotOutput("thermal_profile", height = "360px"),
    p(class = "help-text",
      "Tmrt médiane heure par heure, selon l’exposition. L’écart entre la courbe rouge (plein soleil) et la courbe verte (sous les houppiers) est le bénéfice de l’ombre des arbres ; la courbe pointillée verte montre les mêmes emplacements sans arbres. La nuit, les courbes se rejoignent : le gain est surtout diurne.")
  )
)

# Carte MapLibre d'une grille colorée (propriétés color, masque, label),
# alimentée par le message « grid-data » ; affichage seulement.
grid_map_ui <- function(container_id, quartiers, grid) {
  districts_geojson <- sf_to_geojson(quartiers[, "district_label"])
  bounds <- as.numeric(sf::st_bbox(grid))
  javascript <- sprintf(
    paste0(
      "(function(){var id='%s';function init(){",
      "if(typeof maplibregl==='undefined'){setTimeout(init,100);return;}",
      "if(window.ecodekkGridMaps[id]){try{window.ecodekkGridMaps[id].remove();}catch(e){}}",
      "var districts=%s;var bounds=%s;",
      "var map=new maplibregl.Map({container:id,bounds:bounds,fitBoundsOptions:{padding:20},",
      "style:{version:8,sources:{osm:{type:'raster',tiles:['https://tile.openstreetmap.org/{z}/{x}/{y}.png'],",
      "tileSize:256,attribution:'© contributeurs OpenStreetMap'}},layers:[{id:'osm',type:'raster',source:'osm'}]}});",
      "window.ecodekkGridMaps[id]=map;",
      "map.addControl(new maplibregl.NavigationControl(),'top-left');",
      "map.addControl(new maplibregl.ScaleControl({unit:'metric'}));",
      "map.on('load',function(){",
      "map.addSource('grid',{type:'geojson',data:{type:'FeatureCollection',features:[]}});",
      "map.addLayer({id:'grid-fill',type:'fill',source:'grid',filter:['==',['get','masque'],false],",
      "paint:{'fill-color':['get','color'],'fill-opacity':0.8}});",
      "map.addLayer({id:'grid-hit',type:'fill',source:'grid',paint:{'fill-color':'#000','fill-opacity':0}});",
      "map.addLayer({id:'grid-line',type:'line',source:'grid',paint:{'line-color':'#666','line-width':0.5}});",
      "map.addSource('quartiers',{type:'geojson',data:districts});",
      "map.addLayer({id:'quartiers',type:'line',source:'quartiers',paint:{'line-color':'#222','line-width':1.8,'line-dasharray':[4,3]}});",
      "map.on('click','grid-hit',function(e){new maplibregl.Popup().setLngLat(e.lngLat)",
      ".setText(e.features[0].properties.cell_id+' : '+e.features[0].properties.label).addTo(map);});",
      "map.ecodekkReady=true;window.ecodekkApplyGrid(id);",
      "});}init();})();"
    ),
    container_id, districts_geojson, jsonlite::toJSON(list(bounds[1:2], bounds[3:4]))
  )
  tagList(
    div(id = container_id),
    tags$script(HTML(gsub("</", "<\\/", javascript, fixed = TRUE)))
  )
}

urban_climate_tab <- tabPanel(
  "Climat urbain",
  fluidPage(
    h2("Climat urbain : îlot de chaleur"),
    uiOutput("climate_status"),
    uiOutput("climate_guide"),
    fluidRow(
      column(
        3,
        selectInput("climate_day", "Journée", choices = NULL),
        selectInput("climate_vegetation", "Végétation", choices = target_vegetation_choices),
        radioButtons("climate_indicator", "Indicateur", choices = target_indicator_choices()),
        uiOutput("climate_legend"),
        uiOutput("climate_notes")
      ),
      column(9, uiOutput("climate_map"))
    ),
    h3("Par quartier"),
    plotOutput("climate_bar", height = "320px"),
    p(class = "help-text",
      "Moyenne par quartier pondérée par la surface des mailles, mailles masquées exclues. En mode « effet des arbres », une barre négative est un rafraîchissement."),
    h3("Profil journalier"),
    selectInput("climate_quartier", "Quartier", choices = NULL),
    plotOutput("climate_profile", height = "340px"),
    p(class = "help-text",
      "Évolution horaire de l’indicateur dans le quartier, avec arbres (vert) et sans arbres (brun). Pour la température de l’air, la courbe pointillée est la référence rurale de TARGET : l’écart avec elle est l’îlot de chaleur.")
  )
)

energy_balance_tab <- tabPanel(
  "Bilan énergétique",
  fluidPage(
    h2("Bilan énergétique de surface"),
    uiOutput("energy_status"),
    uiOutput("energy_guide"),
    fluidRow(
      column(
        3,
        selectInput("energy_day", "Journée", choices = NULL),
        selectInput("energy_scenario", "Végétation", choices = energy_scenario_choices),
        radioButtons("energy_variable", "Variable à 14 h",
          choices = stats::setNames(energy_variables$id, energy_variables$label)),
        uiOutput("energy_legend"),
        uiOutput("energy_notes")
      ),
      column(9, uiOutput("energy_map"))
    ),
    h3("Cycle journalier du bilan d’énergie"),
    selectInput("energy_quartier", "Quartier", choices = NULL),
    plotOutput("energy_cycle", height = "360px"),
    p(class = "help-text",
      "Flux horaires moyens du quartier pour la journée choisie. Le jour, Q* (rayonnement net) se répartit entre QH, QE et ΔQS ; la nuit, ΔQS devient négatif : les matériaux restituent la chaleur stockée."),
    h3("Moyennes mensuelles"),
    plotOutput("energy_monthly", height = "320px"),
    p(class = "help-text",
      "Moyenne mensuelle de la variable choisie dans le quartier, avec arbres (vert) et sans arbres (brun), de janvier 2017 à janvier 2018.")
  )
)

ui <- navbarPage(
  title = div(
    class = "ecodekk-brand",
    tags$img(
      src = "branding/logo_phard_600x250.png", alt = "PHARD",
      class = "ecodekk-logo ecodekk-logo-phard"
    ),
    span(class = "ecodekk-title", "Projet ECODEKK"),
    tags$img(
      src = "branding/Safru.png", alt = "SAFRU",
      class = "ecodekk-logo ecodekk-logo-safru"
    )
  ),
  id = "section",
  header = tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://unpkg.com/maplibre-gl@5.6.0/dist/maplibre-gl.css"
    ),
    tags$script(src = "https://unpkg.com/maplibre-gl@5.6.0/dist/maplibre-gl.js"),
    tags$script(HTML(
      "window.ecodekkApplyThermalOverlay = function() {
         var map = window.ecodekkThermalMap, m = window.ecodekkThermalOverlay;
         if (!map || !m || !map.ecodekkReady) return;
         var source = map.getSource('solweig');
         if (m.url && !source) {
           map.addSource('solweig', {type: 'image', url: m.url, coordinates: m.coordinates});
           map.addLayer({id: 'solweig', type: 'raster', source: 'solweig',
             paint: {'raster-opacity': 0.85, 'raster-resampling': 'nearest'}}, 'batiments');
         } else if (m.url) {
           source.updateImage({url: m.url, coordinates: m.coordinates});
           map.setLayoutProperty('solweig', 'visibility', 'visible');
         } else if (source) {
           map.setLayoutProperty('solweig', 'visibility', 'none');
         }
       };
       window.ecodekkGridMaps = window.ecodekkGridMaps || {};
       window.ecodekkGridData = window.ecodekkGridData || {};
       window.ecodekkApplyGrid = function(id) {
         var map = window.ecodekkGridMaps[id], data = window.ecodekkGridData[id];
         if (!map || !data || !map.ecodekkReady) return;
         map.getSource('grid').setData(data);
       };
       Shiny.addCustomMessageHandler('grid-data', function(message) {
         window.ecodekkGridData[message.map] = typeof message.data === 'string' ? JSON.parse(message.data) : message.data;
         window.ecodekkApplyGrid(message.map);
       });
       Shiny.addCustomMessageHandler('umep-overlay', function(message) {
         window.ecodekkThermalOverlay = message;
         window.ecodekkApplyThermalOverlay();
       });"
    )),
    tags$script(HTML(
      "Shiny.addCustomMessageHandler('scenario-loading', function(message) {
         var overlay = document.getElementById('scenario-loading-overlay');
         if (!overlay) return;
         overlay.style.display = message.loading ? 'flex' : 'none';
         var label = overlay.querySelector('.scenario-loading-label');
         if (label && message.label) label.textContent = message.label;
       });"
    )),
    tags$style(HTML(
      ".navbar {min-height:70px;}
       .navbar-brand {height:70px;padding:8px 15px;display:flex;align-items:center;}
       .navbar-nav > li > a {padding-top:25px;padding-bottom:25px;}
       .ecodekk-brand {display:flex;align-items:center;gap:10px;}
       .ecodekk-title {font-weight:700;white-space:nowrap;}
       .ecodekk-logo {display:block;object-fit:contain;background:#fff;padding:2px 4px;border-radius:3px;}
       .ecodekk-logo-phard {height:42px;width:auto;}
       .ecodekk-logo-safru {height:46px;width:52px;}
       .kpi {padding:16px; margin:8px 0; background:#f5f5f5; border-left:4px solid #2c6e49;}\n       .kpi-value {font-size:24px; font-weight:700;}\n       .help-text {color:#666; margin-bottom:12px;}\n       table.dataTable td {white-space: nowrap;}
       .map3d-wrapper {position:relative;}
       #building_map_3d {height:680px;}
       .map3d-legend {position:absolute;right:10px;bottom:30px;z-index:2;background:rgba(255,255,255,.94);padding:10px 12px;max-height:330px;max-width:330px;overflow:auto;box-shadow:0 1px 4px rgba(0,0,0,.3);}
       .map3d-layers {position:absolute;right:10px;top:10px;z-index:3;background:rgba(255,255,255,.96);padding:9px 11px;max-height:310px;overflow:auto;box-shadow:0 1px 4px rgba(0,0,0,.3);}
       .map3d-layers label {display:block;font-weight:400;margin:3px 0;}
       .map3d-layers summary, .map3d-legend summary {font-weight:700;white-space:nowrap;}
       .map3d-legend-row {display:flex;align-items:center;gap:7px;margin:3px 0;}
       .map3d-swatch {width:12px;height:12px;display:inline-block;}
       .map3d-swatch-round {border-radius:50%;}
       .map3d-legend-trees {right:auto;left:10px;bottom:40px;max-width:280px;}
       .map3d-legend-note {margin:6px 0 0;font-size:11px;color:#555;}
       .map3d-code {min-width:42px;font-weight:700;}
       #thermal_map_canvas {height:620px;}
       #climate_map_canvas, #energy_map_canvas {height:560px;}
       .thermal-legend-bar {height:14px;border:1px solid #bbb;margin:4px 0 2px;}
       .thermal-legend-ticks {display:flex;justify-content:space-between;font-size:11px;color:#444;}
       .thermal-note {font-size:12px;color:#555;margin-top:10px;}
       .umep-guide {background:#f7f9f8;border:1px solid #dfe9e3;border-left:4px solid #2c6e49;padding:8px 14px;margin:6px 0 16px;}
       .umep-guide summary {font-weight:700;cursor:pointer;color:#2c6e49;}
       .umep-guide h4 {font-size:15px;margin:12px 0 4px;}
       .umep-guide p, .umep-guide li {font-size:13px;}
       .map3d-view-controls {position:absolute;left:50px;top:10px;z-index:3;display:flex;gap:5px;}
       .map3d-view-controls button {background:rgba(255,255,255,.96);border:1px solid #bbb;border-radius:3px;padding:6px 10px;font-weight:600;box-shadow:0 1px 4px rgba(0,0,0,.2);}
       #scenario-loading-overlay {position:fixed;inset:0;z-index:99999;display:flex;align-items:center;justify-content:center;flex-direction:column;gap:14px;background:rgba(255,255,255,.94);font-size:18px;color:#2c6e49;}
       .scenario-spinner {width:46px;height:46px;border:5px solid #dfe9e3;border-top-color:#2c6e49;border-radius:50%;animation:scenario-spin .9s linear infinite;}
       @keyframes scenario-spin {to {transform:rotate(360deg);}}
       .scenario-controls {padding:12px 15px;margin-bottom:15px;background:#f5f5f5;border:1px solid #ddd;border-radius:4px;}"
    ))
  ),
  tabPanel(
    "Tableau de bord",
    fluidPage(
      div(
        id = "scenario-loading-overlay",
        div(class = "scenario-spinner"),
        div(class = "scenario-loading-label", "Chargement du scénario…")
      ),
      h2("Tableau de bord"),
      div(
        class = "scenario-controls",
        fluidRow(
          column(
            7,
            selectInput(
              "scenario_file", "Scénario",
              choices = initial_scenario_choices,
              selected = default_scenario_selection
            ),
            p(
              class = "help-text",
              "Chaque scénario est un dossier : spatial.gpkg pour QGIS, model.rds pour les hypothèses et calculs."
            )
          ),
          column(
            5,
            br(),
            actionButton("new_scenario", "Nouveau scénario"),
            actionButton("delete_scenario", "Supprimer un scénario", class = "btn-danger"),
            downloadButton(
              "download_scenario", "Télécharger le scénario",
              class = "btn-success"
            ),
            actionButton("refresh_scenarios", "Actualiser la liste"),
            actionButton("reload_scenario", "Recharger", class = "btn-primary")
          )
        ),
        p(class = "help-text", "Avant de fermer une session publiée, téléchargez le scénario pour conserver toutes les modifications et poursuivre le travail localement."),
        uiOutput("scenario_status")
      ),
      uiOutput("model_warning"),
      fluidRow(
        column(3, div(class = "kpi", "SDP totale", div(class = "kpi-value", textOutput("kpi_sdp")))),
        column(3, div(class = "kpi", "Cessions HT", div(class = "kpi-value", textOutput("kpi_land")))),
        column(3, div(class = "kpi", "Coûts de construction HT", div(class = "kpi-value", textOutput("kpi_construction")))),
        column(3, div(class = "kpi", "Résultat d’opération HT", div(class = "kpi-value", textOutput("kpi_result"))))
      ),
      h3("Inventaire cartographique recalculé"),
      fluidRow(
        column(3, div(class = "kpi", "Bâtiments inclus", div(class = "kpi-value", textOutput("kpi_map_buildings")))),
        column(3, div(class = "kpi", "Emprise bâtie", div(class = "kpi-value", textOutput("kpi_map_footprint")))),
        column(3, div(class = "kpi", "SDP cartographique", div(class = "kpi-value", textOutput("kpi_map_sdp")))),
        column(3, div(class = "kpi", "Signalements", div(class = "kpi-value", textOutput("kpi_map_flags"))))
      ),
      h3("Synthèse par quartier"),
      DTOutput("district_summary")
    )
  ),
  tabPanel(
    "Hypothèses",
    fluidPage(
      h2("Hypothèses"),
      fluidRow(
        column(
          4,
          h3("Catégories de construction"),
          p(class = "help-text", "Double-cliquer sur un coût pour le modifier."),
          DTOutput("categories_table")
        ),
        column(
          4,
          h3("Taux du bilan"),
          numericInput("vat_rate", "TVA (%)", 20, min = 0, step = 0.1),
          numericInput("sales_fee_rate", "Frais sur ventes (%)", 3, min = 0, step = 0.1),
          numericInput("financing_cost_rate", "Frais financiers (%)", 5, min = 0, step = 0.1),
          numericInput("safru_fee_rate", "Rémunération SAFRU (%)", 3, min = 0, step = 0.1)
        ),
        column(
          4,
          h3("Hypothèses de hauteur"),
          numericInput("ground_floor_height", "Hauteur du RDC (m)", 4, min = 0.1, step = 0.1),
          numericInput("upper_floor_height", "Hauteur d’un étage courant (m)", 3, min = 0.1, step = 0.1),
          numericInput("default_building_levels", "Niveaux par défaut si absents", 1, min = 1, step = 1)
        )
      ),
      hr(),
      h3("Produits immobiliers et fonciers"),
      p(class = "help-text", "Double-cliquer dans le tableau pour modifier une hypothèse. La SDP/unité décrit un logement ou local ; la SDP moyenne par bâtiment représente un bâtiment complet, tous niveaux compris. Les quantités sont consultables dans la programmation et la carte. Les colonnes finales sont non éditables. Valeurs textuelles : Catégorie ou Dérogatoire ; Simplifiée, Manuelle ou Bilan promoteur ; Oui ou Non."),
      fluidRow(
        column(
          6,
          selectInput("product_id", "Produit", choices = NULL),
          selectInput("product_category", "Catégorie de construction", choices = NULL),
          numericInput("product_sale_price", "Prix de vente HT (CFA/m²)", 0, min = 0),
          selectInput(
            "product_cost_mode", "Mode du coût de construction",
            choices = c("Hérité de la catégorie" = "inherit", "Dérogatoire" = "override")
          ),
          numericInput("product_cost_override", "Coût dérogatoire (CFA/m² SDP)", 0, min = 0)
        ),
        column(
          6,
          selectInput(
            "product_land_method", "Méthode de charge foncière",
            choices = c(
              "Simplifiée" = "simplified", "Manuelle" = "manual",
              "Bilan promoteur — non disponible" = "promoter_balance"
            )
          ),
          numericInput("product_land_factor", "Facteur de charge foncière", 1.45, min = 0.01, step = 0.01),
          numericInput("product_manual_land", "Charge foncière manuelle (CFA/m²)", 0),
          numericInput("product_sdp", "SDP par unité de référence (m²)", 0, min = 0, step = 0.01),
          numericInput("product_land_area", "Surface de terrain par unité (m²)", 0, min = 0, step = 0.01),
          numericInput("product_default_levels", "Nombre de niveaux par défaut", 1, min = 1, step = 1),
          checkboxInput("product_uses_units_per_level", "Compter les logements par niveau", FALSE),
          numericInput("product_units_per_level", "Logements par niveau", 1, min = 1, step = 1),
          checkboxInput("product_ground_floor_commercial", "RDC commercial ou de services", FALSE),
          selectInput(
            "product_ground_floor_product", "Produit affecté au RDC",
            choices = c("Aucun produit distinct" = "")
          ),
          checkboxInput("product_cessible", "Produit cessible", TRUE),
          actionButton("save_product", "Appliquer au produit", class = "btn-primary")
        )
      ),
      fluidRow(
        column(12, DTOutput("products_table"))
      )
    )
  ),
  tabPanel(
    "Programmation",
    fluidPage(
      h2("Programmation"),
      uiOutput("program_source_selector"),
      uiOutput("program_source_help"),
      selectInput("program_district", "Quartier", choices = NULL),
      p(class = "help-text", "Double-cliquer sur une quantité pour la modifier. Le SDP par unité provient de la table Produits."),
      DTOutput("program_table")
    )
  ),
  tabPanel(
    "Bilan d’aménagement",
    fluidPage(
      h2("Bilan d’aménagement"),
      p(class = "help-text", "Montants HT, TVA et TTC. La structure reprend le bilan de référence."),
      DTOutput("balance_table")
    )
  ),
  tabPanel(
    "Cartographie",
    fluidPage(
      h2("Plan masse et volumétrie"),
      p(
        class = "help-text",
        "Vue 2D et 3D du scénario spatial actif : bâtiments, quartiers, voiries, emprises et zones inondables."
      ),
      h3("Carte du scénario"),
      uiOutput("building_3d_map")
    )
  ),
  building_control_tab,
  navbarMenu(
    "Analyses et simulations",
    thermal_comfort_tab,
    urban_climate_tab,
    energy_balance_tab
  )
)

server <- function(input, output, session) {
  empty_allocations <- empty_level_allocations()
  state <- reactiveValues(
    categories = initial_construction_categories(),
    products = initial_products(),
    districts = initial_districts(),
    program = initial_program(),
    expenses = initial_development_expenses(),
    assumptions = initial_financial_assumptions(),
    height_assumptions = initial_height_assumptions(),
    buildings = NULL,
    level_product_overrides = empty_allocations,
    program_source = "manual"
  )
  scenario <- reactiveValues(
    id = NULL,
    path = NULL,
    roads = NULL,
    parcels = NULL,
    context = NULL,
    sources = data.frame(scenario_layer = character(), reference_source = character()),
    reference_building_count = 0L,
    loaded_spatial_signature = NA_character_,
    loaded_model_signature = NA_character_,
    loading = TRUE,
    status = "Chargement du scénario…",
    error = NULL
  )
  scenario_inventory <- reactiveVal(initial_scenario_inventory)

  apply_scenario_data <- function(data, directory) {
    model <- data$model
    validate_scenario_model(model)
    products <- ensure_product_building_assumptions(model$products)
    products$is_cessible <- as.logical(products$is_cessible)

    scenario$id <- scenario_id_from_directory(directory)
    scenario$path <- normalizePath(directory, winslash = "/", mustWork = TRUE)
    scenario$roads <- data$spatial$roads
    scenario$parcels <- data$spatial$parcels
    trees <- resolve_scenario_trees(
      data$spatial, file.path("data", "sig", "arbres.gpkg")
    )
    umep_trees_path <- find_umep_tree_layer(directory)
    scenario$display_trees <- if (is.null(umep_trees_path)) NULL else load_umep_trees(umep_trees_path)
    scenario$tree_source_label <- if (is.null(umep_trees_path)) {
      "Arbres : inventaire CAO, essences non attribuées"
    } else {
      umep_tree_source_label(umep_trees_path)
    }
    scenario$context <- list(
      quartiers = data$spatial$quartiers,
      land_use = data$spatial$land_use,
      trees = trees,
      road_footprints = data$spatial$road_footprints,
      flood_areas = data$spatial$flood_areas,
      project_boundary = data$spatial$project_boundary,
      title_boundary = data$spatial$title_boundary
    )
    scenario$sources <- model$sources
    scenario$reference_building_count <- as.integer(model$reference_building_count)

    state$categories <- model$construction_categories
    state$products <- products
    state$districts <- model$districts
    state$program <- model$program
    state$program_source <- resolve_scenario_program_source(
      scenario$id,
      model$program_source
    )
    state$expenses <- model$development_expenses
    state$assumptions <- model$financial_assumptions
    state$height_assumptions <- model$height_assumptions
    buildings <- normalize_scenario_buildings(
      data$spatial$buildings, products, scenario_id_from_directory(directory)
    )
    buildings <- assign_building_districts(buildings, scenario$context$quartiers)
    state$buildings <- add_building_classification_status(buildings, products)
    state$level_product_overrides <- model$building_level_allocations

    updateNumericInput(session, "vat_rate", value = model$financial_assumptions$vat_rate * 100)
    updateNumericInput(session, "sales_fee_rate", value = model$financial_assumptions$sales_fee_rate * 100)
    updateNumericInput(session, "financing_cost_rate", value = model$financial_assumptions$financing_cost_rate * 100)
    updateNumericInput(session, "safru_fee_rate", value = model$financial_assumptions$safru_fee_rate * 100)
    updateNumericInput(session, "ground_floor_height", value = model$height_assumptions$ground_floor_height_m)
    updateNumericInput(session, "upper_floor_height", value = model$height_assumptions$upper_floor_height_m)
    updateNumericInput(session, "default_building_levels", value = model$height_assumptions$default_levels)
    if (identical(scenario$id, "base")) {
      freezeReactiveValue(input, "program_source")
      updateRadioButtons(session, "program_source", selected = state$program_source)
    }

    paths <- scenario_bundle_paths(scenario$path)
    scenario$loaded_spatial_signature <- scenario_component_signature(paths$spatial)
    scenario$loaded_model_signature <- scenario_component_signature(paths$model)
  }

  load_scenario <- function(directory) {
    scenario$loading <- TRUE
    scenario$error <- NULL
    scenario$status <- "Chargement du scénario…"
    session$sendCustomMessage(
      "scenario-loading",
      list(loading = TRUE, label = "Chargement du scénario…")
    )
    tryCatch(
      withProgress(message = "Chargement du scénario", value = 0, {
        inventory <- discover_scenario_bundles(scenario_root)
        scenario_inventory(inventory)
        known_paths <- inventory$path
        requested <- normalizePath(directory, winslash = "/", mustWork = FALSE)
        default_requested <- normalizePath(
          default_scenario_directory, winslash = "/", mustWork = FALSE
        )
        paths <- scenario_bundle_paths(requested)
        bundle_complete <- all(vapply(
          paths[c("spatial", "model", "manifest")], file.exists, logical(1)
        ))
        if (!bundle_complete && identical(requested, default_requested)) {
          legacy_path <- file.path(requested, "scenario.gpkg")
          if (file.exists(legacy_path)) {
            incProgress(0.15, detail = "Migration du scénario existant")
            data <- migrate_legacy_scenario(legacy_path, requested)
          } else {
            incProgress(0.15, detail = "Initialisation depuis les références")
            data <- initialize_scenario_from_references(requested, default_scenario_id)
          }
        } else {
          if (!requested %in% known_paths) {
            stop("Le scénario sélectionné n’est pas un dossier de scénario valide.")
          }
          incProgress(0.25, detail = "Lecture du modèle et des couches SIG")
          data <- read_scenario_bundle(requested)
        }
        incProgress(0.55, detail = "Application des hypothèses et couches")
        apply_scenario_data(data, requested)
        updateSelectInput(
          session, "scenario_file",
          choices = scenario_choice_values(visible_scenario_inventory(discover_scenario_bundles(scenario_root))),
          selected = scenario$path
        )
        scenario$status <- paste0("Scénario chargé : ", scenario$id)
        incProgress(0.2, detail = "Calculs prêts")
      }),
      error = function(error) {
        scenario$error <- conditionMessage(error)
        scenario$status <- "Échec du chargement du scénario"
        showNotification(conditionMessage(error), type = "error", duration = NULL)
      },
      finally = {
        scenario$loading <- FALSE
        session$sendCustomMessage(
          "scenario-loading",
          list(loading = FALSE, label = scenario$status)
        )
      }
    )
  }

  run_scenario_load_in_session <- function(path) {
    shiny::withReactiveDomain(
      session,
      shiny::isolate(load_scenario(path))
    )
  }

  schedule_scenario_load <- function(path) {
    scenario$loading <- TRUE
    scenario$error <- NULL
    scenario$status <- "Chargement du scénario…"
    session$sendCustomMessage(
      "scenario-loading",
      list(loading = TRUE, label = "Chargement du scénario…")
    )
    later::later(
      function() {
        if (isTRUE(session$isClosed())) return()
        run_scenario_load_in_session(path)
      },
      delay = 0.05
    )
  }

  refresh_scenario_choices <- function(selected = isolate(input$scenario_file)) {
    inventory <- discover_scenario_bundles(scenario_root)
    scenario_inventory(inventory)
    choices <- scenario_choice_values(visible_scenario_inventory(inventory))
    if (!length(choices)) {
      choices <- stats::setNames(default_scenario_directory, default_scenario_id)
    }
    if (is.null(selected) || !selected %in% unname(choices)) {
      selected <- unname(choices)[1]
    }
    updateSelectInput(session, "scenario_file", choices = choices, selected = selected)
    invisible(inventory)
  }

  observeEvent(input$refresh_scenarios, {
    inventory <- refresh_scenario_choices()
    showNotification(
      paste0(nrow(inventory), " scénario(s) disponible(s)."),
      type = "message"
    )
  })

  observeEvent(input$reload_scenario, {
    req(input$scenario_file)
    refresh_scenario_choices(input$scenario_file)
    schedule_scenario_load(input$scenario_file)
  })

  session$onFlushed(
    function() schedule_scenario_load(default_scenario_selection),
    once = TRUE
  )

  output$scenario_status <- renderUI({
    if (scenario$loading) {
      return(div(class = "text-info", "Chargement en cours…"))
    }
    if (!is.null(scenario$error)) {
      return(div(class = "alert alert-danger", scenario$error))
    }
    req(scenario$path)
    div(
      class = "alert alert-success",
      paste0(
        "Scénario actif : ", scenario$id,
        " — ", basename(scenario$path),
        " — ", nrow(state$buildings), " bâtiments."
      )
    )
  })

  current_program <- function() {
    if (!identical(state$program_source, "buildings") || is.null(state$buildings)) {
      return(state$program)
    }
    buildings <- calculate_osm_building_heights(
      state$buildings,
      ground_floor_height_m = state$height_assumptions$ground_floor_height_m,
      upper_floor_height_m = state$height_assumptions$upper_floor_height_m,
      default_levels = state$height_assumptions$default_levels,
      products = state$products
    ) |>
      add_building_classification_status(state$products)
    calculate_program_from_buildings(
      buildings, state$products, state$program, state$level_product_overrides
    )
  }

  current_scenario_model <- function() {
    scenario_model(
      scenario_id = scenario$id,
      reference_building_count = scenario$reference_building_count,
      sources = scenario$sources,
      construction_categories = state$categories,
      products = state$products,
      districts = state$districts,
      program = current_program(),
      development_expenses = state$expenses,
      financial_assumptions = state$assumptions,
      height_assumptions = state$height_assumptions,
      building_level_allocations = state$level_product_overrides,
      program_source = state$program_source
    )
  }

  persist_active_scenario <- function(write_spatial = FALSE) {
    if (scenario$loading) return(invisible(FALSE))
    if (is.null(state$buildings) || is.null(scenario$context) ||
        is.null(scenario$parcels) || is.null(scenario$roads) ||
        is.null(scenario$path)) {
      return(invisible(FALSE))
    }
    paths <- scenario_bundle_paths(scenario$path)
    current_model_signature <- scenario_component_signature(paths$model)
    if (!identical(current_model_signature, scenario$loaded_model_signature)) {
      stop(
        "Le modèle du scénario a été modifié hors de l’application. ",
        "Recharger le scénario avant d’enregistrer une nouvelle modification."
      )
    }
    if (isTRUE(write_spatial)) {
      current_spatial_signature <- scenario_component_signature(paths$spatial)
      if (!identical(current_spatial_signature, scenario$loaded_spatial_signature)) {
        stop(
          "Le GeoPackage spatial a été modifié dans QGIS. ",
          "Recharger le scénario avant d’enregistrer une modification spatiale."
        )
      }
      write_scenario_bundle(
        directory = scenario$path,
        model = current_scenario_model(),
        buildings = state$buildings,
        roads = scenario$roads,
        parcels = scenario$parcels,
        context = scenario$context
      )
    } else {
      write_scenario_model_files(
        directory = scenario$path,
        model = current_scenario_model(),
        building_count = nrow(state$buildings)
      )
    }
    if (isTRUE(write_spatial)) {
      scenario$loaded_spatial_signature <- scenario_component_signature(paths$spatial)
    }
    scenario$loaded_model_signature <- scenario_component_signature(paths$model)
    invisible(TRUE)
  }

  persist_or_notify <- function(success_message = NULL, write_spatial = FALSE) {
    if (scenario$loading) return(FALSE)
    tryCatch(
      {
        saved <- persist_active_scenario(write_spatial = write_spatial)
        if (isTRUE(saved) && !is.null(success_message)) {
          showNotification(success_message, type = "message")
        }
        isTRUE(saved)
      },
      error = function(error) {
        showNotification(conditionMessage(error), type = "error", duration = NULL)
        FALSE
      }
    )
  }

  visible_scenario_choices <- function() {
    scenario_choice_values(visible_scenario_inventory(discover_scenario_bundles(scenario_root)))
  }

  baseline_scenario_path <- function() {
    normalizePath(file.path(scenario_root, baseline_scenario_id), winslash = "/", mustWork = FALSE)
  }

  observeEvent(input$new_scenario, {
    req(!scenario$loading)
    choices <- visible_scenario_choices()
    selected <- if (baseline_scenario_path() %in% unname(choices)) baseline_scenario_path() else unname(choices)[1]
    suggested_id <- paste0("scenario_", format(Sys.time(), "%Y%m%d_%H%M"))
    showModal(modalDialog(
      title = "Créer un nouveau scénario",
      selectInput("new_scenario_source", "À partir du scénario", choices = choices, selected = selected),
      textInput(
        "new_scenario_id", "Identifiant du nouveau scénario",
        value = suggested_id
      ),
      p(
        class = "help-text",
        paste0(
          "Le nouveau scénario est une copie de l’état enregistré du scénario choisi (",
          baseline_scenario_id, " par défaut, scénario de référence). Son programme est recalculé depuis les bâtiments SIG. ",
          "Les résultats des études UMEP ne sont pas copiés : ils restent propres à leur scénario."
        )
      ),
      footer = tagList(
        modalButton("Annuler"),
        actionButton("confirm_new_scenario", "Créer", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$confirm_new_scenario, {
    req(input$new_scenario_id, input$new_scenario_source, !scenario$loading)
    tryCatch(
      {
        new_path <- duplicate_scenario_bundle(
          source_directory = input$new_scenario_source,
          root = scenario_root,
          scenario_id = trimws(input$new_scenario_id)
        )
        removeModal()
        refresh_scenario_choices(new_path)
        schedule_scenario_load(new_path)
        showNotification("Nouveau scénario créé.", type = "message")
      },
      error = function(error) {
        showNotification(conditionMessage(error), type = "error", duration = NULL)
      }
    )
  })

  observeEvent(input$delete_scenario, {
    req(!scenario$loading)
    choices <- visible_scenario_choices()
    choices <- choices[unname(choices) != baseline_scenario_path()]
    if (!length(choices)) {
      showNotification("Aucun scénario supprimable : le scénario de référence est protégé.", type = "warning")
      return()
    }
    showModal(modalDialog(
      title = "Supprimer un scénario",
      selectInput("delete_scenario_path", "Scénario à supprimer", choices = choices),
      textInput("delete_scenario_confirm", "Pour confirmer, saisissez l’identifiant du scénario"),
      p(
        class = "help-text",
        paste0(
          "Le scénario de référence ", baseline_scenario_id, " ne peut pas être supprimé. ",
          "Le dossier supprimé est déplacé dans data/scenarios_supprimes/ et peut être restauré manuellement."
        )
      ),
      footer = tagList(
        modalButton("Annuler"),
        actionButton("confirm_delete_scenario", "Supprimer", class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$confirm_delete_scenario, {
    req(input$delete_scenario_path, !scenario$loading)
    tryCatch(
      {
        label <- scenario_label_from_path(input$delete_scenario_path, scenario_root)
        if (!identical(trimws(input$delete_scenario_confirm), label)) {
          stop("L’identifiant saisi ne correspond pas au scénario à supprimer.")
        }
        was_loaded <- identical(
          normalizePath(input$delete_scenario_path, winslash = "/", mustWork = TRUE),
          scenario$path
        )
        delete_scenario_bundle(input$delete_scenario_path, scenario_root, scenario_trash_root)
        removeModal()
        if (was_loaded) {
          refresh_scenario_choices(baseline_scenario_path())
          schedule_scenario_load(baseline_scenario_path())
        } else {
          refresh_scenario_choices(scenario$path)
        }
        showNotification(paste0("Scénario « ", label, " » supprimé."), type = "message")
      },
      error = function(error) {
        showNotification(conditionMessage(error), type = "error", duration = NULL)
      }
    )
  })

  output$download_scenario <- downloadHandler(
    filename = function() {
      req(scenario$id)
      paste0(
        "ecodekk_", scenario$id, "_", format(Sys.Date(), "%Y%m%d"), ".zip"
      )
    },
    contentType = "application/zip",
    content = function(file) {
      req(scenario$id, !scenario$loading, state$buildings)
      write_scenario_archive(
        zipfile = file,
        scenario_id = scenario$id,
        model = current_scenario_model(),
        buildings = state$buildings,
        roads = scenario$roads,
        parcels = scenario$parcels,
        context = scenario$context
      )
    }
  )

  observe({
    category_choices <- setNames(state$categories$category_id, state$categories$category_label)
    product_choices <- setNames(state$products$product_id, state$products$product_label)
    ground_floor_choices <- c("Aucun produit distinct" = "", product_choices)
    district_choices <- c(
      "Tous les quartiers" = "all",
      setNames(state$districts$district_id, state$districts$district_label)
    )
    selected_product_category <- isolate(input$product_category)
    selected_product <- isolate(input$product_id)
    selected_district <- isolate(input$program_district)
    updateSelectInput(
      session, "product_category", choices = category_choices,
      selected = if (is.null(selected_product_category)) state$categories$category_id[1] else selected_product_category
    )
    updateSelectInput(
      session, "product_id", choices = product_choices,
      selected = if (is.null(selected_product)) state$products$product_id[1] else selected_product
    )
    updateSelectInput(
      session, "product_ground_floor_product", choices = ground_floor_choices
    )
    updateSelectInput(
      session, "program_district", choices = district_choices,
      selected = if (is.null(selected_district)) "all" else selected_district
    )
  })

  output$categories_table <- renderDT({
    data <- data.frame(
      Code = state$categories$category_id,
      Catégorie = state$categories$category_label,
      `Coût de construction (CFA/m² SDP)` = state$categories$construction_cost_cfa_sqm,
      check.names = FALSE
    )
    datatable(
      data,
      rownames = FALSE,
      editable = list(target = "cell", disable = list(columns = c(0, 1))),
      options = list(dom = "t", paging = FALSE)
    ) |>
      formatCurrency(
        "Coût de construction (CFA/m² SDP)", currency = " CFA/m²",
        before = FALSE, digits = 0, mark = " ", dec.mark = ","
      )
  })

  observeEvent(input$categories_table_cell_edit, {
    info <- input$categories_table_cell_edit
    if (info$col == 2) {
      value <- suppressWarnings(as.numeric(info$value))
      if (is.na(value) || value < 0) {
        showNotification("Le coût doit être un nombre positif ou nul.", type = "error")
      } else {
        categories <- state$categories
        categories$construction_cost_cfa_sqm[info$row] <- value
        state$categories <- categories
        persist_or_notify()
      }
    }
  })

  observe({
    req(input$product_id)
    row <- match(input$product_id, state$products$product_id)
    product <- state$products[row, ]
    updateSelectInput(session, "product_category", selected = product$construction_category_id)
    updateNumericInput(session, "product_sale_price", value = ifelse(is.na(product$sale_price_cfa_sqm), 0, product$sale_price_cfa_sqm))
    updateSelectInput(session, "product_cost_mode", selected = product$construction_cost_mode)
    updateNumericInput(session, "product_cost_override", value = ifelse(is.na(product$construction_cost_override_cfa_sqm), 0, product$construction_cost_override_cfa_sqm))
    updateSelectInput(session, "product_land_method", selected = product$land_charge_method)
    updateNumericInput(session, "product_land_factor", value = ifelse(is.na(product$land_charge_factor), 1, product$land_charge_factor))
    updateNumericInput(session, "product_manual_land", value = ifelse(is.na(product$manual_land_charge_cfa_sqm), 0, product$manual_land_charge_cfa_sqm))
    updateNumericInput(session, "product_sdp", value = product$sdp_per_unit)
    updateNumericInput(session, "product_land_area", value = product$land_area_per_unit)
    updateNumericInput(session, "product_default_levels", value = product$default_building_levels)
    updateCheckboxInput(session, "product_uses_units_per_level", value = product$uses_units_per_level)
    updateNumericInput(session, "product_units_per_level", value = product$units_per_level)
    updateCheckboxInput(
      session, "product_ground_floor_commercial",
      value = isTRUE(product$ground_floor_commercial)
    )
    updateSelectInput(
      session, "product_ground_floor_product",
      selected = ifelse(is.na(product$ground_floor_product_id), "", product$ground_floor_product_id)
    )
    updateCheckboxInput(session, "product_cessible", value = product$is_cessible)
  })

  observeEvent(input$save_product, {
    req(input$product_id)
    row <- match(input$product_id, state$products$product_id)
    products <- state$products
    old_product <- products[row, , drop = FALSE]
    levels <- suppressWarnings(as.numeric(input$product_default_levels))
    units_per_level <- suppressWarnings(as.numeric(input$product_units_per_level))
    valid_counts <- !is.na(levels) && levels >= 1 && abs(levels - round(levels)) <= 1e-8 &&
      !is.na(units_per_level) && units_per_level >= 1 &&
      abs(units_per_level - round(units_per_level)) <= 1e-8
    if (!valid_counts) {
      showNotification("Les niveaux et logements par niveau doivent être des entiers strictement positifs.", type = "error")
      return()
    }
    products$construction_category_id[row] <- input$product_category
    products$sale_price_cfa_sqm[row] <- input$product_sale_price
    products$construction_cost_mode[row] <- input$product_cost_mode
    products$construction_cost_override_cfa_sqm[row] <- if (
      input$product_cost_mode == "override"
    ) input$product_cost_override else NA_real_
    products$land_charge_method[row] <- input$product_land_method
    products$land_charge_factor[row] <- if (
      input$product_land_method == "simplified"
    ) input$product_land_factor else NA_real_
    products$manual_land_charge_cfa_sqm[row] <- if (
      input$product_land_method == "manual"
    ) input$product_manual_land else NA_real_
    products$sdp_per_unit[row] <- input$product_sdp
    products$land_area_per_unit[row] <- input$product_land_area
    products$default_building_levels[row] <- as.integer(round(levels))
    products$uses_units_per_level[row] <- input$product_uses_units_per_level
    products$units_per_level[row] <- as.integer(round(units_per_level))
    products$ground_floor_commercial[row] <- input$product_ground_floor_commercial
    products$ground_floor_product_id[row] <- if (
      isTRUE(input$product_ground_floor_commercial) &&
        nzchar(input$product_ground_floor_product)
    ) input$product_ground_floor_product else NA_character_
    products$is_cessible[row] <- input$product_cessible
    state$products <- products
    building_assumptions_changed <-
      old_product$default_building_levels != products$default_building_levels[row] ||
      old_product$units_per_level != products$units_per_level[row] ||
      old_product$uses_units_per_level != products$uses_units_per_level[row]
    if (building_assumptions_changed) {
      state$buildings <- apply_product_building_assumptions(
        state$buildings,
        products$product_id[row],
        products$default_building_levels[row],
        products$units_per_level[row]
      )
    }
    persist_or_notify(write_spatial = building_assumptions_changed)
  })

  observe({
    req(
      input$vat_rate, input$sales_fee_rate,
      input$financing_cost_rate, input$safru_fee_rate
    )
    assumptions <- list(
      vat_rate = input$vat_rate / 100,
      sales_fee_rate = input$sales_fee_rate / 100,
      financing_cost_rate = input$financing_cost_rate / 100,
      safru_fee_rate = input$safru_fee_rate / 100
    )
    if (!identical(assumptions, state$assumptions)) {
      state$assumptions <- assumptions
      persist_or_notify()
    }
  })

  observe({
    req(input$ground_floor_height, input$upper_floor_height, input$default_building_levels)
    if (
      input$ground_floor_height > 0 &&
      input$upper_floor_height > 0 &&
      input$default_building_levels > 0
    ) {
      assumptions <- list(
        ground_floor_height_m = input$ground_floor_height,
        upper_floor_height_m = input$upper_floor_height,
        default_levels = input$default_building_levels
      )
      if (!identical(assumptions, state$height_assumptions)) {
        state$height_assumptions <- assumptions
        persist_or_notify()
      }
    }
  })

  all_buildings <- reactive({
    validate(need(!is.null(state$buildings), "L’inventaire des bâtiments ne peut pas être chargé."))
    calculate_osm_building_heights(
      state$buildings,
      ground_floor_height_m = state$height_assumptions$ground_floor_height_m,
      upper_floor_height_m = state$height_assumptions$upper_floor_height_m,
      default_levels = state$height_assumptions$default_levels,
      products = state$products
    ) |>
      add_building_classification_status(state$products)
  })

  urban_buildings <- reactive({
    buildings <- all_buildings()
    buildings[buildings$included_in_simulation, , drop = FALSE]
  })


  active_program <- reactive(current_program())

  model <- reactive({
    run_financial_model(
      state$categories,
      state$products,
      state$districts,
      active_program(),
      state$expenses,
      state$assumptions
    )
  })

  output$program_source_selector <- renderUI({
    req(scenario$id)
    if (!identical(scenario$id, "base")) {
      return(div(
        class = "alert alert-info",
        strong("Source de la programmation : bâtiments SIG"),
        tags$br(),
        "Cette source est verrouillée pour ce scénario."
      ))
    }
    radioButtons(
      "program_source", "Source de la programmation",
      choices = c(
        "Programme de référence (manuel)" = "manual",
        "Bâtiments SIG" = "buildings"
      ),
      selected = state$program_source, inline = TRUE
    )
  })

  output$program_source_help <- renderUI({
    if (identical(state$program_source, "buildings")) {
      div(
        class = "alert alert-info",
        "Les quantités et SDP des produits bâtis sont recalculées depuis leurs emprises, leurs niveaux et leurs codes produit. Les produits non représentés par un bâtiment conservent leurs hypothèses propres."
      )
    } else {
      p(class = "help-text", "Les quantités du tableau alimentent directement le modèle financier.")
    }
  })

  observeEvent(input$program_source, {
    req(input$program_source)
    if (!identical(scenario$id, "base")) {
      state$program_source <- "buildings"
      return()
    }
    if (scenario$loading || identical(input$program_source, state$program_source)) return()
    state$program_source <- input$program_source
    persist_or_notify("Source de programmation enregistrée.")
  }, ignoreInit = TRUE)

  output$model_warning <- renderUI({
    ev_rows <- model()$program[model()$program$product_id == "ev", ]
    ev_missing <- any(ev_rows$quantity > 0 & ev_rows$total_land_area <= 0)
    if (ev_missing) {
      div(class = "alert alert-warning", "Surface de terrain EV manquante : les recettes EV sont actuellement nulles.")
    }
  })

  output$kpi_sdp <- renderText({
    paste0(format_number_fr(sum(model()$program$total_sdp), 1), " m² SDP")
  })
  output$kpi_land <- renderText({ format_cfa(model()$balance$metrics["total_revenue_ht"]) })
  output$kpi_construction <- renderText({ format_cfa(sum(model()$program$construction_cost_ht)) })
  output$kpi_result <- renderText({ format_cfa(model()$balance$metrics["result_ht"]) })

  cartographic_kpis <- reactive({
    calculate_cartographic_kpis(all_buildings(), state$products)
  })
  output$kpi_map_buildings <- renderText({
    paste0(format_number_fr(cartographic_kpis()$building_count, 0), " unités")
  })
  output$kpi_map_footprint <- renderText({
    paste0(format_number_fr(cartographic_kpis()$footprint_area_sqm, 1), " m²")
  })
  output$kpi_map_sdp <- renderText({
    paste0(format_number_fr(cartographic_kpis()$estimated_sdp_sqm, 1), " m² SDP")
  })
  output$kpi_map_flags <- renderText({
    paste0(format_number_fr(cartographic_kpis()$flagged_building_count, 0), " bâtiments")
  })

  output$district_summary <- renderDT({
    data <- merge(state$districts, model()$districts, by = "district_id", all.x = TRUE, sort = FALSE)
    reference_cessions <- model()$balance$district_cessions
    data$land_revenue_ht <- reference_cessions$amount_ht[
      match(data$district_id, reference_cessions$district_id)
    ]
    data <- data[, c("district_label", "quantity", "total_sdp", "land_revenue_ht")]
    names(data) <- c("Quartier", "Unités", "SDP totale", "Cessions HT")
    datatable(data, rownames = FALSE, options = list(dom = "t", pageLength = 10)) |>
      formatRound("Unités", digits = 0, mark = " ", dec.mark = ",") |>
      formatRound("SDP totale", digits = 1, mark = " ", dec.mark = ",") |>
      formatCurrency("Cessions HT", currency = " CFA", before = FALSE, digits = 0, mark = " ", dec.mark = ",")
  })

  output$products_table <- renderDT({
    products <- state$products
    costs <- get_effective_construction_cost(products, state$categories)
    charges <- calculate_product_land_charge(products, costs)
    product_summary <- model()$products
    summary_index <- match(products$product_id, product_summary$product_id)
    summary_value <- function(field) {
      value <- product_summary[[field]][summary_index]
      replace(value, is.na(value), 0)
    }
    building_summary <- calculate_product_building_summary(
      urban_buildings(), products, state$level_product_overrides
    )
    building_summary_index <- match(products$product_id, building_summary$product_id)
    sdp_per_building <- building_summary$sdp_per_building_sqm[building_summary_index]
    data <- data.frame(
      Code = products$product_id,
      Produit = products$product_label,
      Catégorie = products$construction_category_id,
      `Prix de vente HT (CFA/m²)` = products$sale_price_cfa_sqm,
      `Mode du coût` = ifelse(products$construction_cost_mode == "override", "Dérogatoire", "Catégorie"),
      `Coût dérogatoire (CFA/m² SDP)` = products$construction_cost_override_cfa_sqm,
      `Coût effectif (CFA/m² SDP)` = costs,
      `Méthode de charge foncière` = c(
        simplified = "Simplifiée", manual = "Manuelle",
        promoter_balance = "Bilan promoteur"
      )[products$land_charge_method],
      `Facteur de charge foncière` = products$land_charge_factor,
      `Charge foncière manuelle (CFA/m² SDP)` = products$manual_land_charge_cfa_sqm,
      `Charge foncière effective (CFA/m² SDP)` = charges,
      `SDP/unité de référence (m²)` = products$sdp_per_unit,
      `Comptage des unités` = ifelse(products$uses_units_per_level, "Par niveau", "Par bâtiment"),
      `Logements/niveau` = products$units_per_level,
      `Terrain/unité (m²)` = products$land_area_per_unit,
      Cessible = ifelse(products$is_cessible, "Oui", "Non"),
      `Niveaux par défaut` = products$default_building_levels,
      `RDC commercial` = ifelse(products$ground_floor_commercial, "Oui", "Non"),
      `Produit du RDC` = products$ground_floor_product_id,
      `SDP moyenne par bâtiment (m²)` = sdp_per_building,
      `SDP totale du programme (m²)` = summary_value("total_sdp"),
      `Ventes immobilières HT (CFA)` = summary_value("sales_revenue_ht"),
      `Coût de construction HT (CFA)` = summary_value("construction_cost_ht"),
      `Cessions foncières HT (CFA)` = summary_value("land_revenue_ht"),
      check.names = FALSE
    )
    datatable(
      data,
      rownames = FALSE,
      editable = list(target = "cell", disable = list(columns = c(0, 1, 6, 10, 19:23))),
      options = list(scrollX = TRUE, pageLength = 8)
    ) |>
      formatCurrency(
        c(
          "Prix de vente HT (CFA/m²)", "Coût dérogatoire (CFA/m² SDP)",
          "Coût effectif (CFA/m² SDP)", "Charge foncière manuelle (CFA/m² SDP)",
          "Charge foncière effective (CFA/m² SDP)"
        ),
        currency = " CFA/m²", before = FALSE, digits = 0, mark = " ", dec.mark = ","
      ) |>
      formatRound(
        c("Facteur de charge foncière", "SDP/unité de référence (m²)", "Logements/niveau", "Terrain/unité (m²)"),
        digits = 2, mark = " ", dec.mark = ","
      ) |>
      formatRound(
        "SDP moyenne par bâtiment (m²)", digits = 1, mark = " ", dec.mark = ","
      ) |>
      formatRound(
        "SDP totale du programme (m²)", digits = 1, mark = " ", dec.mark = ","
      ) |>
      formatCurrency(
        c(
          "Ventes immobilières HT (CFA)", "Coût de construction HT (CFA)",
          "Cessions foncières HT (CFA)"
        ), currency = " CFA", before = FALSE, digits = 0, mark = " ", dec.mark = ","
      )
  })

  observeEvent(input$products_table_cell_edit, {
    info <- input$products_table_cell_edit
    products <- state$products
    row <- info$row
    if (row < 1 || row > nrow(products)) return()

    field_by_column <- c(
      `2` = "construction_category_id",
      `3` = "sale_price_cfa_sqm",
      `4` = "construction_cost_mode",
      `5` = "construction_cost_override_cfa_sqm",
      `7` = "land_charge_method",
      `8` = "land_charge_factor",
      `9` = "manual_land_charge_cfa_sqm",
      `11` = "sdp_per_unit",
      `12` = "uses_units_per_level",
      `13` = "units_per_level",
      `14` = "land_area_per_unit",
      `15` = "is_cessible",
      `16` = "default_building_levels",
      `17` = "ground_floor_commercial",
      `18` = "ground_floor_product_id"
    )
    field <- unname(field_by_column[as.character(info$col)])
    if (length(field) == 0 || is.na(field)) return()

    raw_value <- trimws(as.character(info$value))
    value <- raw_value
    valid <- TRUE
    message <- NULL

    if (field == "construction_category_id") {
      value <- toupper(raw_value)
      valid <- value %in% state$categories$category_id
      message <- "Catégorie inconnue. Utiliser ECO, STD, MID, HIGH ou VHIGH."
    } else if (field == "construction_cost_mode") {
      labels <- c(
        "catégorie" = "inherit", "categorie" = "inherit", "hérité" = "inherit",
        "herite" = "inherit", "inherit" = "inherit",
        "dérogatoire" = "override", "derogatoire" = "override", "override" = "override"
      )
      value <- unname(labels[tolower(raw_value)])
      valid <- length(value) == 1 && !is.na(value)
      message <- "Mode invalide. Utiliser Catégorie ou Dérogatoire."
    } else if (field == "land_charge_method") {
      labels <- c(
        "simplifiée" = "simplified", "simplifiee" = "simplified", "simplified" = "simplified",
        "manuelle" = "manual", "manual" = "manual",
        "bilan promoteur" = "promoter_balance", "promoter_balance" = "promoter_balance"
      )
      value <- unname(labels[tolower(raw_value)])
      valid <- length(value) == 1 && !is.na(value)
      message <- "Méthode invalide. Utiliser Simplifiée, Manuelle ou Bilan promoteur."
    } else if (field == "uses_units_per_level") {
      labels <- c(
        "par niveau" = TRUE, "oui" = TRUE, "true" = TRUE, "1" = TRUE,
        "par bâtiment" = FALSE, "par batiment" = FALSE,
        "non" = FALSE, "false" = FALSE, "0" = FALSE
      )
      value <- unname(labels[tolower(raw_value)])
      valid <- length(value) == 1 && !is.na(value)
      message <- "Valeur invalide. Utiliser Par niveau ou Par bâtiment."
    } else if (field == "ground_floor_product_id") {
      value <- if (nzchar(raw_value)) raw_value else NA_character_
      valid <- is.na(value) || value %in% products$product_id
      message <- "Le produit du RDC doit être un code produit existant ou rester vide."
    } else {
      normalized <- gsub(" ", "", raw_value, fixed = TRUE)
      normalized <- sub(",", ".", normalized, fixed = TRUE)
      value <- suppressWarnings(as.numeric(normalized))
      optional <- field %in% c(
        "construction_cost_override_cfa_sqm", "land_charge_factor",
        "manual_land_charge_cfa_sqm"
      )
      if (!nzchar(raw_value) && optional) {
        value <- NA_real_
        valid <- TRUE
      } else {
        valid <- !is.na(value)
      }
      if (field == "land_charge_factor" && !is.na(value)) valid <- value > 0
      if (field %in% c("default_building_levels", "units_per_level") && !is.na(value)) {
        valid <- value >= 1 && abs(value - round(value)) <= 1e-8
        value <- as.integer(round(value))
      }
      if (field != "manual_land_charge_cfa_sqm" && !is.na(value)) valid <- valid && value >= 0
      message <- "Saisir une valeur numérique valide. Seule la charge foncière manuelle peut être négative."
    }

    if (!valid) {
      showNotification(message, type = "error")
      return()
    }

    if (field == "construction_cost_mode" && value == "override" && is.na(products$construction_cost_override_cfa_sqm[row])) {
      products$construction_cost_override_cfa_sqm[row] <- get_effective_construction_cost(
        products[row, , drop = FALSE], state$categories
      )
    }
    if (field == "land_charge_method" && value == "simplified" && is.na(products$land_charge_factor[row])) {
      products$land_charge_factor[row] <- 1.45
    }
    if (field == "land_charge_method" && value == "manual" && is.na(products$manual_land_charge_cfa_sqm[row])) {
      products$manual_land_charge_cfa_sqm[row] <- 0
    }

    products[[field]][row] <- value
    if (field == "ground_floor_commercial" && !isTRUE(value)) {
      products$ground_floor_product_id[row] <- NA_character_
    }
    state$products <- products
    building_assumptions_changed <- field %in% c(
      "default_building_levels", "uses_units_per_level", "units_per_level"
    )
    if (building_assumptions_changed) {
      state$buildings <- apply_product_building_assumptions(
        state$buildings,
        products$product_id[row],
        products$default_building_levels[row],
        products$units_per_level[row]
      )
    }
    persist_or_notify(write_spatial = building_assumptions_changed)
  })

  program_row_indices <- reactive({
    program <- active_program()
    selected <- input$program_district
    if (is.null(selected) || selected == "all") {
      seq_len(nrow(program))
    } else {
      which(program$district_id == selected)
    }
  })

  output$program_table <- renderDT({
    data <- model()$program[program_row_indices(), , drop = FALSE]
    district_labels <- state$districts$district_label[match(data$district_id, state$districts$district_id)]
    display <- data.frame(
      Quartier = district_labels,
      Produit = data$product_label,
      Quantité = data$quantity,
      `SDP/unité` = data$effective_sdp_per_unit,
      `SDP totale` = data$total_sdp,
      `Cessions HT` = data$land_revenue_ht,
      check.names = FALSE
    )
    display <- rbind(
      display,
      data.frame(
        Quartier = "TOTAL", Produit = "", Quantité = sum(data$quantity, na.rm = TRUE),
        `SDP/unité` = NA_real_, `SDP totale` = sum(data$total_sdp, na.rm = TRUE),
        `Cessions HT` = sum(data$land_revenue_ht, na.rm = TRUE),
        check.names = FALSE
      )
    )
    datatable(
      display,
      rownames = FALSE,
      editable = list(target = "cell", disable = list(columns = c(0, 1, 3, 4, 5))),
      options = list(
        scrollX = TRUE, paging = FALSE, scrollY = "520px",
        rowCallback = JS(
          "function(row, data) {",
          "  if (data[0] === 'TOTAL') $(row).css({'font-weight':'bold','border-top':'2px solid #333'});",
          "}"
        )
      )
    ) |>
      formatRound("Quantité", digits = 0, mark = " ", dec.mark = ",") |>
      formatRound("SDP/unité", digits = 2, mark = " ", dec.mark = ",") |>
      formatRound("SDP totale", digits = 1, mark = " ", dec.mark = ",") |>
      formatCurrency("Cessions HT", currency = " CFA", before = FALSE, digits = 0, mark = " ", dec.mark = ",")
  })

  output$spatial_data_warning <- renderUI({
    if (is.null(state$buildings)) {
      return(div(
        class = "alert alert-danger",
        "Les bâtiments corrigés et les attributs OSM ne peuvent pas être consolidés."
      ))
    }
    if (is.null(scenario$context)) {
      return(div(
        class = "alert alert-danger",
        "Les couches contextuelles du projet ne peuvent pas être chargées."
      ))
    }

    missing_levels <- sum(is.na(state$buildings$levels), na.rm = TRUE)
    unmatched <- sum(state$buildings$attribute_match_quality == "non apparié", na.rm = TRUE)
    weak_matches <- sum(state$buildings$attribute_match_quality == "faible", na.rm = TRUE)
    preserved_osm <- sum(state$buildings$geometry_source == "OSM conservé", na.rm = TRUE)
    incongruities <- sum(state$buildings$has_incongruity, na.rm = TRUE)
    messages <- paste0(preserved_osm, " bâtiments OSM sans empreinte corrigée proche sont conservés pour contrôle.")
    if (unmatched > 0) {
      messages <- c(
        messages,
        paste0(
          unmatched,
          " empreintes corrigées sans correspondant OSM à moins de 30 m : fonction non renseignée."
        )
      )
    }
    if (weak_matches > 0) {
      messages <- c(
        messages,
        paste0(weak_matches, " appariements OSM faibles, entre 15 et 30 m, sont à vérifier.")
      )
    }
    if (missing_levels > 0) {
      messages <- c(
        messages,
        paste0(
          missing_levels,
          " bâtiments sans nombre de niveaux utilisent l’hypothèse de leur produit."
        )
      )
    }
    if (incongruities > 0) {
      messages <- c(
        messages,
        paste0(
          incongruities,
          " bâtiments portent un signalement non bloquant de cohérence."
        )
      )
    }
    if (length(messages)) {
      div(class = "alert alert-warning", paste(messages, collapse = " "))
    }
  })


  control_buildings <- reactive({
    buildings <- all_buildings()
    buildings[buildings$control_required, , drop = FALSE]
  })

  review_buildings <- reactive({
    buildings <- all_buildings()
    selected_filter <- input$building_review_filter
    if (is.null(selected_filter) || identical(selected_filter, "control")) {
      keep <- buildings$control_required
    } else if (identical(selected_filter, "flagged")) {
      keep <- buildings$control_required | buildings$has_incongruity
    } else {
      keep <- rep(TRUE, nrow(buildings))
    }
    keep[is.na(keep)] <- FALSE
    buildings[keep, , drop = FALSE]
  })

  output$building_control_status <- renderUI({
    buildings <- all_buildings()
    blocking <- sum(buildings$control_required, na.rm = TRUE)
    flagged <- sum(buildings$has_incongruity & !buildings$control_required, na.rm = TRUE)
    div(
      class = if (blocking == 0) "alert alert-success" else "alert alert-warning",
      paste0(
        blocking, " bâtiment(s) à contrôler ; ", flagged,
        " signalement(s) de cohérence ; ", nrow(review_buildings()),
        " bâtiment(s) affiché(s)."
      )
    )
  })

  output$building_control_map_ui <- renderUI({
    if (nrow(review_buildings()) == 0) return(NULL)
    leafletOutput("building_control_map", height = "520px")
  })

  output$building_control_map <- renderLeaflet({
    buildings <- review_buildings()
    approved_buildings <- all_buildings()
    approved_buildings <- approved_buildings[
      !approved_buildings$building_id %in% buildings$building_id &
        approved_buildings$included_in_simulation,
      ,
      drop = FALSE
    ]
    action_color <- ifelse(
      buildings$control_required, "#d7301f",
      ifelse(buildings$has_incongruity, "#f0ad00", "#7f8c8d")
    )
    validate(need(nrow(buildings) > 0, "Aucun bâtiment pour ce filtre."))
    bounds <- sf::st_bbox(all_buildings())

    leaflet() |>
      addTiles(
        urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
        attribution = "© contributeurs OpenStreetMap"
      ) |>
      addPolygons(
        data = scenario$context$land_use,
        color = "#6c8057",
        weight = 0.5,
        fillColor = "#9bb884",
        fillOpacity = 0.12,
        group = "Occupation du sol"
      ) |>
      addPolygons(
        data = approved_buildings,
        color = "#a6a6a6",
        weight = 0.8,
        fillColor = "#d9d9d9",
        fillOpacity = 0.45,
        label = ~paste0("Approuvé — ", building_id),
        popup = ~paste0(
          "<strong>Bâtiment approuvé et inclus</strong><br>Identifiant : ", building_id
        ),
        group = "Bâtiments approuvés"
      ) |>
      addPolygons(
        data = buildings,
        layerId = ~building_id,
        color = action_color,
        weight = 3,
        fillColor = action_color,
        fillOpacity = 0.35,
        label = ~paste0(ifelse(control_required, control_action, incongruity_flag), " — ", building_id),
        popup = ~paste0(
          "<strong>", ifelse(control_required, control_action, incongruity_flag), "</strong><br>",
          "Identifiant : ", building_id, "<br>",
          "Quartier : ", ifelse(is.na(district_id), "non renseigné", district_id), "<br>",
          "Occupation du sol : ", ifelse(is.na(land_use_zone), "non renseignée", land_use_zone), "<br>",
          "Produit : ", ifelse(is.na(product_id), "non renseigné", product_id)
        ),
        group = "Bâtiments signalés"
      ) |>
      fitBounds(
        lng1 = bounds[["xmin"]], lat1 = bounds[["ymin"]],
        lng2 = bounds[["xmax"]], lat2 = bounds[["ymax"]]
      )
  })

  output$building_review_table <- renderDT({
    comparison <- calculate_building_surface_comparison(
      review_buildings(),
      state$products
    )
    display <- data.frame(
      ID = comparison$building_id,
      Source = comparison$geometry_source,
      Fonction = comparison$function_label,
      Niveaux = comparison$levels,
      `Quartier (ID)` = comparison$district_id,
      `Occupation du sol` = comparison$land_use_zone,
      `Code produit principal` = comparison$product_id,
      `Action requise` = comparison$control_action,
      `Produit RDC` = comparison$ground_floor_product_id,
      `Produit étages` = comparison$upper_floor_product_id,
      `Emprise géométrique (m²)` = comparison$geometry_footprint_sqm,
      `Variation d’emprise à appliquer (m²)` = comparison$footprint_offset_sqm,
      `Emprise retenue (m²)` = comparison$effective_footprint_sqm,
      `Logements/niveau` = comparison$units_per_level_effective,
      `SDP totale calculée (m²)` = comparison$estimated_sdp_sqm,
      `SDP/logement calculée (m²)` = comparison$calculated_sdp_per_unit_sqm,
      `SDP/unité de référence (m²)` = comparison$model_sdp_per_unit,
      `Écart unitaire (m²)` = comparison$sdp_difference_sqm,
      Inclus = ifelse(comparison$included_in_simulation, "Oui", "Non"),
      Signalement = comparison$incongruity_flag,
      check.names = FALSE
    )
    datatable(
      display,
      rownames = FALSE,
      filter = "top",
      selection = "single",
      editable = list(
        target = "cell",
        disable = list(columns = c(0, 1, 4, 7, 8, 9, 10, 12, 13, 14, 15, 16, 17, 19))
      ),
      options = list(
        scrollX = TRUE,
        scrollY = "580px",
        pageLength = 25,
        autoWidth = TRUE
      )
    )
  }, server = FALSE)

  selected_building_levels <- reactive({
    row <- input$building_review_table_rows_selected
    validate(need(length(row) == 1, "Sélectionner un bâtiment dans le tableau ci-dessus."))
    building <- review_buildings()[row, , drop = FALSE]
    calculate_building_level_allocations(
      building,
      state$products,
      state$level_product_overrides
    )
  })

  output$building_level_table <- renderDT({
    levels <- selected_building_levels()
    display <- data.frame(
      Niveau = levels$level_label,
      Usage = levels$use_type,
      `Code produit` = levels$product_id,
      Unités = levels$unit_count,
      `SDP/unité calculée (m²)` = levels$sdp_per_unit_sqm,
      Produit = levels$product_label,
      `Surface estimée (m²)` = levels$estimated_sdp_sqm,
      Source = levels$assignment_source,
      check.names = FALSE
    )
    datatable(
      display,
      rownames = FALSE,
      editable = list(target = "cell", disable = list(columns = c(0, 1, 3, 4, 5))),
      options = list(dom = "t", paging = FALSE, scrollX = TRUE)
    )
  }, server = FALSE)

  observeEvent(input$building_level_table_cell_edit, {
    info <- input$building_level_table_cell_edit
    if (info$col != 2) return()
    value <- trimws(as.character(info$value))
    if (!nzchar(value) || !value %in% state$products$product_id) {
      showNotification(
        paste0("Produit inconnu. Utiliser : ", paste(state$products$product_id, collapse = ", "), "."),
        type = "error"
      )
      return()
    }
    levels <- selected_building_levels()
    target <- levels[info$row, , drop = FALSE]
    overrides <- state$level_product_overrides
    key <- overrides$building_id == target$building_id & overrides$level_number == target$level_number
    if (any(key)) {
      overrides$product_id[key] <- value
    } else {
      overrides <- rbind(
        overrides,
        data.frame(
          building_id = target$building_id,
          level_number = target$level_number,
          product_id = value,
          stringsAsFactors = FALSE
        )
      )
    }
    state$level_product_overrides <- overrides
    persist_or_notify()
  })

  observeEvent(input$building_review_table_cell_edit, {
    info <- input$building_review_table_cell_edit
    row <- info$row
    column <- info$col
    value <- trimws(as.character(info$value))
    buildings <- state$buildings
    visible_buildings <- review_buildings()
    if (row < 1 || row > nrow(visible_buildings)) return()
    row <- match(visible_buildings$building_id[row], buildings$building_id)
    if (is.na(row)) return()

    valid <- TRUE
    error_message <- NULL
    if (column == 2) {
      function_mapping <- c(
        "résidentiel" = "residential",
        "immeuble collectif" = "apartments",
        "bureaux" = "office",
        "usage mixte" = "mixed_use",
        "mosquée" = "mosque",
        "école" = "school",
        "clinique" = "clinic",
        "équipement public" = "civic",
        "hôpital" = "hospital",
        "hôtel" = "hotel",
        "commerce" = "retail",
        "transport" = "transportation",
        "non renseigné" = "non renseigné"
      )
      canonical <- unname(function_mapping[tolower(value)])
      if (length(canonical) == 0 || is.na(canonical)) canonical <- value
      valid <- nzchar(canonical)
      error_message <- "La fonction du bâtiment ne peut pas être vide."
      if (valid) {
        buildings$building_function[row] <- canonical
        buildings$function_label[row] <- building_function_label(canonical)
      }
    } else if (column == 3) {
      levels <- suppressWarnings(as.numeric(sub(",", ".", value, fixed = TRUE)))
      valid <- !nzchar(value) || (!is.na(levels) && levels > 0)
      error_message <- "Le nombre de niveaux doit être positif ou vide."
      if (valid) {
        buildings$levels[row] <- if (nzchar(value)) levels else NA_real_
        buildings$levels_source[row] <- if (nzchar(value)) as.character(levels) else NA_character_
      }
    } else if (column == 4) {
      valid <- !nzchar(value) || value %in% state$districts$district_id
      error_message <- paste0(
        "Quartier inconnu. Utiliser : ",
        paste(state$districts$district_id, collapse = ", "), "."
      )
      if (valid) {
        buildings$district_id[row] <- if (nzchar(value)) value else NA_character_
        buildings$district_label_spatial[row] <- state$districts$district_label[
          match(buildings$district_id[row], state$districts$district_id)
        ]
      }
    } else if (column == 5) {
      valid_zones <- sort(unique(stats::na.omit(scenario$context$land_use$zone)))
      valid <- !nzchar(value) || value %in% valid_zones
      error_message <- paste0(
        "Occupation du sol inconnue. Utiliser : ",
        paste(valid_zones, collapse = ", "), "."
      )
      if (valid) {
        buildings$land_use_zone[row] <- if (nzchar(value)) value else NA_character_
        buildings$land_use_label_spatial[row] <- if (nzchar(value)) value else NA_character_
      }
    } else if (column == 6) {
      valid <- !nzchar(value) || value %in% state$products$product_id
      error_message <- paste0(
        "Produit inconnu. Utiliser : ",
        paste(state$products$product_id, collapse = ", "), "."
      )
      if (valid) {
        buildings$product_id[row] <- if (nzchar(value)) value else NA_character_
        buildings$product_link_source[row] <- if (nzchar(value)) "manuel" else "non renseigné"
        if (nzchar(value)) {
          product_row <- match(value, state$products$product_id)
          buildings$levels[row] <- state$products$default_building_levels[product_row]
          buildings$levels_source[row] <- "hypothèse produit"
          buildings$units_per_level[row] <- state$products$units_per_level[product_row]
        }
      }
    } else if (column == 11) {
      offset <- suppressWarnings(as.numeric(sub(",", ".", value, fixed = TRUE)))
      geometry_area <- as.numeric(sf::st_area(sf::st_transform(buildings[row, ], 32628)))
      valid <- !is.na(offset) && is.finite(offset) && geometry_area + offset > 0
      error_message <- paste0(
        "La variation doit être numérique et conserver une emprise positive. ",
        "Emprise géométrique : ", format_number_fr(geometry_area, 1), " m²."
      )
      if (valid) {
        buildings <- apply_building_footprint_adjustment(buildings, row, offset)
      }
    } else if (column == 18) {
      included_mapping <- c("oui" = TRUE, "non" = FALSE, "true" = TRUE, "false" = FALSE, "1" = TRUE, "0" = FALSE)
      included <- unname(included_mapping[tolower(value)])
      valid <- length(included) == 1 && !is.na(included)
      error_message <- "La colonne Inclus accepte Oui ou Non."
      if (valid) buildings$included_in_simulation[row] <- included
    } else {
      return()
    }

    if (!valid) {
      showNotification(error_message, type = "error")
      return()
    }
    if (column == 18 && !included) {
      removed_building_id <- buildings$building_id[row]
      buildings <- buildings[-row, , drop = FALSE]
      state$level_product_overrides <- state$level_product_overrides[
        state$level_product_overrides$building_id != removed_building_id,
        ,
        drop = FALSE
      ]
      state$buildings <- buildings
      persist_or_notify(
        paste0(
          "Le bâtiment ", removed_building_id,
          " a été retiré définitivement du scénario. La source de référence reste intacte."
        ),
        write_spatial = TRUE
      )
      return()
    }

    buildings$control_edited[row] <- TRUE
    state$buildings <- add_building_classification_status(buildings, state$products)
    persist_or_notify(write_spatial = TRUE)
  })

  output$district_surface_explanation <- renderUI({
    if (identical(state$program_source, "buildings")) {
      p(
        class = "help-text",
        "La SDP retenue est calculée par bâtiment : emprise géométrique × niveaux. Une variation saisie dans le contrôle redimensionne et enregistre le polygone. Pour RC1 à RC4 et RT1, les logements sont comptés par niveau et la SDP/logement est déduite de l’emprise retenue."
      )
    } else {
      p(
        class = "help-text",
        "La SDP SIG indicative (emprise au sol × niveaux) est comparée à la SDP du programme saisi. L’écart n’affecte pas le bilan tant que la source reste « Programme saisi »."
      )
    }
  })

  output$district_surface_comparison <- renderDT({
    comparison <- calculate_district_surface_comparison(
      all_buildings(),
      state$products,
      model()$program,
      state$districts
    )
    if (identical(state$program_source, "buildings")) {
      display <- data.frame(
        Quartier = comparison$district_label,
        Bâtiments = comparison$building_count,
        `Emprise bâtie (m²)` = comparison$footprint_area_sqm,
        `SDP retenue (m²)` = comparison$estimated_sdp_sqm,
        check.names = FALSE
      )
      numeric_columns <- c("Bâtiments", "Emprise bâtie (m²)", "SDP retenue (m²)")
    } else {
      display <- data.frame(
        Quartier = comparison$district_label,
        Bâtiments = comparison$building_count,
        `Emprise bâtie (m²)` = comparison$footprint_area_sqm,
        `SDP SIG indicative (m²)` = comparison$estimated_sdp_sqm,
        `SDP du programme saisi (m²)` = comparison$model_sdp_sqm,
        `Écart SIG - programme (m²)` = comparison$sdp_difference_sqm,
        check.names = FALSE
      )
      numeric_columns <- c(
        "Bâtiments", "Emprise bâtie (m²)",
        "SDP SIG indicative (m²)", "SDP du programme saisi (m²)",
        "Écart SIG - programme (m²)"
      )
    }
    datatable(
      display,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, scrollX = TRUE)
    ) |>
      formatRound(
        numeric_columns,
        digits = 0,
        mark = " ",
        dec.mark = ","
      )
  })

  observeEvent(input$building_review_table_rows_selected, {
    row <- input$building_review_table_rows_selected
    if (length(row) != 1 || row > nrow(review_buildings())) return()
    building <- review_buildings()[row, , drop = FALSE]
    bounds <- sf::st_bbox(building)

    leafletProxy("building_control_map") |>
      clearGroup("Bâtiment sélectionné") |>
      addPolygons(
        data = building,
        color = "#ffcc00",
        weight = 5,
        fillOpacity = 0.15,
        group = "Bâtiment sélectionné"
      ) |>
      fitBounds(
        lng1 = bounds[["xmin"]], lat1 = bounds[["ymin"]],
        lng2 = bounds[["xmax"]], lat2 = bounds[["ymax"]],
        options = list(maxZoom = 19)
      )
  })

  observeEvent(input$building_control_map_shape_click, {
    building_id <- input$building_control_map_shape_click$id
    if (is.null(building_id) || !nzchar(building_id)) return()
    row <- match(building_id, review_buildings()$building_id)
    if (!is.na(row)) selectRows(dataTableProxy("building_review_table"), row)
  })

  observeEvent(input$parcel_map_shape_click, {
    building_id <- input$parcel_map_shape_click$id
    if (is.null(building_id) || !nzchar(building_id)) return()
    row <- match(building_id, review_buildings()$building_id)
    if (!is.na(row)) {
      selectRows(dataTableProxy("building_review_table"), row)
      updateTabsetPanel(session, "section", selected = "Contrôle bâtiments")
    }
  })

  output$parcel_map <- renderLeaflet({
    validate(need(!is.null(state$buildings), "Les bâtiments corrigés ne peuvent pas être chargés."))
    validate(need(!is.null(scenario$parcels), "Les limites parcellaires ne peuvent pas être chargées."))
    validate(need(!is.null(scenario$context), "Les couches contextuelles ne peuvent pas être chargées."))

    buildings <- urban_buildings()
    buildings_to_control <- buildings[buildings$control_required, , drop = FALSE]
    buildings_with_flags <- buildings[
      buildings$has_incongruity & !buildings$control_required,
      ,
      drop = FALSE
    ]
    roads <- scenario$roads
    roads$display_weight <- c(
      trunk = 6, primary = 5, secondary = 4, tertiary = 3,
      residential = 2.5, service = 2, pedestrian = 2
    )[roads$highway]
    roads$display_weight[is.na(roads$display_weight)] <- 2
    functions <- sort(unique(buildings$function_label))
    function_colors <- stats::setNames(
      grDevices::hcl.colors(length(functions), "Dark 3"),
      functions
    )
    palette <- colorFactor(function_colors, domain = functions)
    quartiers <- scenario$context$quartiers
    district_codes <- sort(unique(quartiers$district_code))
    district_palette <- colorFactor(
      grDevices::hcl.colors(length(district_codes), "Dark 2"),
      domain = district_codes
    )
    map_bounds <- sf::st_bbox(c(
      sf::st_geometry(buildings),
      sf::st_geometry(roads),
      sf::st_geometry(scenario$parcels),
      sf::st_geometry(scenario$context$project_boundary),
      sf::st_geometry(scenario$context$title_boundary)
    ))

    trees <- sf::st_geometry(scenario$context$trees)

    map <- leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addTiles(
        urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
        attribution = "© contributeurs OpenStreetMap",
        group = "Fond OpenStreetMap"
      ) |>
      addPolygons(
        data = quartiers,
        color = ~district_palette(district_code),
        weight = 1.5,
        opacity = 0.9,
        fillColor = ~district_palette(district_code),
        fillOpacity = 0.16,
        label = ~district_label,
        group = "Quartiers"
      ) |>
      addPolygons(
        data = scenario$context$land_use,
        color = "#6c8057",
        weight = 0.4,
        opacity = 0.45,
        fillColor = "#9bb884",
        fillOpacity = 0.18,
        label = ~paste0(land_use_label, ifelse(is.na(zone), "", paste0(" — ", zone))),
        group = "Occupation du sol"
      ) |>
      addPolygons(
        data = scenario$context$flood_areas,
        color = "#286f9e",
        weight = 1,
        opacity = 0.75,
        fillColor = "#56a6d8",
        fillOpacity = 0.3,
        label = ~paste0(flood_type, ifelse(is.na(basin), "", paste0(" — ", basin))),
        group = "Zones inondables"
      ) |>
      addPolygons(
        data = scenario$context$road_footprints,
        color = "#777777",
        weight = 0.6,
        opacity = 0.8,
        fillColor = "#b7b7b7",
        fillOpacity = 0.45,
        label = ~road_footprint_label,
        group = "Emprises de voirie"
      ) |>
      addPolylines(
        data = scenario$context$project_boundary,
        color = "#16713d",
        weight = 4,
        opacity = 1,
        label = "Emprise du projet",
        group = "Emprise du projet"
      ) |>
      addPolylines(
        data = scenario$context$title_boundary,
        color = "#174f9e",
        weight = 3,
        opacity = 1,
        dashArray = "8 5",
        label = "Titre foncier",
        group = "Titre foncier"
      ) |>
      addPolylines(
        data = roads,
        color = "#4d4d4d",
        weight = ~display_weight,
        opacity = 0.95,
        label = ~road_label,
        popup = ~paste0(
          "<strong>", htmltools::htmlEscape(road_label), "</strong><br>",
          "Largeur : ", ifelse(is.na(width_m), "—", paste0(width_m, " m")), "<br>",
          "Voies : ", ifelse(is.na(lanes), "—", lanes)
        ),
        group = "Axes de voirie OSM"
      ) |>
      addPolylines(
        data = scenario$parcels,
        color = "#8b6f47",
        weight = 1.2,
        opacity = 0.8,
        dashArray = "4 3",
        label = ~paste(parcel_type, "—", layer),
        group = "Limites parcellaires"
      ) |>
      addCircleMarkers(
        data = trees,
        radius = 1.7,
        stroke = FALSE,
        fillColor = "#238b45",
        fillOpacity = 0.78,
        options = pathOptions(interactive = FALSE),
        group = "Arbres"
      ) |>
      addPolygons(
        data = buildings,
        layerId = ~building_id,
        color = ~ifelse(geometry_source == "OSM conservé", "#8e44ad", "#333333"),
        weight = 0.7,
        fillColor = ~palette(function_label),
        fillOpacity = 0.72,
        label = ~paste0(function_label, " — ", levels_effective, " niveau(x) — ", height_m, " m"),
        popup = ~paste0(
          "<strong>", htmltools::htmlEscape(function_label), "</strong><br>",
          "Identifiant : ", building_id, "<br>",
          "Niveaux source : ", ifelse(is.na(levels), "non renseignés", levels), "<br>",
          "Niveaux retenus : ", levels_effective, "<br>",
          "Hauteur calculée : ", format_number_fr(height_m, 1), " m<br>",
          "Source géométrique : ", geometry_source, "<br>",
          "Appariement OSM : ", attribute_match_quality, "<br>",
          "Quartier : ", ifelse(is.na(district_id), "non renseigné", district_id), "<br>",
          "Occupation du sol : ", ifelse(is.na(land_use_zone), "non renseignée", land_use_zone), "<br>",
          "Produit : ", ifelse(is.na(product_id), "non renseigné", product_id), "<br>",
          "Action de contrôle : ", control_action
        ),
        group = "Bâtiments consolidés"
      )
    if (nrow(buildings_to_control)) {
      map <- map |> addPolygons(
          data = buildings_to_control,
          layerId = ~building_id,
          color = "#d7301f",
          weight = 3,
          fillColor = "#d7301f",
          fillOpacity = 0.18,
          label = ~paste0(control_action, " — ", building_id),
          group = "Bâtiments à contrôler"
        )
    }
    if (nrow(buildings_with_flags)) {
      map <- map |> addPolygons(
          data = buildings_with_flags,
          layerId = ~building_id,
          color = "#f0ad00",
          weight = 2.5,
          fillOpacity = 0,
          label = ~paste0(incongruity_flag, " — ", building_id),
          group = "Signalements de cohérence"
        )
    }
    map |>
      addLegend(
        position = "bottomright",
        colors = unname(function_colors),
        labels = names(function_colors),
        title = "Fonction des bâtiments",
        opacity = 1
      ) |>
      addLayersControl(
        baseGroups = "Fond OpenStreetMap",
        overlayGroups = c(
          "Bâtiments consolidés", "Bâtiments à contrôler",
          "Signalements de cohérence",
          "Axes de voirie OSM", "Emprises de voirie",
          "Limites parcellaires", "Arbres", "Quartiers", "Occupation du sol",
          "Zones inondables", "Emprise du projet", "Titre foncier"
        ),
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      hideGroup(c("Limites parcellaires", "Occupation du sol", "Zones inondables")) |>
      fitBounds(
        lng1 = map_bounds[["xmin"]], lat1 = map_bounds[["ymin"]],
        lng2 = map_bounds[["xmax"]], lat2 = map_bounds[["ymax"]]
      )
  })


  output$building_3d_map <- renderUI({
    validate(need(!is.null(state$buildings), "Les bâtiments corrigés ne peuvent pas être chargés."))
    validate(need(!is.null(scenario$parcels), "Les limites parcellaires ne peuvent pas être chargées."))
    validate(need(!is.null(scenario$context), "Les couches contextuelles ne peuvent pas être chargées."))

    buildings <- urban_buildings()
    roads <- scenario$roads
    level_blocks <- create_building_level_blocks(
      buildings,
      state$products,
      state$height_assumptions$ground_floor_height_m,
      state$height_assumptions$upper_floor_height_m,
      state$level_product_overrides
    )
    typologies <- data.frame(
      code = c("RM1a", "RM1b", "RM2", "RC1", "RC2", "RC3", "RC4", "RV1", "RV2", "RV3", "RT1", "EC", "EP"),
      label = c(
        "Maison en bande PHARD", "Maison en condominium PHARD",
        "Maison de ville en bande R+1", "Condominium Fann Hock R+1",
        "Immeuble-barre mixte R+3", "Immeuble-plot moyen standing R+4",
        "Immeuble-plot haut standing R+4", "Villa moyen standing R+1",
        "Villa haut standing R+1", "Villa très haut standing R+1",
        "Immeuble mixte tertiaire / résidentiel",
        "Immeubles bureaux / administration", "Équipements publics"
      ),
      color = c(
        "#6baed6", "#3182bd", "#08519c", "#9e9ac8", "#6a51a3",
        "#fb6a4a", "#cb181d", "#a1d99b", "#31a354", "#006d2c",
        "#f16913", "#d4a72c", "#737373"
      ),
      stringsAsFactors = FALSE
    )
    product_to_typology <- c(
      rm_1a = "RM1a", rm_1b = "RM1b", rm_2 = "RM2", rc_1 = "RC1",
      rc_2_log = "RC2", rc_2_com = "RC2", rc_3 = "RC3", rc_4 = "RC4",
      rv_1 = "RV1", rv_2 = "RV2", rv_3 = "RV3",
      rt_1_log = "RT1", rt_1_com = "RT1", ec = "EC", ep = "EP"
    )
    level_blocks$typology_code <- unname(product_to_typology[level_blocks$product_id])
    level_blocks$typology_code[is.na(level_blocks$typology_code)] <- "Autre"
    used_typologies <- unique(level_blocks$typology_code)
    legend_typologies <- typologies[typologies$code %in% used_typologies, , drop = FALSE]
    product_colors <- stats::setNames(typologies$color, typologies$code)
    product_colors <- c(product_colors, Autre = "#777777")
    bounds <- sf::st_bbox(scenario$context$project_boundary)

    buildings_geojson <- sf_to_geojson(
      buildings[, c(
        "building_id", "osm_id", "function_label", "levels",
        "levels_effective", "height_m", "attribute_match_quality",
        "geometry_source", "district_id", "land_use_zone", "product_id",
        "control_action", "control_required", "incongruity_flag"
      )]
    )
    level_blocks_geojson <- sf_to_geojson(
      level_blocks[, c(
        "building_id", "level_number", "level_label", "use_type",
        "product_id", "typology_code", "product_label", "estimated_sdp_sqm",
        "unit_count", "sdp_per_unit_sqm",
        "building_total_units", "building_total_sdp_sqm",
        "commercial_ground_floor", "commercial_surface_total_sqm",
        "assignment_source", "control_required", "incongruity_flag",
        "base_height_m", "top_height_m"
      )]
    )
    roads_geojson <- sf_to_geojson(
      roads[, c("osm_id", "road_label", "highway", "width_m", "lanes")]
    )
    parcels_geojson <- sf_to_geojson(
      scenario$parcels[, c("layer", "parcel_type")]
    )
    land_use_geojson <- sf_to_geojson(
      scenario$context$land_use[, c("land_use_label", "zone")]
    )
    road_footprints_geojson <- sf_to_geojson(
      scenario$context$road_footprints[, c("road_footprint_label", "Class")]
    )
    flood_geojson <- sf_to_geojson(
      scenario$context$flood_areas[, c("flood_type", "basin", "surface_sqm")]
    )
    districts_geojson <- sf_to_geojson(
      scenario$context$quartiers[, c("district_code", "district_label")]
    )
    project_boundary_geojson <- sf_to_geojson(scenario$context$project_boundary)
    title_boundary_geojson <- sf_to_geojson(scenario$context$title_boundary)
    display_trees <- if (is.null(scenario$display_trees)) scenario$context$trees else scenario$display_trees
    trees_json <- tree_display_payload_json(display_trees)
    species_legend <- tree_species_legend(display_trees)
    colors_json <- jsonlite::toJSON(as.list(product_colors), auto_unbox = TRUE)
    bounds_json <- jsonlite::toJSON(
      list(
        c(unname(bounds[["xmin"]]), unname(bounds[["ymin"]])),
        c(unname(bounds[["xmax"]]), unname(bounds[["ymax"]]))
      ),
      auto_unbox = TRUE
    )

    javascript <- sprintf(
      paste0(
        "(function initEcodekk3d(){",
        "if(typeof maplibregl===\u0027undefined\u0027){setTimeout(initEcodekk3d,100);return;}",
        "var container=document.getElementById(\u0027building_map_3d\u0027);",
        "if(!container){return;}",
        "if(window.ecodekkMap3d){try{window.ecodekkMap3d.remove();}catch(e){}}",
        "var buildings=%s;var levelBlocks=%s;var roads=%s;var parcels=%s;var landuse=%s;",
        "var roadFootprints=%s;var floods=%s;var districts=%s;",
        "var projectBoundary=%s;var titleBoundary=%s;var trees=%s;var colors=%s;var bounds=%s;",
        "var productExpression=[\u0027match\u0027,[\u0027get\u0027,\u0027typology_code\u0027]];",
        "Object.keys(colors).forEach(function(key){productExpression.push(key,colors[key]);});",
        "productExpression.push(\u0027#777777\u0027);",
        "var expression=[\u0027case\u0027,[\u0027==\u0027,[\u0027get\u0027,\u0027control_required\u0027],true],\u0027#d7301f\u0027,productExpression];",
        "var map=new maplibregl.Map({container:\u0027building_map_3d\u0027,",
        "style:{version:8,sources:{osm:{type:\u0027raster\u0027,tiles:[\u0027https://tile.openstreetmap.org/{z}/{x}/{y}.png\u0027],tileSize:256,attribution:\u0027© contributeurs OpenStreetMap\u0027},terrainSource:{type:\u0027raster-dem\u0027,url:\u0027https://tiles.mapterhorn.com/tilejson.json\u0027}},",
        "layers:[{id:\u0027fond-osm\u0027,type:\u0027raster\u0027,source:\u0027osm\u0027,paint:{\u0027raster-saturation\u0027:-1,\u0027raster-contrast\u0027:0.08,\u0027raster-brightness-max\u0027:0.92}},{id:\u0027relief-ombrage\u0027,type:\u0027hillshade\u0027,source:\u0027terrainSource\u0027,paint:{\u0027hillshade-exaggeration\u0027:0.35}}],terrain:{source:\u0027terrainSource\u0027,exaggeration:1.25}},",
        "center:[(bounds[0][0]+bounds[1][0])/2,(bounds[0][1]+bounds[1][1])/2],zoom:15,pitch:55,bearing:-20,antialias:true});",
        "window.ecodekkMap3d=map;",
        "map.addControl(new maplibregl.NavigationControl({visualizePitch:true}),\u0027top-left\u0027);",
        "if(maplibregl.TerrainControl){map.addControl(new maplibregl.TerrainControl({source:\u0027terrainSource\u0027,exaggeration:1.25}),\u0027top-left\u0027);}",
        "map.addControl(new maplibregl.ScaleControl({unit:\u0027metric\u0027}));",
        "map.on(\u0027load\u0027,function(){",
        "map.addSource(\u0027occupation-sol\u0027,{type:\u0027geojson\u0027,data:landuse});",
        "map.addLayer({id:\u0027occupation-sol\u0027,type:\u0027fill\u0027,source:\u0027occupation-sol\u0027,layout:{visibility:\u0027none\u0027},paint:{\u0027fill-color\u0027:\u0027#9bb884\u0027,\u0027fill-opacity\u0027:0.18,\u0027fill-outline-color\u0027:\u0027#6c8057\u0027}});",
        "map.addSource(\u0027zones-inondables\u0027,{type:\u0027geojson\u0027,data:floods});",
        "map.addLayer({id:\u0027zones-inondables\u0027,type:\u0027fill\u0027,source:\u0027zones-inondables\u0027,paint:{\u0027fill-color\u0027:\u0027#56a6d8\u0027,\u0027fill-opacity\u0027:0.3,\u0027fill-outline-color\u0027:\u0027#286f9e\u0027}});",
        "map.addSource(\u0027emprises-voirie\u0027,{type:\u0027geojson\u0027,data:roadFootprints});",
        "map.addLayer({id:\u0027emprises-voirie\u0027,type:\u0027fill\u0027,source:\u0027emprises-voirie\u0027,paint:{\u0027fill-color\u0027:\u0027#a5a5a5\u0027,\u0027fill-opacity\u0027:0.45,\u0027fill-outline-color\u0027:\u0027#777777\u0027}});",
        "map.addSource(\u0027quartiers\u0027,{type:\u0027geojson\u0027,data:districts});",
        "map.addLayer({id:\u0027quartiers\u0027,type:\u0027line\u0027,source:\u0027quartiers\u0027,paint:{\u0027line-color\u0027:\u0027#9a6700\u0027,\u0027line-width\u0027:2.2,\u0027line-opacity\u0027:0.9,\u0027line-dasharray\u0027:[4,3]}});",
        "map.addSource(\u0027emprise-projet\u0027,{type:\u0027geojson\u0027,data:projectBoundary});",
        "map.addLayer({id:\u0027emprise-projet\u0027,type:\u0027line\u0027,source:\u0027emprise-projet\u0027,paint:{\u0027line-color\u0027:\u0027#16713d\u0027,\u0027line-width\u0027:4}});",
        "map.addSource(\u0027titre-foncier\u0027,{type:\u0027geojson\u0027,data:titleBoundary});",
        "map.addLayer({id:\u0027titre-foncier\u0027,type:\u0027line\u0027,source:\u0027titre-foncier\u0027,paint:{\u0027line-color\u0027:\u0027#174f9e\u0027,\u0027line-width\u0027:3,\u0027line-dasharray\u0027:[4,3]}});",
        "map.addSource(\u0027parcelles\u0027,{type:\u0027geojson\u0027,data:parcels});",
        "map.addLayer({id:\u0027limites-parcellaires\u0027,type:\u0027line\u0027,source:\u0027parcelles\u0027,layout:{visibility:\u0027none\u0027},paint:{\u0027line-color\u0027:\u0027#8b6f47\u0027,\u0027line-width\u0027:1.3,\u0027line-opacity\u0027:0.85,\u0027line-dasharray\u0027:[3,2]}});",
        "map.addSource(\u0027voiries\u0027,{type:\u0027geojson\u0027,data:roads});",
        "map.addLayer({id:\u0027voiries-projet\u0027,type:\u0027line\u0027,source:\u0027voiries\u0027,layout:{visibility:\u0027none\u0027},paint:{\u0027line-color\u0027:\u0027#4d4d4d\u0027,\u0027line-width\u0027:[\u0027interpolate\u0027,[\u0027linear\u0027],[\u0027zoom\u0027],13,1.5,17,5],\u0027line-opacity\u0027:0.95}});",
        "var treeFeatures=trees.lon.map(function(lon,i){return {type:\u0027Feature\u0027,properties:trees.species?{s:trees.species[i]}:{},geometry:{type:\u0027Point\u0027,coordinates:[lon,trees.lat[i]]}};});",
        "map.addSource(\u0027arbres\u0027,{type:\u0027geojson\u0027,data:{type:\u0027FeatureCollection\u0027,features:treeFeatures}});",
        "var treeColor=\u0027#238b45\u0027;if(trees.species){treeColor=[\u0027match\u0027,[\u0027get\u0027,\u0027s\u0027]];trees.species_colors.forEach(function(c,i){treeColor.push(i,c);});treeColor.push(\u0027#238b45\u0027);}",
        "map.addLayer({id:\u0027arbres\u0027,type:\u0027circle\u0027,source:\u0027arbres\u0027,layout:{visibility:\u0027none\u0027},paint:{\u0027circle-radius\u0027:[\u0027interpolate\u0027,[\u0027linear\u0027],[\u0027zoom\u0027],13,1.2,17,3.5],\u0027circle-color\u0027:treeColor,\u0027circle-opacity\u0027:0.82,\u0027circle-stroke-color\u0027:\u0027#0b5d2a\u0027,\u0027circle-stroke-width\u0027:0.5}});",
        "map.addSource(\u0027batiments\u0027,{type:\u0027geojson\u0027,data:buildings});",
        "map.addSource(\u0027niveaux-batiments\u0027,{type:\u0027geojson\u0027,data:levelBlocks});",
        "map.addLayer({id:\u0027batiments-par-niveau\u0027,type:\u0027fill-extrusion\u0027,source:\u0027niveaux-batiments\u0027,paint:{\u0027fill-extrusion-color\u0027:expression,\u0027fill-extrusion-height\u0027:[\u0027get\u0027,\u0027top_height_m\u0027],\u0027fill-extrusion-base\u0027:[\u0027get\u0027,\u0027base_height_m\u0027],\u0027fill-extrusion-opacity\u0027:0.92}});",
        "map.addLayer({id:\u0027signalements-coherence\u0027,type:\u0027line\u0027,source:\u0027batiments\u0027,layout:{visibility:\u0027none\u0027},filter:[\u0027all\u0027,[\u0027==\u0027,[\u0027get\u0027,\u0027control_required\u0027],false],[\u0027!=\u0027,[\u0027get\u0027,\u0027incongruity_flag\u0027],\u0027\u0027]],paint:{\u0027line-color\u0027:\u0027#f0ad00\u0027,\u0027line-width\u0027:3}});",
        "map.addLayer({id:\u0027batiments-controle\u0027,type:\u0027line\u0027,source:\u0027batiments\u0027,filter:[\u0027==\u0027,[\u0027get\u0027,\u0027control_required\u0027],true],paint:{\u0027line-color\u0027:\u0027#d7301f\u0027,\u0027line-width\u0027:4}});",
        "document.querySelectorAll(\u0027#building_map_3d_layers input[data-layers]\u0027).forEach(function(input){input.addEventListener(\u0027change\u0027,function(){input.dataset.layers.split(\u0027,\u0027).forEach(function(layerId){if(map.getLayer(layerId)){map.setLayoutProperty(layerId,\u0027visibility\u0027,input.checked?\u0027visible\u0027:\u0027none\u0027);}});});});",
        "document.getElementById(\u0027map_view_2d\u0027).addEventListener(\u0027click\u0027,function(){map.setTerrain(null);map.easeTo({pitch:0,bearing:0,duration:700});});",
        "document.getElementById(\u0027map_view_3d\u0027).addEventListener(\u0027click\u0027,function(){map.setTerrain({source:\u0027terrainSource\u0027,exaggeration:1.25});map.easeTo({pitch:55,bearing:-20,duration:700});});",
        "map.fitBounds(bounds,{padding:25,duration:0,maxZoom:16});map.setPitch(55);map.setBearing(-20);",
        "map.on(\u0027click\u0027,\u0027batiments-par-niveau\u0027,function(e){var p=e.features[0].properties;",
        "var esc=function(v){return String(v).replace(/[&<>\"\u0027]/g,function(c){return {\u0027&\u0027:\u0027&amp;\u0027,\u0027<\u0027:\u0027&lt;\u0027,\u0027>\u0027:\u0027&gt;\u0027,\u0027\"\u0027:\u0027&quot;\u0027,\u0027\\u0027\u0027:\u0027&#39;\u0027}[c];});};",
        "var popupHtml='<strong>Produit : '+esc(p.product_label || p.product_id)+' ('+esc(p.product_id)+')</strong><br>Surface par étage : '+esc(p.estimated_sdp_sqm)+' m²<br>Unités par étage : '+esc(p.unit_count)+'<br>Unités totales : '+esc(p.building_total_units)+'<br>SDP totale : '+esc(p.building_total_sdp_sqm)+' m²';",
        "var commercial=(p.commercial_ground_floor===true || String(p.commercial_ground_floor).toLowerCase()===\u0027true\u0027);",
        "if(commercial){popupHtml+='<br>RDC commercial : Oui<br>Surface commerciale totale : '+esc(p.commercial_surface_total_sqm)+' m²';}",
        "new maplibregl.Popup().setLngLat(e.lngLat).setHTML(popupHtml).addTo(map);});",
        "map.on(\u0027mouseenter\u0027,\u0027batiments-par-niveau\u0027,function(){map.getCanvas().style.cursor=\u0027pointer\u0027;});",
        "map.on(\u0027mouseleave\u0027,\u0027batiments-par-niveau\u0027,function(){map.getCanvas().style.cursor=\u0027\u0027;});",
        "});",
        "})();"
      ),
      buildings_geojson,
      level_blocks_geojson,
      roads_geojson,
      parcels_geojson,
      land_use_geojson,
      road_footprints_geojson,
      flood_geojson,
      districts_geojson,
      project_boundary_geojson,
      title_boundary_geojson,
      trees_json,
      colors_json,
      bounds_json
    )

    legend_rows <- lapply(seq_len(nrow(legend_typologies)), function(index) {
      typology <- legend_typologies[index, ]
      div(
        class = "map3d-legend-row",
        span(class = "map3d-swatch", style = paste0("background:", typology$color, ";")),
        span(class = "map3d-code", typology$code),
        span(typology$label)
      )
    })

    species_rows <- if (is.null(species_legend)) NULL else lapply(seq_len(nrow(species_legend)), function(index) {
      row <- species_legend[index, ]
      div(
        class = "map3d-legend-row",
        span(class = "map3d-swatch map3d-swatch-round", style = paste0("background:", row$color, ";")),
        span(tags$em(row$species), paste0(" (", format(row$count, big.mark = "\u202f"), ")"))
      )
    })

    layer_specs <- list(
      c("Ombrage du relief", "relief-ombrage", "true"),
      c("Bâtiments par niveau", "batiments-par-niveau,batiments-controle", "true"),
      c("Signalements", "signalements-coherence", "false"),
      c("Axes de voirie", "voiries-projet", "false"),
      c("Arbres", "arbres", "false"),
      c("Emprises de voirie", "emprises-voirie", "true"),
      c("Quartiers", "quartiers", "true"),
      c("Emprise du projet", "emprise-projet", "true"),
      c("Titre foncier", "titre-foncier", "true"),
      c("Limites parcellaires", "limites-parcellaires", "false"),
      c("Occupation du sol", "occupation-sol", "false"),
      c("Zones inondables", "zones-inondables", "true")
    )
    layer_controls <- lapply(layer_specs, function(spec) {
      tags$label(
        tags$input(
          type = "checkbox", `data-layers` = spec[2],
          checked = if (identical(spec[3], "true")) "checked" else NULL
        ),
        paste0(" ", spec[1])
      )
    })

    div(
      class = "map3d-wrapper",
      div(id = "building_map_3d"),
      div(
        class = "map3d-view-controls",
        tags$button(id = "map_view_2d", type = "button", "Vue 2D"),
        tags$button(id = "map_view_3d", type = "button", "Vue 3D")
      ),
      tags$details(
        id = "building_map_3d_layers", class = "map3d-layers",
        tags$summary("Couches"),
        div(layer_controls)
      ),
      tags$details(
        class = "map3d-legend",
        tags$summary("Code / typologie"),
        div(legend_rows)
      ),
      tags$details(
        class = "map3d-legend map3d-legend-trees",
        tags$summary("Essences d'arbres"),
        div(
          species_rows,
          p(class = "map3d-legend-note", scenario$tree_source_label)
        )
      ),
      # Les GeoJSON sont insérés dans un <script> : neutraliser toute séquence "</".
      tags$script(HTML(gsub("</", "<\\/", javascript, fixed = TRUE)))
    )
  })

  umep_display <- reactive({
    req(scenario$path)
    directory <- find_umep_display_directory(scenario$path)
    if (is.null(directory)) return(NULL)
    tryCatch(read_umep_display(directory), error = function(error) {
      structure(list(message = conditionMessage(error)), class = "umep_display_error")
    })
  })

  umep_ready <- reactive({
    display <- umep_display()
    if (is.null(display) || inherits(display, "umep_display_error")) NULL else display
  })

  umep_resource_prefix <- reactive({
    display <- umep_ready()
    req(display)
    prefix <- paste0("umep_display_", gsub("[^A-Za-z0-9_]", "_", scenario$id))
    addResourcePath(prefix, normalizePath(display$directory, winslash = "/", mustWork = TRUE))
    prefix
  })

  observeEvent(umep_ready(), {
    display <- umep_ready()
    updateSelectInput(session, "thermal_day", choices = umep_day_choices(display))
    updateSelectInput(session, "thermal_vegetation", choices = umep_vegetation_choices(display))
  })

  output$thermal_status <- renderUI({
    display <- umep_display()
    if (is.null(display)) {
      return(div(class = "alert alert-info",
        "Aucun résultat SOLWEIG pour ce scénario. L’étude d’ombrage porte sur le scénario scenario_01 : chargez-le pour afficher ses résultats."))
    }
    if (inherits(display, "umep_display_error")) {
      return(div(class = "alert alert-danger", "Résultats SOLWEIG illisibles : ", display$message))
    }
    manifest <- display$manifest
    p(
      class = "help-text",
      paste0(
        "Étude « ", manifest$study, " », scénario ", manifest$scenario, ". ",
        manifest$source, ", pixel de ",
        format_number_fr(manifest$resolution_m, if (manifest$resolution_m %% 1 == 0) 0 else 1), " m. ",
        "Météorologie ERA5 (maille d’environ 31 km) : climat régional, pas le microclimat mesuré du site. ",
        "Dimensions des arbres issues de la littérature, à valider par des relevés de terrain."
      )
    )
  })

  output$thermal_map <- renderUI({
    display <- umep_ready()
    validate(need(!is.null(display), "Aucun résultat SOLWEIG à cartographier."))
    validate(need(!is.null(scenario$context), "Les couches contextuelles ne peuvent pas être chargées."))
    buildings_geojson <- sf_to_geojson(sf::st_sf(geometry = sf::st_geometry(urban_buildings())))
    districts_geojson <- sf_to_geojson(scenario$context$quartiers[, "district_label"])
    coordinates <- display$manifest$coordinates
    center <- colMeans(matrix(unlist(coordinates), ncol = 2, byrow = TRUE))
    javascript <- sprintf(
      paste0(
        "(function(){function init(){",
        "if(typeof maplibregl==='undefined'){setTimeout(init,100);return;}",
        "if(window.ecodekkThermalMap){try{window.ecodekkThermalMap.remove();}catch(e){}}",
        "var coordinates=%s;var buildings=%s;var districts=%s;",
        "var map=new maplibregl.Map({container:'thermal_map_canvas',center:%s,zoom:15,",
        "style:{version:8,sources:{osm:{type:'raster',tiles:['https://tile.openstreetmap.org/{z}/{x}/{y}.png'],",
        "tileSize:256,attribution:'© contributeurs OpenStreetMap'}},layers:[{id:'osm',type:'raster',source:'osm'}]}});",
        "window.ecodekkThermalMap=map;",
        "map.addControl(new maplibregl.NavigationControl(),'top-left');",
        "map.addControl(new maplibregl.ScaleControl({unit:'metric'}));",
        "map.on('load',function(){",
        "map.addSource('batiments',{type:'geojson',data:buildings});",
        "map.addLayer({id:'batiments',type:'fill',source:'batiments',paint:{'fill-color':'#9e9e9e','fill-outline-color':'#424242','fill-opacity':0.9}});",
        "map.addSource('quartiers',{type:'geojson',data:districts});",
        "map.addLayer({id:'quartiers',type:'line',source:'quartiers',paint:{'line-color':'#333','line-width':1.6,'line-dasharray':[4,3]}});",
        "map.fitBounds([coordinates[3],coordinates[1]],{padding:20,duration:0});",
        "map.ecodekkReady=true;window.ecodekkApplyThermalOverlay();",
        "});}init();})();"
      ),
      jsonlite::toJSON(coordinates),
      buildings_geojson,
      districts_geojson,
      jsonlite::toJSON(round(center, 6))
    )
    tagList(
      div(id = "thermal_map_canvas"),
      tags$script(HTML(gsub("</", "<\\/", javascript, fixed = TRUE)))
    )
  })

  observe({
    display <- umep_ready()
    req(display, input$thermal_day, input$thermal_vegetation, input$thermal_indicator)
    file <- umep_layer_file(display, input$thermal_indicator, input$thermal_day, input$thermal_vegetation)
    url <- if (is.na(file)) NULL else {
      stamp <- as.integer(file.mtime(file.path(display$directory, file)))
      paste0(umep_resource_prefix(), "/", file, "?v=", stamp)
    }
    session$sendCustomMessage("umep-overlay", list(
      url = url, coordinates = display$manifest$coordinates
    ))
  })

  output$thermal_legend <- renderUI({
    display <- umep_ready()
    req(display, input$thermal_indicator)
    legend <- umep_legend(display, input$thermal_indicator)
    div(
      tags$strong(legend$label),
      div(class = "thermal-legend-bar", style = paste0("background:", legend$gradient, ";")),
      div(class = "thermal-legend-ticks", lapply(legend$ticks, function(tick) span(format_number_fr(tick, 0))))
    )
  })

  output$thermal_guidance <- renderUI({
    req(input$thermal_indicator)
    p(class = "thermal-note", umep_indicator_guidance(input$thermal_indicator))
  })

  output$thermal_notes <- renderUI({
    display <- umep_ready()
    req(display, input$thermal_day, input$thermal_vegetation, input$thermal_indicator)
    notes <- list(p(class = "thermal-note", "Bâtiments en gris ; le calcul porte sur le sol hors bâtiments."))
    if (identical(input$thermal_indicator, "cooling") && identical(input$thermal_vegetation, "sans_arbres")) {
      notes <- c(notes, list(p(class = "thermal-note",
        "Le gain des arbres compare un calcul avec arbres à la référence sans arbres : choisissez une végétation avec arbres.")))
    }
    if (identical(input$thermal_day, "saison_pluies")) {
      notes <- c(notes, list(p(class = "thermal-note",
        "Saison des pluies : Faidherbia albida, défeuillé, est retiré ; les zones inondables sont en eau.")))
    }
    tagList(notes)
  })

  output$thermal_bar <- renderPlot({
    display <- umep_ready()
    req(display, input$thermal_day, input$thermal_vegetation, input$thermal_indicator)
    values <- umep_indicator_matrix(display, input$thermal_indicator, input$thermal_day, input$thermal_vegetation)
    validate(need(!is.null(values), "Indicateur non disponible pour cette combinaison."))
    legend <- umep_legend(display, input$thermal_indicator)
    colors <- c("Voirie" = "#737373", "Îlots" = "#41ab5d", "Zones inondables" = "#4292c6")[rownames(values)]
    old <- graphics::par(mar = c(11, 5, 3, 1), xpd = NA)
    on.exit(graphics::par(old), add = TRUE)
    graphics::barplot(
      values, beside = TRUE, col = colors, border = NA, las = 2, cex.names = 0.85,
      ylab = legend$label,
      legend.text = rownames(values),
      args.legend = list(x = "top", horiz = TRUE, bty = "n", cex = 0.85, inset = c(0, -0.14))
    )
    graphics::abline(h = 0, col = "#555555")
  })

  output$thermal_table <- renderDT({
    display <- umep_ready()
    req(display, input$thermal_day, input$thermal_vegetation)
    datatable(
      umep_indicator_table(display, input$thermal_day, input$thermal_vegetation),
      rownames = FALSE, options = list(dom = "t", pageLength = 50)
    )
  })

  output$thermal_profile <- renderPlot({
    display <- umep_ready()
    req(display, input$thermal_day, input$thermal_vegetation)
    data <- umep_profile_data(display, input$thermal_day, input$thermal_vegetation)
    validate(need(nrow(data) > 0, "Profil horaire non disponible."))
    reference <- if (identical(input$thermal_vegetation, "sans_arbres")) NULL else {
      subset <- umep_profile_data(display, input$thermal_day, "sans_arbres")
      subset[subset$class == "Sous houppier", , drop = FALSE]
    }
    air <- unique(data[order(data$hour), c("hour", "tair_c")])
    range_y <- range(c(data$tmrt_median_c, air$tair_c, reference$tmrt_median_c), na.rm = TRUE)
    old <- graphics::par(mar = c(5, 5, 2, 1))
    on.exit(graphics::par(old), add = TRUE)
    graphics::plot(NA, xlim = c(1, 24), ylim = range_y, xaxt = "n",
      xlab = "Heure de fin de pas (UTC = heure locale)", ylab = "Température (°C)")
    graphics::axis(1, at = seq(2, 24, 2))
    graphics::grid(col = "#e0e0e0")
    labels <- character(); colors <- character(); types <- integer()
    for (klass in names(umep_profile_colors)) {
      rows <- data[data$class == klass, , drop = FALSE]
      if (!nrow(rows)) next
      graphics::lines(rows$hour, rows$tmrt_median_c, col = umep_profile_colors[[klass]], lwd = 2.4)
      labels <- c(labels, paste("Tmrt,", tolower(klass))); colors <- c(colors, umep_profile_colors[[klass]]); types <- c(types, 1L)
    }
    if (!is.null(reference) && nrow(reference)) {
      graphics::lines(reference$hour, reference$tmrt_median_c, col = umep_profile_colors[["Sous houppier"]], lwd = 2, lty = 3)
      labels <- c(labels, "Tmrt aux emplacements des arbres, sans arbres"); colors <- c(colors, umep_profile_colors[["Sous houppier"]]); types <- c(types, 3L)
    }
    graphics::lines(air$hour, air$tair_c, col = "#000000", lwd = 1.6, lty = 2)
    graphics::legend("topleft", legend = c(labels, "Température de l’air (ERA5)"),
      col = c(colors, "#000000"), lty = c(types, 2L), lwd = 2, bty = "n", cex = 0.85)
  })

  target_display <- reactive({
    req(scenario$path)
    directory <- find_target_display_directory(scenario$path)
    if (is.null(directory)) return(NULL)
    tryCatch(read_target_display(directory), error = function(error) {
      structure(list(message = conditionMessage(error)), class = "umep_display_error")
    })
  })

  target_ready <- reactive({
    display <- target_display()
    if (is.null(display) || inherits(display, "umep_display_error")) NULL else display
  })

  observeEvent(target_ready(), {
    display <- target_ready()
    updateSelectInput(session, "climate_day", choices = stats::setNames(
      display$days$id,
      paste0(display$days$label, " (", format(as.Date(display$days$date), "%d/%m/%Y"), ")")
    ))
    updateSelectInput(session, "climate_quartier", choices = sort(unique(display$hourly$quartier)))
  })

  output$climate_status <- renderUI({
    display <- target_display()
    if (is.null(display)) {
      return(div(class = "alert alert-info",
        "Aucun résultat TARGET pour ce scénario. L’étude de climat urbain porte sur le scénario scenario_01 : chargez-le pour afficher ses résultats."))
    }
    if (inherits(display, "umep_display_error")) {
      return(div(class = "alert alert-danger", "Résultats TARGET illisibles : ", display$message))
    }
    p(class = "help-text", paste0(
      display$manifest$model, ", maille de ", display$manifest$grid$cell_m, " m (",
      display$manifest$grid$cells, " mailles). Météorologie ERA5 : climat régional, pas le microclimat mesuré du site."
    ))
  })

  output$climate_guide <- renderUI({
    display <- target_ready()
    req(display)
    target_reading_guide(display)
  })

  output$climate_map <- renderUI({
    display <- target_ready()
    validate(need(!is.null(display), "Aucun résultat TARGET à cartographier."))
    validate(need(!is.null(scenario$context), "Les couches contextuelles ne peuvent pas être chargées."))
    grid_map_ui("climate_map_canvas", scenario$context$quartiers, display$grid)
  })

  observe({
    display <- target_ready()
    req(display, input$climate_day, input$climate_vegetation, input$climate_indicator)
    values <- target_cell_values(display, input$climate_indicator, input$climate_day, input$climate_vegetation)
    unit <- if (identical(input$climate_vegetation, "effet")) "K" else target_indicator(input$climate_indicator)$unit
    geojson <- target_grid_geojson(display, values,
      target_scale(input$climate_indicator, input$climate_vegetation), unit)
    session$sendCustomMessage("grid-data", list(map = "climate_map_canvas", data = geojson))
  })

  output$climate_legend <- renderUI({
    req(input$climate_indicator, input$climate_vegetation)
    info <- target_indicator(input$climate_indicator)
    scale <- target_scale(input$climate_indicator, input$climate_vegetation)
    title <- if (identical(input$climate_vegetation, "effet")) {
      paste0("Effet des arbres sur ", tolower(substring(info$label, 1, 1)), substring(info$label, 2), " (K)")
    } else paste0(info$label, " (", info$unit, ")")
    div(
      tags$strong(title),
      div(class = "thermal-legend-bar", style = paste0("background:", target_gradient(scale), ";")),
      div(class = "thermal-legend-ticks", lapply(scale$stops, function(tick) span(format_number_fr(tick, 0))))
    )
  })

  output$climate_notes <- renderUI({
    display <- target_ready()
    req(display, input$climate_day, input$climate_indicator, input$climate_vegetation)
    masked <- target_masked_summary(display, input$climate_day)
    tagList(
      p(class = "thermal-note", target_indicator_guidance(input$climate_indicator, input$climate_vegetation)),
      p(class = "thermal-note", paste0(
        masked[["masked"]], " maille(s) sur ", masked[["total"]],
        " masquée(s) pour cette journée (hors domaine de validité de TARGET) : elles restent vides sur la carte."
      ))
    )
  })

  output$climate_bar <- renderPlot({
    display <- target_ready()
    req(display, input$climate_day, input$climate_vegetation, input$climate_indicator)
    values <- target_quartier_values(display, input$climate_indicator, input$climate_day, input$climate_vegetation)
    validate(need(length(values) > 0, "Indicateur non disponible pour cette combinaison."))
    info <- target_indicator(input$climate_indicator)
    values <- sort(values)
    colors <- target_colors(values, target_scale(input$climate_indicator, input$climate_vegetation))
    old <- graphics::par(mar = c(10, 5, 1, 1))
    on.exit(graphics::par(old), add = TRUE)
    graphics::barplot(values, col = colors, border = "#555555", las = 2, cex.names = 0.85,
      ylab = if (identical(input$climate_vegetation, "effet")) "Écart avec − sans arbres (K)" else paste0(info$label, " (", info$unit, ")"))
    graphics::abline(h = 0, col = "#555555")
  })

  output$climate_profile <- renderPlot({
    display <- target_ready()
    req(display, input$climate_day, input$climate_quartier, input$climate_indicator)
    data <- target_hourly_profile(display, input$climate_day, input$climate_quartier)
    validate(need(nrow(data) > 0, "Profil horaire non disponible pour ce quartier."))
    column <- switch(input$climate_indicator, utci_14h = "utci_c", uhi_14h = "uhi_k", "ta_c")
    info <- target_indicator(input$climate_indicator)
    rural <- if (column == "ta_c") unique(data[data$vegetation == "arbres", c("hour", "tb_rur_c")]) else NULL
    range_y <- range(c(data[[column]], rural$tb_rur_c), na.rm = TRUE)
    old <- graphics::par(mar = c(5, 5, 2, 1))
    on.exit(graphics::par(old), add = TRUE)
    graphics::plot(NA, xlim = c(0, 23), ylim = range_y, xaxt = "n",
      xlab = "Heure (UTC = heure locale)", ylab = paste0(sub(" à 14 h| maximale", "", info$label), " (", info$unit, ")"))
    graphics::axis(1, at = seq(0, 22, 2))
    graphics::grid(col = "#e0e0e0")
    series <- list(arbres = c("#238b45", "Avec arbres"), sans_arbres = c("#8c510a", "Sans arbres"))
    for (veg in names(series)) {
      rows <- data[data$vegetation == veg, , drop = FALSE]
      graphics::lines(rows$hour, rows[[column]], col = series[[veg]][1], lwd = 2.4)
    }
    labels <- c("Avec arbres", "Sans arbres"); colors <- c("#238b45", "#8c510a"); types <- c(1, 1)
    if (!is.null(rural)) {
      graphics::lines(rural$hour, rural$tb_rur_c, col = "#000000", lwd = 1.6, lty = 2)
      labels <- c(labels, "Référence rurale (TARGET)"); colors <- c(colors, "#000000"); types <- c(types, 2)
    }
    graphics::legend("topleft", legend = labels, col = colors, lty = types, lwd = 2, bty = "n", cex = 0.85)
  })

  energy_display <- reactive({
    req(scenario$path)
    directory <- find_energy_display_directory(scenario$path)
    if (is.null(directory)) return(NULL)
    tryCatch(read_energy_display(directory), error = function(error) {
      structure(list(message = conditionMessage(error)), class = "umep_display_error")
    })
  })

  energy_ready <- reactive({
    display <- energy_display()
    if (is.null(display) || inherits(display, "umep_display_error")) NULL else display
  })

  observeEvent(energy_ready(), {
    display <- energy_ready()
    labels <- c(chaud_saison_seche = "Journée chaude de saison sèche", saison_pluies = "Journée de saison des pluies",
                frais_saison_seche = "Journée fraîche de saison sèche")
    day_labels <- ifelse(is.na(labels[display$days$id]), display$days$id, labels[display$days$id])
    updateSelectInput(session, "energy_day", choices = stats::setNames(
      display$days$id, paste0(day_labels, " (", format(as.Date(display$days$date), "%d/%m/%Y"), ")")
    ))
    updateSelectInput(session, "energy_quartier", choices = sort(unique(display$quartiers$quartier)))
  })

  output$energy_status <- renderUI({
    display <- energy_display()
    if (is.null(display)) {
      return(div(class = "alert alert-info",
        "Aucun résultat SUEWS pour ce scénario. L’étude de bilan énergétique porte sur le scénario scenario_01."))
    }
    if (inherits(display, "umep_display_error")) {
      return(div(class = "alert alert-danger", "Résultats SUEWS illisibles : ", display$message))
    }
    p(class = "help-text", paste0(
      display$manifest$model, ". Analyse de janvier 2017 à janvier 2018 après une année de mise en route (2016)."
    ))
  })

  output$energy_guide <- renderUI({
    display <- energy_ready()
    req(display)
    energy_reading_guide(display)
  })

  output$energy_map <- renderUI({
    display <- energy_ready()
    validate(need(!is.null(display), "Aucun résultat SUEWS à cartographier."))
    validate(need(!is.null(scenario$context), "Les couches contextuelles ne peuvent pas être chargées."))
    grid_map_ui("energy_map_canvas", scenario$context$quartiers, display$grid)
  })

  observe({
    display <- energy_ready()
    req(display, input$energy_day, input$energy_scenario, input$energy_variable)
    values <- energy_cell_values(display, input$energy_variable, input$energy_day, input$energy_scenario)
    unit <- if (identical(input$energy_scenario, "difference_arbres_moins_sans") && identical(input$energy_variable, "T2")) {
      "K"
    } else energy_variable(input$energy_variable)$unit
    geojson <- energy_grid_geojson(display, values, energy_scale(input$energy_variable, input$energy_scenario), unit)
    session$sendCustomMessage("grid-data", list(map = "energy_map_canvas", data = geojson))
  })

  output$energy_legend <- renderUI({
    req(input$energy_variable, input$energy_scenario)
    info <- energy_variable(input$energy_variable)
    scale <- energy_scale(input$energy_variable, input$energy_scenario)
    effect <- identical(input$energy_scenario, "difference_arbres_moins_sans")
    unit <- if (effect && identical(input$energy_variable, "T2")) "K" else info$unit
    div(
      tags$strong(paste0(if (effect) "Effet des arbres : " else "", info$label, " à 14 h (", unit, ")")),
      div(class = "thermal-legend-bar", style = paste0("background:", target_gradient(scale), ";")),
      div(class = "thermal-legend-ticks", lapply(scale$stops, function(tick) span(format_number_fr(tick, 0))))
    )
  })

  output$energy_notes <- renderUI({
    req(input$energy_variable)
    p(class = "thermal-note", energy_variable_guidance(input$energy_variable))
  })

  output$energy_cycle <- renderPlot({
    display <- energy_ready()
    req(display, input$energy_quartier, input$energy_day, input$energy_scenario)
    data <- energy_daily_cycle(display, input$energy_quartier, input$energy_day, input$energy_scenario)
    validate(need(nrow(data) > 0, "Cycle journalier non disponible pour ce quartier."))
    fluxes <- energy_variables[energy_variables$unit == "W/m²", , drop = FALSE]
    range_y <- range(unlist(data[fluxes$id]), 0, na.rm = TRUE)
    old <- graphics::par(mar = c(5, 5, 2, 1))
    on.exit(graphics::par(old), add = TRUE)
    graphics::plot(NA, xlim = c(1, 24), ylim = range_y, xaxt = "n",
      xlab = "Heure de fin de pas (UTC = heure locale)",
      ylab = if (identical(input$energy_scenario, "difference_arbres_moins_sans")) "Écart avec − sans arbres (W/m²)" else "Flux (W/m²)")
    graphics::axis(1, at = seq(2, 24, 2))
    graphics::grid(col = "#e0e0e0")
    graphics::abline(h = 0, col = "#555555")
    for (index in seq_len(nrow(fluxes))) {
      graphics::lines(data$hour, data[[fluxes$id[index]]], col = fluxes$color[index], lwd = 2.2)
    }
    graphics::legend("topleft", legend = fluxes$label, col = fluxes$color, lwd = 2, bty = "n", cex = 0.8)
  })

  output$energy_monthly <- renderPlot({
    display <- energy_ready()
    req(display, input$energy_quartier, input$energy_variable)
    data <- energy_monthly_series(display, input$energy_quartier, input$energy_variable)
    validate(need(nrow(data) > 0, "Moyennes mensuelles non disponibles."))
    info <- energy_variable(input$energy_variable)
    months <- sort(unique(data$month))
    old <- graphics::par(mar = c(6, 5, 2, 1))
    on.exit(graphics::par(old), add = TRUE)
    graphics::plot(NA, xlim = c(1, length(months)), ylim = range(data$value, na.rm = TRUE), xaxt = "n",
      xlab = "", ylab = paste0(info$label, " (", info$unit, ")"))
    graphics::axis(1, at = seq_along(months), labels = months, las = 2, cex.axis = 0.8)
    graphics::grid(col = "#e0e0e0")
    series <- list(arbres = c("#238b45", "Avec arbres"), sans_arbres = c("#8c510a", "Sans arbres"))
    for (name in names(series)) {
      rows <- data[data$scenario == name, , drop = FALSE]
      graphics::lines(match(rows$month, months), rows$value, col = series[[name]][1], lwd = 2.4, type = "b", pch = 16)
    }
    graphics::legend("topleft", legend = c("Avec arbres", "Sans arbres"), col = c("#238b45", "#8c510a"), lwd = 2, bty = "n", cex = 0.85)
  })

  observeEvent(input$program_table_cell_edit, {
    info <- input$program_table_cell_edit
    visible_row_count <- length(program_row_indices())
    if (info$row > visible_row_count) {
      showNotification(
        "La ligne TOTAL est calculée automatiquement et ne peut pas être modifiée.",
        type = "warning"
      )
      return()
    }
    if (info$col == 2) {
      if (identical(state$program_source, "buildings")) {
        showNotification(
          "Cette quantité est calculée depuis les bâtiments SIG. Modifiez le bâtiment correspondant dans le scénario.",
          type = "warning"
        )
        return()
      }
      value <- suppressWarnings(as.numeric(info$value))
      if (!is.na(value) && value >= 0) {
        program <- state$program
        source_row <- program_row_indices()[info$row]
        program$quantity[source_row] <- value
        state$program <- program
        persist_or_notify()
      }
    }
  })

  output$balance_table <- renderDT({
    table <- build_development_balance_table(model()$balance, state$assumptions)
    display <- data.frame(
      type = table$type,
      `DÉPENSES / RECETTES` = table$label,
      `Valeur / taux` = ifelse(is.na(table$rate), "", paste0(format_number_fr(table$rate * 100, 0), " %")),
      `Prix total HT` = ifelse(is.na(table$amount_ht), "", format_cfa(table$amount_ht)),
      `Taux TVA` = ifelse(is.na(table$vat_rate), "", paste0(format_number_fr(table$vat_rate * 100, 0), " %")),
      TVA = ifelse(is.na(table$vat), "", format_cfa(table$vat)),
      `Total TTC` = ifelse(is.na(table$amount_ttc), "", format_cfa(table$amount_ttc)),
      check.names = FALSE
    )
    datatable(
      display,
      rownames = FALSE,
      escape = TRUE,
      options = list(
        dom = "t",
        paging = FALSE,
        scrollX = TRUE,
        columnDefs = list(list(visible = FALSE, targets = 0)),
        rowCallback = JS(
          "function(row, data) {",
          "  var type = data[0];",
          "  if (type === 'main_header') $(row).css({'background-color':'#111','color':'white','font-weight':'bold'});",
          "  if (type === 'section') $(row).css({'background-color':'#e6e6e6','font-weight':'bold'});",
          "  if (type === 'subsection') $(row).css({'font-weight':'bold'});",
          "  if (type === 'subtotal') $(row).css({'font-weight':'bold','border-top':'1px solid #333'});",
          "  if (type === 'grand_total' || type === 'result') $(row).css({'font-weight':'bold','background-color':'#d9ead3'});",
          "}"
        )
      )
    )
  })
}

shinyApp(ui, server)

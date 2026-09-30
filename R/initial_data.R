initial_construction_categories <- function() {
  data.frame(
    category_id = c("ECO", "STD", "MID", "HIGH", "VHIGH"),
    category_label = c(
      "Économique", "Standard", "Moyen",
      "Haut de gamme", "Très haut de gamme"
    ),
    construction_cost_cfa_sqm = c(180000, 225000, 250000, 320000, 350000),
    stringsAsFactors = FALSE
  )
}

initial_products <- function() {
  data.frame(
    product_id = c(
      "rm_1a", "rm_1b", "rm_2", "rc_1", "rc_2_log", "rc_2_com",
      "rc_3", "rc_4", "rv_1", "rv_2", "rv_3", "rt_1_log",
      "rt_1_com", "ec", "ep", "el", "ev"
    ),
    product_label = c(
      "Lotissement de maisons en bande PHARD",
      "Lotissement de maisons en condominium PHARD",
      "Maison de ville en bande",
      "Logement économique en condominium Fann Hock",
      "Logement en immeuble-barres mixte",
      "RDC commercial",
      "Logements moyen standing en immeuble-plot",
      "Logements haut standing en immeuble-plot",
      "Villa moyen standing",
      "Villa haut standing",
      "Villa très haut standing",
      "Immeuble mixte - résidentiel",
      "RDC commercial/tertiaire",
      "Immeubles bureaux/administration",
      "Équipements publics",
      "Équipements de loisirs",
      "Économie verte"
    ),
    construction_category_id = c(
      "ECO", "ECO", "STD", "STD", "HIGH", "STD", "MID", "HIGH",
      "MID", "HIGH", "VHIGH", "HIGH", "STD", "STD", "STD", "STD",
      "STD"
    ),
    sale_price_cfa_sqm = c(
      225000, 225000, 400000, 400000, 400000, 350000, 400000, 500000,
      400000, 500000, 625000, 400000, 350000, 350000, NA, NA, 5000
    ),
    construction_cost_mode = c(
      "inherit", "override", rep("inherit", 14), "override"
    ),
    construction_cost_override_cfa_sqm = c(
      NA, 200000, rep(NA, 14), 0
    ),
    land_charge_method = c(
      rep("simplified", 14), "manual", "manual", "manual"
    ),
    land_charge_factor = c(
      1.25, 1.25, 1.45, 1.25, 1.45, 1.45, 1.45, 1.45, 1.07,
      1.45, 1.45, 1.45, 1.45, 1.45, NA, NA, NA
    ),
    manual_land_charge_cfa_sqm = c(rep(NA, 14), 0, 0, 5000),
    sdp_per_unit = c(
      182, 167, 266, 43, 85, 0, 85, 133, 133, 184, 347, 115, 0,
      1216.75, 53249 / 21, 13798, 0
    ),
    uses_units_per_level = c(
      FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE,
      FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE
    ),
    units_per_level = c(
      1, 1, 1, 9, 7, 1, 5, 4, 1, 1, 1, 10, 1, 1, 1, 1, 1
    ),
    land_area_per_unit = c(rep(0, 16), 0),
    area_basis = c(rep("sdp", 16), "land"),
    is_cessible = c(rep(TRUE, 14), FALSE, FALSE, TRUE),
    default_building_levels = c(
      1, 1, 2, 2, 4, 1, 5, 5, 1, 1, 1, 4, 1, 1, 1, 1, 1
    ),
    ground_floor_commercial = c(
      FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE
    ),
    ground_floor_product_id = c(
      rep(NA_character_, 4), "rc_2_com", rep(NA_character_, 6),
      "rt_1_com", rep(NA_character_, 5)
    ),
    stringsAsFactors = FALSE
  )
}

initial_districts <- function() {
  data.frame(
    district_id = c("q1", "q2", "q3", "q4", "q5", "q6", "qa"),
    district_label = c(
      "Q1 - Quartier PHARD",
      "Q2 - Quartier Sowou",
      "Q3 - Quartier de la Forêt",
      "Q4 - Quartier du Petit Corridor",
      "Q5 - Quartier des Villas",
      "Q6 - Quartier de l’Agora",
      "QA - Quartier de l’Attractivité"
    ),
    district_vocation = c(
      rep("Résidentiel", 5),
      "Éducation, sport, culture",
      "Activités, commerce, loisirs"
    ),
    stringsAsFactors = FALSE
  )
}

initial_program <- function() {
  program <- list(
    q1 = c(rm_1a = 13, rm_1b = 40, rm_2 = 32, rc_1 = 84, rc_3 = 72,
           rc_4 = 36, ec = 3, ep = 5, ev = 3),
    q2 = c(rm_2 = 82, rc_1 = 140, rc_2_log = 43, rc_3 = 72, rc_4 = 36,
           rt_1_log = 15, rc_2_com = 0, rt_1_com = 0, ec = 1, ep = 4,
           ev = 1),
    q3 = c(rc_2_log = 53, rc_3 = 252, rc_4 = 522, rt_1_log = 24,
           rc_2_com = 0, rt_1_com = 0, ep = 1),
    q4 = c(rc_1 = 84, rv_1 = 64, rv_2 = 65, ep = 3),
    q5 = c(rv_3 = 70, ec = 2, ep = 2, ev = 1),
    q6 = c(ep = 3, ec = 1),
    qa = c(rm_2 = 36, rt_1_log = 260, rt_1_com = 0, ec = 11, el = 1,
           ep = 3, ev = 1)
  )

  rows <- do.call(rbind, lapply(names(program), function(district_id) {
    quantities <- program[[district_id]]
    data.frame(
      program_line_id = paste(district_id, names(quantities), sep = "__"),
      district_id = district_id,
      product_id = names(quantities),
      quantity = as.numeric(quantities),
      stringsAsFactors = FALSE
    )
  }))
  manual_sdp <- c(
    q1__ec = 534.5, q1__ep = 8243.5,
    q2__ec = 692, q2__ep = 7836, q3__ep = 250,
    q4__ep = 6669, q5__ec = 900, q5__ep = 1041,
    q6__ep = 20105, q6__ec = 613,
    qa__ec = 19162, qa__el = 13798, qa__ep = 9104.5
  )
  ev_land_area <- c(
    q1__ev = 16895, q2__ev = 5224, q5__ev = 7267, qa__ev = 28428
  )
  rows$manual_total_sdp <- unname(manual_sdp[rows$program_line_id])
  rows$manual_total_land_area <- unname(ev_land_area[rows$program_line_id])
  rownames(rows) <- NULL
  rows
}


initial_height_assumptions <- function() {
  list(
    ground_floor_height_m = 4,
    upper_floor_height_m = 3,
    default_levels = 1
  )
}

initial_financial_assumptions <- function() {
  list(
    vat_rate = 0.20,
    sales_fee_rate = 0.03,
    financing_cost_rate = 0.05,
    safru_fee_rate = 0.03
  )
}

initial_development_expenses <- function() {
  data.frame(
    line_id = c(
      "programming", "urban_project", "surveyor_preop", "soil_studies",
      "diagnostics", "property_purchases", "deed_costs", "relocation",
      "land_negotiation", "demolition", "network_relocation",
      "site_clearance", "decontamination", "sanitation", "drinking_water",
      "electricity", "public_lighting", "telecom", "roads", "parks",
      "roadside_planting", "agriculture", "family_gardens", "urban_forest",
      "secondary_servicing", "public_facilities_construction", "vrd_fees",
      "other_participations", "sales_surveyor", "notary", "marketing",
      "management_costs", "taxes"
    ),
    section_id = c(
      rep("1A", 5), rep("1B", 4), rep("1C", 4), rep("2A", 6),
      rep("2B", 5), "2C", "2D", "2E", "3A", rep("3B", 3), "3C", "3E"
    ),
    line_label = c(
      "A.1-Programmation", "A.2-Projet urbain", "A.3-Géomètre",
      "A.4-Études de sol - sondages", "A.5-Diagnostics divers",
      "B.1-Achats immobiliers", "B.2-Frais d’acte",
      "B.3-Indemnisation-relogement", "B.4-Négociations foncières",
      "C.1-Démolition", "C.2-Déplacement de réseaux",
      "C.3-Débroussaillage-terrassements généraux", "C.4-Dépollution",
      "A.1-Assainissement", "A.2-Eau potable", "A.3-Électricité",
      "A.4-Éclairage public", "A.5-Téléphone-Fibre",
      "A.6-Aménagement de voirie", "B.1-Parcs, squares et jardins publics",
      "B.2-Plantations voiries", "B.3-Agriculture", "B.4-Jardins familiaux",
      "B.5-Forêt urbaine",
      "C. Viabilisation secondaire-tertiaire des parcelles équipements",
      "D. Construction équipements publics",
      "E. Honoraires de conception et de suivi (VRD)",
      "A. Participations diverses", "B.1-Géomètre", "B.2-Notaire",
      "B.3-Publicité-marketing-communication",
      "C. Frais de gestion", "E. Impôts et taxes"
    ),
    amount_ht = c(
      0, 15000000, 8500000, 12000000, 0, 0, 0, 0, 0, 0, 0,
      658510000, 0, 3169850000, 650000000, 576600000, 882000000,
      108530000, 6155880000, 0, 37500000, 0, 0, 126067500, 0,
      NA, 0, 0, 0, 0, 0, 0, 0
    ),
    stringsAsFactors = FALSE
  )
}

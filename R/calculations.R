assert_columns <- function(data, required, data_name) {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(
      sprintf("Colonnes manquantes dans %s : %s", data_name, paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }
}

get_effective_construction_cost <- function(products, categories) {
  assert_columns(
    products,
    c("product_id", "construction_category_id", "construction_cost_mode",
      "construction_cost_override_cfa_sqm"),
    "les produits"
  )
  assert_columns(
    categories,
    c("category_id", "construction_cost_cfa_sqm"),
    "les catégories de construction"
  )

  category_cost <- categories$construction_cost_cfa_sqm[
    match(products$construction_category_id, categories$category_id)
  ]
  override <- products$construction_cost_override_cfa_sqm
  use_override <- products$construction_cost_mode == "override"

  if (any(use_override & is.na(override))) {
    invalid <- products$product_id[use_override & is.na(override)]
    stop(
      sprintf("Coût dérogatoire manquant pour : %s", paste(invalid, collapse = ", ")),
      call. = FALSE
    )
  }
  if (any(!use_override & is.na(category_cost))) {
    invalid <- products$product_id[!use_override & is.na(category_cost)]
    stop(
      sprintf("Catégorie de construction inconnue pour : %s", paste(invalid, collapse = ", ")),
      call. = FALSE
    )
  }

  ifelse(use_override, override, category_cost)
}

calculate_product_land_charge <- function(products, effective_cost = NULL) {
  assert_columns(
    products,
    c("product_id", "sale_price_cfa_sqm", "land_charge_method",
      "land_charge_factor", "manual_land_charge_cfa_sqm", "is_cessible"),
    "les produits"
  )
  if (is.null(effective_cost)) {
    stop("Le coût de construction effectif est requis.", call. = FALSE)
  }

  result <- rep(NA_real_, nrow(products))
  result[!products$is_cessible] <- 0

  simplified <- products$is_cessible & products$land_charge_method == "simplified"
  if (any(simplified & (is.na(products$land_charge_factor) | products$land_charge_factor <= 0))) {
    invalid <- products$product_id[
      simplified & (is.na(products$land_charge_factor) | products$land_charge_factor <= 0)
    ]
    stop(
      sprintf("Facteur de charge foncière invalide pour : %s", paste(invalid, collapse = ", ")),
      call. = FALSE
    )
  }
  result[simplified] <-
    products$sale_price_cfa_sqm[simplified] /
    products$land_charge_factor[simplified] - effective_cost[simplified]

  manual <- products$is_cessible & products$land_charge_method == "manual"
  result[manual] <- products$manual_land_charge_cfa_sqm[manual]

  promoter <- products$is_cessible & products$land_charge_method == "promoter_balance"
  result[promoter] <- NA_real_
  result
}

calculate_program_sdp <- function(program, products) {
  assert_columns(program, c("program_line_id", "district_id", "product_id", "quantity"), "la programmation")
  assert_columns(products, c("product_id", "sdp_per_unit", "land_area_per_unit", "area_basis"), "les produits")

  if (any(is.na(program$quantity) | program$quantity < 0)) {
    stop("Les quantités doivent être renseignées et positives ou nulles.", call. = FALSE)
  }
  product_index <- match(program$product_id, products$product_id)
  if (anyNA(product_index)) {
    stop(
      sprintf(
        "Produit inconnu dans la programmation : %s",
        paste(unique(program$product_id[is.na(product_index)]), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  output <- program
  output$sdp_per_unit <- products$sdp_per_unit[product_index]
  output$land_area_per_unit <- products$land_area_per_unit[product_index]
  output$area_basis <- products$area_basis[product_index]
  output$total_sdp <- output$quantity * output$sdp_per_unit
  if ("manual_total_sdp" %in% names(output)) {
    use_manual_sdp <- !is.na(output$manual_total_sdp)
    output$total_sdp[use_manual_sdp] <- output$manual_total_sdp[use_manual_sdp]
  }
  output$effective_sdp_per_unit <- ifelse(
    output$quantity > 0, output$total_sdp / output$quantity, output$sdp_per_unit
  )
  output$total_land_area <- output$quantity * output$land_area_per_unit
  if ("manual_total_land_area" %in% names(output)) {
    use_manual_land <- !is.na(output$manual_total_land_area)
    output$total_land_area[use_manual_land] <- output$manual_total_land_area[use_manual_land]
  }
  output$calculation_area <- ifelse(
    output$area_basis == "land",
    output$total_land_area,
    output$total_sdp
  )
  output
}

calculate_program_financials <- function(program, products, categories) {
  calculated_program <- calculate_program_sdp(program, products)
  product_index <- match(calculated_program$product_id, products$product_id)
  effective_cost <- get_effective_construction_cost(products, categories)
  effective_land_charge <- calculate_product_land_charge(products, effective_cost)

  calculated_program$product_label <- products$product_label[product_index]
  calculated_program$is_cessible <- products$is_cessible[product_index]
  calculated_program$sale_price_cfa_sqm <- products$sale_price_cfa_sqm[product_index]
  calculated_program$effective_construction_cost_cfa_sqm <- effective_cost[product_index]
  calculated_program$effective_land_charge_cfa_sqm <- effective_land_charge[product_index]

  calculated_program$sales_revenue_ht <- ifelse(
    calculated_program$is_cessible,
    calculated_program$calculation_area * calculated_program$sale_price_cfa_sqm,
    0
  )
  calculated_program$construction_cost_ht <-
    calculated_program$total_sdp * calculated_program$effective_construction_cost_cfa_sqm
  calculated_program$land_revenue_ht <- ifelse(
    calculated_program$is_cessible,
    calculated_program$calculation_area * calculated_program$effective_land_charge_cfa_sqm,
    0
  )
  calculated_program
}

summarise_program <- function(program_financials, group_column) {
  if (!group_column %in% names(program_financials)) {
    stop(sprintf("Colonne de regroupement inconnue : %s", group_column), call. = FALSE)
  }
  split_rows <- split(program_financials, program_financials[[group_column]])
  result <- do.call(rbind, lapply(names(split_rows), function(group_id) {
    rows <- split_rows[[group_id]]
    data.frame(
      group_id = group_id,
      quantity = sum(rows$quantity),
      total_sdp = sum(rows$total_sdp),
      sales_revenue_ht = sum(rows$sales_revenue_ht, na.rm = TRUE),
      construction_cost_ht = sum(rows$construction_cost_ht, na.rm = TRUE),
      land_revenue_ht = sum(rows$land_revenue_ht, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  rownames(result) <- NULL
  names(result)[1] <- group_column
  result
}

calculate_product_summary <- function(program_financials) {
  summarise_program(program_financials, "product_id")
}

calculate_district_summary <- function(program_financials) {
  summarise_program(program_financials, "district_id")
}

calculate_development_balance <- function(
    program_financials,
    districts,
    fixed_expenses,
    assumptions) {
  required_rates <- c("vat_rate", "sales_fee_rate", "financing_cost_rate", "safru_fee_rate")
  if (!all(required_rates %in% names(assumptions))) {
    stop("Les taux financiers requis ne sont pas tous renseignés.", call. = FALSE)
  }

  vat_rate <- assumptions$vat_rate
  district_summary <- calculate_district_summary(program_financials)
  district_index <- match(districts$district_id, district_summary$district_id)
  cessions_ht <- district_summary$land_revenue_ht[district_index]
  cessions_ht[is.na(cessions_ht)] <- 0


  expenses <- fixed_expenses

  subtotal_1_ht <- sum(expenses$amount_ht[grepl("^1", expenses$section_id)], na.rm = TRUE)
  subtotal_2_ht <- sum(expenses$amount_ht[grepl("^2", expenses$section_id)], na.rm = TRUE)
  base_other_expenses_ht <- sum(expenses$amount_ht[grepl("^3", expenses$section_id)], na.rm = TRUE)
  total_cessions_ht <- sum(cessions_ht, na.rm = TRUE)
  sales_fee_ht <- total_cessions_ht * (1 + vat_rate) * assumptions$sales_fee_rate
  total_revenue_ht <- total_cessions_ht
  safru_fee_ht <- total_revenue_ht * assumptions$safru_fee_rate

  pre_financing_expenses_ht <-
    subtotal_1_ht + subtotal_2_ht + base_other_expenses_ht + sales_fee_ht + safru_fee_ht
  pre_financing_expenses_ttc <- pre_financing_expenses_ht * (1 + vat_rate)
  financing_cost_ht <- pre_financing_expenses_ttc * assumptions$financing_cost_rate

  subtotal_3_ht <-
    base_other_expenses_ht + sales_fee_ht + financing_cost_ht + safru_fee_ht
  total_expenses_ht <- subtotal_1_ht + subtotal_2_ht + subtotal_3_ht
  subtotal_1_vat <- subtotal_1_ht * vat_rate
  subtotal_2_vat <- subtotal_2_ht * vat_rate
  subtotal_3_vat <- (base_other_expenses_ht + sales_fee_ht + safru_fee_ht) * vat_rate
  total_expenses_vat <- subtotal_1_vat + subtotal_2_vat + subtotal_3_vat
  total_expenses_ttc <- total_expenses_ht + total_expenses_vat
  total_revenue_ttc <- total_revenue_ht * (1 + vat_rate)

  list(
    district_cessions = data.frame(
      district_id = districts$district_id,
      district_label = districts$district_label,
      amount_ht = cessions_ht,
      vat_rate = vat_rate,
      vat = cessions_ht * vat_rate,
      amount_ttc = cessions_ht * (1 + vat_rate),
      stringsAsFactors = FALSE
    ),
    expenses = expenses,
    metrics = c(
      subtotal_1_ht = subtotal_1_ht,
      subtotal_1_vat = subtotal_1_vat,
      subtotal_2_ht = subtotal_2_ht,
      subtotal_2_vat = subtotal_2_vat,
      sales_fee_ht = sales_fee_ht,
      financing_cost_ht = financing_cost_ht,
      safru_fee_ht = safru_fee_ht,
      subtotal_3_ht = subtotal_3_ht,
      subtotal_3_vat = subtotal_3_vat,
      total_expenses_ht = total_expenses_ht,
      total_expenses_vat = total_expenses_vat,
      total_revenue_ht = total_revenue_ht,
      result_ht = total_revenue_ht - total_expenses_ht,
      total_expenses_ttc = total_expenses_ttc,
      total_revenue_ttc = total_revenue_ttc,
      result_ttc = total_revenue_ttc - total_expenses_ttc
    )
  )
}

# Population : chaque ligne de programmation (quartier × produit) porte
# quantité × personnes par unité, puis les totaux sont agrégés par produit et
# par quartier. Une ligne programmée sans ratio renseigné reste non calculée et
# rend le total incomplet ; elle n'est jamais remplacée par une valeur plausible.
calculate_program_population <- function(program_financials, products) {
  ratio <- if ("persons_per_unit" %in% names(products)) {
    suppressWarnings(as.numeric(products$persons_per_unit))
  } else {
    rep(NA_real_, nrow(products))
  }
  lines <- data.frame(
    district_id = program_financials$district_id,
    product_id = program_financials$product_id,
    quantity = program_financials$quantity,
    persons_per_unit = ratio[match(program_financials$product_id, products$product_id)],
    stringsAsFactors = FALSE
  )
  lines$population <- ifelse(lines$quantity == 0, 0, lines$quantity * lines$persons_per_unit)
  aggregate_population <- function(group_column) {
    groups <- split(lines, lines[[group_column]])
    result <- data.frame(
      group = names(groups),
      quantity = vapply(groups, function(rows) sum(rows$quantity), numeric(1)),
      population = vapply(groups, function(rows) sum(rows$population, na.rm = TRUE), numeric(1)),
      complete = vapply(groups, function(rows) !anyNA(rows$population), logical(1)),
      stringsAsFactors = FALSE
    )
    names(result)[1] <- group_column
    rownames(result) <- NULL
    result
  }
  by_product <- aggregate_population("product_id")
  by_product$persons_per_unit <- ratio[match(by_product$product_id, products$product_id)]
  by_product$population[!by_product$complete] <- NA_real_
  missing <- unique(lines$product_id[is.na(lines$population)])
  list(
    lines = lines,
    by_product = by_product,
    by_district = aggregate_population("district_id"),
    total = sum(lines$population, na.rm = TRUE),
    complete = !length(missing),
    missing_products = missing
  )
}

run_financial_model <- function(
    categories,
    products,
    districts,
    program,
    fixed_expenses,
    assumptions) {
  program_financials <- calculate_program_financials(program, products, categories)
  list(
    program = program_financials,
    products = calculate_product_summary(program_financials),
    population = calculate_program_population(program_financials, products),
    districts = calculate_district_summary(program_financials),
    balance = calculate_development_balance(
      program_financials, districts, fixed_expenses, assumptions
    )
  )
}

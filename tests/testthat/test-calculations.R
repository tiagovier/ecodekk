test_that("les coûts de construction hérités et dérogatoires sont appliqués", {
  categories <- initial_construction_categories()
  products <- initial_products()
  costs <- get_effective_construction_cost(products, categories)

  expect_equal(costs[products$product_id == "rm_1a"], 180000)
  expect_equal(costs[products$product_id == "rm_1b"], 200000)
  expect_equal(costs[products$product_id == "rv_3"], 350000)
  expect_equal(costs[products$product_id == "ev"], 0)
})

test_that("les charges foncières reproduisent les valeurs de référence", {
  categories <- initial_construction_categories()
  products <- initial_products()
  costs <- get_effective_construction_cost(products, categories)
  charges <- calculate_product_land_charge(products, costs)

  expect_equal(charges[products$product_id == "rm_2"], 50862.0689655173)
  expect_equal(charges[products$product_id == "rv_1"], 123831.775700935)
  expect_equal(charges[products$product_id == "rc_2_log"], -44137.9310344827)
  expect_equal(charges[products$product_id == "ep"], 0)
  expect_equal(charges[products$product_id == "el"], 0)
  expect_equal(charges[products$product_id == "ev"], 5000)
})

test_that("promoter_balance reste explicitement non calculé", {
  categories <- initial_construction_categories()
  products <- initial_products()
  products$land_charge_method[products$product_id == "rm_2"] <- "promoter_balance"
  costs <- get_effective_construction_cost(products, categories)
  charges <- calculate_product_land_charge(products, costs)

  expect_true(is.na(charges[products$product_id == "rm_2"]))
})

test_that("la programmation utilise le SDP par unité du produit", {
  calculated <- calculate_program_sdp(initial_program(), initial_products())

  rm_1b_q1 <- calculated[calculated$program_line_id == "q1__rm_1b", ]
  expect_equal(rm_1b_q1$total_sdp, 6680)
  expect_equal(sum(calculated$total_sdp), 351107.5)
  expect_true(all(calculated$quantity[calculated$product_id %in% c("rc_2_com", "rt_1_com")] == 0))
})

test_that("les recettes foncières conservent la péréquation négative", {
  model <- run_financial_model(
    initial_construction_categories(),
    initial_products(),
    initial_districts(),
    initial_program(),
    initial_development_expenses(),
    initial_financial_assumptions()
  )

  rc_2_log <- model$products[model$products$product_id == "rc_2_log", ]
  expect_lt(rc_2_log$land_revenue_ht, 0)
  expect_equal(sum(model$program$land_revenue_ht), 8075197540.28361, tolerance = 0.01)
})

test_that("EP et EL ne génèrent pas de recettes mais conservent leurs coûts", {
  program_financials <- calculate_program_financials(
    initial_program(), initial_products(), initial_construction_categories()
  )
  public_rows <- program_financials$product_id %in% c("ep", "el")

  expect_equal(sum(program_financials$sales_revenue_ht[public_rows]), 0)
  expect_equal(sum(program_financials$land_revenue_ht[public_rows]), 0)
  expect_equal(sum(program_financials$construction_cost_ht[public_rows]), 15085575000)
})

test_that("la TVA suit les règles du bilan de référence", {
  assumptions <- initial_financial_assumptions()
  model <- run_financial_model(
    initial_construction_categories(),
    initial_products(),
    initial_districts(),
    initial_program(),
    initial_development_expenses(),
    assumptions
  )
  balance <- model$balance

  expect_equal(
    balance$district_cessions$amount_ttc,
    balance$district_cessions$amount_ht * 1.2
  )
  expect_equal(
    unname(balance$metrics["total_expenses_ttc"]),
    unname(balance$metrics["total_expenses_ht"]) + unname(balance$metrics["total_expenses_vat"])
  )
  expect_equal(
    unname(balance$metrics["sales_fee_ht"]),
    unname(balance$metrics["total_revenue_ht"]) * (1 + assumptions$vat_rate) * assumptions$sales_fee_rate
  )
})


test_that("le bilan initial applique les formules corrigées sans calage", {
  model <- run_financial_model(
    initial_construction_categories(),
    initial_products(),
    initial_districts(),
    initial_program(),
    initial_development_expenses(),
    initial_financial_assumptions()
  )

  expected_districts <- c(
    q1 = 1012858017, q2 = 1758446207, q3 = 1956983448,
    q4 = 1694134006, q5 = 2019403966, q6 = 10040517, qa = -376668621
  )
  actual_districts <- setNames(
    round(model$balance$district_cessions$amount_ht),
    model$balance$district_cessions$district_id
  )

  expect_equal(actual_districts[names(expected_districts)], expected_districts)
  expect_equal(round(unname(model$balance$metrics["total_revenue_ht"])), 8075197540)
  expect_equal(round(unname(model$balance$metrics["sales_fee_ht"])), 290707111)
  expect_equal(round(unname(model$balance$metrics["financing_cost_ht"])), 776004032)
  expect_equal(round(unname(model$balance$metrics["safru_fee_ht"])), 242255926)
  expect_equal(round(unname(model$balance$metrics["total_expenses_ht"])), 13709404570)
  expect_equal(round(unname(model$balance$metrics["result_ht"])), -5634207030)
  expect_lt(actual_districts["qa"], 0)
})

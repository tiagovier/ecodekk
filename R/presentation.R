format_cfa <- function(value, digits = 0) {
  ifelse(
    is.na(value),
    "Non disponible",
    paste0(
      formatC(value, format = "f", digits = digits, big.mark = " ", decimal.mark = ","),
      " CFA"
    )
  )
}

format_number_fr <- function(value, digits = 1) {
  ifelse(
    is.na(value),
    "Non disponible",
    formatC(value, format = "f", digits = digits, big.mark = " ", decimal.mark = ",")
  )
}

build_development_balance_table <- function(balance, assumptions) {
  expenses <- balance$expenses
  metrics <- balance$metrics
  vat_rate <- assumptions$vat_rate

  rows <- list()
  add_row <- function(type, label, rate = NA_real_, amount_ht = NA_real_, taxable = NULL) {
    if (is.null(taxable)) taxable <- type == "detail" || type == "calculated"
    vat <- if (taxable && !is.na(amount_ht)) amount_ht * vat_rate else NA_real_
    amount_ttc <- if (!is.na(amount_ht)) {
      if (taxable) amount_ht + vat else amount_ht
    } else {
      NA_real_
    }
    rows[[length(rows) + 1L]] <<- data.frame(
      type = type,
      label = label,
      rate = rate,
      amount_ht = amount_ht,
      vat_rate = if (taxable) vat_rate else NA_real_,
      vat = vat,
      amount_ttc = amount_ttc,
      stringsAsFactors = FALSE
    )
  }
  add_expense_lines <- function(section_id) {
    section <- expenses[expenses$section_id == section_id, , drop = FALSE]
    for (i in seq_len(nrow(section))) {
      add_row("detail", section$line_label[i], amount_ht = section$amount_ht[i])
    }
  }
  add_total <- function(type, label, amount_ht, vat_amount = NULL) {
    if (is.null(vat_amount)) vat_amount <- amount_ht * vat_rate
    rows[[length(rows) + 1L]] <<- data.frame(
      type = type,
      label = label,
      rate = NA_real_,
      amount_ht = amount_ht,
      vat_rate = NA_real_,
      vat = vat_amount,
      amount_ttc = amount_ht + vat_amount,
      stringsAsFactors = FALSE
    )
  }

  add_row("main_header", "DÉPENSES")
  add_row("section", "1. FONCIER - LIBÉRATION DES SOLS")
  add_row("subsection", "A. Études préopérationnelles")
  add_expense_lines("1A")
  add_row("subsection", "B. Acquisitions foncières")
  add_expense_lines("1B")
  add_row("subsection", "C. Travaux de mise en état")
  add_expense_lines("1C")
  add_total("subtotal", "SOUS-TOTAL 1", metrics["subtotal_1_ht"], metrics["subtotal_1_vat"])

  add_row("section", "2. TRAVAUX ET HONORAIRES")
  add_row("subsection", "A. Voiries et réseaux primaires")
  add_expense_lines("2A")
  add_row("subsection", "B. Espaces verts et plantations")
  add_expense_lines("2B")
  add_expense_lines("2C")
  add_expense_lines("2D")
  add_expense_lines("2E")
  add_total("subtotal", "SOUS-TOTAL 2", metrics["subtotal_2_ht"], metrics["subtotal_2_vat"])

  add_row("section", "3. FRAIS DIVERS")
  add_expense_lines("3A")
  add_row(
    "calculated", "B. Frais sur ventes",
    assumptions$sales_fee_rate, metrics["sales_fee_ht"]
  )
  add_expense_lines("3B")
  add_expense_lines("3C")
  add_row(
    "calculated", "D. Frais financiers",
    assumptions$financing_cost_rate, metrics["financing_cost_ht"], taxable = FALSE
  )
  add_expense_lines("3E")
  add_row(
    "calculated", "F. Rémunération SAFRU",
    assumptions$safru_fee_rate, metrics["safru_fee_ht"]
  )
  add_total("subtotal", "SOUS-TOTAL 3", metrics["subtotal_3_ht"], metrics["subtotal_3_vat"])
  add_total("grand_total", "TOTAL DÉPENSES", metrics["total_expenses_ht"], metrics["total_expenses_vat"])

  add_row("main_header", "RECETTES")
  add_row("section", "1. CESSIONS")
  for (i in seq_len(nrow(balance$district_cessions))) {
    add_row(
      "detail",
      balance$district_cessions$district_label[i],
      amount_ht = balance$district_cessions$amount_ht[i]
    )
  }
  add_total("subtotal", "SOUS-TOTAL 1", metrics["total_revenue_ht"])

  add_row("section", "2. PARTICIPATIONS - SUBVENTIONS")
  add_row("detail", "A. Participation collectivités", amount_ht = 0)
  add_row("detail", "B. Participation promoteurs", amount_ht = 0)
  add_row("detail", "C. Subventions", amount_ht = 0)
  add_total("subtotal", "SOUS-TOTAL 2", 0)

  add_row("section", "3. PRODUITS DIVERS")
  add_row("detail", "A. Produits financiers", amount_ht = 0)
  add_row("detail", "B. Produits de gestion", amount_ht = 0)
  add_total("subtotal", "SOUS-TOTAL 3", 0)
  add_total("grand_total", "TOTAL RECETTES", metrics["total_revenue_ht"])
  add_row(
    "result", "RÉSULTAT D’OPÉRATION",
    metrics["result_ht"] / metrics["total_revenue_ht"],
    metrics["result_ht"], taxable = FALSE
  )

  do.call(rbind, rows)
}

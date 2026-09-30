test_that("le chargement différé conserve la session Shiny", {
  app_directory <- normalizePath(file.path("..", ".."), winslash = "/")
  withr::local_dir(app_directory)
  app_environment <- new.env(parent = globalenv())
  sys.source("app.R", envir = app_environment)

  shiny::testServer(app_environment$server, {
    run_scenario_load_in_session(app_environment$default_scenario_selection)

    expect_false(scenario$loading)
    expect_null(scenario$error)
    expect_equal(scenario$id, "base")
    expect_equal(nrow(state$buildings), 528)
    buildings <- all_buildings()
    expect_equal(
      control_buildings()$building_id,
      buildings$building_id[buildings$control_required]
    )
    session$setInputs(building_review_filter = "all")
    expect_equal(nrow(review_buildings()), nrow(buildings))
    map_html <- as.character(output$building_3d_map)
    expect_true(any(grepl("map_view_2d", map_html, fixed = TRUE)))
    expect_true(any(grepl("raster-saturation", map_html, fixed = TRUE)))
    expect_true(any(grepl("typology_code", map_html, fixed = TRUE)))
    expect_true(any(grepl("visibility:'none'", map_html, fixed = TRUE)))
  })
})

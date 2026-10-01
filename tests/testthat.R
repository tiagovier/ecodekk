library(testthat)

source(file.path("R", "initial_data.R"), encoding = "UTF-8")
source(file.path("R", "calculations.R"), encoding = "UTF-8")
source(file.path("R", "spatial_data.R"), encoding = "UTF-8")
source(file.path("R", "presentation.R"), encoding = "UTF-8")
source(file.path("R", "scenario_data.R"), encoding = "UTF-8")
source(file.path("R", "umep_results.R"), encoding = "UTF-8")
source(file.path("R", "umep_climate.R"), encoding = "UTF-8")

test_dir(file.path("tests", "testthat"), reporter = "summary")

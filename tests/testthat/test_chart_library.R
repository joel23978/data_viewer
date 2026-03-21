source(here::here("R", "bootstrap.R"))
source(here::here("external_data.R"))
source(here::here("R", "providers.R"))
source(here::here("R", "chart_helpers.R"))
source(here::here("R", "data_search.R"))
source(here::here("R", "chart_library.R"))
source(here::here("R", "analysis_helpers.R"))
source(here::here("R", "main_app.R"))
source(here::here("tests", "testthat", "helper_app_fixtures.R"))

test_that("chart library uses the configured path and persists records", {
  temp_library <- tempfile(fileext = ".rds")
  temp_presentation_library <- tempfile(fileext = ".rds")
  old_option <- getOption("data_viewer.chart_library_path")
  old_presentation_option <- getOption("data_viewer.chart_presentation_library_path")
  options(data_viewer.chart_library_path = temp_library)
  options(data_viewer.chart_presentation_library_path = temp_presentation_library)
  on.exit(options(data_viewer.chart_library_path = old_option), add = TRUE)
  on.exit(options(data_viewer.chart_presentation_library_path = old_presentation_option), add = TRUE)

  ensure_chart_library()
  initial_library <- read_chart_library()
  expect_equal(nrow(initial_library), 0)

  payload <- build_chart_data(build_test_state())
  record <- new_chart_record(build_test_state(), payload$data, title = "Saved chart", description = "Test record")
  expect_true(is.factor(record$data_snapshot[[1]]$name))
  expect_true(is.factor(record$data_snapshot[[1]]$plotting))
  expect_type(record$data_snapshot[[1]]$date, "integer")
  write_chart_library(upsert_chart_record(initial_library, record))

  updated_library <- read_chart_library()
  expect_equal(nrow(updated_library), 1)
  expect_equal(updated_library$title[[1]], "Saved chart")
  expect_s3_class(updated_library$data_snapshot[[1]]$date, "Date")
  expect_type(updated_library$data_snapshot[[1]]$value, "double")
  expect_type(updated_library$data_snapshot[[1]]$name, "character")
  expect_type(updated_library$data_snapshot[[1]]$plotting, "character")

  presentation_record <- new_chart_presentation_record(
    title = "Macro deck",
    description = "Presentation test",
    chart_ids = updated_library$chart_id
  )
  write_chart_presentation_library(
    upsert_chart_presentation_record(read_chart_presentation_library(), presentation_record)
  )

  updated_presentations <- read_chart_presentation_library()
  expect_equal(nrow(updated_presentations), 1)
  expect_equal(updated_presentations$title[[1]], "Macro deck")
  expect_equal(updated_presentations$chart_count[[1]], 1)
})

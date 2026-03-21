source(here::here("R", "bootstrap.R"))
source(here::here("external_data.R"))
source(here::here("R", "providers.R"))
source(here::here("R", "chart_helpers.R"))
source(here::here("R", "data_search.R"))
source(here::here("R", "chart_library.R"))
source(here::here("R", "analysis_helpers.R"))
source(here::here("R", "main_app.R"))
source(here::here("tests", "testthat", "helper_app_fixtures.R"))

test_that("chart builder pipeline returns plottable data and widgets", {
  state <- build_test_state()
  payload <- build_chart_data(state)

  expect_gt(nrow(payload$data), 0)
  expect_length(unique(payload$data$name), 2)
  expect_s3_class(build_chart_plot(payload$data, state$style), "ggplot")
  expect_s3_class(build_chart_widget(payload$data, state$style), "plotly")

  alias_state <- state
  alias_state$series[[1]]$transform_profile$expression <- "X * 2"
  alias_payload <- build_chart_data(alias_state)
  legacy_alias_state <- state
  legacy_alias_state$series[[1]]$transform_profile$expression <- "data * 2"
  legacy_alias_payload <- build_chart_data(legacy_alias_state)
  expect_equal(alias_payload$data$value, legacy_alias_payload$data$value)

  partial_manual_style <- state$style
  partial_manual_style$auto_y_axis <- FALSE
  partial_manual_style$y_min <- 95
  partial_manual_style$y_max <- NA_real_
  partial_manual_style$y_breaks <- NA_real_
  partial_manual_plot <- build_chart_plot(payload$data, partial_manual_style)
  expect_s3_class(partial_manual_plot, "ggplot")

  widget_with_note <- build_chart_widget(payload$data, state$style)
  expect_true(length(widget_with_note$x$layoutAttrs[[1]]$annotations) >= 1)
})

test_that("rolling sum transformation is applied to chart data", {
  state <- build_test_state()
  state$series[[2]] <- NULL
  state$series[[1]]$transform_profile$rolling_sum <- 3

  base_state <- build_test_state()
  base_state$series[[2]] <- NULL

  rolling_payload <- build_chart_data(state)
  base_payload <- build_chart_data(base_state)

  expect_gt(nrow(rolling_payload$data), 0)
  expect_lte(nrow(rolling_payload$data), nrow(base_payload$data))
  expect_false(identical(rolling_payload$data$value, tail(base_payload$data$value, nrow(rolling_payload$data))))
})

test_that("default source notes include source names and series identifiers", {
  generated_note <- default_source_note(list(
    list(source = "abs", abs_id = c("A1837036T", "A1837037V")),
    list(source = "FRED", fred_series = "UNRATE")
  ))

  expect_equal(generated_note, "Source: abs - A1837036T, A1837037V | FRED - UNRATE")
  expect_equal(default_builder_state()$style$note, "Source: custom query")
})

test_that("presentation export bundle includes chart HTML and widget assets", {
  state <- build_test_state()
  payload <- build_chart_data(state)
  bundle_dir <- tempfile(pattern = "presentation_bundle_test_")
  dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(bundle_dir, recursive = TRUE, force = TRUE), add = TRUE)
  html_file <- file.path(bundle_dir, "presentation.html")
  zip_file <- tempfile(fileext = ".zip")

  htmltools::save_html(
    htmltools::browsable(
      tags$html(
        tags$head(tags$title("Presentation test")),
        tags$body(
          tags$section(
            htmltools::as.tags(build_chart_widget(payload$data, state$style))
          )
        )
      )
    ),
    file = html_file,
    libdir = "lib"
  )

  zip::zipr(
    zipfile = zip_file,
    files = c("presentation.html", "lib"),
    root = bundle_dir
  )

  zipped_files <- unzip(zip_file, list = TRUE)$Name
  exported_html <- paste(readLines(html_file, warn = FALSE), collapse = "\n")

  expect_true("presentation.html" %in% zipped_files)
  expect_true(any(startsWith(zipped_files, "lib/")))
  expect_true(grepl("plotly", exported_html, fixed = TRUE))
  expect_true(grepl("html-widget", exported_html, fixed = TRUE))
})

test_that("series downloads are cached across date range changes", {
  counter <- 0
  original_fred_data <- fred_data
  on.exit(assign("fred_data", original_fred_data, envir = .GlobalEnv), add = TRUE)
  rm(list = ls(envir = series_cache_env), envir = series_cache_env)

  assign(
    "fred_data",
    function(series, start_date = NULL, end_date = NULL) {
      counter <<- counter + 1
      tibble::tibble(
        date = seq(as.Date("2020-01-01"), as.Date("2025-12-01"), by = "month"),
        value = seq_len(72),
        name = series
      )
    },
    envir = .GlobalEnv
  )

  state_one <- build_test_state()
  state_one$series[[1]] <- list(
    index = 1,
    source = "FRED",
    fred_series = "TEST_CACHE_SERIES",
    label = "Cached FRED",
    transform_profile = list(expression = "X", moving_average = 1, rolling_sum = 1, lagged_value = 0, lagged_pct = 0, lagged_ann = 0),
    vis_type = "line"
  )
  state_one$series[[2]] <- NULL
  state_one$date_range <- c(2024, 2024)

  state_two <- state_one
  state_two$date_range <- c(2023, 2025)

  first_payload <- build_chart_data(state_one)
  second_payload <- build_chart_data(state_two)

  expect_equal(counter, 1)
  expect_true(all(lubridate::year(first_payload$data$date) == 2024))
  expect_true(all(lubridate::year(second_payload$data$date) >= 2023))
  expect_true(all(lubridate::year(second_payload$data$date) <= 2025))
})

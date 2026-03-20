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
  expect_lt(nrow(rolling_payload$data), nrow(base_payload$data))
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

test_that("FRED source controls render the restored series ID", {
  withr::local_envvar(FRED_API_KEY = "test-key")
  original_fred_vintage_dates <- fred_vintage_dates
  on.exit(assign("fred_vintage_dates", original_fred_vintage_dates, envir = .GlobalEnv), add = TRUE)

  assign(
    "fred_vintage_dates",
    function(series, force = FALSE) {
      as.Date(c("2019-01-01", "2020-01-01"))
    },
    envir = .GlobalEnv
  )

  fred_ui <- series_source_controls_ui(
    input = list(),
    session = NULL,
    index = 2,
    source_value = "FRED",
    restored_spec = list(
      source = "FRED",
      fred_series = "UNRATE",
      fred_vintage_mode = "compare",
      fred_vintage_date = as.Date("2020-01-01")
    )
  )

  expect_true(grepl("UNRATE", as.character(fred_ui), fixed = TRUE))
  expect_true(grepl("2020-01-01", as.character(fred_ui), fixed = TRUE))
  expect_true(grepl("Vintage mode", as.character(fred_ui), fixed = TRUE))
})

test_that("FRED series supports current, historical, and compare vintage modes", {
  original_fred_data <- fred_data
  on.exit(assign("fred_data", original_fred_data, envir = .GlobalEnv), add = TRUE)

  assign(
    "fred_data",
    function(series, start_date = NULL, end_date = NULL, realtime_start = NULL, realtime_end = NULL, vintage_dates = NULL, name_override = NULL) {
      series_name <- if (!is.null(name_override) && nzchar(name_override)) name_override else series
      tibble::tibble(
        date = seq(as.Date("2020-01-01"), by = "month", length.out = 3),
        value = seq_len(3),
        name = series_name
      )
    },
    envir = .GlobalEnv
  )

  current_spec <- list(
    index = 1,
    source = "FRED",
    fred_series = "UNRATE",
    fred_vintage_mode = "current",
    transform_profile = default_transform_profile(),
    vis_type = "line"
  )
  historical_spec <- current_spec
  historical_spec$fred_vintage_mode <- "historical"
  historical_spec$fred_vintage_date <- as.Date("2020-01-01")
  compare_spec <- historical_spec
  compare_spec$fred_vintage_mode <- "compare"

  current_data <- query_series_history(current_spec)
  historical_data <- query_series_history(historical_spec)
  compare_data <- query_series_history(compare_spec)

  expect_equal(unique(current_data$name), "UNRATE")
  expect_true(any(grepl("2020-01-01 vintage", historical_data$name, fixed = TRUE)))
  expect_equal(length(unique(compare_data$name)), 2)
  expect_true(any(grepl("current", compare_data$name, fixed = TRUE)))
  expect_true(any(grepl("2020-01-01 vintage", compare_data$name, fixed = TRUE)))
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

test_that("analysis helpers produce rolling correlations, regressions, and forecasts", {
  payload <- build_chart_data(build_test_state())
  chart_data <- payload$data
  series_names <- unique(chart_data$name)

  rolling <- rolling_correlation_data(chart_data, series_names[[1]], series_names[[2]], window = 4)
  expect_gt(nrow(rolling), 0)
  expect_true(all(c("date", "correlation") %in% colnames(rolling)))

  regression <- regression_analysis(chart_data, dependent = series_names[[1]], independent = series_names[[2]], error_assumption = "robust")
  expect_true(all(c("term", "Estimate") %in% colnames(regression$coefficients)))
  expect_true(any(grepl("Std", colnames(regression$coefficients))))
  expect_gt(regression$metrics$observations, 5)

  regression_classical <- regression_analysis(chart_data, dependent = series_names[[1]], independent = series_names[[2]], error_assumption = "classical")
  expect_gt(regression_classical$metrics$observations, 5)
  expect_equal(regression$metrics$model_label, "Heteroskedastic-adjusted")
  expect_equal(regression_classical$metrics$model_label, "Homoskedastic OLS")

  set.seed(123)
  synthetic_data <- tibble::tibble(
    date = rep(seq(as.Date("2020-01-01"), by = "month", length.out = 40), 2),
    name = rep(c("X", "Y"), each = 40),
    value = c(
      seq_len(40),
      2 + (1.5 * seq_len(40)) + stats::rnorm(40, sd = seq(0.5, 6, length.out = 40))
    ),
    plotting = "line"
  )
  synthetic_robust <- regression_analysis(synthetic_data, dependent = "Y", independent = "X", error_assumption = "robust")
  synthetic_classical <- regression_analysis(synthetic_data, dependent = "Y", independent = "X", error_assumption = "classical")
  expect_false(isTRUE(all.equal(synthetic_robust$metrics$r_squared, synthetic_classical$metrics$r_squared)))
  expect_false(isTRUE(all.equal(synthetic_robust$metrics$adjusted_r_squared, synthetic_classical$metrics$adjusted_r_squared)))

  forecast <- forecast_analysis(
    chart_data,
    series_name = "All groups CPI",
    model_family = "AR",
    ar_lag = 1,
    ma_lag = 0,
    horizon = 4,
    window_mode = "expanding",
    holdout_size = 4
  )
  expect_equal(nrow(forecast$forecast), 4)
  expect_true(all(c("forecast", "lower_95", "upper_95") %in% colnames(forecast$forecast)))
  expect_equal(nrow(forecast$holdout), 4)
  expect_false(is.na(forecast$metrics$rmse))

  forecast_ma <- forecast_analysis(
    chart_data,
    series_name = "All groups CPI",
    model_family = "MA",
    ar_lag = 0,
    ma_lag = 2,
    horizon = 3,
    window_mode = "fixed",
    window_size = 12,
    holdout_size = 3
  )
  expect_equal(nrow(forecast_ma$forecast), 3)
  expect_equal(nrow(forecast_ma$holdout), 3)

  forecast_arma <- forecast_analysis(
    chart_data,
    series_name = "All groups CPI",
    model_family = "ARMA",
    ar_lag = 2,
    ma_lag = 1,
    horizon = 2,
    window_mode = "rolling",
    window_size = 12,
    holdout_size = 2
  )
  expect_equal(nrow(forecast_arma$forecast), 2)
  expect_equal(nrow(forecast_arma$holdout), 2)
  expect_true(all(c("actual", "forecast", "error") %in% colnames(forecast_arma$holdout)))
})

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

test_that("main server supports save and load flows with transform copying", {
  abs_specs <- build_test_abs_specs()
  temp_library <- tempfile(fileext = ".rds")
  temp_presentation_library <- tempfile(fileext = ".rds")
  old_option <- getOption("data_viewer.chart_library_path")
  old_presentation_option <- getOption("data_viewer.chart_presentation_library_path")
  options(data_viewer.chart_library_path = temp_library)
  options(data_viewer.chart_presentation_library_path = temp_presentation_library)
  on.exit(options(data_viewer.chart_library_path = old_option), add = TRUE)
  on.exit(options(data_viewer.chart_presentation_library_path = old_presentation_option), add = TRUE)

  shiny::testServer(build_main_server, {
    session$setInputs(
      start_date = as.Date("2024-01-01"),
      end_date = as.Date("2025-12-31"),
      viewData1 = "1",
      series_1_enabled = TRUE,
      series_1_source = "abs",
      series_1_abs_id = abs_specs[[1]]$abs_id,
      series_1_label = abs_specs[[1]]$label,
      series_1_vis_type = "line",
      series_2_enabled = TRUE,
      series_2_source = "abs",
      series_2_abs_id = abs_specs[[2]]$abs_id,
      series_2_label = abs_specs[[2]]$label,
      series_2_vis_type = "line",
      transform_1_expression = "X * 2",
      transform_1_moving_average = 2,
      transform_2_expression = "data / 2",
      transform_2_moving_average = 3,
      style_auto_y_axis = "auto",
      style_title = "Server Test Chart",
      style_subtitle = "Server subtitle",
      style_y_axis_label = "Index",
      style_note = "server test note",
      style_legend = "bottom"
    )
    session$flushReact()

    expect_equal(builder_state()$series[[1]]$transform_profile$expression, "X * 2")
    expect_equal(builder_state()$series[[2]]$transform_profile$moving_average, 3)
    expect_equal(builder_state()$style$subtitle, "Server subtitle")
    expect_gt(nrow(chart_data()), 0)
    saved_state <- builder_state()
    saved_chart_data <- chart_data()

    session$setInputs(
      library_title = "Saved from test",
      library_description = "server flow"
    )
    session$setInputs(save_chart = 1)
    session$flushReact()

    saved_library <- read_chart_library()
    expect_equal(nrow(saved_library), 1)
    expect_equal(saved_library$title[[1]], "Saved from test")

    session$setInputs(
      search_source_filter = "Recent",
      search_query = "Saved from test",
      search_type_filter = "all",
      search_location_filter = "all",
      search_frequency_filter = "all"
    )
    session$flushReact()
    expect_gt(nrow(search_results()), 0)
    expect_true(all(search_results()$source == "Recent"))

    session$setInputs(transform_all_expression = "X * 4")
    session$flushReact()
    expect_equal(builder_state()$all_series_transform$expression, "X * 4")

    session$setInputs(
      start_date = as.Date("2020-01-01"),
      end_date = as.Date("2021-12-31"),
      style_title = "Mutated",
      transform_1_expression = "X",
      transform_2_moving_average = 1
    )
    session$flushReact()
    expect_false(chart_states_equal(builder_state(), saved_state))

    session$setInputs(library_table_rows_selected = 1)
    suppressWarnings({
      session$setInputs(load_chart = 1)
      session$flushReact()
    })

    expect_equal(selected_chart_record()$title[[1]], "Saved from test")
    expect_equal(read_chart_library()$title[[1]], "Saved from test")
    expect_true(chart_states_equal(builder_state(), saved_state))
    expect_equal(nrow(chart_data()), nrow(saved_chart_data))
    expect_equal(sort(unique(chart_data()$name)), sort(unique(saved_chart_data$name)))

    session$setInputs(
      style_title = "Loaded chart retitled",
      style_subtitle = "Loaded chart subtitle",
      series_1_label = "Relabelled CPI",
      transform_1_expression = "X * 1.5"
    )
    session$flushReact()

    expect_equal(builder_state()$style$title, "Loaded chart retitled")
    expect_equal(builder_state()$style$subtitle, "Loaded chart subtitle")
    expect_equal(builder_state()$series[[1]]$label, "Relabelled CPI")
    expect_equal(builder_state()$series[[1]]$transform_profile$expression, "X * 1.5")

    session$setInputs(library_table_rows_selected = 1)
    session$setInputs(library_title = "Updated title")
    session$setInputs(update_chart = 1)
    session$flushReact()
    expect_equal(nrow(read_chart_library()), 1)
    expect_equal(read_chart_library()$chart_id[[1]], saved_library$chart_id[[1]])

    session$setInputs(
      side_panel_mode = "analysis",
      analysis_tabs = "Correlations",
      analysis_corr_x = abs_specs[[1]]$label,
      analysis_corr_y = abs_specs[[2]]$label,
      analysis_corr_window = 4
    )
    session$flushReact()

    expect_s3_class(main_panel_widget(), "plotly")

    session$setInputs(
      search_query = "",
      search_source_filter = "ABS",
      search_type_filter = "all",
      search_location_filter = "all",
      search_frequency_filter = "all"
    )
    session$flushReact()
    expect_gt(nrow(search_results()), 0)

    session$setInputs(search_results_table_rows_selected = 1, search_target_series = "3")
    session$setInputs(search_add_series = 1)
    session$flushReact()
    expect_equal(builder_state()$series[[3]]$source, "abs")
    expect_true(length(builder_state()$series[[3]]$abs_id) > 0)

    session$setInputs(library_table_rows_selected = c(1))
    session$setInputs(
      presentation_title = "Macro deck",
      presentation_description = "server presentation flow"
    )
    session$setInputs(create_presentation = 1)
    session$flushReact()

    expect_equal(nrow(read_chart_presentation_library()), 1)
    expect_equal(read_chart_presentation_library()$chart_count[[1]], 1)

    session$setInputs(
      start_date = as.Date("2024-01-01"),
      end_date = as.Date("2025-12-31"),
      series_3_enabled = TRUE,
      series_3_source = "abs",
      series_3_abs_id = abs_specs[[1]]$abs_id,
      series_3_label = paste(abs_specs[[1]]$label, "copy"),
      series_3_vis_type = "line"
    )
    session$flushReact()
    session$setInputs(library_title = "Second chart")
    session$setInputs(save_chart = 2)
    session$flushReact()
    expect_equal(nrow(read_chart_library()), 2)

    session$setInputs(presentation_table_rows_selected = 1)
    session$setInputs(library_table_rows_selected = c(1, 2))
    session$setInputs(add_to_presentation = 1)
    session$flushReact()
    expect_equal(read_chart_presentation_library()$chart_count[[1]], 2)

    session$setInputs(presentation_chart_table_rows_selected = 2)
    session$setInputs(presentation_chart_up = 1)
    session$flushReact()
    expect_equal(length(selected_presentation_charts()$chart_id), 2)

    session$setInputs(library_table_rows_selected = 2)
    session$setInputs(replace_presentation_charts = 1)
    session$flushReact()
    expect_equal(read_chart_presentation_library()$chart_count[[1]], 1)
    expect_equal(length(selected_presentation_charts()$chart_id), 1)
  })
})

test_that("main server can reset the builder to its default state", {
  shiny::testServer(build_main_server, {
    session$setInputs(
      start_date = as.Date("2020-01-01"),
      end_date = as.Date("2021-12-31"),
      style_title = "Mutated title",
      style_subtitle = "Mutated subtitle",
      series_2_enabled = TRUE,
      series_2_source = "FRED",
      series_2_fred_series = "UNRATE",
      series_2_label = "US unemployment"
    )
    session$flushReact()

    session$setInputs(reset_builder = 1)
    session$flushReact()
    session$flushReact()

    expect_true(chart_states_equal(builder_state(), default_builder_state()))
    expect_equal(nrow(chart_data()), 0)
  })
})

test_that("main server can clear series setup, presentation, and workspace panels", {
  shiny::testServer(build_main_server, {
    session$setInputs(
      series_1_enabled = TRUE,
      series_1_label = "CPI one",
      series_2_enabled = TRUE,
      series_2_source = "FRED",
      series_2_fred_series = "UNRATE",
      series_2_label = "Unemployment",
      style_title = "Custom title",
      style_subtitle = "Custom subtitle",
      style_y_axis_label = "Index",
      style_note = "Custom note",
      transform_all_expression = "X * 2",
      transform_1_rolling_sum = 4,
      analysis_corr_window = 8,
      analysis_forecast_horizon = 6
    )
    session$flushReact()

    session$setInputs(clear_workspace_tools = 1)
    session$flushReact()

    expect_equal(builder_state()$all_series_transform, default_transform_profile())
    active_profiles <- lapply(
      Filter(Negate(is.null), builder_state()$series),
      function(spec) spec$transform_profile
    )
    expect_true(all(vapply(active_profiles, identical, logical(1), default_transform_profile())))

    session$setInputs(clear_presentation_panel = 1)
    session$flushReact()

    expect_equal(builder_state()$style$note, default_style_settings()$note)
    expect_equal(builder_state()$style$title, default_style_settings()$title)
    expect_equal(builder_state()$style$subtitle, default_style_settings()$subtitle)
    expect_equal(builder_state()$style$y_axis_label, default_style_settings()$y_axis_label)
    expect_equal(builder_state()$style$renderer, default_style_settings()$renderer)
    expect_equal(builder_state()$style$font_family, default_style_settings()$font_family)
    expect_equal(builder_state()$style$legend, default_style_settings()$legend)
    expect_equal(builder_state()$style$palette, default_style_settings()$palette)

    session$setInputs(clear_series_setup = 1)
    session$flushReact()

    expect_true(all(vapply(builder_state()$series, is.null, logical(1))))
  })
})

test_that("main server restores saved FRED vintage settings", {
  temp_library <- tempfile(fileext = ".rds")
  temp_presentation_library <- tempfile(fileext = ".rds")
  old_option <- getOption("data_viewer.chart_library_path")
  old_presentation_option <- getOption("data_viewer.chart_presentation_library_path")
  original_fred_data <- fred_data
  on.exit(options(data_viewer.chart_library_path = old_option), add = TRUE)
  on.exit(options(data_viewer.chart_presentation_library_path = old_presentation_option), add = TRUE)
  on.exit(assign("fred_data", original_fred_data, envir = .GlobalEnv), add = TRUE)
  options(data_viewer.chart_library_path = temp_library)
  options(data_viewer.chart_presentation_library_path = temp_presentation_library)

  assign(
    "fred_data",
    function(series, start_date = NULL, end_date = NULL, realtime_start = NULL, realtime_end = NULL, vintage_dates = NULL, name_override = NULL) {
      series_name <- if (!is.null(name_override) && nzchar(name_override)) name_override else series
      tibble::tibble(
        date = seq(as.Date("2020-01-01"), by = "month", length.out = 24),
        value = seq_len(24),
        name = series_name
      )
    },
    envir = .GlobalEnv
  )

  shiny::testServer(build_main_server, {
    session$setInputs(
      series_2_enabled = TRUE,
      series_2_source = "FRED",
      series_2_fred_series = "UNRATE",
      series_2_fred_vintage_mode = "compare",
      series_2_fred_vintage_date = "2020-01-01",
      series_2_label = "Unemployment vintages",
      library_title = "Vintage chart"
    )
    session$flushReact()

    expect_equal(builder_state()$series[[2]]$fred_vintage_mode, "compare")
    expect_equal(as.Date(builder_state()$series[[2]]$fred_vintage_date), as.Date("2020-01-01"))
    expect_gt(nrow(chart_data()), 0)

    session$setInputs(save_chart = 1)
    session$flushReact()

    session$setInputs(
      series_2_fred_vintage_mode = "current",
      style_title = "Mutated"
    )
    session$flushReact()

    session$setInputs(library_table_rows_selected = 1)
    suppressWarnings({
      session$setInputs(load_chart = 1)
      session$flushReact()
    })

    expect_equal(builder_state()$series[[2]]$fred_vintage_mode, "compare")
    expect_equal(as.Date(builder_state()$series[[2]]$fred_vintage_date), as.Date("2020-01-01"))
  })
})

test_that("forecast analysis falls back to the first available series and uses shared styling", {
  abs_specs <- build_test_abs_specs()
  shiny::testServer(build_main_server, {
    session$setInputs(
      start_date = as.Date("2024-01-01"),
      end_date = as.Date("2025-12-31"),
      series_1_enabled = TRUE,
      series_1_source = "abs",
      series_1_abs_id = abs_specs[[1]]$abs_id,
      series_1_label = abs_specs[[1]]$label,
      series_1_vis_type = "line",
      series_2_enabled = TRUE,
      series_2_source = "abs",
      series_2_abs_id = abs_specs[[2]]$abs_id,
      series_2_label = abs_specs[[2]]$label,
      series_2_vis_type = "line",
      side_panel_mode = "analysis",
      analysis_tabs = "Forecast"
    )
    session$flushReact()

    expect_equal(selected_forecast_series(), abs_specs[[1]]$label)
    expect_equal(nrow(forecast_result()$forecast), 4)
    expect_s3_class(selected_analysis_widget(), "plotly")
    expect_equal(selected_analysis_widget()$x$layout$paper_bgcolor, "#f5f8f4")
    expect_equal(selected_analysis_widget()$x$layout$yaxis$side, "right")
  })
})

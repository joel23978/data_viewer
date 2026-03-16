source(here::here("R", "bootstrap.R"))
source(here::here("cpi_annual.R"))
source(here::here("external_data.R"))
source(here::here("R", "chart_helpers.R"))
source(here::here("R", "chart_library.R"))
source(here::here("R", "analysis_helpers.R"))
source(here::here("R", "main_app.R"))

build_test_state <- function() {
  list(
    date_range = c(2024, 2025),
    show_table = TRUE,
    series = list(
      list(
        index = 1,
        source = "ABS CPI",
        text = "All groups CPI",
        region = region_list[[1]],
        transform = "index",
        rebase_date = as.Date("2019-12-31"),
        label = "All groups CPI",
        transform_profile = list(expression = "X", moving_average = 1, lagged_value = 0, lagged_pct = 0, lagged_ann = 0),
        vis_type = "line"
      ),
      list(
        index = 2,
        source = "ABS CPI",
        text = "Housing",
        region = region_list[[1]],
        transform = "index",
        rebase_date = as.Date("2019-12-31"),
        label = "Housing CPI",
        transform_profile = list(expression = "X", moving_average = 1, lagged_value = 0, lagged_pct = 0, lagged_ann = 0),
        vis_type = "line"
      ),
      NULL,
      NULL
    ),
    all_series_transform = list(expression = "X", moving_average = 1, lagged_value = 0, lagged_pct = 0, lagged_ann = 0),
    style = list(
      title = "Test chart",
      y_axis_label = "%",
      note = "test note",
      legend = "bottom",
      palette = "Okabe-Ito",
      date_format = "%b-%y",
      x_labels = 6,
      auto_y_axis = TRUE,
      y_min = NA_real_,
      y_max = NA_real_,
      y_breaks = NA_real_,
      invert_y_axis = FALSE,
      horizontal_1 = NA_real_,
      horizontal_2 = NA_real_,
      horizontal_shading = c(NA_real_, NA_real_),
      vertical_1 = as.Date(NA),
      vertical_2 = as.Date(NA),
      recession_shading = "none",
      export_width = 8,
      export_height = 5
    )
  )
}

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
    transform_profile = list(expression = "X", moving_average = 1, lagged_value = 0, lagged_pct = 0, lagged_ann = 0),
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

  rolling <- rolling_correlation_data(chart_data, "All groups CPI", "Housing CPI", window = 4)
  expect_gt(nrow(rolling), 0)
  expect_true(all(c("date", "correlation") %in% colnames(rolling)))

  regression <- regression_analysis(chart_data, dependent = "All groups CPI", independent = "Housing CPI", error_assumption = "robust")
  expect_true(all(c("term", "Estimate") %in% colnames(regression$coefficients)))
  expect_true(any(grepl("Std", colnames(regression$coefficients))))
  expect_gt(regression$metrics$observations, 5)

  regression_classical <- regression_analysis(chart_data, dependent = "All groups CPI", independent = "Housing CPI", error_assumption = "classical")
  expect_gt(regression_classical$metrics$observations, 5)

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
  old_option <- getOption("data_viewer.chart_library_path")
  options(data_viewer.chart_library_path = temp_library)
  on.exit(options(data_viewer.chart_library_path = old_option), add = TRUE)

  ensure_chart_library()
  initial_library <- read_chart_library()
  expect_equal(nrow(initial_library), 0)

  payload <- build_chart_data(build_test_state())
  record <- new_chart_record(build_test_state(), payload$data, title = "Saved chart", description = "Test record")
  write_chart_library(upsert_chart_record(initial_library, record))

  updated_library <- read_chart_library()
  expect_equal(nrow(updated_library), 1)
  expect_equal(updated_library$title[[1]], "Saved chart")
})

test_that("main server supports save and load flows with transform copying", {
  temp_library <- tempfile(fileext = ".rds")
  old_option <- getOption("data_viewer.chart_library_path")
  options(data_viewer.chart_library_path = temp_library)
  on.exit(options(data_viewer.chart_library_path = old_option), add = TRUE)

  shiny::testServer(build_main_server, {
    session$setInputs(
      start_year = 2024,
      end_year = 2025,
      viewData1 = "1",
      series_1_enabled = TRUE,
      series_1_source = "ABS CPI",
      series_1_text = "All groups CPI",
      series_1_region = region_list[[1]],
      series_1_transform = "index",
      series_1_label = "All groups CPI",
      series_1_vis_type = "line",
      series_2_enabled = TRUE,
      series_2_source = "ABS CPI",
      series_2_text = "Housing",
      series_2_region = region_list[[1]],
      series_2_transform = "index",
      series_2_label = "Housing CPI",
      series_2_vis_type = "line",
      transform_1_expression = "X * 2",
      transform_1_moving_average = 2,
      transform_2_expression = "data / 2",
      transform_2_moving_average = 3,
      style_auto_y_axis = "auto",
      style_title = "Server Test Chart",
      style_y_axis_label = "Index",
      style_note = "server test note",
      style_legend = "bottom"
    )
    session$flushReact()

    expect_equal(builder_state()$series[[1]]$transform_profile$expression, "X * 2")
    expect_equal(builder_state()$series[[2]]$transform_profile$moving_average, 3)
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

    session$setInputs(transform_all_expression = "X * 4")
    session$flushReact()
    expect_equal(builder_state()$all_series_transform$expression, "X * 4")

    session$setInputs(
      start_year = 2020,
      end_year = 2021,
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

    session$setInputs(library_title = "Updated title")
    session$setInputs(update_chart = 1)
    session$flushReact()
    expect_equal(read_chart_library()$title[[1]], "Updated title")

    session$setInputs(
      side_panel_mode = "analysis",
      analysis_tabs = "Correlations",
      analysis_corr_x = "All groups CPI",
      analysis_corr_y = "Housing CPI",
      analysis_corr_window = 4
    )
    session$flushReact()

    expect_s3_class(main_panel_widget(), "plotly")
  })
})

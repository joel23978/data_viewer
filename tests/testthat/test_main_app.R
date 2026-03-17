source(here::here("R", "bootstrap.R"))
source(here::here("cpi_annual.R"))
source(here::here("external_data.R"))
source(here::here("R", "chart_helpers.R"))
source(here::here("R", "data_search.R"))
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
      subtitle = "Test subtitle",
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

test_that("FRED source controls render the restored series ID", {
  fred_ui <- series_source_controls_ui(
    input = list(),
    session = NULL,
    index = 2,
    source_value = "FRED",
    restored_spec = list(source = "FRED", fred_series = "UNRATE")
  )

  expect_true(grepl("UNRATE", as.character(fred_ui), fixed = TRUE))
})

test_that("presentation-style HTML export includes chart widgets", {
  state <- build_test_state()
  payload <- build_chart_data(state)
  temp_file <- tempfile(fileext = ".html")

  htmltools::save_html(
    htmltools::browsable(
      tags$html(
        tags$body(
          tags$section(
            htmltools::as.tags(build_chart_widget(payload$data, state$style))
          )
        )
      )
    ),
    file = temp_file
  )

  exported_html <- paste(readLines(temp_file, warn = FALSE), collapse = "\n")
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

test_that("data search index builds and returns relevant local metadata", {
  search_index <- build_search_index(force = TRUE)
  expect_gt(nrow(search_index), 1000)
  expect_true(all(c("title", "source", "type_code", "location_code", "frequency", "load_payload") %in% names(search_index)))

  cpi_results <- filter_search_index(search_index, query = "housing", source_filter = "ABS CPI")
  expect_gt(nrow(cpi_results), 0)
  expect_true(any(grepl("Housing", cpi_results$title, fixed = TRUE)))
  expect_true(all(cpi_results$type_code == "ECON"))
  expect_true(all(cpi_results$location_code == "STATE"))

  abs_full_results <- filter_search_index(
    search_index,
    query = "",
    source_filter = "ABS",
    type_filter = "all",
    location_filter = "all",
    frequency_filter = "all",
    limit = Inf
  )
  expect_gt(nrow(abs_full_results), 250)

  rba_results <- filter_search_index(search_index, query = "balance sheet", source_filter = "RBA")
  expect_gt(nrow(rba_results), 0)
  expect_true(any(rba_results$type_code == "FIN"))

  boolean_results <- filter_search_index(
    search_index,
    query = "housing OR balance",
    source_filter = "all"
  )
  expect_gt(nrow(boolean_results), 0)
  expect_true(any(grepl("Housing", boolean_results$title, fixed = TRUE)) || any(grepl("Balance", boolean_results$title, fixed = TRUE)))

  financial_results <- filter_search_index(
    search_index,
    query = "balance",
    source_filter = "all",
    type_filter = "FIN"
  )
  expect_gt(nrow(financial_results), 0)
  expect_true(all(financial_results$type_code == "FIN"))

  abs_australia_results <- filter_search_index(
    search_index,
    query = "final consumption expenditure",
    source_filter = "ABS",
    location_filter = "AUS"
  )
  expect_gt(nrow(abs_australia_results), 0)
  expect_true(all(abs_australia_results$location_code == "AUS"))

  expect_equal(classify_location_code("Australia CPI", "", "ABS CPI"), "AUS")
  expect_equal(classify_location_code("Sydney CPI", "", "ABS CPI"), "STATE")
  expect_equal(classify_location_code("United States Industrial Production", "", "FRED"), "INTL")
})

test_that("FRED API search results are formatted and cached for the search tab", {
  withr::local_envvar(FRED_API_KEY = "test-key")
  original_fred_search_remote <- fred_search_remote
  on.exit(assign("fred_search_remote", original_fred_search_remote, envir = .GlobalEnv), add = TRUE)

  fetch_counter <- 0
  rm(list = ls(envir = data_search_env), envir = data_search_env)

  assign(
    "fred_search_remote",
    function(query, frequency_filter = "all", search_type = "full_text", limit = 100) {
      fetch_counter <<- fetch_counter + 1
      tibble::tibble(
        id = "UNRATE",
        title = "Unemployment Rate",
        frequency = "Monthly",
        observation_start = as.Date("1948-01-01"),
        observation_end = as.Date("2026-01-01"),
        units = "Percent",
        seasonal_adjustment = "Seasonally Adjusted",
        notes = "Civilian unemployment rate.",
        popularity = 99
      )
    },
    envir = .GlobalEnv
  )

  first_response <- search_fred_series("unemployment", frequency_filter = "Monthly", search_type = "full_text", limit = 25)
  second_response <- search_fred_series("unemployment", frequency_filter = "Monthly", search_type = "full_text", limit = 25)

  expect_equal(fetch_counter, 1)
  expect_equal(nrow(first_response$results), 1)
  expect_equal(first_response$results$source[[1]], "FRED")
  expect_equal(first_response$results$type_code[[1]], "ECON")
  expect_equal(first_response$results$location_code[[1]], "INTL")
  expect_equal(first_response$results$load_payload[[1]]$fred_series, "UNRATE")
  expect_match(first_response$status, "Showing live FRED results", fixed = TRUE)
  expect_equal(second_response$results$search_id[[1]], first_response$results$search_id[[1]])
})

test_that("FRED API key helpers update the session key", {
  withr::local_envvar(FRED_API_KEY = "")

  expect_false(fred_search_available())
  set_fred_api_key("abc123")
  expect_equal(current_fred_api_key(), "abc123")
  expect_true(fred_search_available())
})

test_that("DBnomics API search results are formatted and cached for the search tab", {
  original_dbnomics_search_remote <- dbnomics_search_remote
  on.exit(assign("dbnomics_search_remote", original_dbnomics_search_remote, envir = .GlobalEnv), add = TRUE)

  fetch_counter <- 0
  rm(list = ls(envir = data_search_env), envir = data_search_env)

  assign(
    "dbnomics_search_remote",
    function(query, provider_code = NULL, dataset_code = NULL, limit = 100) {
      fetch_counter <<- fetch_counter + 1
      tibble::tibble(
        series_code = "NGDP_RPCH",
        series_name = "Real GDP growth"
      )
    },
    envir = .GlobalEnv
  )

  first_response <- search_dbnomics_series(
    query = "growth",
    provider_code = "IMF",
    dataset_code = "WEO:2024-10",
    limit = 25
  )
  second_response <- search_dbnomics_series(
    query = "growth",
    provider_code = "IMF",
    dataset_code = "WEO:2024-10",
    limit = 25
  )

  expect_equal(fetch_counter, 1)
  expect_equal(nrow(first_response$results), 1)
  expect_equal(first_response$results$source[[1]], "DBnomics")
  expect_equal(first_response$results$type_code[[1]], "ECON")
  expect_equal(first_response$results$location_code[[1]], "INTL")
  expect_equal(
    first_response$results$load_payload[[1]]$dbnomics_series,
    "IMF/WEO:2024-10/NGDP_RPCH"
  )
  expect_match(first_response$status, "Showing live DBnomics results", fixed = TRUE)
  expect_equal(second_response$results$search_id[[1]], first_response$results$search_id[[1]])
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
  write_chart_library(upsert_chart_record(initial_library, record))

  updated_library <- read_chart_library()
  expect_equal(nrow(updated_library), 1)
  expect_equal(updated_library$title[[1]], "Saved chart")

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

    session$setInputs(
      search_query = "housing OR balance",
      search_source_filter = "ABS CPI",
      search_type_filter = "ECON",
      search_location_filter = "STATE",
      search_frequency_filter = "Quarterly"
    )
    session$flushReact()
    expect_gt(nrow(search_results()), 0)
    expect_true(all(search_results()$type_code == "ECON"))
    expect_true(all(search_results()$location_code == "STATE"))

    session$setInputs(search_results_table_rows_selected = 1, search_target_series = "3")
    session$setInputs(search_add_series = 1)
    session$flushReact()
    expect_equal(builder_state()$series[[3]]$source, "ABS CPI")
    expect_true(length(builder_state()$series[[3]]$text) > 0)

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
      series_3_source = "ABS CPI",
      series_3_text = "Food and non-alcoholic beverages",
      series_3_region = region_list[[1]],
      series_3_transform = "index",
      series_3_label = "Food CPI",
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
    expect_gt(nrow(chart_data()), 0)
  })
})

test_that("main server can add a FRED search result to the builder", {
  withr::local_envvar(FRED_API_KEY = "test-key")
  original_fred_search_remote <- fred_search_remote
  on.exit(assign("fred_search_remote", original_fred_search_remote, envir = .GlobalEnv), add = TRUE)

  assign(
    "fred_search_remote",
    function(query, frequency_filter = "all", search_type = "full_text", limit = 100) {
      tibble::tibble(
        id = "UNRATE",
        title = "Unemployment Rate",
        frequency = "Monthly",
        observation_start = as.Date("1948-01-01"),
        observation_end = as.Date("2026-01-01"),
        units = "Percent",
        seasonal_adjustment = "Seasonally Adjusted",
        notes = "Civilian unemployment rate.",
        popularity = 99
      )
    },
    envir = .GlobalEnv
  )

  shiny::testServer(build_main_server, {
    session$setInputs(
      search_query = "UNRATE",
      search_source_filter = "FRED",
      search_type_filter = "all",
      search_location_filter = "INTL",
      search_fred_mode = "series_id",
      search_frequency_filter = "all"
    )
    session$flushReact()

    expect_gt(nrow(search_results()), 0)
    expect_equal(search_results()$source[[1]], "FRED")
    expect_true(all(search_results()$location_code == "INTL"))

    session$setInputs(search_results_table_rows_selected = 1, search_target_series = "2")
    session$setInputs(search_add_series = 1)
    session$flushReact()
    session$flushReact()

    expect_equal(builder_state()$series[[2]]$source, "FRED")
    expect_equal(builder_state()$series[[2]]$fred_series, "UNRATE")
    expect_equal(builder_state()$series[[2]]$label, "Unemployment Rate")

    session$setInputs(start_date = as.Date("2020-01-01"), end_date = as.Date("2021-12-31"))
    session$flushReact()
    expect_equal(builder_state()$date_range, as.Date(c("2020-01-01", "2021-12-31")))
  })
})

test_that("main server saves a FRED API key from the modal flow", {
  withr::local_envvar(FRED_API_KEY = "")

  shiny::testServer(build_main_server, {
    session$setInputs(open_fred_api_key_modal = 1)
    session$setInputs(fred_api_key_value = "session-key-123")
    session$setInputs(save_fred_api_key = 1)
    session$flushReact()

    expect_equal(current_fred_api_key(), "session-key-123")
    expect_true(fred_search_available())
  })
})

test_that("main server can add a DBnomics search result to the builder", {
  original_dbnomics_search_remote <- dbnomics_search_remote
  on.exit(assign("dbnomics_search_remote", original_dbnomics_search_remote, envir = .GlobalEnv), add = TRUE)

  assign(
    "dbnomics_search_remote",
    function(query, provider_code = NULL, dataset_code = NULL, limit = 100) {
      tibble::tibble(
        series_code = "NGDP_RPCH",
        series_name = "Real GDP growth"
      )
    },
    envir = .GlobalEnv
  )

  shiny::testServer(build_main_server, {
    session$setInputs(
      search_query = "growth",
      search_source_filter = "DBnomics",
      search_type_filter = "ECON",
      search_location_filter = "INTL",
      search_dbnomics_provider = "IMF",
      search_dbnomics_dataset = "WEO:2024-10",
      search_frequency_filter = "all"
    )
    session$flushReact()

    expect_gt(nrow(search_results()), 0)
    expect_equal(search_results()$source[[1]], "DBnomics")
    expect_true(all(search_results()$type_code == "ECON"))
    expect_true(all(search_results()$location_code == "INTL"))

    session$setInputs(search_results_table_rows_selected = 1, search_target_series = "4")
    session$setInputs(search_add_series = 1)
    session$flushReact()

    expect_equal(builder_state()$series[[4]]$source, "dbnomics")
    expect_equal(builder_state()$series[[4]]$dbnomics_series, "IMF/WEO:2024-10/NGDP_RPCH")
    expect_equal(builder_state()$series[[4]]$label, "Real GDP growth")
  })
})

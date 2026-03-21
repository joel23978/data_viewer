source(here::here("R", "bootstrap.R"))
source(here::here("external_data.R"))
source(here::here("R", "providers.R"))
source(here::here("R", "chart_helpers.R"))
source(here::here("R", "data_search.R"))
source(here::here("R", "chart_library.R"))
source(here::here("R", "analysis_helpers.R"))
source(here::here("R", "main_app.R"))
source(here::here("tests", "testthat", "helper_app_fixtures.R"))

add_selected_search_result_to_builder <- function(session, target_series) {
  session$setInputs(
    search_results_table_rows_selected = 1,
    search_target_series = as.character(target_series)
  )
  session$flushReact()
  session$setInputs(search_add_series = 1)
  session$flushReact()
  session$flushReact()
}

test_that("data search index builds and returns relevant local metadata", {
  search_index <- build_search_index(force = TRUE)
  expect_gt(nrow(search_index), 1000)
  expect_true(all(c("title", "source", "type_code", "location_code", "frequency", "load_payload") %in% names(search_index)))

  abs_results <- filter_search_index(search_index, query = "housing", source_filter = "ABS")
  expect_gt(nrow(abs_results), 0)
  expect_true(any(grepl("Housing", abs_results$title, fixed = TRUE)))

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

  expect_equal(classify_location_code("Australia CPI", "", "ABS"), "AUS")
  expect_equal(classify_location_code("Sydney CPI", "", "ABS"), "STATE")
  expect_equal(classify_location_code("United States Industrial Production", "", "FRED"), "INTL")
})

test_that("prebuilt local search assets are used at runtime and boolean lookup uses token index", {
  original_read_index <- read_prebuilt_local_search_index
  original_read_token_index <- read_prebuilt_local_search_token_index
  original_build_recent_search_index <- build_recent_search_index
  on.exit(assign("read_prebuilt_local_search_index", original_read_index, envir = .GlobalEnv), add = TRUE)
  on.exit(assign("read_prebuilt_local_search_token_index", original_read_token_index, envir = .GlobalEnv), add = TRUE)
  on.exit(assign("build_recent_search_index", original_build_recent_search_index, envir = .GlobalEnv), add = TRUE)

  invalidate_local_search_asset_cache()
  invalidate_search_index_cache()

  local_index <- tibble::tibble(
    local_row_id = 1:3,
    search_id = c("abs::1", "abs::2", "rba::1"),
    title = c("Retail turnover", "Industrial production", "Balance sheet"),
    source = c("ABS", "ABS", "RBA"),
    type_code = c("ECON", "ECON", "FIN"),
    location_code = c("AUS", "AUS", "AUS"),
    frequency = c("Monthly", "Monthly", "Monthly"),
    start_date = as.Date(c(NA, NA, NA)),
    end_date = as.Date(c(NA, NA, NA)),
    summary = c("ABS retail", "ABS industry", "RBA finance"),
    search_text = c("retail turnover abs", "industrial production abs", "balance sheet rba"),
    load_payload = list(list(source = "abs"), list(source = "abs"), list(source = "rba"))
  )
  token_index <- build_search_token_index(local_index)

  assign("read_prebuilt_local_search_index", function() local_index, envir = .GlobalEnv)
  assign("read_prebuilt_local_search_token_index", function() token_index, envir = .GlobalEnv)
  assign("build_recent_search_index", function() empty_search_index(), envir = .GlobalEnv)

  search_index <- build_search_index(force = TRUE)
  expect_equal(nrow(search_index), 3)

  results <- filter_search_index(
    search_index,
    query = "retail AND turnover",
    source_filter = "ABS",
    limit = Inf,
    token_index = current_search_token_index(force = TRUE)
  )

  expect_equal(nrow(results), 1)
  expect_equal(results$title[[1]], "Retail turnover")
})

test_that("quoted phrases are matched exactly in local search", {
  local_index <- tibble::tibble(
    local_row_id = 1:3,
    search_id = c("custom::1", "custom::2", "custom::3"),
    title = c("Data centre investment", "Centre for data science", "Housing pipeline"),
    source = c("Recent", "Recent", "Recent"),
    type_code = c("ECON", "ECON", "ECON"),
    location_code = c("AUS", "AUS", "AUS"),
    frequency = c("Unknown", "Unknown", "Unknown"),
    start_date = as.Date(c(NA, NA, NA)),
    end_date = as.Date(c(NA, NA, NA)),
    summary = c("Exact phrase appears", "Words appear separately", "Control row"),
    search_text = c("data centre investment", "centre for data science", "housing pipeline"),
    load_payload = list(list(source = "recent"), list(source = "recent"), list(source = "recent"))
  )
  token_index <- build_search_token_index(local_index)

  loose_results <- filter_search_index(
    local_index,
    query = "data centre",
    source_filter = "all",
    limit = Inf,
    token_index = token_index
  )
  exact_results <- filter_search_index(
    local_index,
    query = "\"data centre\"",
    source_filter = "all",
    limit = Inf,
    token_index = token_index
  )
  phrase_or_results <- filter_search_index(
    local_index,
    query = "\"data centre\" OR housing",
    source_filter = "all",
    limit = Inf,
    token_index = token_index
  )

  expect_equal(sort(loose_results$title), c("Centre for data science", "Data centre investment"))
  expect_equal(exact_results$title, "Data centre investment")
  expect_equal(sort(phrase_or_results$title), c("Data centre investment", "Housing pipeline"))
})

test_that("remote search queries preserve quoted phrases", {
  expect_equal(remote_search_query("\"data centre\""), "\"data centre\"")
  expect_equal(remote_search_query("\"data centre\" OR housing"), "\"data centre\" OR housing")
  expect_equal(remote_search_query("data AND centre"), "data centre")
})

test_that("saved-chart series are indexed as recent search results", {
  temp_library <- tempfile(fileext = ".rds")
  temp_presentation_library <- tempfile(fileext = ".rds")
  old_option <- getOption("data_viewer.chart_library_path")
  old_presentation_option <- getOption("data_viewer.chart_presentation_library_path")
  options(data_viewer.chart_library_path = temp_library)
  options(data_viewer.chart_presentation_library_path = temp_presentation_library)
  on.exit(options(data_viewer.chart_library_path = old_option), add = TRUE)
  on.exit(options(data_viewer.chart_presentation_library_path = old_presentation_option), add = TRUE)

  ensure_chart_library()
  payload <- build_chart_data(build_test_state())
  record <- new_chart_record(build_test_state(), payload$data, title = "Saved chart", description = "Recent lookup test")
  write_chart_library(upsert_chart_record(read_chart_library(), record))
  invalidate_search_index_cache()

  search_index <- build_search_index(force = TRUE)
  recent_results <- filter_search_index(search_index, query = "saved chart", source_filter = "Recent", limit = Inf)

  expect_gt(nrow(recent_results), 0)
  expect_true(all(recent_results$source == "Recent"))
  expect_true(any(grepl("Saved in Saved chart", recent_results$summary, fixed = TRUE)))
  expect_true(all(vapply(recent_results$load_payload, function(payload) !is.null(payload$source), logical(1))))
})

test_that("unsupported saved-chart sources are filtered from library and recent search", {
  temp_library <- tempfile(fileext = ".rds")
  temp_presentation_library <- tempfile(fileext = ".rds")
  old_option <- getOption("data_viewer.chart_library_path")
  old_presentation_option <- getOption("data_viewer.chart_presentation_library_path")
  options(data_viewer.chart_library_path = temp_library)
  options(data_viewer.chart_presentation_library_path = temp_presentation_library)
  on.exit(options(data_viewer.chart_library_path = old_option), add = TRUE)
  on.exit(options(data_viewer.chart_presentation_library_path = old_presentation_option), add = TRUE)

  ensure_chart_library()

  supported_state <- build_test_state()
  unsupported_state <- build_test_state()
  unsupported_state$series[[1]]$source <- "ABS CPI"

  payload <- build_chart_data(supported_state)
  supported_record <- new_chart_record(supported_state, payload$data, title = "Supported chart")
  unsupported_record <- new_chart_record(unsupported_state, payload$data, title = "Unsupported chart")
  write_chart_library(dplyr::bind_rows(supported_record, unsupported_record))
  invalidate_search_index_cache()

  library_data <- read_chart_library()
  recent_index <- build_recent_search_index()

  expect_equal(nrow(library_data), 1)
  expect_equal(library_data$title[[1]], "Supported chart")
  expect_equal(attr(library_data, "unsupported_chart_count", exact = TRUE), 1)
  expect_false(any(grepl("Unsupported chart", recent_index$summary, fixed = TRUE)))
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

test_that("DBnomics dataset browse returns all series for a selected provider and dataset", {
  original_dbnomics_search_remote <- dbnomics_search_remote
  on.exit(assign("dbnomics_search_remote", original_dbnomics_search_remote, envir = .GlobalEnv), add = TRUE)

  fetch_args <- list()
  rm(list = ls(envir = data_search_env), envir = data_search_env)

  assign(
    "dbnomics_search_remote",
    function(query, provider_code = NULL, dataset_code = NULL, limit = 100) {
      fetch_args <<- list(
        query = query,
        provider_code = provider_code,
        dataset_code = dataset_code,
        limit = limit
      )

      tibble::tibble(
        series_code = c("NGDP_RPCH", "PCPIPCH"),
        series_name = c("Real GDP growth", "Inflation")
      )
    },
    envir = .GlobalEnv
  )

  response <- search_remote_provider_responses(
    query = "",
    source_filter = "DBnomics",
    search_contexts = list(
      dbnomics = list(
        provider_code = "IMF",
        dataset_code = "WEO:2024-10",
        browse_dataset = TRUE
      )
    ),
    limit = 100
  )[["DBnomics"]]

  expect_equal(fetch_args$query, "")
  expect_equal(fetch_args$provider_code, "IMF")
  expect_equal(fetch_args$dataset_code, "WEO:2024-10")
  expect_true(is.infinite(fetch_args$limit))
  expect_equal(nrow(response$results), 2)
  expect_equal(response$results$title[[1]], "Inflation")
  expect_match(response$status, "Showing all live DBnomics series", fixed = TRUE)
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
      main_tabs = "search",
      search_query = "UNRATE",
      search_source_filter = "FRED",
      search_type_filter = "all",
      search_location_filter = "INTL",
      search_frequency_filter = "all"
    )
    session$flushReact()

    expect_gt(nrow(search_results()), 0)
    expect_equal(search_results()$source[[1]], "FRED")
    expect_true(all(search_results()$location_code == "INTL"))

    session$setInputs(search_results_table_rows_selected = 1, search_target_series = "2")
    expect_s3_class(search_preview_widget(), "plotly")
    session$setInputs(search_add_series = 1)
    session$flushReact()
    session$flushReact()

    expect_equal(input$main_tabs, "search")
    expect_equal(builder_state()$series[[2]]$source, "FRED")
    expect_equal(builder_state()$series[[2]]$fred_series, "UNRATE")
    expect_equal(builder_state()$series[[2]]$label, "Unemployment Rate")
  })
})

test_that("main server can add an ABS search result to the builder", {
  shiny::testServer(build_main_server, {
    ensure_search_index_loaded()
    session$setInputs(
      main_tabs = "search",
      search_query = "housing",
      search_source_filter = "ABS",
      search_type_filter = "all",
      search_location_filter = "all",
      search_frequency_filter = "all"
    )
    session$flushReact()

    expect_gt(nrow(search_results()), 0)
    abs_row <- search_results()[1, , drop = FALSE]
    abs_spec <- search_result_series_spec(abs_row, 2)

    add_selected_search_result_to_builder(session, 2)

    expect_equal(builder_state()$series[[2]]$source, "abs")
    expect_equal(builder_state()$series[[2]]$abs_id, abs_spec$abs_id)
    expect_equal(builder_state()$series[[2]]$abs_desc, abs_spec$abs_desc)
    expect_equal(builder_state()$series[[2]]$label, abs_spec$label)
  })
})

test_that("main server can add an RBA search result to the builder", {
  original_rba_data <- rba_data
  on.exit(assign("rba_data", original_rba_data, envir = .GlobalEnv), add = TRUE)

  assign(
    "rba_data",
    function(series, ...) {
      tibble::tibble(
        date = as.Date("2024-01-01"),
        value = 1,
        name = "RBA series"
      )
    },
    envir = .GlobalEnv
  )

  shiny::testServer(build_main_server, {
    ensure_search_index_loaded()
    session$setInputs(
      main_tabs = "search",
      search_query = "balance sheet",
      search_source_filter = "RBA",
      search_type_filter = "all",
      search_location_filter = "all",
      search_frequency_filter = "all"
    )
    session$flushReact()

    expect_gt(nrow(search_results()), 0)
    rba_row <- search_results()[1, , drop = FALSE]
    rba_spec <- search_result_series_spec(rba_row, 3)

    add_selected_search_result_to_builder(session, 3)

    expect_equal(builder_state()$series[[3]]$source, "rba")
    expect_equal(builder_state()$series[[3]]$rba_series_id, rba_spec$rba_series_id)
    expect_equal(builder_state()$series[[3]]$rba_desc, rba_spec$rba_desc)
    expect_equal(builder_state()$series[[3]]$label, rba_spec$label)
  })
})

test_that("main server preserves the selected search result across first-load refreshes", {
  withr::local_envvar(FRED_API_KEY = "test-key")
  original_fred_search_remote <- fred_search_remote
  on.exit(assign("fred_search_remote", original_fred_search_remote, envir = .GlobalEnv), add = TRUE)

  assign(
    "fred_search_remote",
    function(query, frequency_filter = "all", search_type = "full_text", limit = 100) {
      tibble::tibble(
        id = "RENTINDEX",
        title = "Rent of primary residence",
        frequency = "Monthly",
        observation_start = as.Date("2010-01-01"),
        observation_end = as.Date("2026-01-01"),
        units = "Index",
        seasonal_adjustment = "Not Seasonally Adjusted",
        notes = "Synthetic search response used for selection stability tests.",
        popularity = 42
      )
    },
    envir = .GlobalEnv
  )

  shiny::testServer(build_main_server, {
    session$setInputs(
      main_tabs = "search",
      search_query = "RENTINDEX",
      search_source_filter = "FRED",
      search_type_filter = "all",
      search_location_filter = "all",
      search_frequency_filter = "all"
    )
    session$flushReact()

    expect_equal(nrow(search_results()), 1)
    expect_equal(search_results()$source[[1]], "FRED")

    session$setInputs(search_results_table_rows_selected = 1, search_target_series = "2")
    session$flushReact()

    expect_equal(selected_search_result()$source[[1]], "FRED")
    expect_equal(search_result_series_spec(selected_search_result(), 2)$fred_series, "RENTINDEX")

    ensure_search_index_loaded()
    session$flushReact()

    expect_equal(nrow(search_results()), 1)
    expect_equal(selected_search_result()$source[[1]], "FRED")
    expect_equal(search_result_series_spec(selected_search_result(), 2)$fred_series, "RENTINDEX")

    session$setInputs(search_add_series = 1)
    session$flushReact()
    session$flushReact()

    expect_equal(builder_state()$series[[2]]$source, "FRED")
    expect_equal(builder_state()$series[[2]]$fred_series, "RENTINDEX")
    expect_equal(builder_state()$series[[2]]$label, "Rent of primary residence")
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
      search_frequency_filter = "all",
      search_dbnomics_provider = "IMF"
    )
    session$flushReact()

    session$setInputs(search_dbnomics_dataset = "WEO:2024-10")
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

test_that("main server lists all DBnomics dataset series when provider and dataset are selected", {
  original_dbnomics_search_remote <- dbnomics_search_remote
  on.exit(assign("dbnomics_search_remote", original_dbnomics_search_remote, envir = .GlobalEnv), add = TRUE)

  assign(
    "dbnomics_search_remote",
    function(query, provider_code = NULL, dataset_code = NULL, limit = 100) {
      expect_equal(query, "")
      expect_equal(provider_code, "IMF")
      expect_equal(dataset_code, "WEO:2024-10")
      expect_true(is.infinite(limit))

      tibble::tibble(
        series_code = c("NGDP_RPCH", "PCPIPCH"),
        series_name = c("Real GDP growth", "Inflation")
      )
    },
    envir = .GlobalEnv
  )

  shiny::testServer(build_main_server, {
    session$setInputs(
      main_tabs = "search",
      search_query = "",
      search_source_filter = "DBnomics",
      search_type_filter = "all",
      search_location_filter = "INTL",
      search_frequency_filter = "all",
      search_dbnomics_provider = "IMF"
    )
    session$flushReact()

    session$setInputs(search_dbnomics_dataset = "WEO:2024-10")
    session$flushReact()
    session$flushReact()

    expect_equal(nrow(search_results()), 2)
    expect_true(all(search_results()$source == "DBnomics"))
    expect_equal(sort(search_results()$title), c("Inflation", "Real GDP growth"))
  })
})

test_that("main server replaces a populated builder slot from a new search result", {
  withr::local_envvar(FRED_API_KEY = "test-key")
  original_fred_search_remote <- fred_search_remote
  on.exit(assign("fred_search_remote", original_fred_search_remote, envir = .GlobalEnv), add = TRUE)

  assign(
    "fred_search_remote",
    function(query, frequency_filter = "all", search_type = "full_text", limit = 100) {
      if (grepl("rent", query, ignore.case = TRUE)) {
        tibble::tibble(
          id = "RENTINDEX",
          title = "Rent of primary residence",
          frequency = "Monthly",
          observation_start = as.Date("2010-01-01"),
          observation_end = as.Date("2026-01-01"),
          units = "Index",
          seasonal_adjustment = "Not Seasonally Adjusted",
          notes = "Replacement search response.",
          popularity = 42
        )
      } else {
        tibble::tibble(
          id = "UNRATE",
          title = "Unemployment Rate",
          frequency = "Monthly",
          observation_start = as.Date("1948-01-01"),
          observation_end = as.Date("2026-01-01"),
          units = "Percent",
          seasonal_adjustment = "Seasonally Adjusted",
          notes = "Initial search response.",
          popularity = 99
        )
      }
    },
    envir = .GlobalEnv
  )

  shiny::testServer(build_main_server, {
    session$setInputs(
      main_tabs = "search",
      search_query = "unemployment",
      search_source_filter = "FRED",
      search_type_filter = "all",
      search_location_filter = "INTL",
      search_frequency_filter = "all"
    )
    session$flushReact()

    add_selected_search_result_to_builder(session, 2)
    expect_equal(builder_state()$series[[2]]$fred_series, "UNRATE")
    expect_equal(builder_state()$series[[2]]$label, "Unemployment Rate")

    session$setInputs(search_query = "rent")
    session$elapse(400)
    session$flushReact()
    session$setInputs(search_results_table_rows_selected = 1, search_target_series = "2")
    session$flushReact()
    session$setInputs(search_add_series = 1)
    session$flushReact()
    session$flushReact()

    expect_equal(builder_state()$series[[2]]$fred_series, "RENTINDEX")
    expect_equal(builder_state()$series[[2]]$label, "Rent of primary residence")
  })
})

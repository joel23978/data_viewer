source(here::here("R", "bootstrap.R"))
source(here::here("external_data.R"))
source(here::here("R", "providers.R"))
source(here::here("R", "chart_helpers.R"))
source(here::here("R", "data_search.R"))

test_that("provider registry resolves all live provider seams", {
  fred_entry <- provider_registry_entry("FRED")
  dbnomics_entry <- provider_registry_entry("DBnomics")
  rba_entry <- provider_registry_entry("RBA")
  abs_entry <- provider_registry_entry("ABS")

  expect_equal(fred_entry$id, "fred")
  expect_equal(dbnomics_entry$id, "dbnomics")
  expect_equal(rba_entry$id, "rba")
  expect_equal(abs_entry$id, "abs")
  expect_true(all(c("fred", "dbnomics", "rba", "abs") %in% names(provider_registry())))
  expect_true(is.function(fred_entry$controls_ui))
  expect_true(is.function(fred_entry$search_remote))
  expect_true(is.function(dbnomics_entry$query_series_history))
  expect_true(is.function(rba_entry$search_index_builder))
  expect_true(is.function(abs_entry$register_dependencies))
})

test_that("provider registry resolves all supported provider seams", {
  registry <- provider_registry()

  expect_true(all(c("fred", "dbnomics", "rba", "abs") %in% names(registry)))
  expect_equal(provider_registry_entry("FRED")$id, "fred")
  expect_equal(provider_registry_entry("DBnomics")$id, "dbnomics")
  expect_equal(provider_registry_entry("ABS")$id, "abs")
  expect_true(is.function(provider_registry_entry("FRED")$query_series_history))
  expect_true(is.function(provider_registry_entry("dbnomics")$search_remote))
  expect_true(is.function(provider_registry_entry("abs")$search_index_builder))
})

test_that("source catalog drives builder sources and search filters", {
  catalog <- source_catalog()

  expect_true(all(c("fred", "dbnomics", "rba", "abs", "recent", "analysis_result", "abs_cpi") %in% catalog$id))
  expect_equal(default_builder_source_value(), "abs")
  expect_equal(builder_source_choices()[["ABS"]], "abs")
  expect_equal(builder_source_choices()[["Analysis result"]], "analysis_result")
  expect_equal(search_source_filter_choices()[["Recent"]], "Recent")
  expect_equal(search_source_filter_choices()[["FRED"]], "FRED")
  expect_equal(search_source_filter_choices()[["DBnomics"]], "DBnomics")
  expect_equal(search_source_filter_choices()[["RBA"]], "RBA")
  expect_equal(search_source_filter_choices()[["ABS"]], "ABS")
  expect_true(search_filter_includes("all", local_search_source_values()))
  expect_true(search_filter_includes("ABS", local_search_source_values()))
  expect_false(search_filter_includes("ABS", remote_search_source_values()))
  expect_false(source_catalog_supports_series_source("ABS CPI"))
  expect_true(source_catalog_supports_series_source("FRED"))
  expect_false(source_catalog_supports_series_source("Unknown legacy source"))
})

test_that("provider registry filters ABS and RBA search results with source-specific contexts", {
  abs_lookup <- purrr::map2_dfr(abs_ref, abs_cat, function(reference_data, catalogue_name) {
    reference_data %>%
      distinct(table_title, series_type) %>%
      mutate(abs_catalogue = catalogue_name)
  })

  abs_target <- abs_lookup %>%
    group_by(abs_catalogue) %>%
    filter(n_distinct(table_title) >= 2) %>%
    ungroup() %>%
    slice(1)

  skip_if(nrow(abs_target) == 0, "Need an ABS catalogue with at least two tables for this filter test.")

  abs_tables <- abs_lookup %>%
    filter(abs_catalogue == abs_target$abs_catalogue[[1]]) %>%
    pull(table_title) %>%
    unique() %>%
    head(2)

  abs_filtered <- provider_registry_filter_search_results(
    "ABS",
    build_abs_search_index(),
    list(
      catalogue = abs_target$abs_catalogue[[1]],
      tables = abs_tables,
      series_type = abs_target$series_type[[1]]
    )
  )

  expect_gt(nrow(abs_filtered), 0)
  expect_true(all(vapply(abs_filtered$load_payload, function(payload) identical(payload$abs_catalogue, abs_target$abs_catalogue[[1]]), logical(1))))
  expect_true(all(vapply(abs_filtered$load_payload, function(payload) payload$abs_table %in% abs_tables, logical(1))))
  expect_true(all(vapply(abs_filtered$load_payload, function(payload) identical(payload$abs_series_type, abs_target$series_type[[1]]), logical(1))))

  rba_row <- build_rba_search_index() %>% slice(1)
  rba_filtered <- provider_registry_filter_search_results(
    "RBA",
    build_rba_search_index(),
    list(table = rba_row$load_payload[[1]]$rba_table)
  )

  expect_gt(nrow(rba_filtered), 0)
  expect_true(all(vapply(rba_filtered$load_payload, function(payload) identical(payload$rba_table, rba_row$load_payload[[1]]$rba_table), logical(1))))
})

test_that("provider registry resolves exact ABS and RBA search rows from direct series IDs", {
  abs_row <- build_abs_search_index() %>% slice(1)
  rba_row <- build_rba_search_index() %>% slice(1)

  abs_lookup <- provider_registry_search_result_lookup("ABS", abs_row$load_payload[[1]]$abs_id)
  rba_lookup <- provider_registry_search_result_lookup("RBA", rba_row$load_payload[[1]]$rba_series_id)

  expect_equal(abs_lookup$result$load_payload[[1]]$abs_id, abs_row$load_payload[[1]]$abs_id)
  expect_match(abs_lookup$status, "Actual series:", fixed = TRUE)
  expect_equal(rba_lookup$result$load_payload[[1]]$rba_series_id, rba_row$load_payload[[1]]$rba_series_id)
  expect_match(rba_lookup$status, "Actual series:", fixed = TRUE)
})

test_that("legacy provider restore helpers are safe with missing ids", {
  expect_no_error(provider_registry_restore_controls(NULL, 1, list(source = "abs", abs_id = NA_character_)))
  expect_no_error(provider_registry_restore_controls(NULL, 1, list(source = "rba", rba_series_id = NA_character_)))
  expect_no_error(provider_registry_restore_controls(NULL, 1, list(source = "FRED", fred_series = NA_character_)))
  expect_no_error(provider_registry_restore_controls(NULL, 1, list(source = "dbnomics", dbnomics_series = NA_character_)))
})

test_that("provider series helper text reuses restored metadata before lookup", {
  lookup_called <- FALSE
  helper_text <- provider_series_helper_text(
    current_value = "UNRATE",
    restored_value = "UNRATE",
    restored_title = "Unemployment Rate",
    empty_status = "Type a FRED series ID and press Enter.",
    lookup_fun = function(series_id) {
      lookup_called <<- TRUE
      list(result = NULL, status = paste("Lookup", series_id))
    }
  )

  expect_false(lookup_called)
  expect_equal(helper_text$status, "Actual series: Unemployment Rate")
})

test_that("FRED and DBnomics specs clear restored titles when the series ID changes", {
  fred_spec <- provider_fred_spec_from_input(
    input = list(
      series_1_fred_series = "GDP",
      series_1_label = "",
      series_1_vis_type = "line",
      series_1_fred_vintage_mode = "current"
    ),
    index = 1,
    transform_profile = default_transform_profile(),
    restored_spec = list(
      fred_series = "UNRATE",
      fred_title = "Unemployment Rate"
    )
  )
  dbnomics_spec <- provider_dbnomics_spec_from_input(
    input = list(
      series_1_dbnomics_series = "IMF/WEO:2024-10/NGDP_RPCH",
      series_1_label = "",
      series_1_vis_type = "line"
    ),
    index = 1,
    transform_profile = default_transform_profile(),
    restored_spec = list(
      dbnomics_series = "AMECO/ZUTN/EA19.0.0.0.0.ZUTN",
      dbnomics_name = "Old series"
    )
  )

  expect_equal(fred_spec$fred_series, "GDP")
  expect_equal(fred_spec$fred_title, "")
  expect_equal(dbnomics_spec$dbnomics_series, "IMF/WEO:2024-10/NGDP_RPCH")
  expect_equal(dbnomics_spec$dbnomics_name, "")
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

test_that("RBA registry direct series ids round-trip cleanly", {
  rba_row <- rba_browse_data %>%
    distinct(table_no, description, series_id, frequency) %>%
    slice(1)

  restored_spec <- list(
    source = "rba",
    rba_table = rba_row$table_no,
    rba_desc = rba_row$description,
    rba_series_id = rba_row$series_id
  )

  input <- list(
    series_1_enabled = TRUE,
    series_1_source = "rba",
    series_1_label = "RBA label",
    series_1_vis_type = "line",
    series_1_rba_series_id = rba_row$series_id
  )

  spec <- series_spec_from_input(input, 1, default_transform_profile(), restored_spec = restored_spec)
  expect_equal(spec$source, "rba")
  expect_equal(spec$rba_table, rba_row$table_no)
  expect_equal(spec$rba_desc, rba_row$description)
  expect_equal(spec$rba_series_id, rba_row$series_id)

  normalized <- normalize_series_spec(spec)
  expect_equal(normalized$rba_table, rba_row$table_no)
  expect_equal(normalized$rba_desc, rba_row$description)
  expect_equal(normalized$rba_series_id, rba_row$series_id)

  original_rba_data <- rba_data
  on.exit(assign("rba_data", original_rba_data, envir = .GlobalEnv), add = TRUE)

  captured_series <- NULL
  assign(
    "rba_data",
    function(series, ...) {
      captured_series <<- series
      tibble::tibble(
        date = as.Date("2024-01-01"),
        value = 1,
        name = rba_row$description
      )
    },
    envir = .GlobalEnv
  )

  fetched <- query_series_history(normalized)
  expect_equal(captured_series, rba_row$series_id)
  expect_equal(fetched$name[[1]], rba_row$description)
})

test_that("RBA spec rehydrates metadata when the series ID changes", {
  rba_rows <- rba_browse_data %>%
    distinct(table_no, description, series_id) %>%
    filter(!is.na(series_id), nzchar(series_id)) %>%
    slice_head(n = 2)

  skip_if(nrow(rba_rows) < 2, "Need at least two RBA series for this test.")

  spec <- provider_rba_spec_from_input(
    input = list(
      series_1_rba_series_id = rba_rows$series_id[[2]],
      series_1_label = "",
      series_1_vis_type = "line"
    ),
    index = 1,
    transform_profile = default_transform_profile(),
    restored_spec = list(
      rba_series_id = rba_rows$series_id[[1]],
      rba_table = rba_rows$table_no[[1]],
      rba_desc = rba_rows$description[[1]]
    )
  )

  expect_equal(spec$rba_series_id, rba_rows$series_id[[2]])
  expect_equal(spec$rba_table, rba_rows$table_no[[2]])
  expect_equal(spec$rba_desc, rba_rows$description[[2]])
})

test_that("RBA registry search payload is restoreable", {
  search_index <- build_rba_search_index()
  expect_gt(nrow(search_index), 0)
  expect_true(all(search_index$source == "RBA"))
  expect_true(all(vapply(search_index$load_payload, function(payload) identical(payload$source, "rba"), logical(1))))

  search_result <- search_index %>% slice(1)
  registry_spec <- provider_registry_search_result_to_spec("RBA", search_result, 2)
  search_result_spec <- search_result_series_spec(search_result, 2)

  expect_equal(registry_spec$source, "rba")
  expect_equal(registry_spec$rba_table, search_result$load_payload[[1]]$rba_table)
  expect_equal(registry_spec$rba_desc, search_result$load_payload[[1]]$rba_desc)
  expect_equal(registry_spec$rba_series_id, search_result$load_payload[[1]]$rba_series_id)
  expect_equal(search_result_spec$source, "rba")
  expect_equal(search_result_spec$rba_desc, search_result$load_payload[[1]]$rba_desc)
  expect_equal(search_result_spec$rba_series_id, search_result$load_payload[[1]]$rba_series_id)
})

test_that("FRED and DBnomics registry round-trip builder specs and fetch dispatch", {
  original_fred_data <- fred_data
  original_db_data <- db_data
  on.exit(assign("fred_data", original_fred_data, envir = .GlobalEnv), add = TRUE)
  on.exit(assign("db_data", original_db_data, envir = .GlobalEnv), add = TRUE)

  fred_capture <- NULL
  db_capture <- NULL

  assign(
    "fred_data",
    function(series, realtime_start = NULL, realtime_end = NULL, name_override = NULL, ...) {
      fred_capture <<- list(series = series, realtime_start = realtime_start, realtime_end = realtime_end, name_override = name_override)
      tibble::tibble(date = as.Date("2024-01-01"), value = 1, name = name_override %||% series)
    },
    envir = .GlobalEnv
  )
  assign(
    "db_data",
    function(series, ...) {
      db_capture <<- series
      tibble::tibble(date = as.Date("2024-01-01"), value = 2, name = series)
    },
    envir = .GlobalEnv
  )

  fred_spec <- normalize_series_spec(list(
    index = 1,
    source = "FRED",
    fred_series = "UNRATE",
    fred_vintage_mode = "historical",
    fred_vintage_date = "2020-01-01",
    label = "",
    transform_profile = default_transform_profile(),
    vis_type = "line"
  ))
  dbnomics_spec <- normalize_series_spec(list(
    index = 2,
    source = "dbnomics",
    dbnomics_series = "AMECO/ZUTN/EA19.0.0.0.0.ZUTN",
    label = "",
    transform_profile = default_transform_profile(),
    vis_type = "line"
  ))

  expect_equal(fred_spec$fred_series, "UNRATE")
  expect_equal(dbnomics_spec$dbnomics_series, "AMECO/ZUTN/EA19.0.0.0.0.ZUTN")
  expect_equal(default_series_label_from_id(fred_spec), "UNRATE")

  query_series_history(fred_spec)
  query_series_history(dbnomics_spec)

  expect_equal(fred_capture$series, "UNRATE")
  expect_equal(as.character(fred_capture$realtime_start), "2020-01-01")
  expect_equal(db_capture, "AMECO/ZUTN/EA19.0.0.0.0.ZUTN")
})

test_that("ABS registry direct series ids normalize and query cleanly", {
  abs_row <- abs_catalogue_data(abs_cat[[1]]) %>%
    filter(!is.na(series_id), nzchar(series_id), !is.na(series), !is.na(series_type), !is.na(table_title)) %>%
    slice(1)

  normalized <- normalize_series_spec(list(
    index = 1,
    source = "abs",
    abs_id = abs_row$series_id,
    label = "",
    transform_profile = default_transform_profile(),
    vis_type = "line"
  ))
  expect_equal(normalized$abs_id, abs_row$series_id)
  expect_equal(normalized$label, abs_row$series)

  search_index <- build_abs_search_index()
  expect_gt(nrow(search_index), 0)
  expect_true(all(search_index$source == "ABS"))
  expect_true(all(vapply(search_index$load_payload, function(payload) identical(payload$source, "abs"), logical(1))))

  original_abs_data <- abs_data
  on.exit(assign("abs_data", original_abs_data, envir = .GlobalEnv), add = TRUE)

  captured_series <- NULL
  assign(
    "abs_data",
    function(series, ...) {
      captured_series <<- series
      tibble::tibble(
        date = as.Date("2024-01-01"),
        value = 3,
        name = abs_row$series
      )
    },
    envir = .GlobalEnv
  )

  fetched <- query_series_history(normalized)
  expect_equal(captured_series, abs_row$series_id)
  expect_equal(fetched$name[[1]], abs_row$series)
})

test_that("registry-backed providers round-trip spec normalization and fetch dispatch", {
  original_fred_data <- fred_data
  original_db_data <- db_data
  original_abs_data <- abs_data
  on.exit(assign("fred_data", original_fred_data, envir = .GlobalEnv), add = TRUE)
  on.exit(assign("db_data", original_db_data, envir = .GlobalEnv), add = TRUE)
  on.exit(assign("abs_data", original_abs_data, envir = .GlobalEnv), add = TRUE)

  fred_calls <- list()
  db_calls <- list()
  abs_calls <- list()

  assign(
    "fred_data",
    function(series, realtime_start = NULL, realtime_end = NULL, name_override = NULL, ...) {
      fred_calls <<- list(
        series = series,
        realtime_start = realtime_start,
        realtime_end = realtime_end,
        name_override = name_override
      )
      tibble::tibble(date = as.Date("2024-01-01"), value = 1, name = name_override %||% series)
    },
    envir = .GlobalEnv
  )
  assign(
    "db_data",
    function(series, ...) {
      db_calls <<- list(series = series)
      tibble::tibble(date = as.Date("2024-01-01"), value = 2, name = series)
    },
    envir = .GlobalEnv
  )
  assign(
    "abs_data",
    function(series, ...) {
      abs_calls <<- list(series = series)
      tibble::tibble(date = as.Date("2024-01-01"), value = 3, name = series[[1]])
    },
    envir = .GlobalEnv
  )

  fred_spec <- normalize_series_spec(list(
    index = 1,
    source = "FRED",
    fred_series = "UNRATE",
    fred_vintage_mode = "historical",
    fred_vintage_date = as.Date("2020-01-01"),
    transform_profile = default_transform_profile(),
    vis_type = "line"
  ))
  db_spec <- normalize_series_spec(list(
    index = 2,
    source = "dbnomics",
    dbnomics_series = "IMF/WEO:2024-10/NGDP_RPCH",
    transform_profile = default_transform_profile(),
    vis_type = "line"
  ))
  abs_row <- build_abs_search_index() %>% slice(1)
  abs_spec <- normalize_series_spec(search_result_series_spec(abs_row, 3))

  fred_result <- query_series_history(fred_spec)
  db_result <- query_series_history(db_spec)
  abs_result <- query_series_history(abs_spec)

  expect_equal(fred_calls$series, "UNRATE")
  expect_equal(as.Date(fred_calls$realtime_start), as.Date("2020-01-01"))
  expect_equal(db_calls$series, "IMF/WEO:2024-10/NGDP_RPCH")
  expect_equal(abs_calls$series, abs_spec$abs_id)
  expect_equal(fred_result$name[[1]], "UNRATE (2020-01-01 vintage)")
  expect_equal(db_result$name[[1]], "IMF/WEO:2024-10/NGDP_RPCH")
  expect_equal(abs_result$name[[1]], abs_spec$abs_id[[1]])
})

test_that("registry-backed search helpers return provider series IDs", {
  fred_result <- tibble::tibble(
    source = "FRED",
    load_payload = list(list(source = "FRED", fred_series = "UNRATE"))
  )
  dbnomics_result <- tibble::tibble(
    source = "DBnomics",
    load_payload = list(list(source = "dbnomics", dbnomics_series = "IMF/WEO:2024-10/NGDP_RPCH"))
  )
  rba_result <- build_rba_search_index() %>% slice(1)
  abs_result <- build_abs_search_index() %>% slice(1)

  expect_equal(provider_registry_search_result_series_id(fred_result), "UNRATE")
  expect_equal(provider_registry_search_result_series_id(dbnomics_result), "IMF/WEO:2024-10/NGDP_RPCH")
  expect_true(nzchar(provider_registry_search_result_series_id(rba_result)))
  expect_true(nzchar(provider_registry_search_result_series_id(abs_result)))
})

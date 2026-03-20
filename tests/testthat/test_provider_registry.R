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

  expect_true(all(c("fred", "dbnomics", "rba", "abs", "recent", "analysis_result") %in% catalog$id))
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
})

test_that("RBA registry controls and fetch dispatch round-trip cleanly", {
  rba_row <- rba_browse_data %>%
    distinct(table_no, description, series_id, frequency) %>%
    slice(1)

  restored_spec <- list(
    source = "rba",
    rba_table = rba_row$table_no,
    rba_desc = rba_row$description
  )

  rba_ui <- series_source_controls_ui(
    input = list(),
    session = NULL,
    index = 1,
    source_value = "rba",
    restored_spec = restored_spec
  )

  expect_true(grepl(rba_row$table_no, as.character(rba_ui), fixed = TRUE))
  expect_true(grepl(rba_row$description, as.character(rba_ui), fixed = TRUE))

  input <- list(
    series_1_enabled = TRUE,
    series_1_source = "rba",
    series_1_label = "RBA label",
    series_1_vis_type = "line",
    series_1_rba_table = rba_row$table_no,
    series_1_rba_desc = rba_row$description
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

  fred_input <- list(
    series_1_enabled = TRUE,
    series_1_source = "FRED",
    series_1_label = "",
    series_1_vis_type = "line",
    series_1_fred_series = "UNRATE",
    series_1_fred_vintage_mode = "historical",
    series_1_fred_vintage_date = "2020-01-01"
  )
  dbnomics_input <- list(
    series_2_enabled = TRUE,
    series_2_source = "dbnomics",
    series_2_label = "",
    series_2_vis_type = "line",
    series_2_dbnomics_series = "AMECO/ZUTN/EA19.0.0.0.0.ZUTN"
  )

  fred_spec <- series_spec_from_input(fred_input, 1, default_transform_profile())
  dbnomics_spec <- series_spec_from_input(dbnomics_input, 2, default_transform_profile())

  expect_equal(normalize_series_spec(fred_spec)$fred_series, "UNRATE")
  expect_equal(normalize_series_spec(dbnomics_spec)$dbnomics_series, "AMECO/ZUTN/EA19.0.0.0.0.ZUTN")
  expect_equal(default_series_label_from_id(fred_spec), "UNRATE")

  query_series_history(normalize_series_spec(fred_spec))
  query_series_history(normalize_series_spec(dbnomics_spec))

  expect_equal(fred_capture$series, "UNRATE")
  expect_equal(as.character(fred_capture$realtime_start), "2020-01-01")
  expect_equal(db_capture, "AMECO/ZUTN/EA19.0.0.0.0.ZUTN")
})

test_that("ABS registry owns controls, normalization, and local search index payloads", {
  abs_row <- abs_catalogue_data(abs_cat[[1]]) %>%
    filter(!is.na(series_id), nzchar(series_id), !is.na(series), !is.na(series_type), !is.na(table_title)) %>%
    slice(1)

  restored_spec <- list(
    source = "abs",
    abs_catalogue = abs_cat[[1]],
    abs_desc = abs_row$series,
    abs_series_type = abs_row$series_type,
    abs_table = abs_row$table_title,
    abs_id = abs_row$series_id
  )

  abs_ui <- series_source_controls_ui(
    input = list(),
    session = NULL,
    index = 1,
    source_value = "abs",
    restored_spec = restored_spec
  )

  expect_true(grepl(abs_row$series_id, as.character(abs_ui), fixed = TRUE))

  spec <- series_spec_from_input(
    list(
      series_1_enabled = TRUE,
      series_1_source = "abs",
      series_1_label = "",
      series_1_vis_type = "line",
      series_1_abs_id = abs_row$series_id
    ),
    1,
    default_transform_profile(),
    restored_spec = restored_spec
  )

  normalized <- normalize_series_spec(spec)
  expect_equal(normalized$abs_id, abs_row$series_id)
  expect_equal(normalized$label, abs_row$series_id)

  search_index <- build_abs_search_index()
  expect_gt(nrow(search_index), 0)
  expect_true(all(search_index$source == "ABS"))
  expect_true(all(vapply(search_index$load_payload, function(payload) identical(payload$source, "abs"), logical(1))))
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

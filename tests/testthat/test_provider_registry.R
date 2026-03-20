source(here::here("R", "bootstrap.R"))
source(here::here("external_data.R"))
source(here::here("R", "providers.R"))
source(here::here("R", "chart_helpers.R"))
source(here::here("R", "data_search.R"))

test_that("provider registry resolves the RBA seam", {
  entry <- provider_registry_entry("rba")
  alias_entry <- provider_registry_entry("RBA")

  expect_equal(entry$id, "rba")
  expect_equal(entry$label, "RBA")
  expect_equal(alias_entry$id, "rba")
  expect_true(is.function(entry$controls_ui))
  expect_true(is.function(entry$spec_from_input))
  expect_true(is.function(entry$normalize_spec))
  expect_true(is.function(entry$query_series_history))
  expect_true(is.function(entry$search_index_builder))
  expect_true("rba" %in% names(provider_registry()))
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

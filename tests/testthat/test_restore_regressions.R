source(here::here("R", "bootstrap.R"))
source(here::here("external_data.R"))
source(here::here("R", "providers.R"))
source(here::here("R", "chart_helpers.R"))
source(here::here("R", "data_search.R"))
source(here::here("R", "chart_library.R"))
source(here::here("R", "analysis_helpers.R"))
source(here::here("R", "main_app.R"))
source(here::here("tests", "testthat", "helper_app_fixtures.R"))

build_restore_abs_specs <- function(env = parent.frame()) {
  local_mock_abs_data(env)
  abs_rows <- valid_abs_restore_rows(abs_cat[[1]]) %>% slice_head(n = 2)

  if (nrow(abs_rows) < 2) {
    stop("Need at least two ABS rows for restore tests.", call. = FALSE)
  }

  purrr::map(seq_len(2), function(index) {
    list(
      index = index,
      source = "abs",
      abs_catalogue = abs_cat[[1]],
      abs_desc = abs_rows$series[[index]],
      abs_series_type = abs_rows$series_type[[index]],
      abs_table = abs_rows$table_title[[index]],
      abs_id = abs_rows$series_id[[index]],
      label = abs_rows$series[[index]],
      transform_profile = list(expression = "X", moving_average = 1, rolling_sum = 1, lagged_value = 0, lagged_pct = 0, lagged_ann = 0),
      vis_type = "line"
    )
  })
}

build_restore_test_state <- function(env = parent.frame()) {
  abs_specs <- build_restore_abs_specs(env)

  list(
    date_range = c(2024, 2025),
    show_table = TRUE,
    series = list(
      abs_specs[[1]],
      abs_specs[[2]],
      NULL,
      NULL
    ),
    all_series_transform = list(expression = "X", moving_average = 1, rolling_sum = 1, lagged_value = 0, lagged_pct = 0, lagged_ann = 0),
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

valid_abs_restore_rows <- function(catalogue_name) {
  abs_catalogue_data(catalogue_name) %>%
    filter(!is.na(series_id), nzchar(series_id), !is.na(series), !is.na(series_type), !is.na(table_title))
}

test_that("presentation-selected preview takes precedence and load uses selected_preview_chart_record", {
  temp_library <- tempfile(fileext = ".rds")
  temp_presentation_library <- tempfile(fileext = ".rds")
  old_option <- getOption("data_viewer.chart_library_path")
  old_presentation_option <- getOption("data_viewer.chart_presentation_library_path")
  options(data_viewer.chart_library_path = temp_library)
  options(data_viewer.chart_presentation_library_path = temp_presentation_library)
  on.exit(options(data_viewer.chart_library_path = old_option), add = TRUE)
  on.exit(options(data_viewer.chart_presentation_library_path = old_presentation_option), add = TRUE)

  first_state <- build_restore_test_state()
  second_state <- build_restore_test_state()
  second_state$style$title <- "Presentation-selected chart"
  second_state$series[[2]]$label <- "Presentation-selected series"

  first_record <- new_chart_record(
    chart_state = first_state,
    data_snapshot = build_chart_data(first_state)$data,
    title = "Library-selected chart",
    description = "First chart"
  )
  second_record <- new_chart_record(
    chart_state = second_state,
    data_snapshot = build_chart_data(second_state)$data,
    title = "Presentation-selected chart",
    description = "Second chart"
  )

  write_chart_library(
    read_chart_library() %>%
      upsert_chart_record(first_record) %>%
      upsert_chart_record(second_record)
  )
  write_chart_presentation_library(
    upsert_chart_presentation_record(
      read_chart_presentation_library(),
      new_chart_presentation_record(
        title = "Macro deck",
        description = "Preview precedence test",
        chart_ids = c(first_record$chart_id[[1]], second_record$chart_id[[1]])
      )
    )
  )

  suppressWarnings(shiny::testServer(build_main_server, {
    ensure_chart_library_loaded()
    ensure_presentation_library_loaded()

    session$setInputs(library_table_rows_selected = 1)
    session$setInputs(presentation_table_rows_selected = 1)
    session$flushReact()
    session$setInputs(presentation_chart_table_rows_selected = 2)
    session$flushReact()

    expect_equal(selected_preview_chart_record()$chart_id[[1]], second_record$chart_id[[1]])

    session$setInputs(load_chart = 1)
    session$flushReact()
    session$flushReact()

    expect_equal(builder_state()$style$title, second_state$style$title)
  }))
})

test_that("main server suppresses stale restore callbacks from earlier apply_builder_state calls", {
  suppressWarnings(shiny::testServer(build_main_server, {
    first_state <- build_restore_test_state()
    first_state$style$title <- "First restore"
    first_state$series[[2]]$label <- "First restore series"

    second_state <- build_restore_test_state()
    second_state$style$title <- "Second restore"
    second_state$series[[2]]$label <- "Second restore series"

    apply_builder_state(first_state, selected_series_index = 2, navigate_builder = FALSE)
    apply_builder_state(second_state, selected_series_index = 2, navigate_builder = FALSE)
    session$flushReact()
    session$flushReact()

    expect_equal(builder_state()$style$title, second_state$style$title)
    expect_equal(builder_state()$series[[2]]$label, second_state$series[[2]]$label)
  }))
})

test_that("ABS source controls render restored saved selections", {
  abs_row <- abs_ref[[1]] %>%
    filter(!is.na(series_id), nzchar(series_id), !is.na(series), !is.na(series_type), !is.na(table_title)) %>%
    slice(1)

  abs_ui <- series_source_controls_ui(
    input = list(),
    session = NULL,
    index = 1,
    source_value = "abs",
    restored_spec = list(
      source = "abs",
      abs_catalogue = abs_cat[[1]],
      abs_desc = abs_row$series[[1]],
      abs_series_type = abs_row$series_type[[1]],
      abs_table = abs_row$table_title[[1]],
      abs_id = abs_row$series_id[[1]]
    )
  )

  expect_true(grepl(abs_cat[[1]], as.character(abs_ui), fixed = TRUE))
  expect_true(grepl(abs_row$series[[1]], as.character(abs_ui), fixed = TRUE))
  expect_true(grepl(abs_row$series_type[[1]], as.character(abs_ui), fixed = TRUE))
  expect_true(grepl(abs_row$table_title[[1]], as.character(abs_ui), fixed = TRUE))
  expect_true(grepl(abs_row$series_id[[1]], as.character(abs_ui), fixed = TRUE))
})

test_that("ABS source controls prefer restored values over stale invalid selections", {
  abs_row <- abs_ref[[1]] %>%
    filter(!is.na(series_id), nzchar(series_id), !is.na(series), !is.na(series_type), !is.na(table_title)) %>%
    slice(1)

  abs_ui <- series_source_controls_ui(
    input = list(
      series_2_abs_catalogue = abs_cat[[1]],
      series_2_abs_desc = "__stale_desc__",
      series_2_abs_series_type = "__stale_type__",
      series_2_abs_table = "__stale_table__",
      series_2_abs_id = "__stale_id__"
    ),
    session = NULL,
    index = 2,
    source_value = "abs",
    restored_spec = list(
      source = "abs",
      abs_catalogue = abs_cat[[1]],
      abs_desc = abs_row$series[[1]],
      abs_series_type = abs_row$series_type[[1]],
      abs_table = abs_row$table_title[[1]],
      abs_id = abs_row$series_id[[1]]
    )
  )

  expect_true(grepl(abs_row$table_title[[1]], as.character(abs_ui), fixed = TRUE))
  expect_true(grepl(abs_row$series_id[[1]], as.character(abs_ui), fixed = TRUE))
})

test_that("manual ABS selection stays blank until the user steps through", {
  initial_state <- resolve_abs_control_state(current_values = list(), restored_spec = NULL)
  expect_length(initial_state$catalogue, 0)
  expect_length(initial_state$desc, 0)
  expect_length(initial_state$series_type, 0)
  expect_length(initial_state$table, 0)
  expect_length(initial_state$ids, 0)

  catalogue_state <- resolve_abs_control_state(
    current_values = list(catalogue = abs_cat[[1]]),
    restored_spec = NULL
  )
  expect_equal(catalogue_state$catalogue, abs_cat[[1]])
  expect_length(catalogue_state$desc, 0)
  expect_length(catalogue_state$series_type, 0)
  expect_length(catalogue_state$table, 0)
  expect_length(catalogue_state$ids, 0)
})

test_that("ABS series ID direct entry works without dropdown selections", {
  abs_row <- abs_ref[[1]] %>%
    filter(!is.na(series_id), nzchar(series_id), !is.na(series), !is.na(series_type), !is.na(table_title)) %>%
    slice(1)

  spec <- series_spec_from_input(
    input = list(
      series_1_enabled = TRUE,
      series_1_source = "abs",
      series_1_label = "Direct ABS ID",
      series_1_vis_type = "line",
      series_1_abs_id = abs_row$series_id[[1]]
    ),
    index = 1,
    transform_profile = default_transform_profile(),
    restored_spec = NULL
  )

  expect_equal(spec$abs_id, abs_row$series_id[[1]])
  expect_equal(spec$abs_catalogue, abs_cat[[1]])
  expect_equal(spec$abs_desc, abs_row$series[[1]])
  expect_equal(spec$abs_series_type, abs_row$series_type[[1]])
  expect_equal(spec$abs_table, abs_row$table_title[[1]])
})

test_that("blank ABS and FRED labels default to series IDs", {
  abs_row <- abs_ref[[1]] %>%
    filter(!is.na(series_id), nzchar(series_id), !is.na(series), !is.na(series_type), !is.na(table_title)) %>%
    slice(1)

  abs_spec <- series_spec_from_input(
    input = list(
      series_1_enabled = TRUE,
      series_1_source = "abs",
      series_1_label = "",
      series_1_vis_type = "line",
      series_1_abs_id = abs_row$series_id[[1]]
    ),
    index = 1,
    transform_profile = default_transform_profile(),
    restored_spec = NULL
  )

  fred_spec <- series_spec_from_input(
    input = list(
      series_2_enabled = TRUE,
      series_2_source = "FRED",
      series_2_label = "",
      series_2_vis_type = "line",
      series_2_fred_series = "UNRATE",
      series_2_fred_vintage_mode = "compare",
      series_2_fred_vintage_date = "2020-01-01"
    ),
    index = 2,
    transform_profile = default_transform_profile(),
    restored_spec = NULL
  )

  fred_data <- tibble::tibble(
    date = as.Date(c("2020-01-01", "2020-01-01")),
    value = c(1, 2),
    name = c("UNRATE (current)", "UNRATE (2020-01-01 vintage)")
  )

  expect_equal(abs_spec$label, abs_row$series_id[[1]])
  expect_equal(fred_spec$label, "UNRATE")
  expect_equal(apply_series_metadata(fred_data, fred_spec)$name, fred_data$name)
})

test_that("builder restore updates plotted data when loading different charts", {
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
      tibble::tibble(
        date = as.Date(c("2020-01-01", "2020-02-01")),
        value = if (identical(series, "UNRATE")) c(1, 2) else c(10, 20),
        name = if (!is.null(name_override) && nzchar(name_override)) name_override else series
      )
    },
    envir = .GlobalEnv
  )

  build_fred_state <- function(series_id, title) {
    state <- default_builder_state()
    state$series[[1]] <- normalize_series_spec(list(
      index = 1,
      source = "FRED",
      fred_series = series_id,
      label = "",
      transform_profile = default_transform_profile(),
      vis_type = "line"
    ))
    state$style$title <- title
    state
  }

  first_record <- new_chart_record(
    chart_state = build_fred_state("UNRATE", "First"),
    data_snapshot = build_chart_data(build_fred_state("UNRATE", "First"))$data,
    title = "First",
    description = ""
  )
  second_record <- new_chart_record(
    chart_state = build_fred_state("CPIAUCSL", "Second"),
    data_snapshot = build_chart_data(build_fred_state("CPIAUCSL", "Second"))$data,
    title = "Second",
    description = ""
  )

  write_chart_library(
    read_chart_library() %>%
      upsert_chart_record(first_record) %>%
      upsert_chart_record(second_record)
  )

  shiny::testServer(build_main_server, {
    ensure_chart_library_loaded()

    session$setInputs(library_table_rows_selected = 1)
    suppressWarnings({
      session$setInputs(load_chart = 1)
      session$flushReact()
      session$flushReact()
      session$flushReact()
    })
    first_loaded_id <- builder_state()$series[[1]]$fred_series
    first_loaded_names <- unique(chart_data()$name)

    session$setInputs(library_table_rows_selected = 2)
    suppressWarnings({
      session$setInputs(load_chart = 2)
      session$flushReact()
      session$flushReact()
      session$flushReact()
    })
    second_loaded_id <- builder_state()$series[[1]]$fred_series
    second_loaded_names <- unique(chart_data()$name)

    expect_false(identical(first_loaded_id, second_loaded_id))
    expect_identical(first_loaded_names, first_loaded_id)
    expect_identical(second_loaded_names, second_loaded_id)
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
    suppressWarnings({
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
    })

    expect_equal(builder_state()$series[[2]]$fred_vintage_mode, "compare")
    expect_equal(as.Date(builder_state()$series[[2]]$fred_vintage_date), as.Date("2020-01-01"))
    expect_gt(nrow(chart_data()), 0)

    suppressWarnings({
      session$setInputs(save_chart = 1)
      session$flushReact()
    })

    suppressWarnings({
      session$setInputs(
        series_2_fred_vintage_mode = "current",
        style_title = "Mutated"
      )
      session$flushReact()
    })

    session$setInputs(library_table_rows_selected = 1)
    suppressWarnings({
      session$setInputs(load_chart = 1)
      session$flushReact()
    })

    expect_equal(builder_state()$series[[2]]$fred_vintage_mode, "compare")
    expect_equal(as.Date(builder_state()$series[[2]]$fred_vintage_date), as.Date("2020-01-01"))
  })
})

test_that("ABS search add-to-builder restores dependent chain", {
  shiny::testServer(build_main_server, {
    ensure_search_index_loaded()
    session$setInputs(
      main_tabs = "search",
      search_query = "",
      search_source_filter = "ABS",
      search_type_filter = "all",
      search_location_filter = "all",
      search_frequency_filter = "all"
    )
    session$flushReact()
    session$flushReact()

    expect_gt(nrow(search_results()), 0)
    abs_row <- search_results()[1, , drop = FALSE]
    abs_spec <- search_result_series_spec(abs_row, 2)

    session$setInputs(search_results_table_rows_selected = 1, search_target_series = "2")
    session$flushReact()
    session$setInputs(search_add_series = 1)
    session$flushReact()
    session$flushReact()

    expect_equal(builder_state()$series[[2]]$source, "abs")
    expect_equal(builder_state()$series[[2]]$abs_catalogue, abs_spec$abs_catalogue)
    expect_equal(builder_state()$series[[2]]$abs_desc, abs_spec$abs_desc)
    expect_equal(builder_state()$series[[2]]$abs_series_type, abs_spec$abs_series_type)
    expect_equal(builder_state()$series[[2]]$abs_table, abs_spec$abs_table)
    expect_equal(builder_state()$series[[2]]$abs_id, abs_spec$abs_id)
    expect_equal(restored_series_spec(session, 2)$abs_catalogue, abs_spec$abs_catalogue)
    expect_equal(restored_series_spec(session, 2)$abs_desc, abs_spec$abs_desc)
  })
})

test_that("ABS saved-chart load restores dependent chain", {
  temp_library <- tempfile(fileext = ".rds")
  old_option <- getOption("data_viewer.chart_library_path")
  options(data_viewer.chart_library_path = temp_library)
  on.exit(options(data_viewer.chart_library_path = old_option), add = TRUE)

  ensure_chart_library()

  abs_result <- build_abs_search_index() %>%
    filter(source == "ABS") %>%
    slice(1)
  abs_spec <- search_result_series_spec(abs_result, 2)

  saved_state <- build_restore_test_state()
  saved_state$series[[2]] <- normalize_series_spec(abs_spec)
  saved_payload <- build_chart_data(saved_state)
  saved_record <- new_chart_record(
    chart_state = saved_state,
    data_snapshot = saved_payload$data,
    title = "ABS saved chart",
    description = "Saved ABS restore test",
    chart_kind = "builder",
    analysis_spec = NULL,
    analysis_payload = NULL
  )
  write_chart_library(upsert_chart_record(read_chart_library(), saved_record))

  suppressWarnings(shiny::testServer(build_main_server, {
    ensure_chart_library_loaded()
    session$setInputs(library_table_rows_selected = 1)
    session$flushReact()
    session$setInputs(load_chart = 1)
    session$flushReact()
    session$flushReact()

    expect_equal(builder_state()$series[[2]]$source, "abs")
    expect_equal(builder_state()$series[[2]]$abs_catalogue, abs_spec$abs_catalogue)
    expect_equal(builder_state()$series[[2]]$abs_desc, abs_spec$abs_desc)
    expect_equal(builder_state()$series[[2]]$abs_series_type, abs_spec$abs_series_type)
    expect_equal(builder_state()$series[[2]]$abs_table, abs_spec$abs_table)
    expect_equal(builder_state()$series[[2]]$abs_id, abs_spec$abs_id)
    expect_equal(restored_series_spec(session, 2)$abs_catalogue, abs_spec$abs_catalogue)
    expect_equal(restored_series_spec(session, 2)$abs_desc, abs_spec$abs_desc)
  }))
})

test_that("upstream ABS selector change after restore re-resolves downstream cleanly", {
  abs_result <- build_abs_search_index() %>%
    filter(source == "ABS") %>%
    slice(1)
  abs_spec <- search_result_series_spec(abs_result, 2)

  alternative_catalogue <- setdiff(abs_cat, abs_spec$abs_catalogue)[1]
  skip_if(is.na(alternative_catalogue) || !nzchar(alternative_catalogue), "Need at least two ABS catalogues for this regression test.")
  alternative_rows <- valid_abs_restore_rows(alternative_catalogue)
  skip_if(nrow(alternative_rows) == 0, "No valid ABS rows found in the alternate catalogue.")

  resolved <- resolve_abs_control_state(
    current_values = list(
      catalogue = alternative_catalogue,
      desc = abs_spec$abs_desc,
      type = abs_spec$abs_series_type,
      table = abs_spec$abs_table,
      ids = abs_spec$abs_id
    ),
    restored_spec = NULL
  )

  expect_equal(resolved$catalogue, alternative_catalogue)
  expect_length(resolved$desc, 0)
  expect_length(resolved$series_type, 0)
  expect_length(resolved$table, 0)
  expect_equal(resolved$ids, abs_spec$abs_id)
})

test_that("restored/live transition does not reapply stale restored values", {
  abs_result <- build_abs_search_index() %>%
    filter(source == "ABS") %>%
    slice(1)
  abs_spec <- search_result_series_spec(abs_result, 2)
  abs_rows <- valid_abs_restore_rows(abs_spec$abs_catalogue)
  alternate_desc <- setdiff(unique(abs_rows$series), abs_spec$abs_desc)[1]
  skip_if(is.na(alternate_desc) || !nzchar(alternate_desc), "Need an alternate ABS description for this regression test.")

  resolved <- resolve_abs_control_state(
    current_values = list(
      catalogue = abs_spec$abs_catalogue,
      desc = alternate_desc,
      type = abs_spec$abs_series_type,
      table = abs_spec$abs_table,
      ids = abs_spec$abs_id
    ),
    restored_spec = NULL
  )

  downstream_rows <- valid_abs_restore_rows(resolved$catalogue) %>%
    filter(
      series == resolved$desc,
      series_type == resolved$series_type,
      table_title == resolved$table
    )

  expect_equal(resolved$desc, alternate_desc)
  expect_gt(nrow(downstream_rows), 0)
  expect_true(all(resolved$ids %in% downstream_rows$series_id))
})

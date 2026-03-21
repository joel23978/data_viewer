local_mock_abs_data <- function(env = parent.frame()) {
  original_abs_data <- abs_data

  if (length(ls(envir = series_cache_env)) > 0) {
    rm(list = ls(envir = series_cache_env), envir = series_cache_env)
  }

  assign(
    "abs_data",
    function(series = NULL) {
      selected_ids <- unique(trimws(as.character(series %||% character())))
      selected_ids <- selected_ids[nzchar(selected_ids)]

      if (length(selected_ids) == 0) {
        return(data_viewer_empty_series())
      }

      purrr::map_dfr(seq_along(selected_ids), function(index) {
        series_id <- selected_ids[[index]]
        matched_row <- abs_rows_by_id(series_id) %>% slice_head(n = 1)
        series_name <- matched_row$series[[1]] %||% series_id

        tibble::tibble(
          date = seq(as.Date("2020-01-01"), by = "quarter", length.out = 24),
          value = seq_len(24) + (index - 1) * 10,
          name = series_name
        )
      })
    },
    envir = .GlobalEnv
  )

  withr::defer(assign("abs_data", original_abs_data, envir = .GlobalEnv), envir = env)
  withr::defer({
    if (length(ls(envir = series_cache_env)) > 0) {
      rm(list = ls(envir = series_cache_env), envir = series_cache_env)
    }
  }, envir = env)
}

valid_abs_fixture_rows <- function(catalogue_name) {
  abs_catalogue_data(catalogue_name) %>%
    filter(!is.na(series_id), nzchar(series_id), !is.na(series), !is.na(series_type), !is.na(table_title))
}

build_test_abs_specs <- function(env = parent.frame()) {
  local_mock_abs_data(env)
  abs_rows <- valid_abs_fixture_rows(abs_cat[[1]]) %>% slice_head(n = 2)

  if (nrow(abs_rows) < 2) {
    stop("Need at least two ABS rows for tests.", call. = FALSE)
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

build_test_state <- function(env = parent.frame()) {
  abs_specs <- build_test_abs_specs(env)

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

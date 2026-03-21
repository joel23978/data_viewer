source(here::here("R", "bootstrap.R"))
source(here::here("external_data.R"))
source(here::here("R", "providers.R"))
source(here::here("R", "chart_helpers.R"))
source(here::here("R", "data_search.R"))
source(here::here("R", "chart_library.R"))
source(here::here("R", "analysis_helpers.R"))
source(here::here("R", "main_app.R"))
source(here::here("tests", "testthat", "helper_app_fixtures.R"))

apply_test_builder_state <- function(session, apply_builder_state, state) {
  apply_builder_state(normalize_chart_state(state), selected_series_index = 1, navigate_builder = FALSE)
  session$flushReact()
  session$flushReact()
}

test_that("main server saves and reloads a builder chart round trip", {
  test_state <- build_test_state()
  test_state$series[[1]]$transform_profile$expression <- "X * 2"
  test_state$series[[1]]$transform_profile$moving_average <- 2
  test_state$series[[2]]$transform_profile$expression <- "data / 2"
  test_state$series[[2]]$transform_profile$moving_average <- 3
  test_state$style$title <- "Server Test Chart"
  test_state$style$subtitle <- "Server subtitle"
  test_state$style$y_axis_label <- "Index"
  test_state$style$note <- "server test note"
  test_state$style$legend <- "bottom"
  temp_library <- tempfile(fileext = ".rds")
  temp_presentation_library <- tempfile(fileext = ".rds")
  old_option <- getOption("data_viewer.chart_library_path")
  old_presentation_option <- getOption("data_viewer.chart_presentation_library_path")
  options(data_viewer.chart_library_path = temp_library)
  options(data_viewer.chart_presentation_library_path = temp_presentation_library)
  on.exit(options(data_viewer.chart_library_path = old_option), add = TRUE)
  on.exit(options(data_viewer.chart_presentation_library_path = old_presentation_option), add = TRUE)

  suppressWarnings(shiny::testServer(build_main_server, {
    suppressWarnings({
      apply_test_builder_state(session, apply_builder_state, test_state)
      session$setInputs(
        start_date = as.Date("2024-01-01"),
        end_date = as.Date("2025-12-31"),
        viewData1 = "1"
      )
      session$flushReact()
    })

    expect_equal(builder_state()$series[[1]]$transform_profile$expression, "X * 2")
    expect_equal(builder_state()$series[[2]]$transform_profile$moving_average, 3)
    expect_equal(builder_state()$style$subtitle, "Server subtitle")
    expect_gt(nrow(chart_data()), 0)
    expect_s3_class(main_panel_widget(), "plotly")

    saved_state <- builder_state()
    saved_chart_data <- chart_data()

    suppressWarnings({
      session$setInputs(
        library_title = "Saved from test",
        library_description = "server flow"
      )
      session$setInputs(save_chart = 1)
      session$flushReact()
    })

    saved_library <- read_chart_library()
    expect_equal(nrow(saved_library), 1)
    expect_equal(saved_library$title[[1]], "Saved from test")

    suppressWarnings({
      session$setInputs(style_title = "Mutated")
      session$flushReact()
    })
    expect_false(chart_states_equal(builder_state(), saved_state))

    session$setInputs(library_table_rows_selected = 1)
    suppressWarnings({
      session$setInputs(load_chart = 1)
      session$flushReact()
      session$flushReact()
    })

    expect_true(chart_states_equal(restored_state(), saved_state))
    expect_equal(nrow(chart_data()), nrow(saved_chart_data))
    expect_equal(sort(unique(chart_data()$name)), sort(unique(saved_chart_data$name)))

    suppressWarnings({
      session$setInputs(library_title = "Updated title")
      session$setInputs(update_chart = 1)
      session$flushReact()
    })

    expect_equal(read_chart_library()$title[[1]], "Updated title")
  }))
})

test_that("main server manages presentation chart membership", {
  abs_specs <- build_test_abs_specs()
  first_state <- build_test_state()
  second_state <- build_test_state()
  second_state$series[[3]] <- modifyList(abs_specs[[1]], list(
    index = 3,
    label = paste(abs_specs[[1]]$label, "copy")
  ))
  temp_library <- tempfile(fileext = ".rds")
  temp_presentation_library <- tempfile(fileext = ".rds")
  old_option <- getOption("data_viewer.chart_library_path")
  old_presentation_option <- getOption("data_viewer.chart_presentation_library_path")
  options(data_viewer.chart_library_path = temp_library)
  options(data_viewer.chart_presentation_library_path = temp_presentation_library)
  on.exit(options(data_viewer.chart_library_path = old_option), add = TRUE)
  on.exit(options(data_viewer.chart_presentation_library_path = old_presentation_option), add = TRUE)

  suppressWarnings(shiny::testServer(build_main_server, {
    suppressWarnings({
      apply_test_builder_state(session, apply_builder_state, first_state)
      session$setInputs(
        start_date = as.Date("2024-01-01"),
        end_date = as.Date("2025-12-31"),
        library_title = "First chart"
      )
      session$flushReact()
      session$setInputs(save_chart = 1)
      session$flushReact()
    })

    suppressWarnings({
      apply_test_builder_state(session, apply_builder_state, second_state)
      session$setInputs(
        library_title = "Second chart"
      )
      session$flushReact()
      session$setInputs(save_chart = 2)
      session$flushReact()
    })

    expect_equal(nrow(read_chart_library()), 2)

    suppressWarnings({
      session$setInputs(library_table_rows_selected = 1)
      session$setInputs(
        presentation_title = "Macro deck",
        presentation_description = "server presentation flow"
      )
      session$setInputs(create_presentation = 1)
      session$flushReact()
    })

    expect_equal(nrow(read_chart_presentation_library()), 1)
    expect_equal(read_chart_presentation_library()$chart_count[[1]], 1)

    suppressWarnings({
      session$setInputs(presentation_table_rows_selected = 1)
      session$setInputs(library_table_rows_selected = c(1, 2))
      session$setInputs(add_to_presentation = 1)
      session$flushReact()
    })

    expect_equal(read_chart_presentation_library()$chart_count[[1]], 2)

    initial_order <- read_chart_presentation_library()$chart_ids[[1]]
    suppressWarnings({
      session$setInputs(presentation_chart_table_rows_selected = 2)
      session$setInputs(presentation_chart_up = 1)
      session$flushReact()
    })
    reordered_ids <- read_chart_presentation_library()$chart_ids[[1]]

    expect_equal(length(reordered_ids), 2)
    expect_false(identical(initial_order, reordered_ids))

    suppressWarnings({
      session$setInputs(library_table_rows_selected = 2)
      session$setInputs(replace_presentation_charts = 1)
      session$flushReact()
    })

    expect_equal(read_chart_presentation_library()$chart_count[[1]], 1)
    expect_equal(read_chart_presentation_library()$chart_ids[[1]], read_chart_library()$chart_id[[2]])
  }))
})

test_that("main server can reset the builder to its default state", {
  suppressWarnings(shiny::testServer(build_main_server, {
    mutated_state <- default_builder_state()
    mutated_state$date_range <- c(as.Date("2020-01-01"), as.Date("2021-12-31"))
    mutated_state$style$title <- "Mutated title"
    mutated_state$style$subtitle <- "Mutated subtitle"
    mutated_state$series[[2]] <- normalize_series_spec(list(
      index = 2,
      source = "FRED",
      fred_series = "UNRATE",
      label = "US unemployment",
      transform_profile = default_transform_profile(),
      vis_type = "line"
    ))

    suppressWarnings({
      apply_test_builder_state(session, apply_builder_state, mutated_state)
    })

    session$setInputs(reset_builder = 1)
    session$flushReact()
    session$flushReact()

    expect_true(chart_states_equal(builder_state(), default_builder_state()))
    expect_equal(nrow(chart_data()), 0)
  }))
})

test_that("main server refreshes populated builder slot metadata controls after restore", {
  suppressWarnings(shiny::testServer(build_main_server, {
    test_state <- build_test_state()

    before_html <- paste(as.character(output[[series_source_controls_id(1)]]), collapse = " ")
    expect_match(before_html, "Add a series from Data Search to populate this slot.")

    apply_test_builder_state(session, apply_builder_state, test_state)

    after_html <- paste(as.character(output[[series_source_controls_id(1)]]), collapse = " ")
    expect_match(after_html, "Actual series:")
    expect_match(after_html, "Chart label shown in the legend")
    expect_match(after_html, "Chart style for this series")
  }))
})

test_that("main server can clear series setup, presentation, and workspace panels", {
  abs_specs <- build_test_abs_specs()

  suppressWarnings(shiny::testServer(build_main_server, {
    seeded_state <- default_builder_state()
    seeded_state$style$title <- "Custom title"
    seeded_state$style$subtitle <- "Custom subtitle"
    seeded_state$style$y_axis_label <- "Index"
    seeded_state$style$note <- "Custom note"
    seeded_state$all_series_transform$expression <- "X * 2"
    seeded_state$series[[1]] <- normalize_series_spec(modifyList(abs_specs[[1]], list(index = 1, label = "CPI one")))
    seeded_state$series[[1]]$transform_profile$rolling_sum <- 4
    seeded_state$series[[2]] <- normalize_series_spec(list(
      index = 2,
      source = "FRED",
      fred_series = "UNRATE",
      label = "Unemployment",
      transform_profile = default_transform_profile(),
      vis_type = "line"
    ))

    suppressWarnings({
      apply_test_builder_state(session, apply_builder_state, seeded_state)
      session$setInputs(analysis_corr_window = 8, analysis_forecast_horizon = 6)
      session$flushReact()
    })

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

    expect_equal(builder_state()$style$note, default_source_note(builder_state()$series))
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
  }))
})

source(here::here("R", "bootstrap.R"))
source(here::here("external_data.R"))
source(here::here("R", "providers.R"))
source(here::here("R", "chart_helpers.R"))
source(here::here("R", "data_search.R"))
source(here::here("R", "chart_library.R"))
source(here::here("R", "analysis_helpers.R"))
source(here::here("R", "main_app.R"))
source(here::here("tests", "testthat", "helper_app_fixtures.R"))

test_that("main server saves and reloads a builder chart round trip", {
  abs_specs <- build_test_abs_specs()
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
      session$setInputs(
        style_title = "Mutated",
        transform_1_expression = "X",
        transform_2_moving_average = 1
      )
      session$flushReact()
    })
    expect_false(chart_states_equal(builder_state(), saved_state))

    session$setInputs(library_table_rows_selected = 1)
    suppressWarnings({
      session$setInputs(load_chart = 1)
      session$flushReact()
      session$flushReact()
    })

    expect_true(chart_states_equal(builder_state(), saved_state))
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
        library_title = "First chart"
      )
      session$flushReact()
      session$setInputs(save_chart = 1)
      session$flushReact()
    })

    suppressWarnings({
      session$setInputs(
        series_3_enabled = TRUE,
        series_3_source = "abs",
        series_3_abs_id = abs_specs[[1]]$abs_id,
        series_3_label = paste(abs_specs[[1]]$label, "copy"),
        series_3_vis_type = "line",
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
    suppressWarnings({
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
    })

    session$setInputs(reset_builder = 1)
    session$flushReact()
    session$flushReact()

    expect_true(chart_states_equal(builder_state(), default_builder_state()))
    expect_equal(nrow(chart_data()), 0)
  }))
})

test_that("main server can clear series setup, presentation, and workspace panels", {
  abs_specs <- build_test_abs_specs()

  suppressWarnings(shiny::testServer(build_main_server, {
    suppressWarnings({
      session$setInputs(
        series_1_enabled = TRUE,
        series_1_source = "abs",
        series_1_abs_id = abs_specs[[1]]$abs_id,
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

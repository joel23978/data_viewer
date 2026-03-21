source(here::here("R", "bootstrap.R"))
source(here::here("external_data.R"))
source(here::here("R", "providers.R"))
source(here::here("R", "chart_helpers.R"))
source(here::here("R", "data_search.R"))
source(here::here("R", "chart_library.R"))
source(here::here("R", "analysis_helpers.R"))
source(here::here("R", "main_app.R"))
source(here::here("tests", "testthat", "helper_app_fixtures.R"))

test_that("forecast analysis falls back to the first available series and uses shared styling", {
  abs_specs <- build_test_abs_specs()

  shiny::testServer(build_main_server, {
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
        side_panel_mode = "analysis",
        analysis_tabs = "Forecast",
        analysis_forecast_horizon = 4
      )
      session$flushReact()
    })

    expect_equal(selected_forecast_series(), unique(chart_data()$name)[[1]])
    expect_equal(nrow(forecast_result()$forecast), 4)
    expect_s3_class(selected_analysis_widget(), "plotly")
    expect_equal(selected_analysis_widget()$x$layout$paper_bgcolor, "rgba(245,248,244,1)")
  })
})

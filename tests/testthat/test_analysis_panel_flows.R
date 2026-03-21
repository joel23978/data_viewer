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
  test_state <- build_test_state()

  shiny::testServer(build_main_server, {
    suppressWarnings({
      apply_builder_state(normalize_chart_state(test_state), selected_series_index = 1, navigate_builder = FALSE)
      session$flushReact()
      session$flushReact()
      session$setInputs(
        start_date = as.Date("2024-01-01"),
        end_date = as.Date("2025-12-31"),
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

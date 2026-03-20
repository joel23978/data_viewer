# app.R

source(here::here("R", "bootstrap.R"))
data_viewer_source(here::here("cpi_annual.R"), "cpi_annual.R")
data_viewer_source(here::here("external_data.R"), "external_data.R")
data_viewer_source(here::here("R", "providers.R"), "R/providers.R")
data_viewer_source(here::here("R", "chart_helpers.R"), "R/chart_helpers.R")
data_viewer_source(here::here("R", "data_search.R"), "R/data_search.R")
data_viewer_source(here::here("R", "chart_library.R"), "R/chart_library.R")
data_viewer_source(here::here("R", "analysis_helpers.R"), "R/analysis_helpers.R")
data_viewer_source(here::here("R", "main_app.R"), "R/main_app.R")

if (!"assets" %in% names(shiny::resourcePaths())) {
  shiny::addResourcePath("assets", here::here("brand"))
}

shinyApp(ui = build_main_ui(), server = build_main_server)

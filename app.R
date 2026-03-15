# app.R

source(here::here("R", "bootstrap.R"))
source(here::here("cpi_annual.R"))
source(here::here("external_data.R"))
source(here::here("R", "chart_helpers.R"))
source(here::here("R", "chart_library.R"))
source(here::here("R", "main_app.R"))

shinyApp(ui = build_main_ui(), server = build_main_server)

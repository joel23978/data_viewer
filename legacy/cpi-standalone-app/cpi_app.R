# Legacy standalone CPI app entrypoint

source(here::here("R", "bootstrap.R"))
source(here::here("cpi_annual.R"))
source(here::here("legacy", "cpi-standalone-app", "R", "cpi_app.R"))

shinyApp(ui = build_cpi_ui(), server = build_cpi_server)

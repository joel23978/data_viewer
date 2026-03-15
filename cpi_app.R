# cpi_app.R

source(here::here("R", "bootstrap.R"))
source(here::here("cpi_annual.R"))
source(here::here("R", "cpi_app.R"))

shinyApp(ui = build_cpi_ui(), server = build_cpi_server)

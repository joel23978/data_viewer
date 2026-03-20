# smoke test for the main app object

source(here::here("R", "bootstrap.R"))
source(here::here("external_data.R"))
source(here::here("R", "providers.R"))
source(here::here("R", "chart_helpers.R"))
source(here::here("R", "data_search.R"))
source(here::here("R", "chart_library.R"))
source(here::here("R", "analysis_helpers.R"))
source(here::here("R", "main_app.R"))

app <- shinyApp(ui = build_main_ui(), server = build_main_server)
stopifnot(inherits(app, "shiny.appobj"))

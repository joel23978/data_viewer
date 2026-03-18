source(here::here("R", "bootstrap.R"))
source(here::here("cpi_annual.R"))
source(here::here("external_data.R"))
source(here::here("R", "chart_helpers.R"))
source(here::here("R", "data_search.R"))

build_started_at <- Sys.time()
message("Building local search assets...")

local_index <- build_local_search_base_index()
token_index <- build_search_token_index(local_index)
write_local_search_assets(local_index, token_index)

elapsed_seconds <- round(as.numeric(difftime(Sys.time(), build_started_at, units = "secs")), 2)

message(sprintf(
  "Wrote %s local rows and %s tokens in %ss.",
  scales::comma(nrow(local_index)),
  scales::comma(length(token_index)),
  elapsed_seconds
))

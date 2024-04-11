#read_rba integration

library(readrba)

rba_tables <- unique(browse_rba_series()$table_no)

tmp <- browse_rba_series() %>%
  select(c(table_no, description, series_id)) %>%
  unique()

rba_series <- list()

for (i in 1:length(rba_tables)){
  rba_series[[i]] <- tmp %>%
    filter(table_no == rba_tables[i]) %>%
    pull(description)
}

names(rba_series) <- rba_tables

rba_desc_id <- tmp

get_rba_data <- function(
    desc = input$rba_desc_1
    , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
    , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
    ){
  tmp <- rba_desc_id %>%
      filter(description == desc) %>%
      pull(series_id)
  
  tmp <- read_rba_seriesid(tmp) %>%
    select(c(date, value, description)) %>%
    rename(name = description) %>%
    filter(date <= end_date
           , date >= start_date) %>%
    drop_na()
  
  return(tmp)
}


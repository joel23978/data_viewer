#### fredr integration

library(fredr)
Sys.getenv("FRED_API_KEY")


fred_data <- function(
    series = input$fred_series
    , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
    , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
){
  tmp <- fredr(
    series_id = series,
    observation_start = start_date,
    observation_end = end_date
  ) %>%
    select(c(date, value, series_id)) %>%
    rename(name = series_id)
  
  return(tmp)
}





#### dbnomics integration

library(rdbnomics)

db_data <- function(
    series = input$db_series
    , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
    , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
){
  tmp <- rdb(
    ids = series
  ) %>%
    select(c(period, value, dataset_name)) %>%
    rename(name = dataset_name
           , date = period) %>%
    mutate(date = as.Date(date)) %>%
    filter(date <= end_date
           , date >= start_date) %>%
    drop_na()
  
  return(tmp)
}






#### read_rba integration

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


rba_data <- function(
    table = input$rba_table_1
    , series = input$rba_desc_1
    , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
    , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
){
  tmp <- rba_desc_id %>%
    filter(table_no == table
             , description == series) %>%
    pull(series_id)
  
  if(length(tmp) !=0){
    tmp <- read_rba_seriesid(tmp[1]) %>%
      select(c(date, value, description)) %>%
      rename(name = description) %>%
      filter(date <= end_date
             , date >= start_date) %>%
      drop_na()
    
    return(tmp)

  }
  

}


# get_rba_series <- function(
#     desc = "Treasury Bond 150 3.00% 21-Mar-2047"){
#   return(
#     rba_desc_id %>%
#       filter(description == desc) %>%
#       pull(series_id)
#   )
# }








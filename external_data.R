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

# rdb(ids = "AMECO/ZUTN/EA19.1.0.0.0.ZUTN") %>%
#   select(c(period, value, dataset_name)) %>%
#   rename(name = dataset_name
#          , date = period) %>%
#   mutate(date = as.Date(date)) %>%
#   filter(date <= end_date
#          , date >= start_date) %>%
#   drop_na()





# fredr(
#   series_id = "UNRATE",
#   observation_start = as.Date("1990-01-01"),
#   observation_end = as.Date("2000-01-01"),
#   frequency = "q", # quarterly
#   units = "chg" # change over previous value
# )
# 
# popular_funds_series <- fredr_series_search_text(
#   search_text = "federal funds",
#   order_by = "popularity",
#   sort_order = "desc",
#   limit = 1
# )
# 
# popular_funds_series_id <- popular_funds_series$id
# 
# popular_funds_series_id %>%
#   fredr(
#     observation_start = as.Date("1990-01-01"),
#     observation_end = as.Date("2000-01-01")
#   ) %>%
#   ggplot(data = ., mapping = aes(x = date, y = value, color = series_id)) +
#   geom_line() +
#   labs(x = "Observation Date", y = "Rate", color = "Series") + 
#   theme_minimal()
# 
# 


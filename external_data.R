# bloomberg  ----

library(Rblpapi)

# path_input_excel <- "//romulus/ecd_h$/Reuters/FMU/fmuRcharts/input_excel.xlsm"
# bbg_ref <- read_excel(path = path_input_excel
#            , sheet = "Queries"
#            , skip = 1)  %>%
#   select(c(9:11)) %>%
#   drop_na()
#
# save(bbg_ref, file = here("data", "bbg_ref.Rda"))

load(here("data", "bbg_ref.Rda"))

bbg_ref <- bbg_ref %>%
  arrange(Category, Security)

bbg_categories <- unique(bbg_ref$Category)

bbg_series <- list()
for (i in 1:length(bbg_categories)){
  bbg_series[[i]] <- bbg_ref %>%
    filter(Category == bbg_categories[i]) %>%
    pull(Description)
}
names(bbg_series) <- bbg_categories

bbg_tickers <- list()
for (i in 1:length(bbg_categories)){
  bbg_tickers[[i]] <- bbg_ref %>%
    filter(Category == bbg_categories[i]) %>%
    pull(Security)
}
names(bbg_tickers) <- bbg_categories

# blpConnect()

bbg_data <- function(
    series = input$bloomberg_ticker_1
    # , desc = input$bloomberg_desc_1
    , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
    , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
){
  shinyCatch(return(
    bdh(securities = series,
              fields = c("PX_LAST"),
              start.date=start_date
  ) %>%
    mutate(security = series) %>%
    select(c(3,1,2)) %>%
    `colnames<-`(c("name", "date", "value"))
  
))
}








# fredr ----

library(fredr)
Sys.getenv("FRED_API_KEY")


fred_data <- function(
    series = input$fred_series
    , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
    , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
){
  shinyCatch(return(
    fredr(
    series_id = series,
    observation_start = start_date,
    observation_end = end_date
  ) %>%
    select(c(date, value, series_id)) %>%
    rename(name = series_id)
  
  ))
}

# recession shading ----
# 
# # aus recession (extract start_rec and end_rec dates rather than boolean values.
# rec_au <- fredr(series_id = "AUSRECDM") %>%
#   select(date, value) %>%
#   filter(value != lag(value, 1)
#          | value != lead(value, 1)
#          | date == min(date)
#          | date == max(date)) %>%
#   mutate(dummy = ifelse(value != lag(value) | date == min(date), "start_rec", "end_rec")) %>%
#   filter(value == 1) %>%
#   select(date, dummy)
# 
# # us recession
# rec_us <- fredr(series_id = "USRECD") %>%
#   select(date, value) %>%
#   filter(value != lag(value, 1)
#          | value != lead(value, 1)
#          | date == min(date)
#          | date == max(date)) %>%
#   mutate(dummy = ifelse(value != lag(value) | date == min(date), "start_rec", "end_rec")) %>%
#   filter(value == 1) %>%
#   select(date, dummy)
# 
# 
# 
# rec_data <- rec_au %>%
#   filter(dummy == "start_rec") %>%
#   select(date) %>%
#   rename(start_rec = date) %>%
#   cbind(
#     rec_au %>%
#       filter(dummy == "end_rec") %>%
#       select(date) %>%
#       rename(end_rec = date)
#   ) %>%
#   mutate(region = "AU") %>%
#   rbind(
#     rec_us %>%
#       filter(dummy == "start_rec") %>%
#       select(date) %>%
#       rename(start_rec = date) %>%
#       cbind(
#         rec_us %>%
#           filter(dummy == "end_rec") %>%
#           select(date) %>%
#           rename(end_rec = date)
#       ) %>%
#       mutate(region = "US")
#   )
# 
# 
# save(rec_data, file = here("data", "rec_data.Rda"))
load(file = here("data", "rec_data.Rda"))




# dbnomics ----

library(rdbnomics)

db_data <- function(
    series = input$db_series
    , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
    , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
){
  
  shinyCatch(return(
    rdb(
    ids = series
  ) %>%
    select(c(period, value, dataset_name)) %>%
    rename(name = dataset_name
           , date = period) %>%
    mutate(date = as.Date(date)) %>%
    filter(date <= end_date
           , date >= start_date) %>%
    drop_na()
  ),
  position = "bottom-right",
  blocking_level = "error"
  )
}






# read_rba ----

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
  
  if(series != ""){
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
  

}


# get_rba_series <- function(
#     desc = "Treasury Bond 150 3.00% 21-Mar-2047"){
#   return(
#     rba_desc_id %>%
#       filter(description == desc) %>%
#       pull(series_id)
#   )
# }



# recession shading (from fredr)




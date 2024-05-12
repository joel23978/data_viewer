# bloomberg  ----

library(Rblpapi)

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

rec_series <- c("AUSRECDP", "USRECDP", "GBRRECDP", "EURORECDP", "CANRECDP", "CHNRECDP", "JPNRECDP")
rec_regions <- c("Australia", "United States", "United Kingdom", "Euro area",  "Canada", "China", "Japan")
# 
# rec_data <- data.frame(peak=as.Date(character()),
#                    trough=as.Date(character()),
#                    region=character(), 
#                    stringsAsFactors=FALSE)
# 
# # aus recession (extract peak and trough dates rather than boolean values.
# for (i in 1:length(rec_series)){
#   tmp <- fredr(series_id = rec_series[i]) %>%
#     select(date, value) %>%
#     filter(value != lag(value, 1)
#            | value != lead(value, 1)
#            | date == min(date)
#            | date == max(date)) %>%
#     mutate(dummy = ifelse(value != lag(value) | date == min(date), "peak", "trough")) %>%
#     filter(value == 1) %>%
#     select(date, dummy)
#   
#   rec_data <- rec_data %>%
#     rbind(
#       tmp %>%
#         filter(dummy == "peak") %>%
#         select(date) %>%
#         rename(peak = date) %>%
#         cbind(
#           tmp %>%
#             filter(dummy == "trough") %>%
#             select(date) %>%
#             rename(trough = date)
#         ) %>%
#         mutate(region = rec_regions[i])
#     )
# }
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
# tmp <- browse_rba_tables()
# rba_table_names <- paste(tmp$no, tmp$title)

rba_tables <- unique(browse_rba_series()$table_no)

rba_desc_id <- browse_rba_series() %>%
  select(c(table_no, description, series_id)) %>%
  unique()

rba_series <- list()

for (i in 1:length(rba_tables)){
  rba_series[[i]] <- rba_desc_id %>%
    filter(table_no == rba_tables[i]) %>%
    pull(description)
}

names(rba_series) <- rba_tables


rba_data <- function(
    series = input$rba_desc_1
){
  if(length(series) != 0){
    tmp <- rba_desc_id %>%
      filter(description %in% series) %>%
      group_by(description) %>%
      slice_head(n=1) %>%
      pull(series_id) 
    
    tmp1 <- data.frame(date=as.Date(character()),
                       value=as.numeric(character()),
                       name=character(), 
                       stringsAsFactors=FALSE)
    
    for (i in 1:length(tmp)){
      tmp1 <- tmp1 %>%
        rbind(
          read_rba_seriesid(tmp[i]) %>%
            select(c(date, value, description)) %>%
            rename(name = description) %>%
            drop_na()
        )}
    return(tmp1)
  }
}

# tmp7 <- c("Australian Government Deposits", "Australian dollar investments")
# 
# rba_data(series = tmp7)


# readabs

library(readabs)
# abs_catalogue <- read_csv(here("data", "abs_catalogue.csv")) %>%
#   filter(`Catalogue Number` %!in% c("3101.0"
#                                     , "3201.0 (Ceased)"
#                                     , "5368.0"
#                                     , "5676.0"
#                                     , "6416.0 (Ceased)"
#                                     , "7218.0.55.001 (Ceased)"
#                                     , "8782.0.65.001 (Ceased)"))
# 
# 
# abs_ref <- list()
# for (i in 1:nrow(abs_catalogue)){
#   abs_ref[[i]] <- read_abs(abs_catalogue$`Catalogue Number`[i]) %>%
#     select(-c(date, value, sheet_no)) %>%
#    # select(c(series, series_type, frequency, series_id)) %>%
#     unique()
# }
# names(abs_ref) <- abs_catalogue$`Catalogue Number` %>% head(39)
# names(abs_ref) <- paste(abs_catalogue$`Catalogue Number`, abs_catalogue$`Topic`) %>% head(39)
# 
# save(abs_ref, file = here("data", "abs_ref.Rda"))
# abs_cat <- paste(abs_catalogue$`Catalogue Number`, abs_catalogue$`Topic`) %>% head(39)
# save(abs_cat, file = here("data", "abs_cat.Rda"))


load(file = here("data", "abs_ref.Rda"))
load(file = here("data", "abs_cat.Rda"))


abs_data <- function(
  series = input$abs_id_1
){
  
  if(length(series) > 0){
    shinyCatch(
      return(
        read_abs(series_id = series) %>%
          select(date, value, series) %>%
          rename(name = series) %>%
          drop_na()
      ))
  }
}






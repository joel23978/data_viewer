## setup ----

library(tidyverse)
library(readabs)
library(here)
library(readxl)
library(janitor)
library(zoo)
library(ggplot2)
library(plotly)
library(writexl)
library(fuzzyjoin)
library(lubridate)

'%!in%' <- function(x,y)!('%in%'(x,y))
Sys.setenv(R_READABS_PATH = here("data"))

# load from disk to reduce run time
load(file = here("data", "cpi_data_all.R"))

# 
# ## data download ----
# 
# categories_cpi_excel <- read_excel(here("data", "categories_cpi_matched.xlsx")) %>%
#   select(c("class_1", "class_2", "class_3"))
# 
# ## index numbers AUS and CAPITAL CITIES
# cpi_9 <- read_abs("6401.0", tables = 9, check_local = F)  %>%
#   separate_series()
# 
# ## percent change yy AUS and CAPITAL CITIES
# cpi_10 <- read_abs("6401.0", tables = 10, check_local = F)  %>%
#   separate_series()
# 
# ## percent change qq AUS and CAPITAL CITIES
# cpi_11 <- read_abs("6401.0", tables = 11, check_local = F)  %>%
#   separate_series()
# 
# 
# ## cpi_wide
# cpi_messy <- cpi_9 %>%
#   rbind(cpi_10) %>%
#   rbind(cpi_11) %>%
#   drop_na() %>%
#   mutate(year = year(date))
# 
# 
# 
# cpi_messy$series_1[cpi_messy$series_1 == "Index Numbers"] <- "index"
# cpi_messy$series_1[cpi_messy$series_1 == "Percentage Change from Corresponding Quarter of Previous Year"] <- "y.y"
# cpi_messy$series_1[cpi_messy$series_1 == "Percentage Change from Previous Period"] <- "q.q"
# 
# 
# ## cpi weights
# file.url <- "https://www.abs.gov.au/articles/annual-weight-update-cpi-and-living-cost-indexes/Consumer%20Price%20Index%20-%202024%20Weighting%20Pattern.xlsx"
# download.file(file.url, destfile = here("data", "abs2024-weights.xlsx"))
# tmp <- read_excel(here("data", "abs2024-weights.xlsx")
#                   , sheet =  "Table 5"
#                   , skip = 6) %>%
#   na.locf(., na.rm = FALSE) %>%
#   drop_na() %>%
#   filter(.[[4]] !=100) %>%
#   clean_names() %>%
#   `colnames<-`(rep(colnames(.)[3*(1:round(ncol(.)/3))-2], each=3))
# 
# cpi_weights <- tmp[3*(1:round(ncol(tmp)/3))] %>%
#   `colnames<-`(c("class_3", colnames(tmp)[3*(2:(round(ncol(tmp)/3)))-2])) %>%
#   pivot_longer(!class_3, names_to = "year", values_to = "weight") %>%
#   mutate(year = as.numeric(str_sub(year, -4))+1) %>%
#   unique()
# 
# 
# 
# 
# ## data clean ----
# #new structure = 4 separate tables (class1, class2, class3, custom groups)
# 
# # catgories
# categories <- stringdist_join(categories_cpi_excel,
#                               cpi_9 %>% select(series_2) %>% unique() %>% rename(class_1=series_2),
#                               by = "class_1",
#                               mode = "left",
#                               ignore_case = FALSE,
#                               method = "jw",
#                               max_dist = 0.5,
#                               distance_col = "dist") %>%
#   group_by(class_1.x) %>%
#   filter(dist == min(dist)) %>%
#   ungroup() %>%
#   rename(class_1=class_1.y) %>%
#   select("class_1", "class_2", "class_3") %>%
#   unique() %>%
# 
#   stringdist_join(cpi_9 %>% select(series_2) %>% unique() %>% rename(class_2=series_2),
#                   by = "class_2",
#                   mode = "left",
#                   ignore_case = FALSE,
#                   method = "jw",
#                   max_dist = 0.5,
#                   distance_col = "dist") %>%
#   group_by(class_2.x) %>%
#   filter(dist == min(dist)) %>%
#   ungroup() %>%
#   rename(class_2=class_2.y) %>%
#   select("class_1", "class_2", "class_3") %>%
#   unique() %>%
# 
#   stringdist_join(cpi_9 %>% select(series_2) %>% unique() %>% rename(class_3=series_2),
#                   by = "class_3",
#                   mode = "left",
#                   ignore_case = FALSE,
#                   method = "jw",
#                   max_dist = 0.5,
#                   distance_col = "dist") %>%
#   group_by(class_3.x) %>%
#   filter(dist == min(dist)) %>%
#   ungroup() %>%
#   rename(class_3=class_3.y) %>%
#   select("class_1", "class_2", "class_3") %>%
#   unique()  %>%
# 
#   stringdist_join(cpi_weights,
#                   by = "class_3",
#                   mode = "left",
#                   ignore_case = FALSE,
#                   method = "jw",
#                   max_dist = 0.5,
#                   distance_col = "dist") %>%
#   group_by(class_3.x) %>%
#   filter(dist == min(dist)) %>%
#   ungroup() %>%
#   rename(class_3=class_3.x) %>%
#   select(-c("class_3.y", "dist"))  %>%
#   unique()
# 
# 
# categories_merging <- categories %>%
#   select(c(1:3)) %>%
#   `colnames<-`(c("class_1_name", "class_2_name", "class_3_name")) %>%
#   unique()
# 
# # cpi_class_1
# cpi_class_1 <- cpi_messy %>%
#   drop_na() %>%
#   rename(class_1_name = series_2
#          , region = series_3) %>%
#   filter(class_1_name %in%  unique(categories$class_1)) %>%
#   mutate(class_0 = 0
#          , class_1 = 1
#          , class_2 = 0
#          , class_3 = 0
#          , series = series_1
#          , class_2_name = class_1_name
#          , class_3_name = class_1_name) %>%
#   select(class_1_name, class_2_name, class_3_name, region, class_0, class_1, class_2, class_3, series, frequency, year,  date, value) %>%
#   unique()
# 
# 
# # cpi_class_2
# cpi_class_2 <- cpi_messy %>%
#   drop_na() %>%
#   rename(class_2_name = series_2
#          , region = series_3) %>%
#   filter(class_2_name %in%  unique(categories$class_2)) %>%
#   merge(categories_merging[1:2] %>% unique()
#         , by = "class_2_name"
#   ) %>%
#   mutate(series = series_1
#          , class_0 = 0
#          , class_1 = 0
#          , class_2 = 1
#          , class_3 = 0
#          , series = series_1
#          , class_3_name = class_2_name
#   ) %>%
#   select(class_1_name, class_2_name, class_3_name, region, class_0, class_1, class_2, class_3, series, frequency, year,  date, value) %>%
#   unique()
# 
# 
# # cpi_class_3
# cpi_class_3 <- cpi_messy %>%
#   drop_na() %>%
#   rename(class_3_name = series_2
#          , region = series_3) %>%
#   filter(class_3_name %in%  unique(categories$class_3)) %>%
#   merge(categories_merging
#         , by = "class_3_name"
#   ) %>%
#   mutate(series = series_1
#          , class_0 = 0
#          , class_1 = 0
#          , class_2 = 0
#          , class_3 = 1
#          , series = series_1
#   ) %>%
#   select(class_1_name, class_2_name, class_3_name, region, class_0, class_1, class_2, class_3, series, frequency, year,  date, value) %>%
#   unique()
# 
# # cpi_class_other
# cpi_other <- cpi_messy %>%
#   drop_na() %>%
#   filter(series_2 %!in% unique(c(categories$class_1, categories$class_2, categories$class_3))) %>%
#   rename(class_1_name = series_2
#          , region = series_3) %>%
#   mutate(series = series_1
#          , class_0 = 1
#          , class_1 = 0
#          , class_2 = 0
#          , class_3 = 0
#          , series = series_1
#          , class_2_name = class_1_name
#          , class_3_name = class_1_name
#   ) %>%
#   select(class_1_name, class_2_name, class_3_name, region, class_0, class_1, class_2, class_3, series, frequency, year,  date, value) %>%
#   unique()
# 
# 
# #all the data
# cpi_data_all <- cpi_class_1 %>%
#   rbind(cpi_class_2) %>%
#   rbind(cpi_class_3) %>%
#   rbind(cpi_other)
# 
# 
# save(cpi_data_all, file = here("data", "cpi_data_all.R"))
# 
# 
# 
# 

tmp <- sort(unique(cpi_data_all$class_1_name[cpi_data_all$class_1==1]))
name_list <- list()
for ( i in 1: length(tmp)) {
  
  name_list[[i]] <- unique(cpi_data_all$class_2_name[
    cpi_data_all$class_2==1 &
      cpi_data_all$class_1_name== tmp[i]
  ])
}
names(name_list) <- tmp





## functions ----

cpi_splits <- function(
    cpi_data = cpi_data_all
    , pick_split = "class_0" #input$splits
    , region_split = "Australia" # input$region
    , transformation = "y.y"
    , dates = c(2020, 2024) # input$year
){
  
  # Data selection
  
  if(pick_split == "class_0"){
    cpi_data <- cpi_data %>%
      # dates and region split
      filter(date > lubridate::ymd(dates[1]-1, truncated = 2L)
             , date < lubridate::ymd(dates[2]+1, truncated = 2L)
             , region == region_split
             , eval(as.symbol(pick_split)) ==1
             , series == transformation
      ) %>%
      mutate(name = class_1_name) %>%
      select(name, region, series, date, value) %>%
      drop_na() %>%
      ungroup() %>%
      filter(date > lubridate::ymd(dates[1], truncated = 2L))
    
  } else {
    cpi_data <- cpi_data %>%
      # dates and region split
      filter(date > lubridate::ymd(dates[1]-1, truncated = 2L)
             , date < lubridate::ymd(dates[2]+1, truncated = 2L)
             , region == region_split
             , eval(as.symbol(pick_split)) ==1
             , series == transformation
      ) %>%
      rename(name = eval(paste0(as.symbol(pick_split), "_name"))) %>%
      select(name, region, series, date, value) %>%
      drop_na() %>%
      ungroup() %>%
      filter(date > lubridate::ymd(dates[1], truncated = 2L))
    
  }

  return(cpi_data)
}


cpi_sub_splits <- function(
    cpi_data = cpi_data_all
    , pick_sub_split = "Alcohol and tobacco" #input$sub_split
    , region_split = "Australia" # input$region
    , transformation = "y.y"
    , dates = c(2020, 2024) # input$year
){

  # Data selection
  cpi_data <- cpi_data %>%
    # dates and region split
    filter(date > lubridate::ymd(dates[1]-1, truncated = 2L)
           , date < lubridate::ymd(dates[2]+1, truncated = 2L)
           , region == region_split
           , class_1_name == pick_sub_split
           , class_2 ==1
           , series == transformation
    ) %>%
    rename(name = class_2_name) %>%
    select(name, region, series, date, value) %>%
    drop_na() %>%
    ungroup() %>%
    filter(date > lubridate::ymd(dates[1], truncated = 2L))

  return(cpi_data)
  }


cpi_sub_sub_splits <- function(
    cpi_data = cpi_data_all
    , pick_sub_sub_split = "Garments" #input$sub_split
    , region_split = "Australia" # input$region
    , transformation = "y.y"
    , dates = c(2020, 2024) # input$year
){

    # Data selection
    cpi_data <- cpi_data %>%
      # dates and region split
      filter(date > lubridate::ymd(dates[1]-1, truncated = 2L)
             , date < lubridate::ymd(dates[2]+1, truncated = 2L)
             , region == region_split
             , class_2_name == pick_sub_sub_split
             , class_3 ==1
             , series == transformation
      ) %>%
      rename(name = class_3_name
             , parent = class_2_name) %>%
      select(parent, name, region, series, date, value) %>%
      drop_na() %>%
      ungroup() %>%
      filter(date > lubridate::ymd(dates[1], truncated = 2L))
    
    return(cpi_data)
  
}






# testing ----

## functions
# tmp0 <- cpi_splits(cpi_data = cpi_data_all
#     , pick_split = "class_1" # input$splits
#     , region_split = "Australia" # input$region
#     , dates = c(2021, 2024) # input$year
# )
# tmp0
# 
# tmp1 <- cpi_sub_splits(cpi_data = cpi_data_all
#                            , pick_sub_split = "Alcohol and tobacco" #input$sub_split
#                            , region_split = "Australia" # input$region
#                            , transformation = "y.y"
#                            , dates = c(2020, 2024) # input$year) # input$year
# )
# tmp1
# 
# tmp2 <- cpi_sub_sub_splits(cpi_data = cpi_data_all
#                        , pick_sub_sub_split = "Garments" #input$sub_split
#                        , region_split = "Australia" # input$region
#                        , transformation = "y.y"
#                        , dates = c(2020, 2024) # input$year) # input$year
# )
# tmp2



# ## plots
# 
# ggplotly(
#   tmp0 %>%
#   ggplot() +
#   geom_line(aes(x=date, y=value, colour = name)) +
#   theme_minimal() +
#   xlab("Date") +
#   ylab("Percent")
# )
# 
# 
# ggplotly(
#   tmp1 %>%
#     ggplot() +
#     geom_line(aes(x=date, y=value, colour = name)) +
#     theme_minimal() +
#     xlab("Date") +
#     ylab("Percent")
# )
# 
# 
# for (i in 1:(length(unique(tmp2$parent)))){
#   ggplotly(
#   tmp2 %>%
#     filter(parent == unique(tmp2$parent)[i]) %>%
#     ggplot() +
#     geom_line(aes(x=date, y=value, colour = name)) +
#     theme_minimal() +
#     xlab("Date") +
#     ylab("Percent")
#   )
# }
#   



## tables
# tmp0 %>%
#   pivot_wider(id_cols = c(name, region, series), names_from = date, values_from = value) %>%
#   arrange(name) %>%
#   mutate(group = 1)
# 
# tmp1 %>%
#   pivot_wider(id_cols = c(name, region, series), names_from = date, values_from = value) %>%
#   arrange(name) %>%
#   mutate(group = 2)
# 
# tmp2 %>%
#   pivot_wider(id_cols = c(name, region, series), names_from = date, values_from = value) %>%
#   arrange(name) %>%
#   mutate(group = 3)

# 
# 
# tmp1 %>%
#   select(c("name", "value", "date")) %>%
#   pivot_wider(names_from = date, values_from = value)
# 
# for (i in 1:length(unique(tmp2$parent))){
#   tmp2 %>%
#     filter(parent == unique(parent)[i]) %>%
#     select(c("name", "value", "date")) %>%
#     pivot_wider(names_from = date, values_from = value) %>%
#     print()
#   
# }
# 





















## version 1 ----
# 
# ## cpi weights 2024
# file.url <- "https://www.abs.gov.au/articles/annual-weight-update-cpi-and-living-cost-indexes/Consumer%20Price%20Index%20-%202024%20Weighting%20Pattern.xlsx"
# download.file(file.url, destfile = here("data", "abs2024-weights.xlsx"))
# tmp <- read_excel(here("data", "abs2024-weights.xlsx")
#            , sheet =  "Table 2"
#            , skip = 6) %>%
#   na.locf(., na.rm = FALSE) %>%
#   drop_na() %>%
#   filter(.[[4]] !=100) %>%
#   clean_names() %>%
#   `colnames<-`(rep(colnames(.)[3*(1:round(ncol(.)/3))-2], each=3))
# 
# cpi_weight_dec23_city <- tmp[3*(1:round(ncol(tmp)/3))] %>%
#   `colnames<-`(c("group", colnames(tmp)[3*(2:(round(ncol(tmp)/3)-1))-2], "australia")) %>%
#   pivot_longer(!group, names_to = "region", values_to = "weight_city")
#  
# tmp <- read_excel(here("data", "abs2024-weights.xlsx")
#                   , sheet =  "Table 3"
#                   , skip = 7) %>%
#   na.locf(., na.rm = FALSE) %>%
#   drop_na() %>%
#   filter(.[[4]] !=100) %>%
#   clean_names() %>%
#   `colnames<-`(rep(colnames(.)[3*(1:round(ncol(.)/3))-2], each=3)) 
# 
# cpi_weight_dec23 <- tmp[3*(1:round(ncol(tmp)/3))] %>%
#   `colnames<-`(c("group", colnames(tmp)[3*(2:(round(ncol(tmp)/3)-1))-2], "australia")) %>%
#   pivot_longer(!group, names_to = "region", values_to = "weight_cityxtotal") %>%
#   merge(cpi_weight_dec23_city
#         , by = c("group", "region")) %>%
#   mutate(weight_cityxtotal = weight_cityxtotal/100
#         , weight_city = weight_city/100) %>%
#   unique()
# 
# categories_cpi <- stringdist_join(categories_cpi_excel,
#                                   cpi_9 %>% select(series_2) %>% unique() %>% rename(class_3=series_2), 
#                 by = "class_3",
#                 mode = "left",
#                 ignore_case = FALSE, 
#                 method = "jw", 
#                 max_dist = 0.5, 
#                 distance_col = "dist") %>%
#   group_by(class_3.x) %>%
#   filter(dist == min(dist)) %>%
#   ungroup() %>%
#   rename(class_3=class_3.y) %>%
#   select("class_1", "class_2", "class_3") %>%
#   stringdist_join(cpi_weight_dec23 %>% rename(class_3 = group), 
#                   by = "class_3",
#                   mode = "left",
#                   ignore_case = FALSE, 
#                   method = "jw", 
#                   max_dist = 0.5, 
#                   distance_col = "dist") %>%
#   group_by(class_3.x) %>%
#   filter(dist == min(dist)) %>%
#   ungroup() %>%
#   rename(class_3=class_3.x) %>%
#   select(-c("class_3.y", "dist")) %>%
#   stringdist_join(cpi_9 %>% select(series_3) %>% unique() %>% rename(region=series_3), 
#                   by = "region",
#                   mode = "left",
#                   ignore_case = FALSE, 
#                   method = "jw", 
#                   max_dist = 0.5, 
#                   distance_col = "dist") %>%
#   group_by(region.y) %>%
#   filter(dist == min(dist)) %>%
#   ungroup() %>%
#   rename(region=region.y) %>%
#   select(-c("region.x", "dist")) 
# 
# 
# cpi_long <- cpi_9 %>%
#   drop_na() %>% 
#   filter(series_1 == "Index Numbers") %>%
#   rename(class_3 = series_2
#          , region = series_3) %>%
#   merge(categories_cpi
#         , by = c("class_3", "region")) %>%
#   unique() %>%
#   select(class_1, class_2, class_3, region, weight_city, weight_cityxtotal, series_type, frequency, date, value)
# 
# 
# 
# 
# 
# cpi_splits <- function(
#     cpi_data = cpi_long
#     , pick_split = "class_1" # input$splits
#     , region_split = "Australia" # input$region
#     , pick_sub_split = "Alcohol and tobacco" # input$sub_split
#     , dates = c(2004, 2024) # input$year
# ){
#   
#   # Data selection
#   
#   if(pick_sub_split == "All"){
#     cpi_data <- cpi_data %>%
#       # dates and region split
#       filter(date > lubridate::ymd(dates[1]-1, truncated = 2L)
#              , date < lubridate::ymd(dates[2]+1, truncated = 2L)
#              , region == region_split
#       ) %>%
#       drop_na() %>%
#       ungroup()
#       #rename(pick_split1 = as.symbol(pick_split))
#     
#     
#   } else {
#     cpi_data <- cpi_data %>%
#       # dates and region split
#       filter(date > lubridate::ymd(dates[1]-1, truncated = 2L)
#              , date < lubridate::ymd(dates[2]+1, truncated = 2L)
#              , region == region_split
#              , class_1 == pick_sub_split
#              
#       ) %>%
#       drop_na() %>%
#       ungroup()
#       #rename(pick_split1 = class_2) 
#     
#   }
#   
#   
#   return(cpi_data)
# }
# 
# 
# 
# 
# 
# 
# 
# cpi_transformation <- function( # index = 0, percent =2, contribution=2
#     cpi_data = cpi_data
#     , pick_split = "class_1" # input$splits
#     , transformation = 0
#     , period_change = 12
#     , annualise = F
#     , dates = c(2004, 2024) # input$year
#     
# ){
# 
#   dates_reverse <- rev(sort(unique(cpi_data$date)))
#   obs_per_yr <- round(1/(as.numeric(difftime(dates_reverse[1], dates_reverse[2]))/365))
#   obs_freq_mo <- 12/obs_per_yr
#   obs_lag <- as.numeric(period_change)/obs_freq_mo
#   
#   
#   # group by split
#   cpi_data <- cpi_data %>%
#     group_by(class_1, class_2, class_3) %>%
#     arrange(date) %>%
#     # apply weights
#     mutate(change = (value - lag(value, obs_lag))
#               , change_pct = (100*value/lag(value, obs_lag)-100)
#           ) %>%
#     drop_na() %>%
#     ungroup() %>%
#     rename(pick_split1 = as.symbol(pick_split)) %>%
#     group_by(pick_split1, date) %>%
#     summarise(change_pct = sum(change_pct*weight_city)/sum(weight_city)
#               , change_pct_cont = sum(change_pct*weight_city)
#               ) %>%
#     ungroup() %>%
#     group_by(date) %>%
#     mutate(change_cont = change_pct_cont/sum(change_pct_cont, na.rm = T)) %>%
#     ungroup() 
#   
#   
#   if (annualise == T){
#     cpi_data <- cpi_data %>%
#       mutate(change_pct = (1+change_pct)^(12/period_change)-1
#              , change_pct_cont = (1+change_pct_cont)^(12/period_change)-1
#              , change_cont = (1+change_cont)^(12/period_change)-1
#              ) 
#   }
#   
#   return(cpi_data %>%
#            filter(date >= lubridate::ymd(dates[1], truncated = 2L)) %>%
#            drop_na())
# }
# 

# # version 1 testing ----
# 
# tmp0 <- cpi_splits(cpi_data = cpi_long
#     , pick_split = "class_1" # input$splits
#     , region_split = "Australia" # input$region
#     , pick_sub_split = "All" # input$sub_split
#     , dates = c(2021, 2024) # input$year
# )
# tmp0
# 
# tmp1 <- cpi_transformation( # index = 0, percent =2, contribution=2
#   cpi_data = tmp0
#   , transformation = 1
#   , period_change = 12
#   , annualise = F
#   , dates = c(2021, 2024) # input$yea
# )
# tmp1
# 
# 
# ggplotly(
#   tmp1 %>%
#   ggplot() +
#   geom_col(aes(x=date, y=change_pct_cont, fill = pick_split1)) +
#   geom_line(data = tmp1 %>%
#               group_by(date) %>%
#               summarise(change_pct_cont = sum(change_pct_cont))
#             , aes(x=date, y=change_pct_cont)) +
#   theme_minimal() +
#   xlab("Date") +
#   ylab("reactive")
# )
# 
# 
# tmp1 %>%
#   select(c("date", "change_pct", "pick_split1")) %>%
#   pivot_wider(names_from = date, values_from = change_pct)
# 
# 
# tmp1 %>%
#   select(c("date", "change_pct_cont", "pick_split1")) %>%
#   pivot_wider(names_from = date, values_from = change_pct_cont) %>%
#   adorn_totals("row")
# 
# 












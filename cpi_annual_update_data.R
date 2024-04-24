Sys.setenv(R_READABS_PATH = here("data_abs"))

## data download ----

categories_cpi_excel <- read_excel(here("data", "categories_cpi_matched.xlsx")) %>%
  select(c("class_1", "class_2", "class_3"))

## index numbers AUS and CAPITAL CITIES
cpi_9 <- read_abs("6401.0", tables = 9, check_local = F)  %>%
  separate_series()

## percent change yy AUS and CAPITAL CITIES
cpi_10 <- read_abs("6401.0", tables = 10, check_local = F)  %>%
  separate_series()

## percent change qq AUS and CAPITAL CITIES
cpi_11 <- read_abs("6401.0", tables = 11, check_local = F)  %>%
  separate_series()


## cpi_wide
cpi_messy <- cpi_9 %>%
  rbind(cpi_10) %>%
  rbind(cpi_11) %>%
  drop_na() %>%
  mutate(year = year(date))



cpi_messy$series_1[cpi_messy$series_1 == "Index Numbers"] <- "index"
cpi_messy$series_1[cpi_messy$series_1 == "Percentage Change from Corresponding Quarter of Previous Year"] <- "y.y"
cpi_messy$series_1[cpi_messy$series_1 == "Percentage Change from Previous Period"] <- "q.q"


## cpi weights
file.url <- "https://www.abs.gov.au/articles/annual-weight-update-cpi-and-living-cost-indexes/Consumer%20Price%20Index%20-%202024%20Weighting%20Pattern.xlsx"
download.file(file.url, destfile = here("data", "abs2024-weights.xlsx"))
tmp <- read_excel(here("data", "abs2024-weights.xlsx")
                  , sheet =  "Table 5"
                  , skip = 6) %>%
  na.locf(., na.rm = FALSE) %>%
  drop_na() %>%
  filter(.[[4]] !=100) %>%
  clean_names() %>%
  `colnames<-`(rep(colnames(.)[3*(1:round(ncol(.)/3))-2], each=3))

cpi_weights <- tmp[3*(1:round(ncol(tmp)/3))] %>%
  `colnames<-`(c("class_3", colnames(tmp)[3*(2:(round(ncol(tmp)/3)))-2])) %>%
  pivot_longer(!class_3, names_to = "year", values_to = "weight") %>%
  mutate(year = as.numeric(str_sub(year, -4))+1) %>%
  unique()




## data clean ----
#new structure = 4 separate tables (class1, class2, class3, custom groups)

# catgories
categories <- stringdist_join(categories_cpi_excel,
                              cpi_9 %>% select(series_2) %>% unique() %>% rename(class_1=series_2),
                              by = "class_1",
                              mode = "left",
                              ignore_case = FALSE,
                              method = "jw",
                              max_dist = 0.5,
                              distance_col = "dist") %>%
  group_by(class_1.x) %>%
  filter(dist == min(dist)) %>%
  ungroup() %>%
  rename(class_1=class_1.y) %>%
  select("class_1", "class_2", "class_3") %>%
  unique() %>%

  stringdist_join(cpi_9 %>% select(series_2) %>% unique() %>% rename(class_2=series_2),
                  by = "class_2",
                  mode = "left",
                  ignore_case = FALSE,
                  method = "jw",
                  max_dist = 0.5,
                  distance_col = "dist") %>%
  group_by(class_2.x) %>%
  filter(dist == min(dist)) %>%
  ungroup() %>%
  rename(class_2=class_2.y) %>%
  select("class_1", "class_2", "class_3") %>%
  unique() %>%

  stringdist_join(cpi_9 %>% select(series_2) %>% unique() %>% rename(class_3=series_2),
                  by = "class_3",
                  mode = "left",
                  ignore_case = FALSE,
                  method = "jw",
                  max_dist = 0.5,
                  distance_col = "dist") %>%
  group_by(class_3.x) %>%
  filter(dist == min(dist)) %>%
  ungroup() %>%
  rename(class_3=class_3.y) %>%
  select("class_1", "class_2", "class_3") %>%
  unique()  %>%

  stringdist_join(cpi_weights,
                  by = "class_3",
                  mode = "left",
                  ignore_case = FALSE,
                  method = "jw",
                  max_dist = 0.5,
                  distance_col = "dist") %>%
  group_by(class_3.x) %>%
  filter(dist == min(dist)) %>%
  ungroup() %>%
  rename(class_3=class_3.x) %>%
  select(-c("class_3.y", "dist"))  %>%
  unique()


categories_merging <- categories %>%
  select(c(1:3)) %>%
  `colnames<-`(c("class_1_name", "class_2_name", "class_3_name")) %>%
  unique()

# cpi_class_1
cpi_class_1 <- cpi_messy %>%
  drop_na() %>%
  rename(class_1_name = series_2
         , region = series_3) %>%
  filter(class_1_name %in%  unique(categories$class_1)) %>%
  mutate(class_0 = 0
         , class_1 = 1
         , class_2 = 0
         , class_3 = 0
         , series = series_1
         , class_2_name = class_1_name
         , class_3_name = class_1_name) %>%
  select(class_1_name, class_2_name, class_3_name, region, class_0, class_1, class_2, class_3, series, frequency, year,  date, value) %>%
  unique()


# cpi_class_2
cpi_class_2 <- cpi_messy %>%
  drop_na() %>%
  rename(class_2_name = series_2
         , region = series_3) %>%
  filter(class_2_name %in%  unique(categories$class_2)) %>%
  merge(categories_merging[1:2] %>% unique()
        , by = "class_2_name"
  ) %>%
  mutate(series = series_1
         , class_0 = 0
         , class_1 = 0
         , class_2 = 1
         , class_3 = 0
         , series = series_1
         , class_3_name = class_2_name
  ) %>%
  select(class_1_name, class_2_name, class_3_name, region, class_0, class_1, class_2, class_3, series, frequency, year,  date, value) %>%
  unique()


# cpi_class_3
cpi_class_3 <- cpi_messy %>%
  drop_na() %>%
  rename(class_3_name = series_2
         , region = series_3) %>%
  filter(class_3_name %in%  unique(categories$class_3)) %>%
  merge(categories_merging
        , by = "class_3_name"
  ) %>%
  mutate(series = series_1
         , class_0 = 0
         , class_1 = 0
         , class_2 = 0
         , class_3 = 1
         , series = series_1
  ) %>%
  select(class_1_name, class_2_name, class_3_name, region, class_0, class_1, class_2, class_3, series, frequency, year,  date, value) %>%
  unique()

# cpi_class_other
cpi_other <- cpi_messy %>%
  drop_na() %>%
  filter(series_2 %!in% unique(c(categories$class_1, categories$class_2, categories$class_3))) %>%
  rename(class_1_name = series_2
         , region = series_3) %>%
  mutate(series = series_1
         , class_0 = 1
         , class_1 = 0
         , class_2 = 0
         , class_3 = 0
         , series = series_1
         , class_2_name = class_1_name
         , class_3_name = class_1_name
  ) %>%
  select(class_1_name, class_2_name, class_3_name, region, class_0, class_1, class_2, class_3, series, frequency, year,  date, value) %>%
  unique()


#all the data
cpi_data_all <- cpi_class_1 %>%
  rbind(cpi_class_2) %>%
  rbind(cpi_class_3) %>%
  rbind(cpi_other)


save(cpi_data_all, file = here("data", "cpi_data_all.Rda"))




library(tidyverse)
library(readabs)
library(here)
library(readxl)
library(janitor)
library(zoo)
library(ggplot2)
library(plotly)
library(writexl)

Sys.setenv(R_READABS_PATH = here("data"))


coverage <- read_excel(here("data", "appendix1_coverage.xlsx")
                         , skip = 1) %>%
  clean_names() %>%
  mutate(class_1 = ifelse(is.na(weight)==T, class, NA)
         , class_1 = na.locf(class_1)
         , weight = as.numeric(gsub("%", "", weight))/100
         , month_1 = ifelse(month_1 == "y", 1, 0)
         , month_2 = ifelse(month_2 == "y", 1, 0)
         , month_3 = ifelse(month_3 == "y", 1, 0)
  ) %>%
  rename(class_2 = class) %>%
  drop_na(weight) 

categories_cpi <- coverage %>%
  select(c("class_1", "class_2", "weight"))


all_cpi_monthly <- read_abs('6484.0') %>% 
  separate_series()


categories_cpi_monthly <- all_cpi_monthly %>%
  select(series_2) %>%
  unique()

write_xlsx(categories_cpi, path = here("data", "categories_cpi.xlsx"))
write_xlsx(categories_cpi_monthly, path = here("data", "categories_cpi_monthly.xlsx"))

# category matching remains a fucking mess
categories_matched <- read_excel(here("data", "categories_cpi_matched.xlsx"))



cpi_monthly_long <- all_cpi_monthly %>%
  filter(series_1 == "Index Numbers") %>%
  select(c("date", "value", "series_2")) %>%
  # merge(categories_matched
  #   , by.x = "series_2"
  #   , by.y = "class_2a"
  #   , all.x = T) %>%
  rename(class_2 = series_2) %>%
  group_by(class_2) %>%
  arrange(date) %>%
  mutate(
    `chg_1mo` = value - lag(value),
    `chg_3mo` = value - lag(value, 3),
    `chg_6mo` = value - lag(value, 6),
    `chg_12mo` = value - lag(value, 12),
    `chg_pct_1mo` = 100*value/lag(value)-100,
    `chg_pct_3mo` = 100*value/lag(value, 3)-100,
    `chg_pct_6mo` = 100*value/lag(value, 6)-100,
    `chg_pct_12mo` = 100*value/lag(value, 12)-100,
    `ctg_1mo` = chg_1mo/sum(chg_1mo, na.rm=T)*weight*100,
    `ctg_3mo` = chg_3mo/sum(chg_3mo, na.rm=T)*weight*100,
    `ctg_6mo` = chg_6mo/sum(chg_6mo, na.rm=T)*weight*100,
    `ctg_12mo` = chg_12mo/sum(chg_12mo, na.rm=T)*weight*100,
  ) %>%
  ungroup() %>%
  rename(index = value) %>%
  pivot_longer(!c(class_1, class_2, #weight, 
                  date)) %>%
  drop_na()




p <- cpi_monthly_long %>% 
  filter(name == "chg_12mo") %>%
  ggplot(aes(x = date, y = value, fill = class_2)) +
  geom_col() +
  theme_minimal()

p
ggplotly(p)




unique(cpi_monthly_long$class_1)
unique(cpi_monthly_long$class_2)














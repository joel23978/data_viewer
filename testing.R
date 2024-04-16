# #read_rba integration
# 
# 
# library(readAOFM)
# read_aofm("aggregate", "dealt")


# library(readabs)
# abs_catalogue <- read_csv(here("data", "abs_catalogue.csv")) %>%
#   filter(`Catalogue Number` %!in% c("3101.0"
#                                     , "3201.0 (Ceased)"
#                                     , "5368.0"
#                                     , "5676.0"
#                                     , "6416.0 (Ceased)"
#                                     , "7218.0.55.001 (Ceased)"
#                                     , "8782.0.65.001 (Ceased)"))
# 
# abs_ref <- list()
# for (i in 1:nrow(abs_catalogue)){
#   abs_ref[[i]] <- read_abs(abs_catalogue$`Catalogue Number`[i]) %>%
#     select(c(series, series_type, frequency, series_id)) %>%
#     unique()
# }
# names(abs_ref) <- abs_catalogue$`Catalogue Number` %>% head(39)
# names(abs_ref) <- paste(abs_catalogue$`Catalogue Number`, abs_catalogue$`Topic`) %>% head(39)
# 
# save(abs_ref, file = here("data", "abs_ref.Rda"))
# abs_cat <- paste(abs_catalogue$`Catalogue Number`, abs_catalogue$`Topic`) %>% head(39)
# save(abs_cat, file = here("data", "abs_cat.Rda"))

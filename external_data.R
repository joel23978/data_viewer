# fredr ----

Sys.getenv("FRED_API_KEY")

fred_vintage_cache_env <- new.env(parent = emptyenv())

fred_vintage_dates <- function(series, force = FALSE) {
  cleaned_series <- trimws(if (is.null(series) || length(series) == 0) "" else as.character(series)[1])
  if (!nzchar(cleaned_series)) {
    return(as.Date(character()))
  }

  if (!requireNamespace("fredr", quietly = TRUE)) {
    return(as.Date(character()))
  }

  cache_key <- cleaned_series
  if (!force && exists(cache_key, envir = fred_vintage_cache_env, inherits = FALSE)) {
    return(get(cache_key, envir = fred_vintage_cache_env, inherits = FALSE))
  }

  vintage_dates <- tryCatch(
    {
      response <- fredr::fredr_series_vintagedates(series_id = cleaned_series)
      matching_columns <- intersect(c("vintage_date", "date"), names(response))
      date_column <- if (length(matching_columns) > 0) matching_columns[1] else names(response)[1]
      as.Date(sort(unique(response[[date_column]])))
    },
    error = function(error) {
      as.Date(character())
    }
  )

  assign(cache_key, vintage_dates, envir = fred_vintage_cache_env)
  vintage_dates
}

fred_data <- function(
    series = NULL
    , start_date = NULL
    , end_date = NULL
    , realtime_start = NULL
    , realtime_end = NULL
    , vintage_dates = NULL
    , name_override = NULL
){
  if (!data_viewer_has_text(series)) {
    return(data_viewer_empty_series())
  }

  if (!requireNamespace("fredr", quietly = TRUE)) {
    return(data_viewer_empty_series())
  }

  query_args <- list(series_id = series)
  if (!is.null(start_date)) {
    query_args$observation_start <- start_date
  }
  if (!is.null(end_date)) {
    query_args$observation_end <- end_date
  }
  if (!is.null(realtime_start)) {
    query_args$realtime_start <- realtime_start
  }
  if (!is.null(realtime_end)) {
    query_args$realtime_end <- realtime_end
  }
  if (!is.null(vintage_dates)) {
    query_args$vintage_dates <- vintage_dates
  }

  data_viewer_safe_fetch(
    "FRED",
    {
      fred_result <- do.call(fredr::fredr_series_observations, query_args) %>%
        select(c(date, value, series_id)) %>%
        rename(name = series_id)

      if (!is.null(name_override) && nzchar(trimws(name_override))) {
        fred_result <- fred_result %>%
          mutate(name = trimws(name_override))
      }

      fred_result
    }
  )
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




# dbnomics ----

db_data <- function(
    series = NULL
    , start_date = NULL
    , end_date = NULL
){
  if (!data_viewer_has_text(series)) {
    return(data_viewer_empty_series())
  }

  if (!requireNamespace("rdbnomics", quietly = TRUE)) {
    return(data_viewer_empty_series())
  }

  data_viewer_safe_fetch(
    "DBnomics",
    rdbnomics::rdb(
      ids = series
    ) %>%
      select(c(period, value, dataset_name)) %>%
      rename(name = dataset_name
             , date = period) %>%
      mutate(date = as.Date(date)) %>%
      {
        if (!is.null(start_date)) filter(., date >= start_date) else .
      } %>%
      {
        if (!is.null(end_date)) filter(., date <= end_date) else .
      } %>%
      drop_na()
  )
}






# read_rba ----

# tmp <- browse_rba_tables()
# rba_table_names <- paste(tmp$no, tmp$title)

get_rba_browse_data <- function() {
  data_viewer_cache_get(
    "rba_browse_data",
    function() {
      if (!requireNamespace("readrba", quietly = TRUE)) {
        return(tibble::tibble(
          table_no = character(),
          table_title = character(),
          description = character(),
          series_id = character(),
          frequency = character()
        ))
      }

      if (exists("browse_rba_series", envir = .GlobalEnv, inherits = FALSE) &&
          is.function(get("browse_rba_series", envir = .GlobalEnv, inherits = FALSE))) {
        get("browse_rba_series", envir = .GlobalEnv, inherits = FALSE)()
      } else {
        readrba::browse_rba_series()
      }
    },
    "Load RBA browse metadata"
  )
}

get_rba_table_meta <- function() {
  data_viewer_cache_get(
    "rba_table_meta",
    function() {
      get_rba_browse_data() %>%
        select(table_no, table_title) %>%
        distinct() %>%
        group_by(table_no) %>%
        summarise(table_title = table_title[1], .groups = "drop") %>%
        arrange(table_no)
    },
    "Build RBA table metadata"
  )
}

get_rba_tables <- function() {
  data_viewer_cache_get(
    "rba_tables",
    function() get_rba_table_meta()$table_no,
    "Build RBA table list"
  )
}

get_rba_table_choices <- function() {
  data_viewer_cache_get(
    "rba_table_choices",
    function() {
      table_meta <- get_rba_table_meta()
      stats::setNames(
        table_meta$table_no,
        mapply(compact_rba_table_title, table_meta$table_no, table_meta$table_title, USE.NAMES = FALSE)
      )
    },
    "Build RBA table choices"
  )
}

get_rba_desc_id <- function() {
  data_viewer_cache_get(
    "rba_desc_id",
    function() {
      get_rba_browse_data() %>%
        select(c(table_no, description, series_id)) %>%
        unique()
    },
    "Build RBA description lookup"
  )
}

get_rba_series <- function() {
  data_viewer_cache_get(
    "rba_series",
    function() {
      desc_id <- get_rba_desc_id()
      tables <- get_rba_tables()
      series_list <- vector("list", length(tables))

      for (i in seq_along(tables)) {
        series_list[[i]] <- desc_id %>%
          filter(table_no == tables[[i]]) %>%
          pull(description)
      }

      names(series_list) <- tables
      series_list
    },
    "Build RBA series lookup"
  )
}

data_viewer_register_active_binding("rba_browse_data", function() get_rba_browse_data())
data_viewer_register_active_binding("rba_table_meta", function() get_rba_table_meta())
data_viewer_register_active_binding("rba_tables", function() get_rba_tables())
data_viewer_register_active_binding("rba_table_choices", function() get_rba_table_choices())
data_viewer_register_active_binding("rba_desc_id", function() get_rba_desc_id())
data_viewer_register_active_binding("rba_series", function() get_rba_series())

get_rec_data <- function() {
  data_viewer_cache_get(
    "rec_data",
    function() data_viewer_load_rda(here("data", "rec_data.Rda"), "rec_data"),
    "Load recession data"
  )
}

data_viewer_register_active_binding("rec_data", function() get_rec_data())

compact_rba_table_title <- function(table_no, table_title) {
  short_title <- table_title %>%
    stringr::str_remove(paste0("^", stringr::fixed(table_no), "\\s*")) %>%
    stringr::str_replace("^Reserve Bank Of Australia\\s*-\\s*", "RBA ") %>%
    stringr::str_replace("\\s*-\\s*.*$", "") %>%
    stringr::str_squish()

  paste(table_no, short_title)
}

rba_data <- function(
    series = NULL
){
  if (!data_viewer_has_text(series)) {
    return(data_viewer_empty_series())
  }

  if (!requireNamespace("readrba", quietly = TRUE)) {
    return(data_viewer_empty_series())
  }

  data_viewer_safe_fetch(
    "RBA",
    {
      tmp <- get_rba_desc_id() %>%
        filter(description %in% series) %>%
        group_by(description) %>%
        slice_head(n = 1) %>%
        pull(series_id)

      tmp1 <- data.frame(
        date = as.Date(character()),
        value = as.numeric(character()),
        name = character(),
        stringsAsFactors = FALSE
      )

      for (i in seq_along(tmp)) {
        tmp1 <- tmp1 %>%
          rbind(
            readrba::read_rba_seriesid(tmp[[i]]) %>%
              select(c(date, value, description)) %>%
              rename(name = description) %>%
              drop_na()
          )
      }

      tmp1
    }
  )
}

# tmp7 <- c("Australian Government Deposits", "Australian dollar investments")
# 
# rba_data(series = tmp7)


# readabs

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


get_abs_ref <- function() {
  data_viewer_cache_get(
    "abs_ref",
    function() data_viewer_load_rda(here("data", "abs_ref.Rda"), "abs_ref"),
    "Load ABS reference data"
  )
}

get_abs_cat <- function() {
  data_viewer_cache_get(
    "abs_cat",
    function() data_viewer_load_rda(here("data", "abs_cat.Rda"), "abs_cat"),
    "Load ABS catalogue labels"
  )
}

data_viewer_register_active_binding("abs_ref", function() get_abs_ref())
data_viewer_register_active_binding("abs_cat", function() get_abs_cat())


abs_data <- function(
  series = NULL
){
  if (!data_viewer_has_text(series)) {
    return(data_viewer_empty_series())
  }

  if (!requireNamespace("readabs", quietly = TRUE)) {
    return(data_viewer_empty_series())
  }

  data_viewer_safe_fetch(
    "ABS",
    readabs::read_abs(series_id = series) %>%
      select(date, value, series) %>%
      rename(name = series) %>%
      drop_na()
  )
}

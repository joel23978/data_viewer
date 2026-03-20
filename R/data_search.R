empty_search_index <- function() {
  tibble::tibble(
    local_row_id = integer(),
    search_id = character(),
    title = character(),
    source = character(),
    type_code = character(),
    location_code = character(),
    frequency = character(),
    start_date = as.Date(character()),
    end_date = as.Date(character()),
    summary = character(),
    search_text = character(),
    load_payload = list()
  )
}

empty_recent_search_index <- function() {
  empty_search_index()
}

if (!exists("provider_registry_entry", mode = "function")) {
  source(here::here("R", "providers.R"))
}

data_search_env <- new.env(parent = emptyenv())

invalidate_search_index_cache <- function() {
  if (exists("index", envir = data_search_env, inherits = FALSE)) {
    rm("index", envir = data_search_env)
  }

  invisible(NULL)
}

invalidate_local_search_asset_cache <- function() {
  for (cache_name in c("local_index", "token_index")) {
    if (exists(cache_name, envir = data_search_env, inherits = FALSE)) {
      rm(list = cache_name, envir = data_search_env)
    }
  }

  invisible(NULL)
}

SEARCH_TYPE_CODES <- c(
  "All" = "all",
  "Econ" = "ECON",
  "Fin" = "FIN",
  "Other" = "OTHER"
)

SEARCH_LOCATION_CODES <- c(
  "All" = "all",
  "Intl" = "INTL",
  "Australia" = "AUS",
  "State" = "STATE",
  "Other" = "OTHER"
)

FRED_SEARCH_FREQUENCIES <- c(
  "Annual",
  "Semiannual",
  "Quarterly",
  "Monthly",
  "Biweekly",
  "Weekly",
  "Daily"
)

LOCAL_SEARCH_INDEX_PATH <- here::here("data", "local_search_index.rds")
LOCAL_SEARCH_TOKEN_INDEX_PATH <- here::here("data", "local_search_token_index.rds")

clean_search_text <- function(text) {
  text %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", " ") %>%
    stringr::str_squish()
}

search_tokens <- function(query) {
  cleaned_query <- clean_search_text(query %||% "")

  if (!nzchar(cleaned_query)) {
    return(character())
  }

  strsplit(cleaned_query, "\\s+")[[1]]
}

parse_boolean_query <- function(query) {
  query_text <- trimws(query %||% "")

  if (!nzchar(query_text)) {
    return(list())
  }

  raw_tokens <- strsplit(query_text, "\\s+")[[1]]
  clauses <- list()
  current_clause <- character()

  for (token in raw_tokens) {
    operator_token <- toupper(token)

    if (operator_token == "OR") {
      if (length(current_clause) > 0) {
        clauses[[length(clauses) + 1]] <- unique(current_clause)
        current_clause <- character()
      }
      next
    }

    if (operator_token == "AND") {
      next
    }

    cleaned_token <- clean_search_text(token)

    if (nzchar(cleaned_token)) {
      current_clause <- c(current_clause, cleaned_token)
    }
  }

  if (length(current_clause) > 0) {
    clauses[[length(clauses) + 1]] <- unique(current_clause)
  }

  clauses
}

boolean_query_terms <- function(query) {
  unique(unlist(parse_boolean_query(query), use.names = FALSE))
}

normalized_query_text <- function(query) {
  paste(boolean_query_terms(query), collapse = " ")
}

search_text_tokens <- function(text) {
  unique(search_tokens(text))
}

boolean_query_matches <- function(search_text, query) {
  clauses <- parse_boolean_query(query)

  if (length(clauses) == 0) {
    return(TRUE)
  }

  any(vapply(
    clauses,
    function(clause_terms) {
      all(vapply(
        clause_terms,
        function(term) stringr::str_detect(search_text %||% "", stringr::fixed(term)),
        logical(1)
      ))
    },
    logical(1)
  ))
}

build_search_token_index <- function(search_index) {
  token_env <- new.env(parent = emptyenv(), hash = TRUE)

  for (row_index in seq_len(nrow(search_index))) {
    local_row_id <- search_index$local_row_id[[row_index]] %||% NA_integer_

    if (!is.finite(local_row_id)) {
      next
    }

    row_tokens <- search_text_tokens(search_index$search_text[[row_index]])

    if (length(row_tokens) == 0) {
      next
    }

    for (token in row_tokens) {
      existing_ids <- token_env[[token]]

      if (is.null(existing_ids)) {
        token_env[[token]] <- local_row_id
      } else {
        token_env[[token]] <- c(existing_ids, local_row_id)
      }
    }
  }

  token_index <- as.list(token_env, all.names = TRUE)
  lapply(token_index, function(row_ids) sort(unique(as.integer(row_ids))))
}

boolean_query_candidate_ids <- function(query, token_index) {
  clauses <- parse_boolean_query(query)

  if (length(clauses) == 0 || length(token_index %||% list()) == 0) {
    return(integer())
  }

  clause_matches <- lapply(clauses, function(clause_terms) {
    postings <- lapply(
      clause_terms,
      function(term) token_index[[term]] %||% integer()
    )

    if (length(postings) == 0 || any(lengths(postings) == 0)) {
      return(integer())
    }

    Reduce(intersect, postings)
  })

  matched_ids <- Reduce(union, clause_matches, init = integer())
  sort(unique(as.integer(matched_ids)))
}

remote_search_query <- function(query) {
  normalized_query <- normalized_query_text(query)

  if (nzchar(normalized_query)) {
    return(normalized_query)
  }

  trimws(query %||% "")
}

format_frequency <- function(value) {
  if (is.null(value) || length(value) == 0 || is.na(value) || !nzchar(value)) {
    return("Unknown")
  }

  stringr::str_to_title(value)
}

type_code_label <- function(type_code) {
  dplyr::case_when(
    type_code == "ECON" ~ "Economic",
    type_code == "FIN" ~ "Financial",
    TRUE ~ "Other"
  )
}

location_code_label <- function(location_code) {
  dplyr::case_when(
    location_code == "INTL" ~ "International",
    location_code == "AUS" ~ "Australia",
    location_code == "STATE" ~ "States / Cities",
    TRUE ~ "Other"
  )
}

recent_series_title <- function(spec) {
  if (nzchar(trimws(spec$label %||% ""))) {
    return(trimws(spec$label))
  }

  provider_title <- provider_registry_recent_title(spec)
  if (nzchar(provider_title)) {
    return(provider_title)
  }

  trimws(spec$source %||% "Saved series")
}

recent_series_frequency <- function(spec) {
  "Unknown"
}

classify_location_code <- function(title = "", summary = "", source = "") {
  combined_text <- clean_search_text(paste(title %||% "", summary %||% "", source %||% ""))

  state_patterns <- c(
    "new south wales", "nsw", "victoria", "queensland", "south australia",
    "western australia", "tasmania", "northern territory", "australian capital territory",
    "act", "sydney", "melbourne", "brisbane", "perth", "adelaide", "hobart", "darwin", "canberra"
  )
  australia_patterns <- c(
    "australia", "australian", "commonwealth", "national accounts"
  )
  international_patterns <- c(
    "united states", "us ", " u s ", "america", "federal reserve", "euro area", "europe",
    "united kingdom", "uk ", "canada", "china", "japan", "world", "international",
    "imf", "oecd", "ecb", "fred", "dbnomics"
  )

  if (any(stringr::str_detect(combined_text, stringr::fixed(state_patterns)))) {
    return("STATE")
  }

  if (any(stringr::str_detect(combined_text, stringr::fixed(australia_patterns)))) {
    return("AUS")
  }

  if (any(stringr::str_detect(combined_text, stringr::fixed(international_patterns)))) {
    return("INTL")
  }

  if (identical(source, "ABS") || identical(source, "RBA")) {
    return("AUS")
  }

  if (source %in% c("FRED", "DBnomics")) {
    return("INTL")
  }

  "OTHER"
}

classify_data_type <- function(title = "", summary = "", source = "") {
  combined_text <- clean_search_text(paste(title %||% "", summary %||% "", source %||% ""))

  financial_patterns <- c(
    "yield", "bond", "equity", "share", "stock", "market", "balance sheet", "exchange rate",
    "fx", "currency", "credit", "spread", "cash rate", "interest rate", "mortgage rate",
    "asset", "liability", "securities", "financ"
  )
  economic_patterns <- c(
    "gdp", "gross domestic product", "cpi", "inflation", "consumer price", "producer price",
    "unemployment", "employment", "labour", "labor", "wage", "earnings", "retail",
    "industrial production", "production", "exports", "imports", "trade", "housing",
    "population", "income", "sales", "consumption", "activity", "economic"
  )

  if (any(stringr::str_detect(combined_text, stringr::fixed(financial_patterns)))) {
    return("FIN")
  }

  if (any(stringr::str_detect(combined_text, stringr::fixed(economic_patterns)))) {
    return("ECON")
  }

  "OTHER"
}

empty_search_response <- function(results = empty_search_index(), status = NULL) {
  list(
    results = results,
    status = status %||% ""
  )
}

current_fred_api_key <- function() {
  trimws(Sys.getenv("FRED_API_KEY"))
}

set_fred_api_key <- function(api_key) {
  cleaned_key <- trimws(api_key %||% "")
  Sys.setenv(FRED_API_KEY = cleaned_key)
  invisible(cleaned_key)
}

fred_search_available <- function() {
  nzchar(current_fred_api_key())
}

parse_dbnomics_series_id <- function(query) {
  query_parts <- strsplit(trimws(query %||% ""), "/", fixed = TRUE)[[1]]
  query_parts <- query_parts[nzchar(query_parts)]

  if (length(query_parts) < 3) {
    return(NULL)
  }

  list(
    provider_code = query_parts[[1]],
    dataset_code = query_parts[[2]],
    series_query = paste(query_parts[-c(1, 2)], collapse = "/"),
    exact_id = paste(query_parts, collapse = "/")
  )
}

dbnomics_search_cache_key <- function(query, provider_code = "", dataset_code = "", limit = 100) {
  paste(
    clean_search_text(query),
    trimws(provider_code %||% ""),
    trimws(dataset_code %||% ""),
    as.integer(limit %||% 100),
    sep = "::"
  )
}

fred_search_cache_key <- function(query, frequency_filter = "all", search_type = "full_text", limit = 100) {
  paste(
    clean_search_text(query),
    frequency_filter %||% "all",
    search_type %||% "full_text",
    as.integer(limit %||% 100),
    sep = "::"
  )
}

fred_search_remote <- function(query, frequency_filter = "all", search_type = "full_text", limit = 100) {
  fred_key <- current_fred_api_key()
  fredr::fredr_set_key(fred_key)

  search_args <- list(
    search_text = remote_search_query(query),
    limit = max(1L, min(as.integer(limit %||% 100), 1000L))
  )

  if (!identical(frequency_filter %||% "all", "all")) {
    search_args$filter_variable <- "frequency"
    search_args$filter_value <- frequency_filter
  }

  if (identical(search_type %||% "full_text", "series_id")) {
    search_args$order_by <- "series_id"
    search_args$sort_order <- "asc"
    return(do.call(fredr::fredr_series_search_id, search_args))
  }

  search_args$order_by <- "search_rank"
  search_args$sort_order <- "desc"
  do.call(fredr::fredr_series_search_text, search_args)
}

format_fred_search_results <- function(search_results) {
  if (is.null(search_results) || nrow(search_results %||% tibble::tibble()) == 0) {
    return(empty_search_index())
  }

  search_results %>%
    mutate(
      observation_start = as.Date(observation_start),
      observation_end = as.Date(observation_end),
      seasonal_adjustment = seasonal_adjustment %||% "",
      units = units %||% "",
      notes = notes %||% "",
      popularity = popularity %||% 0
    ) %>%
    transmute(
      search_id = paste("fred", id, sep = "::"),
      title = title,
      source = "FRED",
      type_code = vapply(
        seq_along(title),
        function(index) classify_data_type(title[[index]], notes[[index]], "FRED"),
        character(1)
      ),
      location_code = vapply(
        seq_along(title),
        function(index) classify_location_code(title[[index]], notes[[index]], "FRED"),
        character(1)
      ),
      frequency = frequency,
      start_date = observation_start,
      end_date = observation_end,
      summary = dplyr::case_when(
        nzchar(seasonal_adjustment) & nzchar(units) ~ paste(id, "|", units, "|", seasonal_adjustment),
        nzchar(units) ~ paste(id, "|", units),
        TRUE ~ id
      ),
      search_text = clean_search_text(paste(id, title, frequency, units, seasonal_adjustment, notes, "fred")),
      load_payload = purrr::pmap(
        list(id, title),
        function(id, title) {
          list(
            source = "FRED",
            fred_series = id,
            label = title,
            vis_type = "line",
            transform_profile = default_transform_profile()
          )
        }
      ),
      score = as.numeric(popularity %||% 0)
    ) %>%
    arrange(desc(score), title) %>%
    select(-score)
}

search_fred_series <- function(query, frequency_filter = "all", search_type = "full_text", limit = 100, force = FALSE) {
  cleaned_query <- trimws(query %||% "")

  if (!fred_search_available()) {
    return(empty_search_response(status = "Set `FRED_API_KEY` to enable live FRED search."))
  }

  if (nchar(cleaned_query) < 2) {
    return(empty_search_response(status = "Enter at least 2 characters to search FRED."))
  }

  cache_key <- fred_search_cache_key(cleaned_query, frequency_filter, search_type, limit)

  if (!force && exists(cache_key, envir = data_search_env, inherits = FALSE)) {
    return(get(cache_key, envir = data_search_env, inherits = FALSE))
  }

  search_response <- tryCatch(
    {
      remote_results <- fred_search_remote(
        query = cleaned_query,
        frequency_filter = frequency_filter,
        search_type = search_type,
        limit = limit
      )

      empty_search_response(
        results = format_fred_search_results(remote_results),
        status = sprintf("Showing live FRED results for \"%s\".", cleaned_query)
      )
    },
    error = function(error) {
      empty_search_response(
        status = paste("FRED search is unavailable:", conditionMessage(error))
      )
    }
  )

  assign(cache_key, search_response, envir = data_search_env)
  search_response
}

dbnomics_search_remote <- function(query, provider_code = NULL, dataset_code = NULL, limit = 100) {
  search_scope <- parse_dbnomics_series_id(query)

  resolved_provider <- trimws((if (is.null(search_scope)) NULL else search_scope$provider_code) %||% provider_code %||% "")
  resolved_dataset <- trimws((if (is.null(search_scope)) NULL else search_scope$dataset_code) %||% dataset_code %||% "")
  resolved_query <- remote_search_query(
    trimws((if (is.null(search_scope)) NULL else search_scope$series_query) %||% query %||% "")
  )

  search_results <- rdbnomics::rdb_series(
    provider_code = resolved_provider,
    dataset_code = resolved_dataset,
    query = resolved_query,
    simplify = TRUE,
    progress_bar = FALSE
  )

  if (is.null(search_results)) {
    return(tibble::tibble())
  }

  tibble::as_tibble(search_results) %>%
    dplyr::slice_head(n = limit)
}

format_dbnomics_search_results <- function(search_results, provider_code, dataset_code, query) {
  if (is.null(search_results) || nrow(search_results %||% tibble::tibble()) == 0) {
    return(empty_search_index())
  }

  parsed_id <- parse_dbnomics_series_id(query %||% "")
  exact_id <- if (is.null(parsed_id)) "" else parsed_id$exact_id
  query_text <- trimws(query %||% "")

  search_results %>%
    mutate(
      series_code = as.character(series_code),
      series_name = as.character(series_name),
      full_id = paste(provider_code, dataset_code, series_code, sep = "/"),
      score = 0 +
        if_else(full_id == exact_id, 100, 0) +
        if_else(series_code == query_text, 60, 0) +
        if_else(series_name == query_text, 40, 0) +
        if_else(stringr::str_detect(stringr::str_to_lower(series_name), stringr::fixed(stringr::str_to_lower(query_text))), 15, 0)
    ) %>%
    transmute(
      search_id = paste("dbnomics", full_id, sep = "::"),
      title = dplyr::coalesce(series_name, series_code),
      source = "DBnomics",
      type_code = vapply(
        seq_along(full_id),
        function(index) classify_data_type(series_name[[index]], paste(provider_code, dataset_code, series_code[[index]]), "DBnomics"),
        character(1)
      ),
      location_code = vapply(
        seq_along(full_id),
        function(index) classify_location_code(series_name[[index]], paste(provider_code, dataset_code, series_code[[index]]), "DBnomics"),
        character(1)
      ),
      frequency = "Unknown",
      start_date = as.Date(NA),
      end_date = as.Date(NA),
      summary = paste(provider_code, dataset_code, series_code, sep = " / "),
      search_text = clean_search_text(paste(provider_code, dataset_code, series_code, series_name, "dbnomics")),
      load_payload = purrr::pmap(
        list(full_id, series_name),
        function(full_id, series_name) {
          list(
            source = "dbnomics",
            dbnomics_series = full_id,
            label = series_name %||% "",
            vis_type = "line",
            transform_profile = default_transform_profile()
          )
        }
      ),
      score = score
    ) %>%
    arrange(desc(score), title) %>%
    select(-score)
}

search_dbnomics_series <- function(query, provider_code = NULL, dataset_code = NULL, limit = 100, force = FALSE) {
  cleaned_query <- trimws(query %||% "")
  parsed_id <- parse_dbnomics_series_id(cleaned_query)
  provider_text <- trimws(provider_code %||% "")
  dataset_text <- trimws(dataset_code %||% "")

  if (!nzchar(cleaned_query)) {
    return(empty_search_response())
  }

  if (is.null(parsed_id) && (!nzchar(provider_text) || !nzchar(dataset_text))) {
    return(
      empty_search_response(
        status = "For DBnomics, enter a full series ID or provide both provider and dataset codes."
      )
    )
  }

  resolved_provider <- parsed_id$provider_code %||% provider_text
  resolved_dataset <- parsed_id$dataset_code %||% dataset_text
  cache_key <- dbnomics_search_cache_key(cleaned_query, resolved_provider, resolved_dataset, limit)

  if (!force && exists(cache_key, envir = data_search_env, inherits = FALSE)) {
    return(get(cache_key, envir = data_search_env, inherits = FALSE))
  }

  search_response <- tryCatch(
    {
      remote_results <- dbnomics_search_remote(
        query = cleaned_query,
        provider_code = resolved_provider,
        dataset_code = resolved_dataset,
        limit = limit
      )

      empty_search_response(
        results = format_dbnomics_search_results(remote_results, resolved_provider, resolved_dataset, cleaned_query),
        status = sprintf("Showing live DBnomics results for %s / %s.", resolved_provider, resolved_dataset)
      )
    },
    error = function(error) {
      empty_search_response(
        status = paste("DBnomics search is unavailable:", conditionMessage(error))
      )
    }
  )

  assign(cache_key, search_response, envir = data_search_env)
  search_response
}

build_rba_search_index <- function() {
  provider_search_index <- provider_registry_search_index_builder("rba")

  if (is.function(provider_search_index)) {
    return(provider_search_index())
  }

  empty_search_index()
}

build_abs_search_index <- function() {
  provider_search_index <- provider_registry_search_index_builder("abs")

  if (is.function(provider_search_index)) {
    return(provider_search_index())
  }

  empty_search_index()
}

search_remote_provider_sources <- function(source_filter = "all") {
  all_sources <- remote_search_source_values()
  selected_source <- source_filter %||% "all"

  if (identical(selected_source, "all")) {
    return(all_sources)
  }

  all_sources[vapply(
    all_sources,
    function(source_value) identical(provider_registry_source_id(source_value), provider_registry_source_id(selected_source)),
    logical(1)
  )]
}

search_remote_provider_responses <- function(query, source_filter = "all", search_contexts = list(), limit = 100, force = FALSE) {
  remote_sources <- search_remote_provider_sources(source_filter)

  if (length(remote_sources) == 0) {
    return(list())
  }

  setNames(
    lapply(remote_sources, function(source_value) {
      provider_id <- provider_registry_source_id(source_value)
      search_context <- modifyList(
        list(limit = limit),
        search_contexts[[provider_id]] %||% list()
      )

      provider_registry_remote_search_response(
        source_value = source_value,
        query = query,
        search_context = search_context,
        force = force
      )
    }),
    remote_sources
  )
}

search_remote_provider_results <- function(query, source_filter = "all", search_contexts = list(), limit = 100, force = FALSE) {
  bind_rows(search_remote_provider_responses(
    query = query,
    source_filter = source_filter,
    search_contexts = search_contexts,
    limit = limit,
    force = force
  ) %>% lapply(`[[`, "results")) %>%
    distinct(search_id, .keep_all = TRUE)
}

search_remote_provider_status <- function(query, source_filter = "all", search_contexts = list(), limit = 100, force = FALSE) {
  responses <- search_remote_provider_responses(
    query = query,
    source_filter = source_filter,
    search_contexts = search_contexts,
    limit = limit,
    force = force
  )

  unique(vapply(responses, function(response) trimws(response$status %||% ""), character(1)))
}

build_recent_search_index <- function() {
  if (!exists("read_chart_library", mode = "function")) {
    return(empty_recent_search_index())
  }

  chart_library <- tryCatch(
    read_chart_library(),
    error = function(error) NULL
  )

  if (is.null(chart_library) || nrow(chart_library) == 0) {
    return(empty_recent_search_index())
  }

  recent_series <- purrr::map_dfr(seq_len(nrow(chart_library)), function(row_index) {
    chart_record <- chart_library[row_index, , drop = FALSE]
    chart_state <- normalize_chart_state(chart_record$chart_state[[1]])
    saved_title <- trimws(chart_record$title[[1]] %||% "")
    saved_at <- chart_record$saved_at[[1]]
    date_range <- normalize_date_range(chart_state$date_range)

    purrr::map_dfr(Filter(Negate(is.null), chart_state$series), function(spec) {
      normalized_spec <- normalize_series_spec(spec)
      if (identical(normalized_spec$source %||% "", "ABS CPI")) {
        return(tibble::tibble())
      }
      series_title <- recent_series_title(normalized_spec)
      summary_text <- paste(
        c(
          sprintf("Saved in %s", if (nzchar(saved_title)) saved_title else "Untitled chart"),
          sprintf("Source: %s", normalized_spec$source %||% "Unknown"),
          format(saved_at, "%Y-%m-%d %H:%M")
        ),
        collapse = " | "
      )

      tibble::tibble(
        local_row_id = NA_integer_,
        recent_key = series_cache_key(normalized_spec),
        search_id = paste("recent", chart_record$chart_id[[1]], normalized_spec$index, sep = "::"),
        title = series_title,
        source = "Recent",
        type_code = classify_data_type(series_title, summary_text, normalized_spec$source %||% "Recent"),
        location_code = classify_location_code(series_title, summary_text, normalized_spec$source %||% "Recent"),
        frequency = recent_series_frequency(normalized_spec),
        start_date = date_range[[1]],
        end_date = date_range[[2]],
        summary = summary_text,
        search_text = clean_search_text(paste(
          "recent",
          saved_title,
          normalized_spec$source %||% "",
          series_title,
          summary_text
        )),
        load_payload = list({
          spec_payload <- normalized_spec
          spec_payload$index <- NULL
          spec_payload
        }),
        saved_at = saved_at
      )
    })
  })

  if (nrow(recent_series) == 0) {
    return(empty_recent_search_index())
  }

  recent_series %>%
    arrange(desc(saved_at)) %>%
    distinct(recent_key, .keep_all = TRUE) %>%
    select(-recent_key, -saved_at)
}

build_local_search_base_index <- function() {
  bind_rows(lapply(provider_registry_entries(), function(entry) {
    search_index_builder <- entry$search_index_builder %||% NULL

    if (is.null(search_index_builder)) {
      return(NULL)
    }

    search_index_builder()
  })) %>%
    mutate(
      frequency = coalesce(frequency, "Unknown"),
      summary = coalesce(summary, ""),
      search_text = coalesce(search_text, "")
    ) %>%
    arrange(source, title, search_id) %>%
    mutate(local_row_id = dplyr::row_number()) %>%
    select(local_row_id, everything())
}

write_local_search_assets <- function(index = NULL, token_index = NULL) {
  local_index <- index %||% build_local_search_base_index()
  local_token_index <- token_index %||% build_search_token_index(local_index)

  saveRDS(local_index, LOCAL_SEARCH_INDEX_PATH)
  saveRDS(local_token_index, LOCAL_SEARCH_TOKEN_INDEX_PATH)

  invisible(list(index = local_index, token_index = local_token_index))
}

read_prebuilt_local_search_index <- function() {
  if (!file.exists(LOCAL_SEARCH_INDEX_PATH)) {
    stop(
      sprintf(
        "Local search index is missing at %s. Run `Rscript scripts/generate_search_index.R` to rebuild it.",
        LOCAL_SEARCH_INDEX_PATH
      ),
      call. = FALSE
    )
  }

  readRDS(LOCAL_SEARCH_INDEX_PATH)
}

read_prebuilt_local_search_token_index <- function() {
  if (!file.exists(LOCAL_SEARCH_TOKEN_INDEX_PATH)) {
    stop(
      sprintf(
        "Local search token index is missing at %s. Run `Rscript scripts/generate_search_index.R` to rebuild it.",
        LOCAL_SEARCH_TOKEN_INDEX_PATH
      ),
      call. = FALSE
    )
  }

  readRDS(LOCAL_SEARCH_TOKEN_INDEX_PATH)
}

load_prebuilt_local_search_assets <- function(force = FALSE) {
  if (force) {
    invalidate_local_search_asset_cache()
  }

  if (!exists("local_index", envir = data_search_env, inherits = FALSE)) {
    assign("local_index", read_prebuilt_local_search_index(), envir = data_search_env)
  }

  if (!exists("token_index", envir = data_search_env, inherits = FALSE)) {
    assign("token_index", read_prebuilt_local_search_token_index(), envir = data_search_env)
  }

  list(
    index = get("local_index", envir = data_search_env, inherits = FALSE),
    token_index = get("token_index", envir = data_search_env, inherits = FALSE)
  )
}

current_search_token_index <- function(force = FALSE) {
  load_prebuilt_local_search_assets(force = force)$token_index
}

build_search_index <- function(force = FALSE) {
  if (!force && exists("index", envir = data_search_env, inherits = FALSE)) {
    return(get("index", envir = data_search_env, inherits = FALSE))
  }

  local_assets <- load_prebuilt_local_search_assets(force = force)

  search_index <- bind_rows(
    build_recent_search_index(),
    local_assets$index
  ) %>%
    mutate(
      frequency = coalesce(frequency, "Unknown"),
      summary = coalesce(summary, ""),
      search_text = coalesce(search_text, "")
    )

  assign("index", search_index, envir = data_search_env)
  search_index
}

filter_search_index <- function(search_index, query = "", source_filter = "all", type_filter = "all", location_filter = "all", frequency_filter = "all", limit = Inf, token_index = NULL) {
  filtered_index <- search_index

  if (!identical(source_filter %||% "all", "all")) {
    filtered_index <- filtered_index %>%
      filter(source == source_filter)
  }

  if (!identical(type_filter %||% "all", "all")) {
    filtered_index <- filtered_index %>%
      filter(type_code == type_filter)
  }

  if (!identical(location_filter %||% "all", "all")) {
    filtered_index <- filtered_index %>%
      filter(location_code == location_filter)
  }

  if (!identical(frequency_filter %||% "all", "all")) {
    filtered_index <- filtered_index %>%
      filter(frequency == frequency_filter)
  }

  search_terms <- boolean_query_terms(query)
  limit <- suppressWarnings(as.numeric(limit %||% Inf))
  unlimited_results <- is.infinite(limit) || is.na(limit)

  if (length(search_terms) == 0) {
    sorted_index <- filtered_index %>%
      arrange(source, title)

    if (unlimited_results) {
      return(sorted_index)
    }

    return(slice_head(sorted_index, n = limit))
  }

  exact_query <- normalized_query_text(query)

  indexed_rows <- filtered_index %>%
    filter(!is.na(local_row_id))

  dynamic_rows <- filtered_index %>%
    filter(is.na(local_row_id))

  matched_indexed_rows <- if (!is.null(token_index) && nrow(indexed_rows) > 0) {
    matched_ids <- boolean_query_candidate_ids(query, token_index)

    if (length(matched_ids) == 0) {
      indexed_rows[0, , drop = FALSE]
    } else {
      indexed_rows %>%
        filter(local_row_id %in% matched_ids)
    }
  } else {
    indexed_rows %>%
      filter(vapply(search_text, boolean_query_matches, logical(1), query = query))
  }

  matched_dynamic_rows <- dynamic_rows %>%
    filter(vapply(search_text, boolean_query_matches, logical(1), query = query))

  scored_index <- bind_rows(matched_dynamic_rows, matched_indexed_rows) %>%
    mutate(
      title_text = clean_search_text(title),
      summary_text = clean_search_text(summary)
    )

  scored_results <- scored_index %>%
    mutate(
      score = 0 +
        if_else(title_text == exact_query, 100, 0) +
        if_else(stringr::str_detect(title_text, stringr::fixed(exact_query)), 40, 0) +
        if_else(stringr::str_detect(summary_text, stringr::fixed(exact_query)), 15, 0) +
        purrr::map_int(
          title_text,
          ~ sum(vapply(search_terms, function(term) stringr::str_detect(.x, stringr::fixed(term)), logical(1)))
        ) * 8 +
        purrr::map_int(
          summary_text,
          ~ sum(vapply(search_terms, function(term) stringr::str_detect(.x, stringr::fixed(term)), logical(1)))
        ) * 3
    ) %>%
    arrange(desc(score), title, source) %>%
    select(-title_text, -summary_text)

  if (unlimited_results) {
    return(scored_results)
  }

  slice_head(scored_results, n = limit)
}

search_type_choices <- function() {
  SEARCH_TYPE_CODES
}

search_location_choices <- function() {
  SEARCH_LOCATION_CODES
}

apply_search_filters <- function(search_index, query = "", source_filter = "all", type_filter = "all", location_filter = "all", frequency_filter = "all", limit = Inf) {
  filter_search_index(
    search_index,
    query = query,
    source_filter = source_filter,
    type_filter = type_filter,
    location_filter = location_filter,
    frequency_filter = frequency_filter,
    limit = limit
  )
}

search_frequency_choices <- function(search_index) {
  frequency_values <- search_index %>%
    pull(frequency) %>%
    unique() %>%
    c(FRED_SEARCH_FREQUENCIES) %>%
    unique() %>%
    sort()

  stats::setNames(c("all", frequency_values), c("All", frequency_values))
}

search_result_target_choices <- function() {
  c(
    "Next" = "next",
    "S1" = "1",
    "S2" = "2",
    "S3" = "3",
    "S4" = "4"
  )
}

series_slot_from_search_target <- function(builder_state, target_value) {
  if (!identical(target_value %||% "next", "next")) {
    return(as.integer(target_value))
  }

  active_slots <- vapply(builder_state$series, Negate(is.null), logical(1))
  empty_slots <- which(!active_slots)

  if (length(empty_slots) == 0) {
    return(1L)
  }

  empty_slots[[1]]
}

search_result_series_spec <- function(search_result, index) {
  provider_spec <- provider_registry_search_result_to_spec(search_result$source[[1]] %||% "", search_result, index)
  if (!is.null(provider_spec)) {
    return(provider_spec)
  }

  spec <- search_result$load_payload[[1]]
  spec$index <- index
  spec
}

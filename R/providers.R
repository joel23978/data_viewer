provider_registry_source_id <- function(source_value) {
  source_text <- trimws(as.character(source_value %||% ""))

  if (!nzchar(source_text)) {
    return("")
  }

  normalized_text <- tolower(source_text)

  switch(
    normalized_text,
    "fred" = "fred",
    "dbnomics" = "dbnomics",
    "rba" = "rba",
    "abs" = "abs",
    source_text
  )
}

provider_registry <- function() {
  list(
    fred = list(
      id = "fred",
      source = "FRED",
      label = "FRED",
      controls_ui = provider_fred_controls_ui,
      spec_from_input = provider_fred_spec_from_input,
      normalize_spec = provider_fred_normalize_spec,
      query_series_history = provider_fred_query_series_history,
      restore_controls = provider_fred_restore_controls,
      search_remote = provider_fred_search_remote,
      search_context_from_input = provider_fred_search_context_from_input,
      search_result_lookup = provider_fred_search_result_lookup,
      search_result_series_id = provider_fred_search_result_series_id,
      recent_title = provider_fred_recent_title,
      default_label = provider_fred_default_label,
      source_note_value = provider_fred_source_note_value,
      cache_key = provider_fred_cache_key
    ),
    dbnomics = list(
      id = "dbnomics",
      source = "dbnomics",
      label = "DBnomics",
      controls_ui = provider_dbnomics_controls_ui,
      spec_from_input = provider_dbnomics_spec_from_input,
      normalize_spec = provider_dbnomics_normalize_spec,
      query_series_history = provider_dbnomics_query_series_history,
      restore_controls = provider_dbnomics_restore_controls,
      search_remote = provider_dbnomics_search_remote,
      search_controls_ui = provider_dbnomics_search_controls_ui,
      search_context_from_input = provider_dbnomics_search_context_from_input,
      search_result_lookup = provider_dbnomics_search_result_lookup,
      search_result_series_id = provider_dbnomics_search_result_series_id,
      recent_title = provider_dbnomics_recent_title,
      default_label = provider_dbnomics_default_label,
      source_note_value = provider_dbnomics_source_note_value,
      cache_key = provider_dbnomics_cache_key
    ),
    rba = list(
      id = "rba",
      source = "rba",
      label = "RBA",
      controls_ui = provider_rba_controls_ui,
      register_dependencies = provider_rba_register_dependencies,
      spec_from_input = provider_rba_spec_from_input,
      normalize_spec = provider_rba_normalize_spec,
      query_series_history = provider_rba_query_series_history,
      restore_controls = provider_rba_restore_controls,
      search_index_builder = provider_rba_search_index_builder,
      search_controls_ui = provider_rba_search_controls_ui,
      search_context_from_input = provider_rba_search_context_from_input,
      filter_search_results = provider_rba_filter_search_results,
      search_result_lookup = provider_rba_search_result_lookup,
      search_result_to_spec = provider_rba_search_result_to_spec,
      search_result_series_id = provider_rba_search_result_series_id,
      recent_title = provider_rba_recent_title,
      default_label = provider_rba_default_label,
      source_note_value = provider_rba_source_note_value,
      cache_key = provider_rba_cache_key
    ),
    abs = list(
      id = "abs",
      source = "abs",
      label = "ABS",
      controls_ui = provider_abs_controls_ui,
      register_dependencies = provider_abs_register_dependencies,
      spec_from_input = provider_abs_spec_from_input,
      normalize_spec = provider_abs_normalize_spec,
      query_series_history = provider_abs_query_series_history,
      restore_controls = provider_abs_restore_controls,
      search_index_builder = provider_abs_search_index_builder,
      search_controls_ui = provider_abs_search_controls_ui,
      search_context_from_input = provider_abs_search_context_from_input,
      filter_search_results = provider_abs_filter_search_results,
      search_result_lookup = provider_abs_search_result_lookup,
      search_result_series_id = provider_abs_search_result_series_id,
      recent_title = provider_abs_recent_title,
      default_label = provider_abs_default_label,
      source_note_value = provider_abs_source_note_value,
      cache_key = provider_abs_cache_key
    )
  )
}

provider_registry_entry <- function(source_value) {
  registry_id <- provider_registry_source_id(source_value)

  if (!nzchar(registry_id)) {
    return(NULL)
  }

  provider_registry()[[registry_id]]
}

provider_registry_entries <- function() {
  Filter(Negate(is.null), provider_registry())
}

source_catalog <- function() {
  tibble::tribble(
    ~id, ~label, ~provider_id, ~builder_value, ~search_value, ~search_kind, ~builder_enabled, ~search_enabled, ~builder_default, ~series_supported,
    "recent", "Recent", NA_character_, NA_character_, "Recent", "recent", FALSE, TRUE, FALSE, TRUE,
    "fred", "FRED", "fred", "FRED", "FRED", "remote", TRUE, TRUE, FALSE, TRUE,
    "dbnomics", "DBnomics", "dbnomics", "dbnomics", "DBnomics", "remote", TRUE, TRUE, FALSE, TRUE,
    "rba", "RBA", "rba", "rba", "RBA", "local", TRUE, TRUE, FALSE, TRUE,
    "abs", "ABS", "abs", "abs", "ABS", "local", TRUE, TRUE, TRUE, TRUE,
    "analysis_result", "Analysis result", NA_character_, "analysis_result", NA_character_, "none", TRUE, FALSE, FALSE, TRUE,
    "abs_cpi", "ABS CPI", NA_character_, "ABS CPI", NA_character_, "none", FALSE, FALSE, FALSE, FALSE
  )
}

builder_source_choices <- function() {
  catalog <- source_catalog() %>%
    filter(builder_enabled)

  stats::setNames(catalog$builder_value, catalog$label)
}

default_builder_source_value <- function() {
  catalog <- source_catalog() %>%
    filter(builder_enabled, builder_default)

  catalog$builder_value[[1]] %||% builder_source_choices()[[1]]
}

search_source_filter_choices <- function() {
  catalog <- source_catalog() %>%
    filter(search_enabled)

  c("All" = "all", stats::setNames(catalog$search_value, catalog$label))
}

source_catalog_search_values <- function(search_kind = NULL) {
  catalog <- source_catalog() %>%
    filter(search_enabled)

  if (!is.null(search_kind)) {
    catalog <- catalog[catalog$search_kind %in% search_kind, , drop = FALSE]
  }

  catalog$search_value
}

local_search_source_values <- function() {
  source_catalog_search_values(c("recent", "local"))
}

remote_search_source_values <- function() {
  source_catalog_search_values("remote")
}

search_filter_includes <- function(source_filter, search_values) {
  selected_filter <- source_filter %||% "all"
  identical(selected_filter, "all") || selected_filter %in% (search_values %||% character())
}

search_activity_target <- function(source_filter = "all", query_text = "") {
  selected_filter <- source_filter %||% "all"
  cleaned_query <- trimws(query_text %||% "")

  if (identical(selected_filter, "all")) {
    if (nzchar(cleaned_query)) {
      return("recent series, local metadata, and live search")
    }

    return("recent series and local metadata")
  }

  catalog_entry <- source_catalog_entry(selected_filter)
  if (is.null(catalog_entry) || nrow(catalog_entry) == 0) {
    return("search sources")
  }

  search_kind <- catalog_entry$search_kind[[1]] %||% "none"
  label <- catalog_entry$label[[1]] %||% "search source"

  if (identical(search_kind, "recent")) {
    return("recent saved series")
  }

  if (identical(search_kind, "local")) {
    return(paste("local", label, "metadata"))
  }

  if (identical(search_kind, "remote")) {
    if (identical(label, "FRED")) {
      return("FRED")
    }

    return(paste("live", label))
  }

  "search sources"
}

source_catalog_entry <- function(source_value) {
  source_text <- trimws(as.character(source_value %||% ""))

  if (!nzchar(source_text)) {
    return(NULL)
  }

  catalog <- source_catalog()
  normalized_text <- tolower(source_text)
  matches <- catalog %>%
    filter(
      tolower(id) == normalized_text |
        tolower(coalesce(builder_value, "")) == normalized_text |
        tolower(coalesce(search_value, "")) == normalized_text
    )

  if (nrow(matches) == 0) {
    return(NULL)
  }

  matches[1, , drop = FALSE]
}

source_catalog_supported_builder_values <- function() {
  source_catalog() %>%
    filter(builder_enabled) %>%
    pull(builder_value) %>%
    unique()
}

source_catalog_supports_series_source <- function(source_value) {
  source_text <- trimws(as.character(source_value %||% ""))

  if (!nzchar(source_text)) {
    return(FALSE)
  }

  catalog_entry <- source_catalog_entry(source_text)

  if (is.null(catalog_entry) || nrow(catalog_entry) == 0) {
    return(FALSE)
  }

  isTRUE(catalog_entry$series_supported[[1]])
}

provider_registry_search_controls_ui <- function(source_filter = "all", input = NULL) {
  selected_filter <- source_filter %||% "all"
  controls <- lapply(provider_registry_entries(), function(entry) {
    controls_ui <- entry$search_controls_ui %||% NULL

    if (is.null(controls_ui)) {
      return(NULL)
    }

    if (!identical(selected_filter, "all") &&
        !identical(provider_registry_source_id(selected_filter), entry$id %||% "")) {
      return(NULL)
    }

    if (identical(selected_filter, "all")) {
      return(NULL)
    }

    controls_ui(input)
  })

  controls <- Filter(Negate(is.null), controls)

  if (length(controls) == 0) {
    return(NULL)
  }

  do.call(tagList, controls)
}

provider_registry_search_contexts_from_input <- function(input) {
  stats::setNames(
    lapply(provider_registry_entries(), function(entry) {
      context_from_input <- entry$search_context_from_input %||% NULL

      if (is.null(context_from_input)) {
        return(list())
      }

      context_from_input(input)
    }),
    vapply(provider_registry_entries(), function(entry) entry$id %||% "", character(1))
  )
}

provider_registry_filter_search_results <- function(source_value, search_results, search_context = list()) {
  provider_entry <- provider_registry_entry(source_value)
  filter_search_results <- provider_entry$filter_search_results %||% NULL

  if (is.null(filter_search_results)) {
    return(search_results %||% empty_search_index())
  }

  filter_search_results(search_results %||% empty_search_index(), search_context %||% list())
}

provider_registry_search_result_lookup <- function(source_value, series_id, search_context = list()) {
  provider_entry <- provider_registry_entry(source_value)
  search_result_lookup <- provider_entry$search_result_lookup %||% NULL

  if (is.null(search_result_lookup)) {
    return(list(result = NULL, status = "Series lookup is unavailable for this source."))
  }

  search_result_lookup(trimws(series_id %||% ""), search_context %||% list())
}

provider_registry_register_dependencies <- function(input, output, session, index) {
  purrr::walk(
    provider_registry_entries(),
    function(entry) {
      register_dependencies <- entry$register_dependencies %||% NULL
      if (!is.null(register_dependencies)) {
        register_dependencies(input, output, session, index)
      }
    }
  )

  invisible(NULL)
}

provider_registry_search_index_builder <- function(source_value) {
  provider_entry <- provider_registry_entry(source_value)
  if (is.null(provider_entry)) {
    return(NULL)
  }

  provider_entry$search_index_builder %||% NULL
}

provider_registry_search_result_to_spec <- function(source_value, search_result, index) {
  provider_entry <- provider_registry_entry(source_value)
  if (is.null(provider_entry) || is.null(provider_entry$search_result_to_spec)) {
    return(provider_generic_search_result_to_spec(search_result, index))
  }

  provider_entry$search_result_to_spec(search_result, index)
}

provider_registry_restore_controls <- function(session, index, spec) {
  provider_entry <- provider_registry_entry(spec$source %||% "")
  restore_controls <- provider_entry$restore_controls %||% NULL

  if (is.null(restore_controls)) {
    return(invisible(NULL))
  }

  restore_controls(session, index, spec)
}

provider_registry_default_label <- function(spec) {
  provider_entry <- provider_registry_entry(spec$source %||% "")
  default_label <- provider_entry$default_label %||% NULL

  if (is.null(default_label)) {
    return("")
  }

  default_label(spec)
}

provider_registry_source_note_value <- function(spec) {
  provider_entry <- provider_registry_entry(spec$source %||% "")
  source_note_value <- provider_entry$source_note_value %||% NULL

  if (is.null(source_note_value)) {
    return(spec$source %||% "Unknown")
  }

  source_note_value(spec)
}

provider_registry_cache_key <- function(spec) {
  provider_entry <- provider_registry_entry(spec$source %||% "")
  cache_key <- provider_entry$cache_key %||% NULL

  if (is.null(cache_key)) {
    return(NULL)
  }

  cache_key(spec)
}

provider_registry_remote_search_response <- function(source_value, query, search_context = list(), force = FALSE) {
  provider_entry <- provider_registry_entry(source_value)
  search_remote <- provider_entry$search_remote %||% NULL

  if (is.null(search_remote)) {
    return(empty_search_response())
  }

  search_remote(query = query, search_context = search_context, force = force)
}

provider_registry_search_result_series_id <- function(search_result) {
  source_value <- search_result$source[[1]] %||% ""
  provider_entry <- provider_registry_entry(source_value)
  search_result_series_id <- provider_entry$search_result_series_id %||% NULL

  if (is.null(search_result_series_id)) {
    return("")
  }

  search_result_series_id(search_result)
}

provider_registry_recent_title <- function(spec) {
  provider_entry <- provider_registry_entry(spec$source %||% "")
  recent_title <- provider_entry$recent_title %||% NULL

  if (is.null(recent_title)) {
    return("")
  }

  recent_title(spec)
}

provider_generic_search_result_to_spec <- function(search_result, index) {
  spec <- search_result$load_payload[[1]]
  spec$index <- index
  spec
}

provider_search_controls_env <- new.env(parent = emptyenv())

provider_merge_choice_values <- function(...) {
  choice_values <- unlist(list(...), recursive = FALSE, use.names = TRUE)

  if (length(choice_values) == 0) {
    return(character())
  }

  choice_values <- choice_values[!duplicated(unname(choice_values))]
  stats::setNames(unname(choice_values), names(choice_values))
}

provider_named_choices_from_frame <- function(data, value_columns, label_columns) {
  if (is.null(data) || length(data) == 0) {
    return(character())
  }

  if (is.character(data)) {
    if (!is.null(names(data)) && all(nzchar(names(data)))) {
      return(data)
    }

    values <- as.character(data)
    return(stats::setNames(values, values))
  }

  if (!is.data.frame(data)) {
    return(character())
  }

  data <- tibble::as_tibble(data)
  value_column <- intersect(value_columns, names(data))[1] %||% NA_character_
  label_column <- intersect(label_columns, names(data))[1] %||% NA_character_

  if (is.na(value_column) || !nzchar(value_column)) {
    return(character())
  }

  values <- trimws(as.character(data[[value_column]] %||% character()))
  labels <- if (!is.na(label_column) && nzchar(label_column)) {
    trimws(as.character(data[[label_column]] %||% character()))
  } else {
    values
  }

  keep_rows <- nzchar(values)
  values <- values[keep_rows]
  labels <- labels[keep_rows]

  if (length(values) == 0) {
    return(character())
  }

  labels[!nzchar(labels)] <- values[!nzchar(labels)]
  choices <- stats::setNames(values, labels)
  choices[!duplicated(unname(choices))]
}

provider_filter_search_rows <- function(search_results, predicate) {
  search_results <- search_results %||% empty_search_index()

  if (nrow(search_results) == 0) {
    return(search_results)
  }

  keep_rows <- vapply(
    search_results$load_payload,
    function(payload) isTRUE(predicate(payload %||% list())),
    logical(1)
  )

  search_results[keep_rows, , drop = FALSE]
}

provider_exact_search_result <- function(search_results, predicate) {
  filtered_results <- provider_filter_search_rows(search_results, predicate)

  if (nrow(filtered_results) == 0) {
    return(NULL)
  }

  filtered_results[1, , drop = FALSE]
}

provider_clean_scalar_text <- function(value = "") {
  if (is.null(value) || length(value) == 0) {
    return("")
  }

  first_value <- value[[1]]
  if (is.null(first_value) || (length(first_value) == 1 && is.na(first_value))) {
    return("")
  }

  cleaned_value <- trimws(as.character(first_value))
  if (length(cleaned_value) == 0 || is.na(cleaned_value[1])) {
    return("")
  }

  cleaned_value[1]
}

provider_clean_text_values <- function(value = character()) {
  if (is.null(value) || length(value) == 0) {
    return(character())
  }

  cleaned_values <- trimws(as.character(value))
  cleaned_values <- cleaned_values[!is.na(cleaned_values) & nzchar(cleaned_values)]
  unique(cleaned_values)
}

provider_series_helper_text <- function(current_value = "", restored_value = "", restored_title = character(), empty_status = "", lookup_fun = NULL) {
  current_value <- provider_clean_scalar_text(current_value)
  restored_value <- provider_clean_scalar_text(restored_value)
  restored_title <- provider_clean_text_values(restored_title)

  if (!nzchar(current_value)) {
    return(list(result = NULL, status = empty_status))
  }

  if (nzchar(restored_value) && identical(current_value, restored_value) && length(restored_title) > 0) {
    return(list(result = NULL, status = paste("Actual series:", paste(restored_title, collapse = ", "))))
  }

  if (is.null(lookup_fun)) {
    return(list(result = NULL, status = empty_status))
  }

  lookup_fun(current_value)
}

dbnomics_provider_choice_values <- function(force = FALSE) {
  cache_key <- "provider_choices"

  if (!force && exists(cache_key, envir = provider_search_controls_env, inherits = FALSE)) {
    return(get(cache_key, envir = provider_search_controls_env, inherits = FALSE))
  }

  choices <- tryCatch(
    {
      if (!requireNamespace("rdbnomics", quietly = TRUE)) {
        return(character())
      }

      provider_named_choices_from_frame(
        suppressWarnings(rdbnomics::rdb_providers(code = TRUE)),
        value_columns = c("code", "provider_code", "id"),
        label_columns = c("name", "provider_name", "label", "title", "code")
      )
    },
    error = function(error) {
      character()
    }
  )

  assign(cache_key, choices, envir = provider_search_controls_env)
  choices
}

dbnomics_dataset_choice_values <- function(provider_code = "", force = FALSE) {
  cleaned_provider <- trimws(provider_code %||% "")

  if (!nzchar(cleaned_provider)) {
    return(character())
  }

  cache_key <- paste("dataset_choices", cleaned_provider, sep = "::")

  if (!force && exists(cache_key, envir = provider_search_controls_env, inherits = FALSE)) {
    return(get(cache_key, envir = provider_search_controls_env, inherits = FALSE))
  }

  choices <- tryCatch(
    {
      if (!requireNamespace("rdbnomics", quietly = TRUE)) {
        return(character())
      }

      provider_named_choices_from_frame(
        suppressWarnings(rdbnomics::rdb_datasets(provider_code = cleaned_provider, simplify = TRUE)),
        value_columns = c("code", "dataset_code", "id"),
        label_columns = c("name", "dataset_name", "label", "title", "code")
      )
    },
    error = function(error) {
      character()
    }
  )

  assign(cache_key, choices, envir = provider_search_controls_env)
  choices
}

provider_fred_controls_ui <- function(input, session, index, restored_spec = NULL) {
  current_series <- provider_clean_scalar_text(input[[series_input_id(index, "fred_series")]] %||% restored_spec$fred_series %||% "")
  restored_series <- provider_clean_scalar_text(restored_spec$fred_series %||% "")
  lookup <- provider_series_helper_text(
    current_value = current_series,
    restored_value = restored_series,
    restored_title = restored_spec$fred_title %||% character(),
    empty_status = "Type a FRED series ID and press Enter.",
    lookup_fun = provider_fred_search_result_lookup
  )
  vintage_mode <- input[[series_input_id(index, "fred_vintage_mode")]] %||% restored_spec$fred_vintage_mode %||% "current"
  selected_vintage <- input[[series_input_id(index, "fred_vintage_date")]] %||%
    if (!is.null(restored_spec$fred_vintage_date) && !is.na(restored_spec$fred_vintage_date)) format(as.Date(restored_spec$fred_vintage_date), "%Y-%m-%d") else ""
  load_vintage_choices <- !identical(vintage_mode, "current") &&
    nzchar(current_series) &&
    (!nzchar(restored_series) || !identical(current_series, restored_series))
  vintage_choices <- if (identical(vintage_mode, "current")) {
    character()
  } else if (isTRUE(load_vintage_choices)) {
    fred_vintage_choice_values(current_series, selected_vintage)
  } else {
    seed_selectize_choices(selected_vintage)
  }
  default_vintage <- if (nzchar(trimws(selected_vintage %||% ""))) {
    trimws(selected_vintage)
  } else if (length(vintage_choices) > 0) {
    unname(tail(vintage_choices, 1))
  } else {
    NULL
  }

  tagList(
    selectizeInput(
      series_input_id(index, "fred_series"),
      "FRED series ID",
      choices = seed_selectize_choices(current_series),
      selected = if (nzchar(trimws(current_series))) current_series else NULL,
      options = list(
        create = TRUE,
        maxItems = 1,
        persist = FALSE,
        placeholder = "Type a FRED series ID and press Enter"
      )
    ),
    tags$p(class = "muted-copy", trimws(lookup$status %||% "")),
    radioGroupButtons(
      series_input_id(index, "fred_vintage_mode"),
      "Vintage mode",
      choices = fred_vintage_mode_choices(),
      selected = vintage_mode,
      justified = TRUE,
      checkIcon = list(yes = icon("check"))
    ),
    if (!identical(vintage_mode, "current")) {
      selectizeInput(
        series_input_id(index, "fred_vintage_date"),
        "Vintage date",
        choices = vintage_choices,
        selected = default_vintage,
        options = list(
          create = TRUE,
          placeholder = "YYYY-MM-DD"
        )
      )
    }
  )
}

provider_fred_spec_from_input <- function(input, index, transform_profile = default_transform_profile(), restored_spec = NULL) {
  current_series <- provider_clean_scalar_text(input[[series_input_id(index, "fred_series")]] %||% restored_spec$fred_series %||% "")
  restored_series <- provider_clean_scalar_text(restored_spec$fred_series %||% "")

  spec <- list(
    index = index,
    source = "FRED",
    label = trimws(input[[series_input_id(index, "label")]] %||% ""),
    transform_profile = transform_profile,
    vis_type = input[[series_input_id(index, "vis_type")]] %||% "line",
    fred_series = current_series,
    fred_title = if (nzchar(current_series) && identical(current_series, restored_series)) provider_clean_scalar_text(restored_spec$fred_title %||% "") else "",
    fred_vintage_mode = input[[series_input_id(index, "fred_vintage_mode")]] %||% restored_spec$fred_vintage_mode %||% "current",
    fred_vintage_date = parse_vintage_date(input[[series_input_id(index, "fred_vintage_date")]] %||% restored_spec$fred_vintage_date %||% NA)
  )

  if (!nzchar(spec$fred_series)) {
    return(NULL)
  }

  if (!nzchar(spec$label)) {
    spec$label <- provider_fred_default_label(spec)
  }

  spec
}

provider_fred_normalize_spec <- function(spec, normalized_spec = NULL) {
  normalized_spec <- normalized_spec %||% list(
    index = as.integer(spec$index),
    source = "FRED",
    label = trimws(spec$label %||% ""),
    transform_profile = normalize_transform_profile(spec$transform_profile),
    vis_type = spec$vis_type %||% "line"
  )

  normalized_spec$source <- "FRED"
  normalized_spec$fred_series <- trimws(spec$fred_series %||% "")
  normalized_spec$fred_title <- trimws(spec$fred_title %||% "")
  normalized_spec$fred_vintage_mode <- spec$fred_vintage_mode %||% "current"
  normalized_spec$fred_vintage_date <- parse_vintage_date(spec$fred_vintage_date %||% NA)

  if (!nzchar(normalized_spec$label %||% "")) {
    normalized_spec$label <- provider_fred_default_label(normalized_spec)
  }

  normalized_spec
}

provider_fred_query_series_history <- function(spec) {
  vintage_mode <- spec$fred_vintage_mode %||% "current"
  vintage_date <- parse_vintage_date(spec$fred_vintage_date %||% NA)

  if (identical(vintage_mode, "historical")) {
    if (is.na(vintage_date)) {
      stop("Choose a FRED vintage date.", call. = FALSE)
    }

    return(
      fred_data(
        series = spec$fred_series,
        realtime_start = vintage_date,
        realtime_end = vintage_date,
        name_override = sprintf("%s (%s vintage)", spec$fred_series, format(vintage_date, "%Y-%m-%d"))
      )
    )
  }

  if (identical(vintage_mode, "compare")) {
    if (is.na(vintage_date)) {
      stop("Choose a FRED vintage date.", call. = FALSE)
    }

    return(bind_rows(
      fred_data(
        series = spec$fred_series,
        name_override = sprintf("%s (current)", spec$fred_series)
      ),
      fred_data(
        series = spec$fred_series,
        realtime_start = vintage_date,
        realtime_end = vintage_date,
        name_override = sprintf("%s (%s vintage)", spec$fred_series, format(vintage_date, "%Y-%m-%d"))
      )
    ))
  }

  fred_data(series = spec$fred_series)
}

provider_fred_restore_controls <- function(session, index, spec) {
  series_id <- provider_clean_scalar_text(spec$fred_series %||% "")
  if (!nzchar(series_id)) {
    return(invisible(NULL))
  }

  updateSelectizeInput(
    session,
    series_input_id(index, "fred_series"),
    choices = seed_selectize_choices(series_id),
    selected = series_id,
    server = TRUE
  )
  updateRadioGroupButtons(session, series_input_id(index, "fred_vintage_mode"), selected = spec$fred_vintage_mode %||% "current")

  if (!identical(spec$fred_vintage_mode %||% "current", "current")) {
    updateSelectizeInput(
      session,
      series_input_id(index, "fred_vintage_date"),
      choices = seed_selectize_choices(if (!is.null(spec$fred_vintage_date) && !is.na(spec$fred_vintage_date)) format(as.Date(spec$fred_vintage_date), "%Y-%m-%d") else ""),
      selected = if (!is.null(spec$fred_vintage_date) && !is.na(spec$fred_vintage_date)) format(as.Date(spec$fred_vintage_date), "%Y-%m-%d") else NULL,
      server = TRUE
    )
  }
}

provider_fred_search_context_from_input <- function(input) {
  list(
    frequency_filter = input$search_frequency_filter %||% "all"
  )
}

provider_fred_search_remote <- function(query, search_context = list(), force = FALSE) {
  search_fred_series(
    query = query,
    frequency_filter = search_context$frequency_filter %||% "all",
    search_type = "full_text",
    limit = search_context$limit %||% 100,
    force = force
  )
}

provider_fred_search_result_lookup <- function(series_id, search_context = list()) {
  cleaned_id <- trimws(series_id %||% "")

  if (!nzchar(cleaned_id)) {
    return(list(result = NULL, status = "Type a FRED series ID and press Enter."))
  }

  lookup_response <- search_fred_series(
    query = cleaned_id,
    frequency_filter = "all",
    search_type = "series_id",
    limit = search_context$limit %||% 25,
    force = TRUE
  )

  exact_result <- provider_exact_search_result(
    lookup_response$results,
    function(payload) identical(trimws(payload$fred_series %||% ""), cleaned_id)
  )

  if (is.null(exact_result)) {
    status_text <- trimws(lookup_response$status %||% "")
    if (!nzchar(status_text)) {
      status_text <- sprintf("No exact FRED match was found for %s.", cleaned_id)
    }

    return(list(result = NULL, status = status_text))
  }

  list(
    result = exact_result,
    status = sprintf("Actual series: %s", trimws(exact_result$title[[1]] %||% cleaned_id))
  )
}

provider_fred_search_result_series_id <- function(search_result) {
  trimws(search_result$load_payload[[1]]$fred_series %||% "")
}

provider_fred_recent_title <- function(spec) {
  base_title <- provider_clean_scalar_text(spec$fred_title %||% "")
  if (!nzchar(base_title)) {
    base_title <- provider_clean_scalar_text(spec$fred_series %||% "FRED series")
  }
  vintage_mode <- spec$fred_vintage_mode %||% "current"
  vintage_date <- spec$fred_vintage_date

  if (identical(vintage_mode, "historical") && !is.na(as.Date(vintage_date))) {
    return(sprintf("%s (%s vintage)", base_title, format(as.Date(vintage_date), "%Y-%m-%d")))
  }

  if (identical(vintage_mode, "compare") && !is.na(as.Date(vintage_date))) {
    return(sprintf("%s current vs %s vintage", base_title, format(as.Date(vintage_date), "%Y-%m-%d")))
  }

  base_title
}

provider_fred_default_label <- function(spec) {
  title_text <- provider_clean_scalar_text(spec$fred_title %||% "")

  if (nzchar(title_text)) {
    return(title_text)
  }

  provider_clean_scalar_text(spec$fred_series %||% "")
}

provider_fred_source_note_value <- function(spec) {
  id_values <- unique(trimws(as.character(spec$fred_series %||% character())))
  id_values <- id_values[nzchar(id_values)]

  if (length(id_values) == 0) {
    "FRED"
  } else {
    paste0("FRED - ", paste(id_values, collapse = ", "))
  }
}

provider_fred_cache_key <- function(spec) {
  paste(
    "FRED",
    cache_key_value(spec$fred_series),
    cache_key_value(spec$fred_vintage_mode),
    cache_key_value(spec$fred_vintage_date),
    sep = "::"
  )
}

provider_dbnomics_controls_ui <- function(input, session, index, restored_spec = NULL) {
  current_series <- provider_clean_scalar_text(input[[series_input_id(index, "dbnomics_series")]] %||% restored_spec$dbnomics_series %||% "")
  restored_series <- provider_clean_scalar_text(restored_spec$dbnomics_series %||% "")
  lookup <- provider_series_helper_text(
    current_value = current_series,
    restored_value = restored_series,
    restored_title = restored_spec$dbnomics_name %||% character(),
    empty_status = "Type a DBnomics series ID and press Enter.",
    lookup_fun = provider_dbnomics_search_result_lookup
  )

  tagList(
    selectizeInput(
      series_input_id(index, "dbnomics_series"),
      "DBnomics series ID",
      choices = seed_selectize_choices(current_series),
      selected = if (nzchar(trimws(current_series))) current_series else NULL,
      options = list(
        create = TRUE,
        persist = FALSE,
        maxItems = 1,
        placeholder = "Type a DBnomics series ID and press Enter"
      )
    ),
    tags$p(class = "muted-copy", trimws(lookup$status %||% ""))
  )
}

provider_dbnomics_spec_from_input <- function(input, index, transform_profile = default_transform_profile(), restored_spec = NULL) {
  current_series <- provider_clean_scalar_text(input[[series_input_id(index, "dbnomics_series")]] %||% restored_spec$dbnomics_series %||% "")
  restored_series <- provider_clean_scalar_text(restored_spec$dbnomics_series %||% "")

  spec <- list(
    index = index,
    source = "dbnomics",
    label = trimws(input[[series_input_id(index, "label")]] %||% ""),
    transform_profile = transform_profile,
    vis_type = input[[series_input_id(index, "vis_type")]] %||% "line",
    dbnomics_series = current_series,
    dbnomics_name = if (nzchar(current_series) && identical(current_series, restored_series)) provider_clean_scalar_text(restored_spec$dbnomics_name %||% "") else ""
  )

  if (!nzchar(spec$dbnomics_series)) {
    return(NULL)
  }

  if (!nzchar(spec$label)) {
    spec$label <- provider_dbnomics_default_label(spec)
  }

  spec
}

provider_dbnomics_normalize_spec <- function(spec, normalized_spec = NULL) {
  normalized_spec <- normalized_spec %||% list(
    index = as.integer(spec$index),
    source = "dbnomics",
    label = trimws(spec$label %||% ""),
    transform_profile = normalize_transform_profile(spec$transform_profile),
    vis_type = spec$vis_type %||% "line"
  )

  normalized_spec$source <- "dbnomics"
  normalized_spec$dbnomics_series <- trimws(spec$dbnomics_series %||% "")
  normalized_spec$dbnomics_name <- trimws(spec$dbnomics_name %||% "")
  normalized_spec
}

provider_dbnomics_query_series_history <- function(spec) {
  db_data(series = spec$dbnomics_series)
}

provider_dbnomics_restore_controls <- function(session, index, spec) {
  series_id <- provider_clean_scalar_text(spec$dbnomics_series %||% "")
  if (!nzchar(series_id)) {
    return(invisible(NULL))
  }

  updateSelectizeInput(
    session,
    series_input_id(index, "dbnomics_series"),
    choices = seed_selectize_choices(series_id),
    selected = series_id,
    server = TRUE
  )
}

provider_dbnomics_search_controls_ui <- function(input = NULL) {
  selected_provider <- trimws(input[["search_dbnomics_provider"]] %||% "")
  selected_dataset <- if (nzchar(selected_provider)) trimws(input[["search_dbnomics_dataset"]] %||% "") else ""
  provider_choices <- provider_merge_choice_values(
    c("All providers" = ""),
    dbnomics_provider_choice_values(),
    seed_selectize_choices(selected_provider)
  )
  dataset_choices <- provider_merge_choice_values(
    c(if (nzchar(selected_provider)) "All datasets" else "Choose provider first" = ""),
    dbnomics_dataset_choice_values(selected_provider),
    seed_selectize_choices(selected_dataset)
  )

  tagList(
    div(
      class = "search-toolbar__group search-toolbar__group--select",
      div(
        class = "search-toolbar__group-label",
        "DBnomics provider"
      ),
      selectizeInput(
        "search_dbnomics_provider",
        label = NULL,
        choices = provider_choices,
        selected = if (nzchar(selected_provider)) selected_provider else "",
        options = list(
          create = TRUE,
          persist = FALSE,
          maxItems = 1,
          placeholder = "All providers"
        )
      )
    ),
    div(
      class = "search-toolbar__group search-toolbar__group--select",
      div(
        class = "search-toolbar__group-label",
        "DBnomics dataset"
      ),
      selectizeInput(
        "search_dbnomics_dataset",
        label = NULL,
        choices = dataset_choices,
        selected = if (nzchar(selected_dataset)) selected_dataset else "",
        options = list(
          create = TRUE,
          persist = FALSE,
          maxItems = 1,
          placeholder = if (nzchar(selected_provider)) "All datasets" else "Choose provider first"
        )
      )
    )
  )
}

provider_dbnomics_search_context_from_input <- function(input) {
  list(
    provider_code = trimws(input$search_dbnomics_provider %||% ""),
    dataset_code = trimws(input$search_dbnomics_dataset %||% "")
  )
}

provider_dbnomics_search_remote <- function(query, search_context = list(), force = FALSE) {
  search_dbnomics_series(
    query = query,
    provider_code = search_context$provider_code %||% "",
    dataset_code = search_context$dataset_code %||% "",
    limit = search_context$limit %||% 100,
    force = force
  )
}

provider_dbnomics_search_result_lookup <- function(series_id, search_context = list()) {
  cleaned_id <- trimws(series_id %||% "")
  provider_code <- trimws(search_context$provider_code %||% "")
  dataset_code <- trimws(search_context$dataset_code %||% "")

  if (!nzchar(cleaned_id)) {
    return(list(result = NULL, status = "Type a DBnomics series ID and press Enter."))
  }

  lookup_response <- search_dbnomics_series(
    query = cleaned_id,
    provider_code = provider_code,
    dataset_code = dataset_code,
    limit = search_context$limit %||% 25,
    force = TRUE
  )

  parsed_id <- parse_dbnomics_series_id(cleaned_id)
  exact_id <- if (!is.null(parsed_id)) {
    trimws(parsed_id$exact_id %||% "")
  } else if (nzchar(provider_code) && nzchar(dataset_code)) {
    paste(provider_code, dataset_code, cleaned_id, sep = "/")
  } else {
    ""
  }

  exact_result <- provider_exact_search_result(
    lookup_response$results,
    function(payload) {
      payload_id <- trimws(payload$dbnomics_series %||% "")
      payload_id == exact_id || payload_id == cleaned_id
    }
  )

  if (is.null(exact_result)) {
    status_text <- trimws(lookup_response$status %||% "")
    if (!nzchar(status_text)) {
      status_text <- sprintf("No exact DBnomics match was found for %s.", cleaned_id)
    }

    return(list(result = NULL, status = status_text))
  }

  list(
    result = exact_result,
    status = sprintf("Actual series: %s", trimws(exact_result$title[[1]] %||% cleaned_id))
  )
}

provider_dbnomics_search_result_series_id <- function(search_result) {
  trimws(search_result$load_payload[[1]]$dbnomics_series %||% "")
}

provider_dbnomics_recent_title <- function(spec) {
  title_text <- provider_clean_scalar_text(spec$dbnomics_name %||% "")

  if (nzchar(title_text)) {
    return(title_text)
  }

  provider_clean_scalar_text(spec$dbnomics_series %||% "DBnomics series")
}

provider_dbnomics_default_label <- function(spec) {
  title_text <- provider_clean_scalar_text(spec$dbnomics_name %||% "")

  if (nzchar(title_text)) {
    return(title_text)
  }

  provider_clean_scalar_text(spec$dbnomics_series %||% "")
}

provider_dbnomics_source_note_value <- function(spec) {
  id_values <- unique(trimws(as.character(spec$dbnomics_series %||% character())))
  id_values <- id_values[nzchar(id_values)]

  if (length(id_values) == 0) {
    "dbnomics"
  } else {
    paste0("dbnomics - ", paste(id_values, collapse = ", "))
  }
}

provider_dbnomics_cache_key <- function(spec) {
  paste("dbnomics", cache_key_value(spec$dbnomics_series), sep = "::")
}

hydrate_rba_spec_from_series_ids <- function(spec = list()) {
  spec <- spec %||% list()
  selected_ids <- unique(trimws(as.character(spec$rba_series_id %||% character())))
  selected_ids <- selected_ids[nzchar(selected_ids)]

  if (length(selected_ids) == 0) {
    return(spec)
  }

  matching_rows <- get_rba_desc_id() %>%
    filter(series_id %in% selected_ids)

  if (nrow(matching_rows) == 0) {
    spec$rba_series_id <- selected_ids
    return(spec)
  }

  spec$rba_series_id <- selected_ids
  spec$rba_table <- spec$rba_table %||% matching_rows$table_no[[1]]
  spec$rba_desc <- spec$rba_desc %||% unique(matching_rows$description)
  spec
}

provider_rba_controls_ui <- function(input, session, index, restored_spec = NULL) {
  current_series_id <- provider_clean_scalar_text(input[[series_input_id(index, "rba_series_id")]] %||% restored_spec$rba_series_id %||% "")
  restored_series_id <- provider_clean_scalar_text(restored_spec$rba_series_id %||% "")
  restored_desc <- provider_clean_text_values(restored_spec$rba_desc %||% character())
  lookup <- provider_series_helper_text(
    current_value = current_series_id,
    restored_value = restored_series_id,
    restored_title = restored_desc,
    empty_status = "Type an RBA series ID and press Enter.",
    lookup_fun = provider_rba_search_result_lookup
  )

  tagList(
    selectizeInput(
      series_input_id(index, "rba_series_id"),
      "RBA series ID",
      choices = seed_selectize_choices(current_series_id),
      selected = if (nzchar(trimws(current_series_id))) current_series_id else NULL,
      options = list(
        create = TRUE,
        persist = FALSE,
        maxItems = 1,
        placeholder = "Type an RBA series ID and press Enter"
      )
    ),
    tags$p(class = "muted-copy", trimws(lookup$status %||% ""))
  )
}

provider_rba_register_dependencies <- function(input, output, session, index) {
  invisible(NULL)
}

provider_rba_spec_from_input <- function(input, index, transform_profile = default_transform_profile(), restored_spec = NULL) {
  current_series_id <- provider_clean_scalar_text(input[[series_input_id(index, "rba_series_id")]] %||% restored_spec$rba_series_id %||% "")
  restored_series_id <- provider_clean_scalar_text(restored_spec$rba_series_id %||% "")
  use_restored_metadata <- nzchar(current_series_id) && identical(current_series_id, restored_series_id)
  restored_desc <- provider_clean_text_values(restored_spec$rba_desc %||% character())

  spec <- list(
    index = index,
    source = "rba",
    label = trimws(input[[series_input_id(index, "label")]] %||% ""),
    transform_profile = transform_profile,
    vis_type = input[[series_input_id(index, "vis_type")]] %||% "line",
    rba_series_id = current_series_id,
    rba_table = if (use_restored_metadata) restored_spec$rba_table %||% NULL else NULL,
    rba_desc = if (use_restored_metadata) restored_desc else character()
  )

  if (!nzchar(spec$rba_series_id)) {
    return(NULL)
  }

  spec <- hydrate_rba_spec_from_series_ids(spec)

  if (!nzchar(spec$label %||% "")) {
    spec$label <- provider_rba_default_label(spec)
  }

  spec
}

provider_rba_normalize_spec <- function(spec, normalized_spec = NULL) {
  normalized_spec <- normalized_spec %||% list(
    index = as.integer(spec$index),
    source = "rba",
    label = trimws(spec$label %||% ""),
    transform_profile = normalize_transform_profile(spec$transform_profile),
    vis_type = spec$vis_type %||% "line"
  )

  normalized_spec$source <- "rba"
  hydrated_spec <- hydrate_rba_spec_from_series_ids(spec)
  normalized_spec$rba_table <- hydrated_spec$rba_table %||% rba_tables[[1]]
  normalized_spec$rba_desc <- hydrated_spec$rba_desc %||% character()
  normalized_spec$rba_series_id <- hydrated_spec$rba_series_id %||% character()

  if (!nzchar(normalized_spec$label %||% "")) {
    normalized_spec$label <- provider_rba_default_label(normalized_spec)
  }

  normalized_spec
}

provider_rba_query_series_history <- function(spec) {
  series_ids <- spec$rba_series_id %||% character()

  if (length(series_ids) == 0 && length(spec$rba_desc %||% character()) > 0) {
    series_ids <- get_rba_desc_id() %>%
      filter(table_no == (spec$rba_table %||% rba_tables[[1]]), description %in% spec$rba_desc) %>%
      pull(series_id) %>%
      unique()
  }

  rba_data(series = series_ids)
}

provider_rba_restore_controls <- function(session, index, spec) {
  series_id <- provider_clean_scalar_text(spec$rba_series_id %||% "")
  if (!nzchar(series_id)) {
    return(invisible(NULL))
  }

  updateSelectizeInput(
    session,
    series_input_id(index, "rba_series_id"),
    choices = seed_selectize_choices(series_id),
    selected = series_id,
    server = TRUE
  )
}

provider_rba_search_controls_ui <- function(input = NULL) {
  selected_table <- trimws(input[["search_rba_table"]] %||% "")
  table_choices <- provider_merge_choice_values(
    c("All tables" = ""),
    rba_table_choices,
    seed_selectize_choices(selected_table)
  )

  div(
    class = "search-toolbar__group search-toolbar__group--select",
    div(
      class = "search-toolbar__group-label",
      "RBA table"
    ),
    selectizeInput(
      "search_rba_table",
      label = NULL,
      choices = table_choices,
      selected = if (nzchar(selected_table)) selected_table else "",
      options = list(
        create = FALSE,
        maxItems = 1,
        placeholder = "All tables"
      )
    )
  )
}

provider_rba_search_context_from_input <- function(input) {
  list(
    table = trimws(input$search_rba_table %||% "")
  )
}

provider_rba_filter_search_results <- function(search_results, search_context = list()) {
  selected_table <- trimws(search_context$table %||% "")

  if (!nzchar(selected_table)) {
    return(search_results)
  }

  provider_filter_search_rows(
    search_results,
    function(payload) identical(trimws(payload$rba_table %||% ""), selected_table)
  )
}

provider_rba_search_result_lookup <- function(series_id, search_context = list()) {
  cleaned_id <- trimws(series_id %||% "")

  if (!nzchar(cleaned_id)) {
    return(list(result = NULL, status = "Type an RBA series ID and press Enter."))
  }

  exact_result <- provider_exact_search_result(
    build_rba_search_index(),
    function(payload) cleaned_id %in% trimws(as.character(payload$rba_series_id %||% character()))
  )

  if (is.null(exact_result)) {
    return(list(result = NULL, status = sprintf("No exact RBA match was found for %s.", cleaned_id)))
  }

  list(
    result = exact_result,
    status = sprintf("Actual series: %s", trimws(exact_result$load_payload[[1]]$rba_desc %||% exact_result$title[[1]] %||% cleaned_id))
  )
}

provider_rba_search_result_to_spec <- function(search_result, index) {
  provider_generic_search_result_to_spec(search_result, index)
}

provider_rba_search_result_series_id <- function(search_result) {
  payload <- search_result$load_payload[[1]] %||% list()
  cache_key_value(payload$rba_series_id %||% character())
}

provider_rba_recent_title <- function(spec) {
  title_values <- provider_clean_text_values(spec$rba_desc %||% character())

  if (length(title_values) == 0) {
    title_values <- provider_clean_text_values(spec$rba_series_id %||% character())
  }

  paste(title_values, collapse = ", ")
}

provider_rba_default_label <- function(spec) {
  label_values <- provider_clean_text_values(spec$rba_desc %||% character())

  if (length(label_values) == 0) {
    return(provider_clean_scalar_text(spec$rba_series_id %||% ""))
  }

  paste(label_values, collapse = ", ")
}

provider_rba_source_note_value <- function(spec) {
  id_values <- provider_clean_text_values(spec$rba_series_id %||% spec$rba_desc %||% character())

  if (length(id_values) == 0) {
    "rba"
  } else {
    paste0("rba - ", paste(id_values, collapse = ", "))
  }
}

provider_rba_cache_key <- function(spec) {
  paste(
    "rba",
    cache_key_value(spec$rba_table),
    cache_key_value(spec$rba_desc),
    cache_key_value(spec$rba_series_id),
    sep = "::"
  )
}

provider_rba_search_index_builder <- function() {
  rba_browse_data %>%
    distinct(table_no, table_title, description, series_id, frequency) %>%
    transmute(
      search_id = paste("rba", series_id, sep = "::"),
      title = paste(table_no, description),
      source = "RBA",
      type_code = vapply(
        seq_along(description),
        function(index) classify_data_type(description[[index]], table_title[[index]], "RBA"),
        character(1)
      ),
      location_code = vapply(
        seq_along(description),
        function(index) classify_location_code(description[[index]], table_title[[index]], "RBA"),
        character(1)
      ),
      frequency = vapply(frequency, format_frequency, character(1)),
      start_date = as.Date(NA),
      end_date = as.Date(NA),
      summary = compact_rba_table_title(table_no, table_title),
      search_text = clean_search_text(paste(table_no, table_title, description, series_id, "rba")),
      load_payload = purrr::pmap(
        list(table_no, description, series_id),
        function(table_no, description, series_id) {
          list(
            source = "rba",
            rba_table = table_no,
            rba_desc = description,
            rba_series_id = series_id,
            label = description,
            vis_type = "line",
            transform_profile = default_transform_profile()
          )
        }
      )
    )
}

provider_abs_controls_ui <- function(input, session, index, restored_spec = NULL) {
  current_abs_id <- provider_clean_scalar_text(input[[series_input_id(index, "abs_id")]] %||% restored_spec$abs_id %||% "")
  restored_abs_id <- provider_clean_scalar_text(restored_spec$abs_id %||% "")
  restored_desc <- provider_clean_text_values(restored_spec$abs_desc %||% character())
  lookup <- provider_series_helper_text(
    current_value = current_abs_id,
    restored_value = restored_abs_id,
    restored_title = restored_desc,
    empty_status = "Type an ABS series ID and press Enter.",
    lookup_fun = provider_abs_search_result_lookup
  )

  tagList(
    selectizeInput(
      series_input_id(index, "abs_id"),
      "ABS series ID",
      choices = seed_selectize_choices(current_abs_id),
      selected = if (nzchar(current_abs_id)) current_abs_id else NULL,
      options = list(
        create = TRUE,
        persist = FALSE,
        maxItems = 1,
        placeholder = "Type an ABS series ID and press Enter"
      )
    ),
    tags$p(class = "muted-copy", trimws(lookup$status %||% ""))
  )
}

provider_abs_search_controls_ui <- function(input = NULL) {
  selected_catalogue <- trimws(input[["search_abs_catalogue"]] %||% "")
  selected_tables <- unique(trimws(as.character(input[["search_abs_table"]] %||% character())))
  selected_tables <- selected_tables[nzchar(selected_tables)]
  selected_type <- trimws(input[["search_abs_series_type"]] %||% "all")
  table_values <- if (nzchar(selected_catalogue)) {
    abs_choice_values(abs_catalogue_data(selected_catalogue), column = table_title)
  } else {
    character()
  }
  selected_tables <- selected_tables[selected_tables %in% table_values]
  table_choices <- provider_merge_choice_values(
    c("All tables" = ""),
    stats::setNames(table_values, table_values),
    seed_selectize_choices(selected_tables)
  )

  tagList(
    div(
      class = "search-toolbar__group search-toolbar__group--select",
      div(
        class = "search-toolbar__group-label",
        "ABS catalogue"
      ),
      selectizeInput(
        "search_abs_catalogue",
        label = NULL,
        choices = c("All catalogues" = "", stats::setNames(abs_cat, abs_cat)),
        selected = if (nzchar(selected_catalogue)) selected_catalogue else "",
        options = list(
          create = FALSE,
          maxItems = 1,
          placeholder = "All catalogues"
        )
      )
    ),
    div(
      class = "search-toolbar__group search-toolbar__group--select",
      div(
        class = "search-toolbar__group-label",
        "ABS table"
      ),
      selectizeInput(
        "search_abs_table",
        label = NULL,
        choices = table_choices,
        selected = selected_tables,
        multiple = TRUE,
        options = list(
          create = FALSE,
          placeholder = if (nzchar(selected_catalogue)) "All tables" else "Choose a catalogue to narrow tables"
        )
      )
    ),
    div(
      class = "search-toolbar__group search-toolbar__group--select",
      div(
        class = "search-toolbar__group-label",
        "ABS type"
      ),
      compact_single_choice_input(
        "search_abs_series_type",
        label = NULL,
        choices = c(
          "All" = "all",
          "Original" = "Original",
          "Seasonally adjusted" = "Seasonally Adjusted",
          "Trend" = "Trend"
        ),
        selected = if (selected_type %in% c("all", "Original", "Seasonally Adjusted", "Trend")) selected_type else "all"
      )
    )
  )
}

provider_abs_search_context_from_input <- function(input) {
  selected_tables <- unique(trimws(as.character(input$search_abs_table %||% character())))

  list(
    catalogue = trimws(input$search_abs_catalogue %||% ""),
    tables = selected_tables[nzchar(selected_tables)],
    series_type = trimws(input$search_abs_series_type %||% "all")
  )
}

provider_abs_filter_search_results <- function(search_results, search_context = list()) {
  selected_catalogue <- trimws(search_context$catalogue %||% "")
  selected_tables <- unique(trimws(as.character(search_context$tables %||% character())))
  selected_tables <- selected_tables[nzchar(selected_tables)]
  selected_type <- trimws(search_context$series_type %||% "all")

  provider_filter_search_rows(
    search_results,
    function(payload) {
      payload_catalogue <- trimws(payload$abs_catalogue %||% "")
      payload_table <- trimws(payload$abs_table %||% "")
      payload_type <- trimws(payload$abs_series_type %||% "")

      (!nzchar(selected_catalogue) || identical(payload_catalogue, selected_catalogue)) &&
        (length(selected_tables) == 0 || payload_table %in% selected_tables) &&
        (!nzchar(selected_type) || identical(selected_type, "all") || identical(payload_type, selected_type))
    }
  )
}

provider_abs_search_result_lookup <- function(series_id, search_context = list()) {
  cleaned_id <- trimws(series_id %||% "")

  if (!nzchar(cleaned_id)) {
    return(list(result = NULL, status = "Type an ABS series ID and press Enter."))
  }

  exact_result <- provider_exact_search_result(
    build_abs_search_index(),
    function(payload) cleaned_id %in% trimws(as.character(payload$abs_id %||% character()))
  )

  if (is.null(exact_result)) {
    return(list(result = NULL, status = sprintf("No exact ABS match was found for %s.", cleaned_id)))
  }

  list(
    result = exact_result,
    status = sprintf("Actual series: %s", trimws(exact_result$load_payload[[1]]$abs_desc %||% exact_result$title[[1]] %||% cleaned_id))
  )
}

provider_abs_register_dependencies <- function(input, output, session, index) {
  invisible(NULL)
}

provider_abs_spec_from_input <- function(input, index, transform_profile = default_transform_profile(), restored_spec = NULL) {
  spec <- list(
    index = index,
    source = "abs",
    label = trimws(input[[series_input_id(index, "label")]] %||% ""),
    transform_profile = transform_profile,
    vis_type = input[[series_input_id(index, "vis_type")]] %||% "line"
  )

  direct_abs_ids <- provider_clean_text_values(input[[series_input_id(index, "abs_id")]] %||% restored_spec$abs_id %||% character())

  if (length(direct_abs_ids) > 0) {
    spec$abs_id <- direct_abs_ids
    spec <- hydrate_abs_spec_from_ids(spec)
  } else {
    abs_state <- resolve_abs_control_state(
      current_values = list(
        catalogue = input[[series_input_id(index, "abs_catalogue")]],
        desc = input[[series_input_id(index, "abs_desc")]],
        type = input[[series_input_id(index, "abs_series_type")]],
        table = input[[series_input_id(index, "abs_table")]],
        ids = input[[series_input_id(index, "abs_id")]]
      ),
      restored_spec = restored_spec
    )
    spec$abs_catalogue <- abs_state$catalogue
    spec$abs_desc <- abs_state$desc
    spec$abs_series_type <- abs_state$series_type
    spec$abs_table <- abs_state$table
    spec$abs_id <- abs_state$ids
  }

  if (length(spec$abs_id %||% character()) == 0) {
    return(NULL)
  }

  if (!nzchar(spec$label %||% "")) {
    spec$label <- provider_abs_default_label(spec)
  }

  spec
}

provider_abs_normalize_spec <- function(spec, normalized_spec = NULL) {
  normalized_spec <- normalized_spec %||% list(
    index = as.integer(spec$index),
    source = "abs",
    label = trimws(spec$label %||% ""),
    transform_profile = normalize_transform_profile(spec$transform_profile),
    vis_type = spec$vis_type %||% "line"
  )

  normalized_spec$source <- "abs"
  hydrated_spec <- hydrate_abs_spec_from_ids(spec)
  abs_state <- resolve_abs_control_state(
    current_values = list(
      catalogue = hydrated_spec$abs_catalogue,
      desc = hydrated_spec$abs_desc,
      type = hydrated_spec$abs_series_type,
      table = hydrated_spec$abs_table,
      ids = hydrated_spec$abs_id
    ),
    restored_spec = hydrated_spec
  )
  normalized_spec$abs_catalogue <- abs_state$catalogue
  normalized_spec$abs_desc <- abs_state$desc
  normalized_spec$abs_series_type <- abs_state$series_type
  normalized_spec$abs_table <- abs_state$table
  normalized_spec$abs_id <- abs_state$ids

  if (!nzchar(normalized_spec$label %||% "")) {
    normalized_spec$label <- provider_abs_default_label(normalized_spec)
  }

  normalized_spec
}

provider_abs_query_series_history <- function(spec) {
  abs_data(series = spec$abs_id)
}

provider_abs_restore_controls <- function(session, index, spec) {
  series_id <- provider_clean_scalar_text(spec$abs_id %||% "")
  if (!nzchar(series_id)) {
    return(invisible(NULL))
  }

  updateSelectizeInput(
    session,
    series_input_id(index, "abs_id"),
    choices = seed_selectize_choices(series_id),
    selected = series_id,
    server = TRUE
  )
}

provider_abs_search_result_series_id <- function(search_result) {
  cache_key_value(search_result$load_payload[[1]]$abs_id %||% character())
}

provider_abs_recent_title <- function(spec) {
  provider_clean_scalar_text(spec$abs_desc %||% spec$abs_id %||% "ABS series")
}

provider_abs_default_label <- function(spec) {
  desc_values <- provider_clean_text_values(spec$abs_desc %||% character())

  if (length(desc_values) == 1) {
    return(desc_values[[1]])
  }

  selected_ids <- provider_clean_text_values(spec$abs_id %||% character())

  if (length(selected_ids) == 1) {
    return(selected_ids[[1]])
  }

  ""
}

provider_abs_source_note_value <- function(spec) {
  id_values <- provider_clean_text_values(spec$abs_id %||% character())

  if (length(id_values) == 0) {
    "abs"
  } else {
    paste0("abs - ", paste(id_values, collapse = ", "))
  }
}

provider_abs_cache_key <- function(spec) {
  paste(
    "abs",
    cache_key_value(spec$abs_catalogue),
    cache_key_value(spec$abs_desc),
    cache_key_value(spec$abs_series_type),
    cache_key_value(spec$abs_table),
    cache_key_value(spec$abs_id),
    sep = "::"
  )
}

provider_abs_search_index_builder <- function() {
  catalogue_labels <- abs_cat

  if (length(catalogue_labels) < length(abs_ref)) {
    fallback_names <- names(abs_ref)
    catalogue_labels <- c(
      catalogue_labels,
      fallback_names[seq.int(length(catalogue_labels) + 1, length(abs_ref))]
    )
  }

  purrr::map2_dfr(abs_ref, catalogue_labels, function(reference_data, catalogue_name) {
    catalogue_name <- catalogue_name %||% ""

    reference_data %>%
      distinct(table_title, series, series_type, frequency, unit, series_id) %>%
      transmute(
        search_id = paste("abs", series_id, sep = "::"),
        title = paste(series, series_type, sep = " - "),
        source = "ABS",
        type_code = vapply(
          seq_along(series),
          function(index) classify_data_type(series[[index]], paste(series_type[[index]], table_title[[index]], unit[[index]]), "ABS"),
          character(1)
        ),
        location_code = vapply(
          seq_along(series),
          function(index) classify_location_code(series[[index]], paste(series_type[[index]], table_title[[index]], unit[[index]]), "ABS"),
          character(1)
        ),
        frequency = vapply(frequency, format_frequency, character(1)),
        start_date = as.Date(NA),
        end_date = as.Date(NA),
        summary = paste(catalogue_name, table_title, sep = " | "),
        search_text = clean_search_text(paste(catalogue_name, table_title, series, series_type, unit, series_id, "abs")),
        load_payload = purrr::pmap(
          list(catalogue_name, series, series_type, table_title, series_id),
          function(catalogue_name, series, series_type, table_title, series_id) {
            list(
              source = "abs",
              abs_catalogue = catalogue_name,
              abs_desc = series,
              abs_series_type = series_type,
              abs_table = table_title,
              abs_id = series_id,
              label = series,
              vis_type = "line",
              transform_profile = default_transform_profile()
            )
          }
        )
      )
  })
}

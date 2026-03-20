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
      search_controls_ui = provider_fred_search_controls_ui,
      search_context_from_input = provider_fred_search_context_from_input,
      show_search_controls_in_all = TRUE,
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
      show_search_controls_in_all = FALSE,
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

provider_registry_search_controls_ui <- function(source_filter = "all") {
  selected_filter <- source_filter %||% "all"
  controls <- lapply(provider_registry_entries(), function(entry) {
    controls_ui <- entry$search_controls_ui %||% NULL

    if (is.null(controls_ui)) {
      return(NULL)
    }

    if (identical(selected_filter, "all") && !isTRUE(entry$show_search_controls_in_all)) {
      return(NULL)
    }

    if (!identical(selected_filter, "all") &&
        !identical(provider_registry_source_id(selected_filter), entry$id %||% "")) {
      return(NULL)
    }

    controls_ui()
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

provider_fred_controls_ui <- function(input, session, index, restored_spec = NULL) {
  current_series <- input[[series_input_id(index, "fred_series")]] %||% restored_spec$fred_series %||% ""
  vintage_mode <- input[[series_input_id(index, "fred_vintage_mode")]] %||% restored_spec$fred_vintage_mode %||% "current"
  selected_vintage <- input[[series_input_id(index, "fred_vintage_date")]] %||%
    if (!is.null(restored_spec$fred_vintage_date) && !is.na(restored_spec$fred_vintage_date)) format(as.Date(restored_spec$fred_vintage_date), "%Y-%m-%d") else ""
  vintage_choices <- fred_vintage_choice_values(current_series, selected_vintage)
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
  spec <- list(
    index = index,
    source = "FRED",
    label = trimws(input[[series_input_id(index, "label")]] %||% ""),
    transform_profile = transform_profile,
    vis_type = input[[series_input_id(index, "vis_type")]] %||% "line",
    fred_series = trimws(input[[series_input_id(index, "fred_series")]] %||% ""),
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
  updateSelectizeInput(
    session,
    series_input_id(index, "fred_series"),
    choices = seed_selectize_choices(spec$fred_series %||% ""),
    selected = spec$fred_series %||% "",
    server = TRUE
  )
  updateRadioGroupButtons(session, series_input_id(index, "fred_vintage_mode"), selected = spec$fred_vintage_mode %||% "current")

  if (!identical(spec$fred_vintage_mode %||% "current", "current")) {
    updateSelectizeInput(
      session,
      series_input_id(index, "fred_vintage_date"),
      choices = fred_vintage_choice_values(spec$fred_series %||% "", spec$fred_vintage_date),
      selected = if (!is.null(spec$fred_vintage_date) && !is.na(spec$fred_vintage_date)) format(as.Date(spec$fred_vintage_date), "%Y-%m-%d") else NULL,
      server = TRUE
    )
  }
}

provider_fred_search_controls_ui <- function() {
  div(
    class = "search-toolbar__group search-toolbar__group--segmented search-toolbar__group--compact",
    style = "padding-bottom: 0;",
    div(
      class = "search-toolbar__group-label",
      "FRED mode"
    ),
    div(
      class = "search-segmented-control",
      radioGroupButtons(
        "search_fred_mode",
        NULL,
        choices = c("Text" = "full_text", "ID" = "series_id"),
        selected = "full_text",
        justified = FALSE,
        checkIcon = list(yes = icon("check"))
      )
    )
  )
}

provider_fred_search_context_from_input <- function(input) {
  list(
    frequency_filter = input$search_frequency_filter %||% "all",
    search_type = input$search_fred_mode %||% "full_text"
  )
}

provider_fred_search_remote <- function(query, search_context = list(), force = FALSE) {
  search_fred_series(
    query = query,
    frequency_filter = search_context$frequency_filter %||% "all",
    search_type = search_context$search_type %||% "full_text",
    limit = search_context$limit %||% 100,
    force = force
  )
}

provider_fred_search_result_series_id <- function(search_result) {
  trimws(search_result$load_payload[[1]]$fred_series %||% "")
}

provider_fred_recent_title <- function(spec) {
  base_title <- trimws(spec$fred_series %||% "FRED series")
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
  trimws(spec$fred_series %||% "")
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
  textInput(
    series_input_id(index, "dbnomics_series"),
    "DBnomics series ID",
    value = input[[series_input_id(index, "dbnomics_series")]] %||% restored_spec$dbnomics_series %||% "AMECO/ZUTN/EA19.0.0.0.0.ZUTN"
  )
}

provider_dbnomics_spec_from_input <- function(input, index, transform_profile = default_transform_profile(), restored_spec = NULL) {
  spec <- list(
    index = index,
    source = "dbnomics",
    label = trimws(input[[series_input_id(index, "label")]] %||% ""),
    transform_profile = transform_profile,
    vis_type = input[[series_input_id(index, "vis_type")]] %||% "line",
    dbnomics_series = trimws(input[[series_input_id(index, "dbnomics_series")]] %||% restored_spec$dbnomics_series %||% "")
  )

  if (!nzchar(spec$dbnomics_series)) {
    return(NULL)
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
  normalized_spec
}

provider_dbnomics_query_series_history <- function(spec) {
  db_data(series = spec$dbnomics_series)
}

provider_dbnomics_restore_controls <- function(session, index, spec) {
  updateTextInput(session, series_input_id(index, "dbnomics_series"), value = spec$dbnomics_series %||% "")
}

provider_dbnomics_search_controls_ui <- function() {
  tagList(
    div(
      class = "search-toolbar__group search-toolbar__group--select",
      div(
        class = "search-toolbar__group-label",
        "DBnomics provider"
      ),
      textInput("search_dbnomics_provider", label = NULL, value = "", placeholder = "e.g. IMF")
    ),
    div(
      class = "search-toolbar__group search-toolbar__group--select",
      div(
        class = "search-toolbar__group-label",
        "DBnomics dataset"
      ),
      textInput("search_dbnomics_dataset", label = NULL, value = "", placeholder = "e.g. WEO:2024-10")
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

provider_dbnomics_search_result_series_id <- function(search_result) {
  trimws(search_result$load_payload[[1]]$dbnomics_series %||% "")
}

provider_dbnomics_recent_title <- function(spec) {
  trimws(spec$dbnomics_series %||% "DBnomics series")
}

provider_dbnomics_default_label <- function(spec) {
  ""
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

provider_rba_controls_ui <- function(input, session, index, restored_spec = NULL) {
  current_table <- input[[series_input_id(index, "rba_table")]] %||% restored_spec$rba_table %||% rba_tables[[1]]
  table_choices <- rba_series[[current_table]] %||% rba_series[[1]]
  selected_series <- input[[series_input_id(index, "rba_desc")]] %||% restored_spec$rba_desc %||% character()

  tagList(
    compact_single_choice_input(
      series_input_id(index, "rba_table"),
      "RBA table",
      choices = rba_table_choices,
      selected = current_table
    ),
    selectizeInput(
      series_input_id(index, "rba_desc"),
      "RBA series",
      choices = table_choices,
      selected = selected_series,
      multiple = TRUE
    )
  )
}

provider_rba_register_dependencies <- function(input, output, session, index) {
  observeEvent(input[[series_input_id(index, "rba_table")]], {
    if (!identical(input[[series_input_id(index, "source")]] %||% "", "rba")) {
      return(invisible(NULL))
    }

    table_value <- input[[series_input_id(index, "rba_table")]]
    series_choices <- rba_series[[table_value]] %||% character()

    updateSelectizeInput(
      session,
      series_input_id(index, "rba_desc"),
      choices = series_choices,
      selected = character(),
      server = TRUE
    )
  }, ignoreInit = FALSE)

  invisible(NULL)
}

provider_rba_spec_from_input <- function(input, index, transform_profile = default_transform_profile(), restored_spec = NULL) {
  selected_table <- input[[series_input_id(index, "rba_table")]] %||% restored_spec$rba_table %||% rba_tables[[1]]
  selected_desc <- input[[series_input_id(index, "rba_desc")]] %||% restored_spec$rba_desc
  selected_series_id <- get_rba_desc_id() %>%
    filter(table_no == selected_table, description %in% selected_desc) %>%
    pull(series_id) %>%
    unique()

  spec <- list(
    index = index,
    source = "rba",
    label = trimws(input[[series_input_id(index, "label")]] %||% ""),
    transform_profile = transform_profile,
    vis_type = input[[series_input_id(index, "vis_type")]] %||% "line",
    rba_table = selected_table,
    rba_desc = selected_desc,
    rba_series_id = if (length(selected_series_id) > 0) selected_series_id else restored_spec$rba_series_id %||% character()
  )

  if (length(spec$rba_desc) == 0) {
    return(NULL)
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
  normalized_spec$rba_table <- spec$rba_table %||% rba_tables[[1]]
  normalized_spec$rba_desc <- spec$rba_desc %||% character()
  normalized_spec$rba_series_id <- spec$rba_series_id %||% character()

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
  rba_choices <- rba_series[[spec$rba_table]] %||% character()
  updateSelectInput(session, series_input_id(index, "rba_table"), selected = spec$rba_table)
  updateSelectizeInput(
    session,
    series_input_id(index, "rba_desc"),
    choices = rba_choices,
    selected = spec$rba_desc,
    server = TRUE
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
  paste(spec$rba_desc %||% character(), collapse = ", ")
}

provider_rba_default_label <- function(spec) {
  ""
}

provider_rba_source_note_value <- function(spec) {
  id_values <- as.character(spec$rba_desc %||% character())
  id_values <- unique(trimws(id_values[nzchar(trimws(id_values))]))

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
  current_abs_ids <- unique(trimws(as.character(input[[series_input_id(index, "abs_id")]] %||% restored_spec$abs_id %||% character())))
  current_abs_ids <- current_abs_ids[nzchar(current_abs_ids)]
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

  tagList(
    selectizeInput(
      series_input_id(index, "abs_catalogue"),
      "ABS catalogue",
      choices = abs_cat,
      selected = abs_state$catalogue,
      multiple = FALSE,
      options = list(placeholder = "Select a catalogue")
    ),
    selectizeInput(
      series_input_id(index, "abs_desc"),
      "ABS series description",
      choices = abs_state$desc_choices,
      selected = abs_state$desc,
      multiple = FALSE,
      options = list(placeholder = "Select a series description")
    ),
    selectizeInput(
      series_input_id(index, "abs_series_type"),
      "ABS series type",
      choices = abs_state$type_choices,
      selected = abs_state$series_type,
      multiple = FALSE,
      options = list(placeholder = "Select a series type")
    ),
    selectInput(
      series_input_id(index, "abs_table"),
      "ABS table",
      choices = c("Select a table" = "", stats::setNames(abs_state$table_choices, abs_state$table_choices)),
      selected = abs_state$table
    ),
    selectizeInput(
      series_input_id(index, "abs_id"),
      "ABS series ID",
      choices = seed_selectize_choices(c(abs_state$id_choices, current_abs_ids)),
      selected = current_abs_ids %||% abs_state$ids,
      options = list(create = TRUE, placeholder = "Select a series ID"),
      multiple = TRUE
    )
  )
}

provider_abs_register_dependencies <- function(input, output, session, index) {
  observeEvent(input[[series_input_id(index, "abs_catalogue")]], {
    if (!identical(input[[series_input_id(index, "source")]] %||% "", "abs")) {
      return(invisible(NULL))
    }

    restored_spec_value <- restored_series_spec(session, index)
    abs_state <- resolve_abs_control_state(
      current_values = list(
        catalogue = input[[series_input_id(index, "abs_catalogue")]],
        desc = input[[series_input_id(index, "abs_desc")]]
      ),
      restored_spec = restored_spec_value
    )

    updateSelectizeInput(
      session,
      series_input_id(index, "abs_desc"),
      choices = abs_state$desc_choices,
      selected = abs_state$desc,
      server = TRUE
    )
  }, ignoreInit = FALSE)

  observeEvent(
    {
      list(
        input[[series_input_id(index, "abs_catalogue")]],
        input[[series_input_id(index, "abs_desc")]]
      )
    },
    {
      if (!identical(input[[series_input_id(index, "source")]] %||% "", "abs")) {
        return(invisible(NULL))
      }

      restored_spec_value <- restored_series_spec(session, index)
      abs_state <- resolve_abs_control_state(
        current_values = list(
          catalogue = input[[series_input_id(index, "abs_catalogue")]],
          desc = input[[series_input_id(index, "abs_desc")]],
          type = input[[series_input_id(index, "abs_series_type")]]
        ),
        restored_spec = restored_spec_value
      )

      updateSelectizeInput(
        session,
        series_input_id(index, "abs_series_type"),
        choices = abs_state$type_choices,
        selected = abs_state$series_type,
        server = TRUE
      )
    },
    ignoreInit = FALSE
  )

  observeEvent(
    {
      list(
        input[[series_input_id(index, "abs_catalogue")]],
        input[[series_input_id(index, "abs_desc")]],
        input[[series_input_id(index, "abs_series_type")]]
      )
    },
    {
      if (!identical(input[[series_input_id(index, "source")]] %||% "", "abs")) {
        return(invisible(NULL))
      }

      restored_spec_value <- restored_series_spec(session, index)
      abs_state <- resolve_abs_control_state(
        current_values = list(
          catalogue = input[[series_input_id(index, "abs_catalogue")]],
          desc = input[[series_input_id(index, "abs_desc")]],
          type = input[[series_input_id(index, "abs_series_type")]],
          table = input[[series_input_id(index, "abs_table")]]
        ),
        restored_spec = restored_spec_value
      )

      updateSelectInput(
        session,
        series_input_id(index, "abs_table"),
        choices = c("Select a table" = "", stats::setNames(abs_state$table_choices, abs_state$table_choices)),
        selected = abs_state$table
      )
    },
    ignoreInit = FALSE
  )

  observeEvent(
    {
      list(
        input[[series_input_id(index, "abs_catalogue")]],
        input[[series_input_id(index, "abs_desc")]],
        input[[series_input_id(index, "abs_series_type")]],
        input[[series_input_id(index, "abs_table")]]
      )
    },
    {
      if (!identical(input[[series_input_id(index, "source")]] %||% "", "abs")) {
        return(invisible(NULL))
      }

      restored_spec_value <- restored_series_spec(session, index)
      abs_state <- resolve_abs_control_state(
        current_values = list(
          catalogue = input[[series_input_id(index, "abs_catalogue")]],
          desc = input[[series_input_id(index, "abs_desc")]],
          type = input[[series_input_id(index, "abs_series_type")]],
          table = input[[series_input_id(index, "abs_table")]],
          ids = input[[series_input_id(index, "abs_id")]]
        ),
        restored_spec = restored_spec_value
      )

      updateSelectizeInput(
        session,
        series_input_id(index, "abs_id"),
        choices = abs_state$id_choices,
        selected = abs_state$ids,
        server = TRUE
      )
    },
    ignoreInit = FALSE
  )

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

  direct_abs_ids <- unique(trimws(as.character(input[[series_input_id(index, "abs_id")]] %||% restored_spec$abs_id %||% character())))
  direct_abs_ids <- direct_abs_ids[nzchar(direct_abs_ids)]

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
  abs_state <- resolve_abs_control_state(
    current_values = list(
      catalogue = spec$abs_catalogue,
      desc = spec$abs_desc,
      type = spec$abs_series_type,
      table = spec$abs_table,
      ids = spec$abs_id
    ),
    restored_spec = spec
  )

  updateSelectizeInput(
    session,
    series_input_id(index, "abs_catalogue"),
    choices = abs_cat,
    selected = abs_state$catalogue,
    server = TRUE
  )
  updateSelectizeInput(
    session,
    series_input_id(index, "abs_desc"),
    choices = abs_state$desc_choices,
    selected = abs_state$desc,
    server = TRUE
  )
  updateSelectizeInput(
    session,
    series_input_id(index, "abs_series_type"),
    choices = abs_state$type_choices,
    selected = abs_state$series_type,
    server = TRUE
  )
  updateSelectInput(
    session,
    series_input_id(index, "abs_table"),
    choices = c("Select a table" = "", stats::setNames(abs_state$table_choices, abs_state$table_choices)),
    selected = abs_state$table
  )
  updateSelectizeInput(
    session,
    series_input_id(index, "abs_id"),
    choices = seed_selectize_choices(c(abs_state$id_choices, spec$abs_id %||% character())),
    selected = spec$abs_id %||% abs_state$ids,
    server = TRUE
  )
}

provider_abs_search_result_series_id <- function(search_result) {
  cache_key_value(search_result$load_payload[[1]]$abs_id %||% character())
}

provider_abs_recent_title <- function(spec) {
  trimws(spec$abs_desc %||% spec$abs_id %||% "ABS series")
}

provider_abs_default_label <- function(spec) {
  selected_ids <- unique(trimws(as.character(spec$abs_id %||% character())))
  selected_ids <- selected_ids[nzchar(selected_ids)]

  if (length(selected_ids) == 1) {
    return(selected_ids[[1]])
  }

  ""
}

provider_abs_source_note_value <- function(spec) {
  id_values <- as.character(spec$abs_id %||% character())
  id_values <- unique(trimws(id_values[nzchar(trimws(id_values))]))

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

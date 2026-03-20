provider_registry_source_id <- function(source_value) {
  source_text <- trimws(as.character(source_value %||% ""))

  if (!nzchar(source_text)) {
    return("")
  }

  if (identical(source_text, "RBA")) {
    return("rba")
  }

  source_text
}

provider_registry <- function() {
  list(
    rba = list(
      id = "rba",
      label = "RBA",
      controls_ui = provider_rba_controls_ui,
      register_dependencies = provider_rba_register_dependencies,
      spec_from_input = provider_rba_spec_from_input,
      normalize_spec = provider_rba_normalize_spec,
      query_series_history = provider_rba_query_series_history,
      search_index_builder = provider_rba_search_index_builder,
      search_result_to_spec = provider_rba_search_result_to_spec
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

provider_registry_register_dependencies <- function(input, output, session, index) {
  purrr::walk(
    Filter(Negate(is.null), provider_registry()),
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

provider_generic_search_result_to_spec <- function(search_result, index) {
  spec <- search_result$load_payload[[1]]
  spec$index <- index
  spec
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

provider_rba_search_result_to_spec <- function(search_result, index) {
  provider_generic_search_result_to_spec(search_result, index)
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

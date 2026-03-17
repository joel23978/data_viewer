`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    y
  } else {
    x
  }
}

series_input_id <- function(index, field) {
  paste0("series_", index, "_", field)
}

series_source_controls_id <- function(index) {
  series_input_id(index, "source_controls")
}

series_enabled <- function(input, index) {
  enabled_value <- input[[series_input_id(index, "enabled")]]
  isTRUE(enabled_value) || identical(as.character(enabled_value), "1")
}

chart_card <- function(title, ..., class = "", header_actions = NULL) {
  div(
    class = paste("app-card", class),
    div(
      class = "app-card__header",
      div(class = "app-card__title", title),
      if (!is.null(header_actions)) div(class = "app-card__header-actions", header_actions)
    ),
    div(class = "app-card__body", ...)
  )
}

summary_chip <- function(label, value) {
  div(
    class = "summary-chip",
    tags$span(class = "summary-chip__label", label),
    tags$strong(class = "summary-chip__value", value)
  )
}

series_source_note_value <- function(spec) {
  if (is.null(spec)) {
    return(NULL)
  }

  source_label <- spec$source %||% "Unknown"
  id_values <- character()

  if (identical(source_label, "ABS CPI")) {
    id_values <- as.character(spec$text %||% character())
  }

  if (identical(source_label, "FRED")) {
    id_values <- as.character(spec$fred_series %||% character())
  }

  if (identical(source_label, "dbnomics")) {
    id_values <- as.character(spec$dbnomics_series %||% character())
  }

  if (identical(source_label, "rba")) {
    id_values <- as.character(spec$rba_desc %||% character())
  }

  if (identical(source_label, "abs")) {
    id_values <- as.character(spec$abs_id %||% character())
  }

  id_values <- unique(trimws(id_values[nzchar(trimws(id_values))]))

  if (length(id_values) == 0) {
    source_label
  } else {
    paste0(source_label, " - ", paste(id_values, collapse = ", "))
  }
}

default_source_note <- function(series_specs = NULL) {
  active_specs <- Filter(Negate(is.null), series_specs %||% list())

  if (length(active_specs) == 0) {
    return("Source: custom query")
  }

  note_parts <- unique(vapply(active_specs, series_source_note_value, character(1)))
  note_parts <- note_parts[nzchar(note_parts)]

  if (length(note_parts) == 0) {
    "Source: custom query"
  } else {
    paste("Source:", paste(note_parts, collapse = " | "))
  }
}

compact_single_choice_input <- function(input_id, label, choices, selected = NULL) {
  if (length(choices) <= 2) {
    return(
      radioGroupButtons(
        input_id,
        label,
        choices = choices,
        selected = selected %||% unname(choices[[1]]),
        justified = TRUE,
        checkIcon = list(yes = icon("check"))
      )
    )
  }

  selectInput(
    input_id,
    label,
    choices = choices,
    selected = selected %||% unname(choices[[1]])
  )
}

fred_vintage_mode_choices <- function() {
  c("Current" = "current", "Vintage" = "historical", "Compare" = "compare")
}

parse_vintage_date <- function(value) {
  parsed_date <- suppressWarnings(as.Date(value))
  if (length(parsed_date) == 0 || all(is.na(parsed_date))) {
    as.Date(NA)
  } else {
    parsed_date[1]
  }
}

fred_vintage_choice_values <- function(series_id, selected_date = NULL) {
  cleaned_series <- trimws(series_id %||% "")
  cleaned_selected <- trimws(as.character(selected_date %||% ""))

  available_dates <- if (nzchar(cleaned_series) && nzchar(trimws(Sys.getenv("FRED_API_KEY")))) {
    fred_vintage_dates(cleaned_series)
  } else {
    as.Date(character())
  }

  if (length(available_dates) > 250) {
    available_dates <- tail(available_dates, 250)
  }

  if (nzchar(cleaned_selected)) {
    selected_as_date <- parse_vintage_date(cleaned_selected)
    if (!is.na(selected_as_date) && !selected_as_date %in% available_dates) {
      available_dates <- sort(unique(c(available_dates, selected_as_date)))
    }
  }

  if (!length(available_dates)) {
    return(character())
  }

  stats::setNames(format(available_dates, "%Y-%m-%d"), format(available_dates, "%Y-%m-%d"))
}

default_year_bounds <- function() {
  min_date <- as.Date(min(cpi_data_all$date))
  max_date <- as.Date(max(cpi_data_all$date))
  year_min <- as.numeric(lubridate::year(min_date))
  year_max <- as.numeric(lubridate::year(max_date))
  default_start_date <- max(min_date, as.Date("2019-01-01"))

  list(
    min = year_min,
    max = year_max,
    min_date = min_date,
    max_date = max_date,
    start_date = default_start_date,
    end_date = max_date
  )
}

default_style_settings <- function() {
  list(
    title = "Custom data view",
    subtitle = "",
    y_axis_label = "%",
    note = "Source: custom query",
    legend = "bottom",
    palette = APP_PALETTES[[1]],
    date_format = APP_DATE_FORMATS[[2]],
    x_labels = 6,
    auto_y_axis = TRUE,
    y_min = NA_real_,
    y_max = NA_real_,
    y_breaks = NA_real_,
    invert_y_axis = FALSE,
    horizontal_1 = NA_real_,
    horizontal_2 = NA_real_,
    horizontal_shading = c(NA_real_, NA_real_),
    vertical_1 = as.Date(NA),
    vertical_2 = as.Date(NA),
    recession_shading = "none",
    export_width = 8,
    export_height = 5
  )
}

default_builder_state <- function() {
  year_bounds <- default_year_bounds()
  default_series <- c(
    list(
      list(
        index = 1,
        source = "ABS CPI",
        text = "All groups CPI",
        region = region_list[[1]],
        transform = "index",
        rebase_date = as.Date("2019-12-31"),
        label = "",
        transform_profile = default_transform_profile(),
        vis_type = "line"
      )
    ),
    rep(list(NULL), MAX_SERIES - 1)
  )
  default_style <- default_style_settings()
  default_style$note <- default_source_note(default_series)

  list(
    date_range = c(year_bounds$start_date, year_bounds$end_date),
    show_table = FALSE,
    series = default_series,
    all_series_transform = default_transform_profile(),
    style = default_style
  )
}

normalize_date_range <- function(date_range = NULL) {
  year_bounds <- default_year_bounds()
  fallback_range <- c(year_bounds$start_date, year_bounds$end_date)
  values <- date_range %||% fallback_range

  if (inherits(values, "Date")) {
    normalized_dates <- as.Date(values)
  } else {
    numeric_values <- suppressWarnings(as.numeric(values))
    numeric_values <- numeric_values[is.finite(numeric_values)]

    if (length(numeric_values) >= 2) {
      if (max(abs(numeric_values), na.rm = TRUE) > 3000) {
        normalized_dates <- as.Date(numeric_values, origin = "1970-01-01")
      } else {
        normalized_dates <- as.Date(c(
          sprintf("%s-01-01", round(min(numeric_values))),
          sprintf("%s-12-31", round(max(numeric_values)))
        ))
      }
    } else {
      normalized_dates <- suppressWarnings(as.Date(values))
    }
  }

  normalized_dates <- normalized_dates[!is.na(normalized_dates)]

  if (length(normalized_dates) < 2) {
    normalized_dates <- fallback_range
  }

  as.Date(sort(as.numeric(as.Date(normalized_dates[seq_len(2)]))), origin = "1970-01-01")
}

series_cache_env <- new.env(parent = emptyenv())

cache_key_value <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return("")
  }

  paste(sort(as.character(value)), collapse = "||")
}

series_cache_key <- function(spec) {
  base_parts <- c(
    spec$source %||% "",
    cache_key_value(spec$text),
    cache_key_value(spec$region),
    cache_key_value(spec$transform),
    cache_key_value(spec$rebase_date),
    cache_key_value(spec$fred_series),
    cache_key_value(spec$fred_vintage_mode),
    cache_key_value(spec$fred_vintage_date),
    cache_key_value(spec$dbnomics_series),
    cache_key_value(spec$rba_table),
    cache_key_value(spec$rba_desc),
    cache_key_value(spec$abs_catalogue),
    cache_key_value(spec$abs_desc),
    cache_key_value(spec$abs_series_type),
    cache_key_value(spec$abs_table),
    cache_key_value(spec$abs_id)
  )

  paste(base_parts, collapse = "::")
}

read_cached_series <- function(cache_key) {
  if (!exists(cache_key, envir = series_cache_env, inherits = FALSE)) {
    return(NULL)
  }

  get(cache_key, envir = series_cache_env, inherits = FALSE)
}

write_cached_series <- function(cache_key, data) {
  assign(cache_key, data, envir = series_cache_env)
  invisible(data)
}

builder_series_ui <- function(index) {
  enabled_id <- series_input_id(index, "enabled")
  source_id <- series_input_id(index, "source")

  tabPanel(
    title = paste("Series", index),
    radioGroupButtons(
      enabled_id,
      "Include this series in the chart",
      choices = c("Included" = "1", "Excluded" = "0"),
      selected = "1",
      justified = TRUE,
      checkIcon = list(yes = icon("check"))
    ),
    conditionalPanel(
      condition = sprintf("input.%s == '1'", enabled_id),
      selectInput(
        source_id,
        "Data source",
        choices = c("ABS CPI", "FRED", "dbnomics", "rba", "abs"),
        selected = "ABS CPI"
      ),
      uiOutput(series_source_controls_id(index)),
      textInput(series_input_id(index, "label"), "Chart label shown in the legend", value = ""),
      radioGroupButtons(
        series_input_id(index, "vis_type"),
        "Chart style for this series",
        choices = c("Line" = "line", "Bars" = "bar", "Dots" = "scatter"),
        selected = "line",
        justified = TRUE,
        checkIcon = list(yes = icon("check"))
      )
    )
  )
}

restored_series_spec <- function(session, index) {
  restored_specs <- session$userData$restored_series_specs %||% list()
  restored_specs[[as.character(index)]] %||% NULL
}

series_source_controls_ui <- function(input, session, index, source_value = "ABS CPI", restored_spec = NULL) {
  default_cpi_series <- if (index == 1) "All groups CPI" else character()

  if (identical(source_value, "FRED")) {
    current_series <- input[[series_input_id(index, "fred_series")]] %||% restored_spec$fred_series %||% "UNRATE"
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

    return(
      tagList(
        textInput(
          series_input_id(index, "fred_series"),
          "FRED series ID",
          value = current_series
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
    )
  }

  if (identical(source_value, "dbnomics")) {
    return(
      textInput(
        series_input_id(index, "dbnomics_series"),
        "DBnomics series ID",
        value = input[[series_input_id(index, "dbnomics_series")]] %||% restored_spec$dbnomics_series %||% "AMECO/ZUTN/EA19.0.0.0.0.ZUTN"
      )
    )
  }

  if (identical(source_value, "ABS CPI")) {
    selected_transform <- input[[series_input_id(index, "transform")]] %||% restored_spec$transform %||% "index"

    controls <- list(
      selectizeInput(
        series_input_id(index, "text"),
        "CPI series to include",
        choices = list(
          Category_1 = cat1,
          Category_2 = cat2,
          Category_3 = cat3,
          Category_4 = cat4
        ),
        selected = input[[series_input_id(index, "text")]] %||% restored_spec$text %||% default_cpi_series,
        multiple = TRUE
      ),
      selectizeInput(
        series_input_id(index, "region"),
        "Geographic areas to include",
        choices = region_list,
        multiple = TRUE,
        selected = input[[series_input_id(index, "region")]] %||% restored_spec$region %||% region_list[[1]]
      ),
      selectInput(
        series_input_id(index, "transform"),
        "Base CPI calculation before panel transforms",
        choices = c("index", "y.y", "q.q", "rebased index"),
        selected = selected_transform
      )
    )

    if (identical(selected_transform, "rebased index")) {
      controls <- c(
        controls,
        list(
          dateInput(
            series_input_id(index, "rebase_date"),
            "Reference date for the rebased index",
            value = as.Date(input[[series_input_id(index, "rebase_date")]] %||% restored_spec$rebase_date %||% "2019-12-31")
          )
        )
      )
    }

    return(do.call(tagList, controls))
  }

  if (identical(source_value, "rba")) {
    current_table <- input[[series_input_id(index, "rba_table")]] %||% restored_spec$rba_table %||% rba_tables[[1]]
    table_choices <- rba_series[[current_table]] %||% rba_series[[1]]

    return(
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
          selected = input[[series_input_id(index, "rba_desc")]] %||% restored_spec$rba_desc %||% table_choices[1],
          multiple = TRUE
        )
      )
    )
  }

  if (identical(source_value, "abs")) {
    current_catalogue <- input[[series_input_id(index, "abs_catalogue")]] %||% restored_spec$abs_catalogue %||% abs_cat[[1]]
    catalogue_index <- match(current_catalogue, abs_cat)
    catalogue_data <- abs_ref[[catalogue_index]] %||% abs_ref[[1]]
    desc_choices <- unique(catalogue_data$series)
    current_desc <- input[[series_input_id(index, "abs_desc")]] %||% restored_spec$abs_desc %||% desc_choices[1]
    type_choices <- catalogue_data %>%
      filter(series == current_desc) %>%
      pull(series_type) %>%
      unique()
    current_type <- input[[series_input_id(index, "abs_series_type")]] %||% restored_spec$abs_series_type %||% type_choices[1]
    table_choices <- catalogue_data %>%
      filter(series == current_desc, series_type == current_type) %>%
      pull(table_title) %>%
      unique()
    current_table <- input[[series_input_id(index, "abs_table")]] %||% restored_spec$abs_table %||% table_choices[1]
    id_choices <- catalogue_data %>%
      filter(series == current_desc, series_type == current_type, table_title == current_table) %>%
      pull(series_id) %>%
      unique()

    return(
      tagList(
        compact_single_choice_input(
          series_input_id(index, "abs_catalogue"),
          "ABS catalogue",
          choices = abs_cat,
          selected = current_catalogue
        ),
        selectizeInput(
          series_input_id(index, "abs_desc"),
          "ABS series description",
          choices = desc_choices,
          selected = current_desc
        ),
        compact_single_choice_input(
          series_input_id(index, "abs_series_type"),
          "ABS series type",
          choices = type_choices,
          selected = current_type
        ),
        compact_single_choice_input(
          series_input_id(index, "abs_table"),
          "ABS table",
          choices = table_choices,
          selected = current_table
        ),
        selectizeInput(
          series_input_id(index, "abs_id"),
          "ABS series ID",
          choices = id_choices,
          selected = input[[series_input_id(index, "abs_id")]] %||% restored_spec$abs_id %||% id_choices[1],
          options = list(create = TRUE),
          multiple = TRUE
        )
      )
    )
  }

  tagList()
}

register_series_dependencies <- function(input, output, session, index) {
  output[[series_source_controls_id(index)]] <- renderUI({
    req(series_enabled(input, index))
    source_value <- input[[series_input_id(index, "source")]] %||% "ABS CPI"
    restored_spec_value <- restored_series_spec(session, index)

    if (!is.null(restored_spec_value) && !identical(restored_spec_value$source %||% source_value, source_value)) {
      restored_spec_value <- NULL
    }

    series_source_controls_ui(input, session, index, source_value, restored_spec_value)
  })

  choose_single_restore_value <- function(current_value, restored_value, choices) {
    valid_choices <- as.character(choices %||% character())

    if (length(valid_choices) == 0) {
      return(character())
    }

    current_choice <- as.character(current_value %||% character())
    restored_choice <- as.character(restored_value %||% character())

    if (length(current_choice) > 0 && current_choice[1] %in% valid_choices) {
      return(current_choice[1])
    }

    if (length(restored_choice) > 0 && restored_choice[1] %in% valid_choices) {
      return(restored_choice[1])
    }

    valid_choices[1]
  }

  choose_multi_restore_value <- function(current_value, restored_value, choices) {
    valid_choices <- as.character(choices %||% character())

    if (length(valid_choices) == 0) {
      return(character())
    }

    current_choices <- intersect(as.character(current_value %||% character()), valid_choices)
    restored_choices <- intersect(as.character(restored_value %||% character()), valid_choices)

    if (length(current_choices) > 0) {
      return(current_choices)
    }

    if (length(restored_choices) > 0) {
      return(restored_choices)
    }

    valid_choices[1]
  }

  observeEvent(input[[series_input_id(index, "rba_table")]], {
    table_value <- input[[series_input_id(index, "rba_table")]]
    series_choices <- rba_series[[table_value]] %||% character()

    updateSelectizeInput(
      session,
      series_input_id(index, "rba_desc"),
      choices = series_choices,
      selected = series_choices[1],
      server = TRUE
    )
  }, ignoreInit = FALSE)

  observeEvent(input[[series_input_id(index, "abs_catalogue")]], {
    catalogue_value <- input[[series_input_id(index, "abs_catalogue")]]
    if (is.null(catalogue_value) || !length(catalogue_value)) {
      return(invisible(NULL))
    }
    catalogue_index <- match(catalogue_value, abs_cat)
    if (is.na(catalogue_index) || catalogue_index < 1) {
      return(invisible(NULL))
    }
    catalogue_data <- abs_ref[[catalogue_index]] %||% abs_ref[[1]]
    desc_choices <- catalogue_data %>%
      pull(series) %>%
      unique() %>%
      na.omit()
    restored_spec_value <- restored_series_spec(session, index)
    selected_desc <- choose_single_restore_value(
      input[[series_input_id(index, "abs_desc")]],
      restored_spec_value$abs_desc %||% character(),
      desc_choices
    )

    updateSelectizeInput(
      session,
      series_input_id(index, "abs_desc"),
      choices = desc_choices,
      selected = selected_desc,
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
      catalogue_value <- input[[series_input_id(index, "abs_catalogue")]]
      desc_value <- input[[series_input_id(index, "abs_desc")]]
      if (is.null(catalogue_value) || !length(catalogue_value) || is.null(desc_value) || !length(desc_value)) {
        return(invisible(NULL))
      }
      catalogue_index <- match(catalogue_value, abs_cat)
      if (is.na(catalogue_index) || catalogue_index < 1) {
        return(invisible(NULL))
      }
      catalogue_data <- abs_ref[[catalogue_index]] %||% abs_ref[[1]]

      type_choices <- catalogue_data %>%
        filter(series == desc_value) %>%
        pull(series_type) %>%
        unique() %>%
        na.omit()
      restored_spec_value <- restored_series_spec(session, index)
      selected_type <- choose_single_restore_value(
        input[[series_input_id(index, "abs_series_type")]],
        restored_spec_value$abs_series_type %||% character(),
        type_choices
      )

      updateSelectInput(
        session,
        series_input_id(index, "abs_series_type"),
        choices = type_choices,
        selected = selected_type
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
      catalogue_value <- input[[series_input_id(index, "abs_catalogue")]]
      desc_value <- input[[series_input_id(index, "abs_desc")]]
      type_value <- input[[series_input_id(index, "abs_series_type")]]
      if (is.null(catalogue_value) || !length(catalogue_value) || is.null(desc_value) || !length(desc_value) || is.null(type_value) || !length(type_value)) {
        return(invisible(NULL))
      }
      catalogue_index <- match(catalogue_value, abs_cat)
      if (is.na(catalogue_index) || catalogue_index < 1) {
        return(invisible(NULL))
      }
      catalogue_data <- abs_ref[[catalogue_index]] %||% abs_ref[[1]]

      table_choices <- catalogue_data %>%
        filter(series == desc_value, series_type == type_value) %>%
        pull(table_title) %>%
        unique() %>%
        na.omit()
      restored_spec_value <- restored_series_spec(session, index)
      selected_table <- choose_single_restore_value(
        input[[series_input_id(index, "abs_table")]],
        restored_spec_value$abs_table %||% character(),
        table_choices
      )

      updateSelectInput(
        session,
        series_input_id(index, "abs_table"),
        choices = table_choices,
        selected = selected_table
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
      catalogue_value <- input[[series_input_id(index, "abs_catalogue")]]
      desc_value <- input[[series_input_id(index, "abs_desc")]]
      type_value <- input[[series_input_id(index, "abs_series_type")]]
      table_value <- input[[series_input_id(index, "abs_table")]]
      if (is.null(catalogue_value) || !length(catalogue_value) || is.null(desc_value) || !length(desc_value) || is.null(type_value) || !length(type_value) || is.null(table_value) || !length(table_value)) {
        return(invisible(NULL))
      }
      catalogue_index <- match(catalogue_value, abs_cat)
      if (is.na(catalogue_index) || catalogue_index < 1) {
        return(invisible(NULL))
      }
      catalogue_data <- abs_ref[[catalogue_index]] %||% abs_ref[[1]]

      id_choices <- catalogue_data %>%
        filter(
          series == desc_value,
          series_type == type_value,
          table_title == table_value
        ) %>%
        pull(series_id) %>%
        na.omit()
      restored_spec_value <- restored_series_spec(session, index)
      selected_ids <- choose_multi_restore_value(
        input[[series_input_id(index, "abs_id")]],
        restored_spec_value$abs_id %||% character(),
        id_choices
      )

      updateSelectizeInput(
        session,
        series_input_id(index, "abs_id"),
        choices = id_choices,
        selected = selected_ids,
        server = TRUE
      )
    },
    ignoreInit = FALSE
  )
}

transform_input_id <- function(prefix, field) {
  paste(prefix, field, sep = "_")
}

default_transform_profile <- function() {
  list(
    expression = "X",
    moving_average = 1,
    rolling_sum = 1,
    lagged_value = 0,
    lagged_pct = 0,
    lagged_ann = 0,
    subtract_series = "none"
  )
}

transform_difference_choices <- function() {
  c("None" = "none", stats::setNames(as.character(seq_len(MAX_SERIES)), paste("Series", seq_len(MAX_SERIES))))
}

transform_profile_ui <- function(prefix, title, include_copy_button = FALSE) {
  controls <- list(
    numericInput(
      transform_input_id(prefix, "moving_average"),
      "Rolling average window length (observations)",
      value = 1,
      min = 1,
      step = 1
    ),
    numericInput(
      transform_input_id(prefix, "rolling_sum"),
      "Rolling sum window length (observations)",
      value = 1,
      min = 1,
      step = 1
    ),
    numericInput(
      transform_input_id(prefix, "lagged_value"),
      "Difference from an earlier value (observations)",
      value = 0,
      min = 0,
      step = 1
    ),
    numericInput(
      transform_input_id(prefix, "lagged_pct"),
      "Percent change from an earlier value (observations)",
      value = 0,
      min = 0,
      step = 1
    ),
    numericInput(
      transform_input_id(prefix, "lagged_ann"),
      "Annualised percent change from an earlier value (observations)",
      value = 0,
      min = 0,
      step = 1
    ),
    selectInput(
      transform_input_id(prefix, "subtract_series"),
      "Subtract another series",
      choices = transform_difference_choices(),
      selected = "none"
    ),
    textInput(
      transform_input_id(prefix, "expression"),
      "Custom formula applied after the transformations above. Use X as the series value.",
      value = "X"
    )
  )

  do.call(tabPanel, c(list(title = title), controls))
}

transform_profile_from_input <- function(input, prefix) {
  list(
    expression = trimws(input[[transform_input_id(prefix, "expression")]] %||% "X"),
    moving_average = as.numeric(input[[transform_input_id(prefix, "moving_average")]] %||% 1),
    rolling_sum = as.numeric(input[[transform_input_id(prefix, "rolling_sum")]] %||% 1),
    lagged_value = as.numeric(input[[transform_input_id(prefix, "lagged_value")]] %||% 0),
    lagged_pct = as.numeric(input[[transform_input_id(prefix, "lagged_pct")]] %||% 0),
    lagged_ann = as.numeric(input[[transform_input_id(prefix, "lagged_ann")]] %||% 0),
    subtract_series = as.character(input[[transform_input_id(prefix, "subtract_series")]] %||% "none")
  )
}

restore_transform_profile <- function(session, prefix, profile) {
  profile <- profile %||% default_transform_profile()

  updateTextInput(session, transform_input_id(prefix, "expression"), value = profile$expression %||% "X")
  updateNumericInput(session, transform_input_id(prefix, "moving_average"), value = profile$moving_average %||% 1)
  updateNumericInput(session, transform_input_id(prefix, "rolling_sum"), value = profile$rolling_sum %||% 1)
  updateNumericInput(session, transform_input_id(prefix, "lagged_value"), value = profile$lagged_value %||% 0)
  updateNumericInput(session, transform_input_id(prefix, "lagged_pct"), value = profile$lagged_pct %||% 0)
  updateNumericInput(session, transform_input_id(prefix, "lagged_ann"), value = profile$lagged_ann %||% 0)
  updateSelectInput(session, transform_input_id(prefix, "subtract_series"), selected = profile$subtract_series %||% "none")
}

series_spec_from_input <- function(input, index, transform_profile = default_transform_profile()) {
  if (!series_enabled(input, index)) {
    return(NULL)
  }

  source_value <- input[[series_input_id(index, "source")]] %||% "ABS CPI"
  spec <- list(
    index = index,
    source = source_value,
    label = trimws(input[[series_input_id(index, "label")]] %||% ""),
    transform_profile = transform_profile,
    vis_type = input[[series_input_id(index, "vis_type")]] %||% "line"
  )

  if (identical(source_value, "ABS CPI")) {
    selected_series <- input[[series_input_id(index, "text")]]
    if (length(selected_series) == 0) {
      return(NULL)
    }

    spec$text <- selected_series
    spec$region <- input[[series_input_id(index, "region")]] %||% region_list[[1]]
    spec$transform <- input[[series_input_id(index, "transform")]] %||% "index"
    spec$rebase_date <- as.Date(input[[series_input_id(index, "rebase_date")]] %||% "2019-12-31")
  }

  if (identical(source_value, "FRED")) {
    spec$fred_series <- trimws(input[[series_input_id(index, "fred_series")]] %||% "")
    if (!nzchar(spec$fred_series)) {
      return(NULL)
    }
    spec$fred_vintage_mode <- input[[series_input_id(index, "fred_vintage_mode")]] %||% "current"
    spec$fred_vintage_date <- parse_vintage_date(input[[series_input_id(index, "fred_vintage_date")]] %||% NA)
  }

  if (identical(source_value, "dbnomics")) {
    spec$dbnomics_series <- trimws(input[[series_input_id(index, "dbnomics_series")]] %||% "")
    if (!nzchar(spec$dbnomics_series)) {
      return(NULL)
    }
  }

  if (identical(source_value, "rba")) {
    spec$rba_table <- input[[series_input_id(index, "rba_table")]] %||% rba_tables[[1]]
    spec$rba_desc <- input[[series_input_id(index, "rba_desc")]]
    if (length(spec$rba_desc) == 0) {
      return(NULL)
    }
  }

  if (identical(source_value, "abs")) {
    spec$abs_catalogue <- input[[series_input_id(index, "abs_catalogue")]] %||% abs_cat[[1]]
    spec$abs_desc <- input[[series_input_id(index, "abs_desc")]] %||% unique(abs_ref[[1]]$series)[1]
    spec$abs_series_type <- input[[series_input_id(index, "abs_series_type")]] %||% unique(abs_ref[[1]]$series_type)[1]
    spec$abs_table <- input[[series_input_id(index, "abs_table")]] %||% unique(abs_ref[[1]]$table_title)[1]
    spec$abs_id <- input[[series_input_id(index, "abs_id")]]
    if (length(spec$abs_id) == 0) {
      return(NULL)
    }
  }

  spec
}

style_settings_from_input <- function(input) {
  numeric_or_default <- function(value, default = NA_real_) {
    if (is.null(value) || length(value) == 0 || all(is.na(value))) {
      return(default)
    }

    parsed_value <- suppressWarnings(as.numeric(value))
    if (length(parsed_value) == 0 || all(is.na(parsed_value))) {
      return(default)
    }

    parsed_value
  }

  date_or_default <- function(value, default = as.Date(NA)) {
    if (is.null(value) || length(value) == 0 || all(is.na(value))) {
      return(default)
    }

    as.Date(value)
  }

  list(
    title = trimws(input$style_title %||% "Custom data view"),
    subtitle = trimws(input$style_subtitle %||% ""),
    y_axis_label = trimws(input$style_y_axis_label %||% "%"),
    note = trimws(input$style_note %||% "Source: custom query"),
    legend = input$style_legend %||% "bottom",
    palette = input$style_palette %||% APP_PALETTES[[1]],
    date_format = input$style_date_format %||% APP_DATE_FORMATS[[2]],
    x_labels = numeric_or_default(input$style_x_labels, 6),
    auto_y_axis = !identical(input$style_auto_y_axis %||% "auto", "manual"),
    y_min = numeric_or_default(input$style_y_min),
    y_max = numeric_or_default(input$style_y_max),
    y_breaks = numeric_or_default(input$style_y_breaks),
    invert_y_axis = identical(input$style_invert_y_axis %||% "standard", "inverted"),
    horizontal_1 = numeric_or_default(input$style_horizontal_1),
    horizontal_2 = numeric_or_default(input$style_horizontal_2),
    horizontal_shading = c(
      numeric_or_default(input$style_horizontal_shading_min),
      numeric_or_default(input$style_horizontal_shading_max)
    ),
    vertical_1 = date_or_default(input$style_vertical_1),
    vertical_2 = date_or_default(input$style_vertical_2),
    recession_shading = input$style_recession_shading %||% "none",
    export_width = numeric_or_default(input$export_width, 8),
    export_height = numeric_or_default(input$export_height, 5)
  )
}

builder_state_from_input <- function(input) {
  year_bounds <- default_year_bounds()
  start_date <- as.Date(input$start_date %||% year_bounds$start_date)
  end_date <- as.Date(input$end_date %||% year_bounds$end_date)

  list(
    date_range = normalize_date_range(c(start_date, end_date)),
    show_table = identical(as.character(input$viewData1), "1"),
    series = lapply(
      seq_len(MAX_SERIES),
      function(index) {
        series_spec_from_input(
          input,
          index,
          transform_profile_from_input(input, paste0("transform_", index))
        )
      }
    ),
    all_series_transform = transform_profile_from_input(input, "transform_all"),
    style = style_settings_from_input(input)
  )
}

normalize_transform_profile <- function(profile) {
  profile <- profile %||% default_transform_profile()

  list(
    expression = trimws(profile$expression %||% "X"),
    moving_average = as.numeric(profile$moving_average %||% 1),
    rolling_sum = as.numeric(profile$rolling_sum %||% 1),
    lagged_value = as.numeric(profile$lagged_value %||% 0),
    lagged_pct = as.numeric(profile$lagged_pct %||% 0),
    lagged_ann = as.numeric(profile$lagged_ann %||% 0),
    subtract_series = as.character(profile$subtract_series %||% "none")
  )
}

normalize_style_settings <- function(style) {
  style <- style %||% list()

  list(
    title = trimws(style$title %||% "Custom data view"),
    subtitle = trimws(style$subtitle %||% ""),
    y_axis_label = trimws(style$y_axis_label %||% "%"),
    note = trimws(style$note %||% "Source: custom query"),
    legend = style$legend %||% "bottom",
    palette = style$palette %||% APP_PALETTES[[1]],
    date_format = style$date_format %||% APP_DATE_FORMATS[[2]],
    x_labels = as.numeric(style$x_labels %||% 6),
    auto_y_axis = if (is.null(style$auto_y_axis)) TRUE else isTRUE(style$auto_y_axis),
    y_min = if (length(style$y_min %||% numeric()) == 0) NA_real_ else as.numeric(style$y_min)[1],
    y_max = if (length(style$y_max %||% numeric()) == 0) NA_real_ else as.numeric(style$y_max)[1],
    y_breaks = if (length(style$y_breaks %||% numeric()) == 0) NA_real_ else as.numeric(style$y_breaks)[1],
    invert_y_axis = isTRUE(style$invert_y_axis),
    horizontal_1 = if (length(style$horizontal_1 %||% numeric()) == 0) NA_real_ else as.numeric(style$horizontal_1)[1],
    horizontal_2 = if (length(style$horizontal_2 %||% numeric()) == 0) NA_real_ else as.numeric(style$horizontal_2)[1],
    horizontal_shading = {
      shading <- as.numeric(style$horizontal_shading %||% c(NA_real_, NA_real_))
      if (length(shading) < 2) {
        c(shading, rep(NA_real_, 2 - length(shading)))
      } else {
        shading[seq_len(2)]
      }
    },
    vertical_1 = if (length(style$vertical_1 %||% as.Date(character())) == 0) as.Date(NA) else as.Date(style$vertical_1)[1],
    vertical_2 = if (length(style$vertical_2 %||% as.Date(character())) == 0) as.Date(NA) else as.Date(style$vertical_2)[1],
    recession_shading = style$recession_shading %||% "none",
    export_width = as.numeric(style$export_width %||% 8),
    export_height = as.numeric(style$export_height %||% 5)
  )
}

normalize_series_spec <- function(spec) {
  if (is.null(spec)) {
    return(NULL)
  }

  normalized_spec <- list(
    index = as.integer(spec$index),
    source = spec$source %||% "ABS CPI",
    label = trimws(spec$label %||% ""),
    transform_profile = normalize_transform_profile(spec$transform_profile),
    vis_type = spec$vis_type %||% "line"
  )

  if (identical(normalized_spec$source, "ABS CPI")) {
    normalized_spec$text <- spec$text %||% character()
    normalized_spec$region <- spec$region %||% region_list[[1]]
    normalized_spec$transform <- spec$transform %||% "index"
    normalized_spec$rebase_date <- as.Date(spec$rebase_date %||% "2019-12-31")
  }

  if (identical(normalized_spec$source, "FRED")) {
    normalized_spec$fred_series <- trimws(spec$fred_series %||% "")
    normalized_spec$fred_vintage_mode <- spec$fred_vintage_mode %||% "current"
    normalized_spec$fred_vintage_date <- parse_vintage_date(spec$fred_vintage_date %||% NA)
  }

  if (identical(normalized_spec$source, "dbnomics")) {
    normalized_spec$dbnomics_series <- trimws(spec$dbnomics_series %||% "")
  }

  if (identical(normalized_spec$source, "rba")) {
    normalized_spec$rba_table <- spec$rba_table %||% rba_tables[[1]]
    normalized_spec$rba_desc <- spec$rba_desc %||% character()
  }

  if (identical(normalized_spec$source, "abs")) {
    normalized_spec$abs_catalogue <- spec$abs_catalogue %||% abs_cat[[1]]
    normalized_spec$abs_desc <- spec$abs_desc %||% unique(abs_ref[[1]]$series)[1]
    normalized_spec$abs_series_type <- spec$abs_series_type %||% unique(abs_ref[[1]]$series_type)[1]
    normalized_spec$abs_table <- spec$abs_table %||% unique(abs_ref[[1]]$table_title)[1]
    normalized_spec$abs_id <- spec$abs_id %||% character()
  }

  normalized_spec
}

normalize_chart_state <- function(chart_state) {
  chart_state <- chart_state %||% list()

  list(
    date_range = normalize_date_range(chart_state$date_range),
    show_table = isTRUE(chart_state$show_table),
    series = lapply(seq_len(MAX_SERIES), function(index) normalize_series_spec((chart_state$series %||% list())[[index]])),
    all_series_transform = normalize_transform_profile(chart_state$all_series_transform),
    style = normalize_style_settings(chart_state$style)
  )
}

chart_states_equal <- function(state_a, state_b) {
  identical(normalize_chart_state(state_a), normalize_chart_state(state_b))
}

safe_transform_expression <- function(data, expression, label) {
  expression_text <- trimws(expression %||% "X")

  if (!nzchar(expression_text) || expression_text %in% c("X", "data")) {
    return(data)
  }

  tryCatch(
    {
      data %>%
        mutate(value = eval(parse(text = expression_text), envir = list(X = value, data = value)))
    },
    error = function(error) {
      stop(sprintf("%s expression is invalid: %s", label, conditionMessage(error)), call. = FALSE)
    }
  )
}

apply_transform_profile <- function(data, transform_profile, label) {
  transformed <- data

  transformed <- apply_moving_average(transformed, transform_profile$moving_average)
  transformed <- apply_rolling_sum(transformed, transform_profile$rolling_sum)
  transformed <- apply_lagged_value(transformed, transform_profile$lagged_value)
  transformed <- apply_lagged_pct(transformed, transform_profile$lagged_pct)
  transformed <- apply_lagged_annualised(transformed, transform_profile$lagged_ann)
  transformed <- safe_transform_expression(transformed, transform_profile$expression, label)

  transformed
}

apply_series_metadata <- function(data, spec) {
  data <- apply_transform_profile(data, spec$transform_profile, paste("Series", spec$index))

  if (nzchar(spec$label)) {
    if (dplyr::n_distinct(data$name) == 1) {
      data <- data %>% mutate(name = spec$label)
    } else {
      data <- data %>% mutate(name = paste(spec$label, name, sep = " - "))
    }
  }

  data %>%
    mutate(plotting = spec$vis_type)
}

query_series_history <- function(spec) {
  if (identical(spec$source, "ABS CPI")) {
    full_year_range <- c(
      as.numeric(lubridate::year(min(cpi_data_all$date))),
      as.numeric(lubridate::year(max(cpi_data_all$date)))
    )

    return(
      cpi_splits_cust(
        cpi_data = cpi_data_all,
        transformation = spec$transform,
        dates = full_year_range,
        pick_split_1 = spec$text,
        region_1_split = spec$region,
        rebase_date = spec$rebase_date
      )
    )
  }

  if (identical(spec$source, "FRED")) {
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

    return(fred_data(series = spec$fred_series))
  }

  if (identical(spec$source, "dbnomics")) {
    return(db_data(series = spec$dbnomics_series))
  }

  if (identical(spec$source, "rba")) {
    return(rba_data(series = spec$rba_desc))
  }

  if (identical(spec$source, "abs")) {
    return(abs_data(series = spec$abs_id))
  }

  tibble::tibble()
}

query_series_data <- function(spec) {
  cache_key <- series_cache_key(spec)
  cached_data <- read_cached_series(cache_key)

  if (!is.null(cached_data)) {
    return(cached_data)
  }

  queried_data <- query_series_history(spec)
  write_cached_series(cache_key, queried_data)
}

filter_series_date_range <- function(data, date_range) {
  if (nrow(data) == 0) {
    return(data)
  }

  normalized_range <- normalize_date_range(date_range)
  start_date <- min(normalized_range)
  end_date <- max(normalized_range)

  data %>%
    filter(date >= start_date, date <= end_date)
}

latest_chart_observation_date <- function(chart_state) {
  specs <- Filter(Negate(is.null), normalize_chart_state(chart_state)$series)

  if (length(specs) == 0) {
    return(as.Date(NA))
  }

  latest_dates <- vapply(
    specs,
    function(spec) {
      series_data <- query_series_data(spec)

      if (is.null(series_data) || nrow(series_data) == 0) {
        return(as.Date(NA))
      }

      max(as.Date(series_data$date), na.rm = TRUE)
    },
    as.Date(NA)
  )

  latest_dates <- as.Date(latest_dates, origin = "1970-01-01")
  latest_dates <- latest_dates[!is.na(latest_dates)]

  if (length(latest_dates) == 0) {
    as.Date(NA)
  } else {
    max(latest_dates)
  }
}

preview_chart_state <- function(series_spec, date_range = NULL) {
  state <- default_builder_state()
  normalized_spec <- normalize_series_spec(series_spec)
  state$series[[1]] <- normalized_spec
  state$series[seq(2, MAX_SERIES)] <- rep(list(NULL), MAX_SERIES - 1)

  resolved_range <- if (!is.null(date_range) && length(date_range) >= 2 && all(!is.na(date_range[seq_len(2)]))) {
    normalize_date_range(date_range)
  } else {
    series_data <- tryCatch(query_series_data(normalized_spec), error = function(error) NULL)

    if (!is.null(series_data) && nrow(series_data) > 0) {
      min_date <- min(as.Date(series_data$date), na.rm = TRUE)
      max_date <- max(as.Date(series_data$date), na.rm = TRUE)
      lookback_start <- max_date %m-% years(12)
      normalize_date_range(c(max(min_date, lookback_start), max_date))
    } else {
      default_builder_state()$date_range
    }
  }

  state$date_range <- resolved_range
  state$style$title <- normalized_spec$label %||% state$style$title
  state$style$subtitle <- ""
  state$style$note <- "Preview"

  normalize_chart_state(state)
}

make_unique_series_names <- function(data) {
  if (nrow(data) == 0) {
    return(data)
  }

  unique_names <- unique(data$name)
  updated_names <- make.unique(unique_names)
  name_lookup <- stats::setNames(updated_names, unique_names)

  data %>%
    mutate(name = unname(name_lookup[name]))
}

empty_chart_data <- function() {
  tibble::tibble(
    date = as.Date(character()),
    value = numeric(),
    name = character(),
    plotting = character()
  )
}

apply_moving_average <- function(data, periods) {
  if (is.na(periods) || periods <= 1) {
    return(data)
  }

  data %>%
    group_by(name) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(value = zoo::rollmean(value, periods, fill = NA, align = "right")) %>%
    ungroup() %>%
    drop_na(value)
}

apply_rolling_sum <- function(data, periods) {
  if (is.na(periods) || periods <= 1) {
    return(data)
  }

  data %>%
    group_by(name) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(value = zoo::rollsum(value, periods, fill = NA, align = "right")) %>%
    ungroup() %>%
    drop_na(value)
}

apply_lagged_value <- function(data, periods) {
  if (is.na(periods) || periods <= 0) {
    return(data)
  }

  data %>%
    group_by(name) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(value = value - dplyr::lag(value, periods)) %>%
    ungroup() %>%
    drop_na(value)
}

apply_lagged_pct <- function(data, periods) {
  if (is.na(periods) || periods <= 0) {
    return(data)
  }

  data %>%
    group_by(name) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(value = (100 * value / dplyr::lag(value, periods)) - 100) %>%
    ungroup() %>%
    drop_na(value)
}

apply_lagged_annualised <- function(data, periods) {
  if (is.na(periods) || periods <= 0) {
    return(data)
  }

  data %>%
    group_by(name) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(
      months = round(as.numeric(difftime(date, dplyr::lag(date, 1), units = "days")) / 365 * 12),
      months = zoo::na.locf(months, fromLast = TRUE),
      value = 100 * (value / dplyr::lag(value, periods)) ^ ((12 / months) / periods) - 100
    ) %>%
    ungroup() %>%
    drop_na(value)
}

apply_series_difference <- function(data, reference_data, reference_index) {
  if (nrow(data) == 0) {
    return(data)
  }

  if (is.null(reference_data) || nrow(reference_data) == 0) {
    stop(sprintf("Series %s has no data to subtract.", reference_index), call. = FALSE)
  }

  if (dplyr::n_distinct(reference_data$name) != 1) {
    stop(sprintf("Series %s must resolve to a single line before it can be subtracted.", reference_index), call. = FALSE)
  }

  reference_lookup <- reference_data %>%
    group_by(date) %>%
    summarise(reference_value = dplyr::last(value), .groups = "drop")

  data %>%
    left_join(reference_lookup, by = "date") %>%
    mutate(value = value - reference_value) %>%
    select(-reference_value) %>%
    drop_na(value)
}

build_chart_data <- function(chart_state) {
  specs <- Filter(Negate(is.null), chart_state$series)
  messages <- character()

  if (length(specs) == 0) {
    return(list(data = empty_chart_data(), messages = "Choose at least one series to build a chart."))
  }

  series_results <- lapply(specs, function(spec) {
    tryCatch(
      {
        queried_data <- query_series_data(spec)

        if (is.null(queried_data) || nrow(queried_data) == 0) {
          return(list(
            data = empty_chart_data(),
            message = sprintf("Series %s returned no rows.", spec$index)
          ))
        }

        list(
          data = filter_series_date_range(
            apply_series_metadata(queried_data, spec),
            chart_state$date_range
          ),
          message = NULL
        )
      },
      error = function(error) {
        list(
          data = empty_chart_data(),
          message = sprintf("Series %s (%s): %s", spec$index, spec$source, conditionMessage(error))
        )
      }
    )
  })

  messages <- c(messages, vapply(series_results, function(result) result$message %||% "", character(1)))
  messages <- messages[nzchar(messages)]

  series_data_by_index <- stats::setNames(
    lapply(series_results, `[[`, "data"),
    vapply(specs, function(spec) as.character(spec$index), character(1))
  )

  series_results <- lapply(seq_along(series_results), function(result_index) {
    result <- series_results[[result_index]]
    spec <- specs[[result_index]]
    subtract_series <- spec$transform_profile$subtract_series %||% "none"

    if (identical(subtract_series, "none")) {
      return(result)
    }

    reference_data <- series_data_by_index[[subtract_series]]

    tryCatch(
      {
        result$data <- apply_series_difference(result$data, reference_data, subtract_series)
        result
      },
      error = function(error) {
        result$data <- empty_chart_data()
        result$message <- paste(
          c(
            result$message %||% "",
            sprintf("Series %s difference vs Series %s: %s", spec$index, subtract_series, conditionMessage(error))
          )[nzchar(c(result$message %||% "", sprintf("Series %s difference vs Series %s: %s", spec$index, subtract_series, conditionMessage(error))))],
          collapse = " "
        )
        result
      }
    )
  })

  messages <- c(messages, vapply(series_results, function(result) result$message %||% "", character(1)))
  messages <- unique(messages[nzchar(messages)])

  combined_data <- series_results %>%
    lapply(`[[`, "data") %>%
    bind_rows()

  if (!all(c("date", "name", "value", "plotting") %in% names(combined_data))) {
    combined_data <- empty_chart_data()
  }

  combined_data <- combined_data %>%
    arrange(date, name)

  if (nrow(combined_data) == 0) {
    return(list(data = combined_data, messages = unique(messages)))
  }

  combined_data <- make_unique_series_names(combined_data)

  list(data = combined_data, messages = unique(messages))
}

pretty_axis_breaks <- function(data, style) {
  if (nrow(data) == 0) {
    return(list(min = 0, max = 1, breaks = seq(0, 1, by = 0.25)))
  }

  data_range <- range(data$value, na.rm = TRUE)

  if (!all(is.finite(data_range))) {
    return(list(min = 0, max = 1, breaks = seq(0, 1, by = 0.25)))
  }

  if (diff(data_range) == 0) {
    data_range <- data_range + c(-0.5, 0.5)
  }

  if (isTRUE(style$auto_y_axis)) {
    axis_values <- pretty(data_range, n = 6)

    return(list(
      min = min(axis_values),
      max = max(axis_values),
      breaks = axis_values
    ))
  }

  axis_min <- if (is.finite(style$y_min)) style$y_min else data_range[1]
  axis_max <- if (is.finite(style$y_max)) style$y_max else data_range[2]

  if (!is.finite(axis_min) || !is.finite(axis_max) || axis_max <= axis_min) {
    axis_values <- pretty(data_range, n = 6)

    return(list(
      min = min(axis_values),
      max = max(axis_values),
      breaks = axis_values
    ))
  }

  axis_breaks <- if (is.finite(style$y_breaks) && style$y_breaks > 0) {
    seq(axis_min, axis_max, by = style$y_breaks)
  } else {
    pretty(c(axis_min, axis_max), n = 6)
  }

  list(
    min = axis_min,
    max = axis_max,
    breaks = axis_breaks[axis_breaks >= axis_min & axis_breaks <= axis_max]
  )
}

palette_values <- function(data, palette_name) {
  n_series <- max(1, dplyr::n_distinct(data$name))
  requested_palette <- palette_name %||% APP_PALETTES[[1]]
  available_palettes <- grDevices::palette.pals()

  if (requested_palette %in% available_palettes) {
    return(grDevices::palette.colors(n_series, palette = requested_palette))
  }

  grDevices::palette.colors(n_series, palette = APP_PALETTES[[1]])
}

build_chart_plot <- function(data, style) {
  if (nrow(data) == 0) {
    stop("No chart data is available yet.", call. = FALSE)
  }

  axis_settings <- pretty_axis_breaks(data, style)
  x_breaks <- scales::breaks_pretty(n = max(2, round(style$x_labels %||% 6)))

  chart_plot <- ggplot(data, aes(x = date, y = value, colour = name, fill = name))

  if (any(data$plotting == "bar")) {
    chart_plot <- chart_plot +
      geom_col(
        data = filter(data, plotting == "bar"),
        position = "dodge",
        alpha = 0.85
      )
  }

  if (any(data$plotting == "line")) {
    chart_plot <- chart_plot +
      geom_line(
        data = filter(data, plotting == "line"),
        linewidth = 0.9
      )
  }

  if (any(data$plotting == "scatter")) {
    chart_plot <- chart_plot +
      geom_point(
        data = filter(data, plotting == "scatter"),
        size = 2.2
      )
  }

  if (!is.na(style$horizontal_1)) {
    chart_plot <- chart_plot + geom_hline(yintercept = style$horizontal_1, linetype = "dashed", colour = "#6b7280")
  }

  if (!is.na(style$horizontal_2)) {
    chart_plot <- chart_plot + geom_hline(yintercept = style$horizontal_2, linetype = "dashed", colour = "#9ca3af")
  }

  if (length(style$horizontal_shading) == 2 && all(is.finite(style$horizontal_shading))) {
    chart_plot <- chart_plot +
      annotate(
        "rect",
        xmin = min(data$date, na.rm = TRUE),
        xmax = max(data$date, na.rm = TRUE),
        ymin = style$horizontal_shading[1],
        ymax = style$horizontal_shading[2],
        alpha = 0.08,
        fill = "#2563eb"
      )
  }

  if (!is.null(style$vertical_1) && length(style$vertical_1) == 1 && !is.na(style$vertical_1)) {
    chart_plot <- chart_plot + geom_vline(xintercept = as.numeric(style$vertical_1), linetype = "dashed", colour = "#6b7280")
  }

  if (!is.null(style$vertical_2) && length(style$vertical_2) == 1 && !is.na(style$vertical_2)) {
    chart_plot <- chart_plot + geom_vline(xintercept = as.numeric(style$vertical_2), linetype = "dashed", colour = "#9ca3af")
  }

  if (!identical(style$recession_shading, "none")) {
    chart_plot <- chart_plot +
      geom_rect(
        data = rec_data %>%
          filter(region == style$recession_shading),
        aes(xmin = peak, xmax = trough, ymin = -Inf, ymax = Inf),
        inherit.aes = FALSE,
        fill = "#111827",
        alpha = 0.08
      )
  }

  chart_plot <- chart_plot +
    scale_colour_manual(values = palette_values(data, style$palette)) +
    scale_fill_manual(values = palette_values(data, style$palette)) +
    scale_x_date(
      breaks = x_breaks,
      date_labels = style$date_format,
      expand = expansion(mult = c(0.01, 0.04))
    ) +
    labs(
      title = style$title,
      subtitle = style$subtitle,
      tag = style$y_axis_label,
      x = NULL,
      y = NULL,
      caption = style$note,
      colour = NULL,
      fill = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.tag = element_text(face = "plain", size = 16, colour = "#0f172a", hjust = 0),
      plot.tag.position = c(0, 0.965),
      plot.title = element_text(face = "bold", size = 16, hjust = 0, margin = margin(b = 12)),
      plot.subtitle = element_text(size = 11, colour = "#475569", hjust = 0, margin = margin(b = 10)),
      plot.caption = element_text(size = 9, colour = "#4b5563"),
      plot.title.position = "panel",
      plot.caption.position = "plot",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = style$legend,
      legend.title = element_blank(),
      plot.background = element_rect(fill = "white", colour = NA)
    )

  if (isTRUE(style$invert_y_axis)) {
    chart_plot <- chart_plot +
      scale_y_reverse(
        limits = c(axis_settings$max, axis_settings$min),
        breaks = rev(axis_settings$breaks),
        expand = expansion(mult = c(0.02, 0.08))
      )
  } else {
    chart_plot <- chart_plot +
      scale_y_continuous(
        limits = c(axis_settings$min, axis_settings$max),
        breaks = axis_settings$breaks,
        expand = expansion(mult = c(0.02, 0.08))
      )
  }

  chart_plot
}

build_chart_widget <- function(data, style) {
  widget <- ggplotly(build_chart_plot(data, style), tooltip = c("x", "y", "colour"))
  note_text <- trimws(style$note %||% "")
  y_axis_label <- trimws(style$y_axis_label %||% "")
  layout_args <- list()
  annotations <- list()

  if (nzchar(note_text)) {
    layout_args$margin <- list(b = 90)
    annotations <- c(annotations, list(
      list(
        text = note_text,
        x = 0,
        y = -0.2,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        yanchor = "top",
        showarrow = FALSE,
        align = "left",
        font = list(size = 12, color = "#4b5563")
      )
    ))
  }

  if (nzchar(y_axis_label)) {
    existing_margin <- layout_args$margin %||% list()
    layout_args$margin <- modifyList(list(t = 92), existing_margin)
    annotations <- c(annotations, list(
      list(
        text = y_axis_label,
        x = 0,
        y = 1.02,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        yanchor = "bottom",
        showarrow = FALSE,
        align = "left",
        font = list(size = 16, color = "#0f172a")
      )
    ))
  }

  if (length(annotations) > 0) {
    layout_args$annotations <- annotations
  }

  layout_args$title <- list(x = 0.07, xanchor = "left")

  if (identical(style$legend, "none")) {
    layout_args$showlegend <- FALSE
  } else {
    layout_args$legend <- list(
      orientation = if (identical(style$legend, "bottom")) "h" else "v"
    )
  }

  if (length(layout_args) > 0) {
    widget <- do.call(
      plotly::layout,
      c(list(p = widget), layout_args)
    )
  }

  widget
}

empty_plotly_widget <- function(message) {
  plotly::plot_ly(type = "scatter", mode = "markers", x = numeric(0), y = numeric(0)) %>%
    layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      annotations = list(
        list(
          text = message,
          x = 0.5,
          y = 0.5,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = 16, color = "#64748b")
        )
      )
    )
}

wide_chart_data <- function(data) {
  if (nrow(data) == 0) {
    return(tibble::tibble())
  }

  data %>%
    select(date, name, value) %>%
    arrange(date, name) %>%
    pivot_wider(names_from = name, values_from = value)
}

chart_summary_metrics <- function(data) {
  if (nrow(data) == 0) {
    return(list(series_count = 0, rows = 0, range = "No data"))
  }

  list(
    series_count = dplyr::n_distinct(data$name),
    rows = nrow(data),
    range = paste(format(min(data$date), "%b %Y"), "to", format(max(data$date), "%b %Y"))
  )
}

restore_series_spec <- function(session, index, spec = NULL) {
  restored_specs <- session$userData$restored_series_specs %||% list()
  restored_specs[[as.character(index)]] <- if (is.null(spec)) NULL else normalize_series_spec(spec)
  session$userData$restored_series_specs <- restored_specs

  updateRadioGroupButtons(session, series_input_id(index, "enabled"), selected = if (is.null(spec)) "0" else "1")

  if (is.null(spec)) {
    return(invisible(NULL))
  }

  updateSelectInput(session, series_input_id(index, "source"), selected = spec$source)
  updateTextInput(session, series_input_id(index, "label"), value = spec$label %||% "")
  updateRadioGroupButtons(session, series_input_id(index, "vis_type"), selected = spec$vis_type %||% "line")

  session$onFlushed(function() {
    if (identical(spec$source, "ABS CPI")) {
      updateSelectizeInput(session, series_input_id(index, "text"), selected = spec$text, server = TRUE)
      updateSelectizeInput(session, series_input_id(index, "region"), selected = spec$region, server = TRUE)
      updateSelectInput(session, series_input_id(index, "transform"), selected = spec$transform)
      updateDateInput(session, series_input_id(index, "rebase_date"), value = spec$rebase_date)
    }

    if (identical(spec$source, "FRED")) {
      updateTextInput(session, series_input_id(index, "fred_series"), value = spec$fred_series %||% "")
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

    if (identical(spec$source, "dbnomics")) {
      updateTextInput(session, series_input_id(index, "dbnomics_series"), value = spec$dbnomics_series %||% "")
    }

    if (identical(spec$source, "rba")) {
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

    if (identical(spec$source, "abs")) {
      catalogue_index <- match(spec$abs_catalogue, abs_cat)
      catalogue_data <- abs_ref[[catalogue_index]] %||% abs_ref[[1]]
      desc_choices <- unique(catalogue_data$series)
      type_choices <- catalogue_data %>%
        filter(series == spec$abs_desc) %>%
        pull(series_type) %>%
        unique()
      table_choices <- catalogue_data %>%
        filter(series == spec$abs_desc, series_type == spec$abs_series_type) %>%
        pull(table_title) %>%
        unique()
      id_choices <- catalogue_data %>%
        filter(
          series == spec$abs_desc,
          series_type == spec$abs_series_type,
          table_title == spec$abs_table
        ) %>%
        pull(series_id)

      updateSelectInput(session, series_input_id(index, "abs_catalogue"), selected = spec$abs_catalogue)
      updateSelectizeInput(
        session,
        series_input_id(index, "abs_desc"),
        choices = desc_choices,
        selected = spec$abs_desc,
        server = TRUE
      )
      updateSelectInput(
        session,
        series_input_id(index, "abs_series_type"),
        choices = type_choices,
        selected = spec$abs_series_type
      )
      updateSelectInput(
        session,
        series_input_id(index, "abs_table"),
        choices = table_choices,
        selected = spec$abs_table
      )
      updateSelectizeInput(
        session,
        series_input_id(index, "abs_id"),
        choices = id_choices,
        selected = spec$abs_id,
        server = TRUE
      )
    }
  }, once = TRUE)

  invisible(NULL)
}

restore_chart_state <- function(session, chart_state) {
  chart_state <- normalize_chart_state(chart_state)

  updateDateInput(session, "start_date", value = min(chart_state$date_range))
  updateDateInput(session, "end_date", value = max(chart_state$date_range))
  updateRadioGroupButtons(session, "viewData1", selected = if (isTRUE(chart_state$show_table)) "1" else "0")

  updateTextInput(session, "style_title", value = chart_state$style$title)
  updateTextInput(session, "style_subtitle", value = chart_state$style$subtitle)
  updateTextInput(session, "style_y_axis_label", value = chart_state$style$y_axis_label)
  updateTextInput(session, "style_note", value = chart_state$style$note)
  updateRadioGroupButtons(session, "style_legend", selected = chart_state$style$legend)
  updateSelectInput(session, "style_palette", selected = chart_state$style$palette)
  updateSelectInput(session, "style_date_format", selected = chart_state$style$date_format)
  updateNumericInput(session, "style_x_labels", value = chart_state$style$x_labels)
  updateRadioGroupButtons(session, "style_auto_y_axis", selected = if (isTRUE(chart_state$style$auto_y_axis)) "auto" else "manual")
  updateNumericInput(session, "style_y_min", value = chart_state$style$y_min)
  updateNumericInput(session, "style_y_max", value = chart_state$style$y_max)
  updateNumericInput(session, "style_y_breaks", value = chart_state$style$y_breaks)
  updateRadioGroupButtons(session, "style_invert_y_axis", selected = if (isTRUE(chart_state$style$invert_y_axis)) "inverted" else "standard")
  updateNumericInput(session, "style_horizontal_1", value = chart_state$style$horizontal_1)
  updateNumericInput(session, "style_horizontal_2", value = chart_state$style$horizontal_2)
  updateNumericInput(session, "style_horizontal_shading_min", value = chart_state$style$horizontal_shading[1])
  updateNumericInput(session, "style_horizontal_shading_max", value = chart_state$style$horizontal_shading[2])
  updateDateInput(session, "style_vertical_1", value = if (is.na(chart_state$style$vertical_1)) NULL else chart_state$style$vertical_1)
  updateDateInput(session, "style_vertical_2", value = if (is.na(chart_state$style$vertical_2)) NULL else chart_state$style$vertical_2)
  updateSelectInput(session, "style_recession_shading", selected = chart_state$style$recession_shading)
  updateNumericInput(session, "export_width", value = chart_state$style$export_width)
  updateNumericInput(session, "export_height", value = chart_state$style$export_height)

  restore_transform_profile(session, "transform_all", chart_state$all_series_transform)

  for (index in seq_len(MAX_SERIES)) {
    restore_series_spec(session, index, chart_state$series[[index]])
    restore_transform_profile(
      session,
      paste0("transform_", index),
      chart_state$series[[index]]$transform_profile %||% default_transform_profile()
    )
  }

  invisible(NULL)
}

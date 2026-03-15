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

series_enabled <- function(input, index) {
  isTRUE(input[[series_input_id(index, "enabled")]])
}

chart_card <- function(title, ..., class = "") {
  div(
    class = paste("app-card", class),
    div(class = "app-card__header", title),
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

builder_series_ui <- function(index) {
  enabled_id <- series_input_id(index, "enabled")
  source_id <- series_input_id(index, "source")
  transform_id <- series_input_id(index, "transform")

  tabPanel(
    title = paste("Series", index),
    checkboxInput(enabled_id, "Include this series", value = index == 1),
    conditionalPanel(
      condition = sprintf("input.%s", enabled_id),
      selectInput(
        source_id,
        "Data source",
        choices = c("ABS CPI", "FRED", "dbnomics", "rba", "abs"),
        selected = "ABS CPI"
      ),
      conditionalPanel(
        condition = sprintf("input.%s == 'FRED'", source_id),
        textInput(series_input_id(index, "fred_series"), "FRED series ID", value = "UNRATE")
      ),
      conditionalPanel(
        condition = sprintf("input.%s == 'dbnomics'", source_id),
        textInput(
          series_input_id(index, "dbnomics_series"),
          "DBnomics series ID",
          value = "AMECO/ZUTN/EA19.1.0.0.0.ZUTN"
        )
      ),
      conditionalPanel(
        condition = sprintf("input.%s == 'ABS CPI'", source_id),
        selectizeInput(
          series_input_id(index, "text"),
          "Data series",
          choices = list(
            Category_1 = cat1,
            Category_2 = cat2,
            Category_3 = cat3,
            Category_4 = cat4
          ),
          multiple = TRUE
        ),
        selectizeInput(
          series_input_id(index, "region"),
          "Region",
          choices = region_list,
          multiple = TRUE,
          selected = region_list[[1]]
        ),
        selectInput(
          transform_id,
          "Transformation",
          choices = c("index", "y.y", "q.q", "rebased index"),
          selected = "y.y"
        ),
        conditionalPanel(
          condition = sprintf(
            "input.%s == 'ABS CPI' && input.%s == 'rebased index'",
            source_id,
            transform_id
          ),
          dateInput(
            series_input_id(index, "rebase_date"),
            "Rebase date",
            value = as.Date("2019-12-31")
          )
        )
      ),
      conditionalPanel(
        condition = sprintf("input.%s == 'rba'", source_id),
        selectInput(
          series_input_id(index, "rba_table"),
          "RBA table",
          choices = rba_tables,
          selected = rba_tables[[1]]
        ),
        selectizeInput(
          series_input_id(index, "rba_desc"),
          "RBA series",
          choices = rba_series[[1]],
          selected = rba_series[[1]][1],
          multiple = TRUE
        )
      ),
      conditionalPanel(
        condition = sprintf("input.%s == 'abs'", source_id),
        selectInput(
          series_input_id(index, "abs_catalogue"),
          "ABS catalogue",
          choices = abs_cat,
          selected = abs_cat[[1]]
        ),
        selectizeInput(
          series_input_id(index, "abs_desc"),
          "ABS series",
          choices = unique(abs_ref[[1]]$series),
          selected = unique(abs_ref[[1]]$series)[1]
        ),
        selectInput(
          series_input_id(index, "abs_series_type"),
          "Series type",
          choices = unique(abs_ref[[1]]$series_type),
          selected = unique(abs_ref[[1]]$series_type)[1]
        ),
        selectInput(
          series_input_id(index, "abs_table"),
          "Table",
          choices = unique(abs_ref[[1]]$table_title),
          selected = unique(abs_ref[[1]]$table_title)[1]
        ),
        selectizeInput(
          series_input_id(index, "abs_id"),
          "Series ID",
          choices = abs_ref[[1]]$series_id,
          selected = abs_ref[[1]]$series_id[1],
          options = list(create = TRUE),
          multiple = TRUE
        )
      ),
      textInput(series_input_id(index, "label"), "Legend label override", value = ""),
      textInput(series_input_id(index, "expression"), "Series expression", value = "data"),
      selectInput(
        series_input_id(index, "vis_type"),
        "Visual treatment",
        choices = c("line", "bar", "scatter"),
        selected = "line"
      )
    )
  )
}

register_series_dependencies <- function(input, session, index) {
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
    catalogue_index <- match(catalogue_value, abs_cat)
    catalogue_data <- abs_ref[[catalogue_index]] %||% abs_ref[[1]]
    desc_choices <- catalogue_data %>%
      pull(series) %>%
      unique() %>%
      na.omit()

    updateSelectizeInput(
      session,
      series_input_id(index, "abs_desc"),
      choices = desc_choices,
      selected = desc_choices[1],
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
      catalogue_index <- match(catalogue_value, abs_cat)
      catalogue_data <- abs_ref[[catalogue_index]] %||% abs_ref[[1]]

      type_choices <- catalogue_data %>%
        filter(series == desc_value) %>%
        pull(series_type) %>%
        unique() %>%
        na.omit()

      updateSelectInput(
        session,
        series_input_id(index, "abs_series_type"),
        choices = type_choices,
        selected = type_choices[1]
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
      catalogue_index <- match(catalogue_value, abs_cat)
      catalogue_data <- abs_ref[[catalogue_index]] %||% abs_ref[[1]]

      table_choices <- catalogue_data %>%
        filter(series == desc_value, series_type == type_value) %>%
        pull(table_title) %>%
        unique() %>%
        na.omit()

      updateSelectInput(
        session,
        series_input_id(index, "abs_table"),
        choices = table_choices,
        selected = table_choices[1]
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
      catalogue_index <- match(catalogue_value, abs_cat)
      catalogue_data <- abs_ref[[catalogue_index]] %||% abs_ref[[1]]

      id_choices <- catalogue_data %>%
        filter(
          series == desc_value,
          series_type == type_value,
          table_title == table_value
        ) %>%
        pull(series_id) %>%
        na.omit()

      updateSelectizeInput(
        session,
        series_input_id(index, "abs_id"),
        choices = id_choices,
        selected = id_choices[1],
        server = TRUE
      )
    },
    ignoreInit = FALSE
  )
}

series_spec_from_input <- function(input, index) {
  if (!series_enabled(input, index)) {
    return(NULL)
  }

  source_value <- input[[series_input_id(index, "source")]] %||% "ABS CPI"
  spec <- list(
    index = index,
    source = source_value,
    label = trimws(input[[series_input_id(index, "label")]] %||% ""),
    expression = trimws(input[[series_input_id(index, "expression")]] %||% "data"),
    vis_type = input[[series_input_id(index, "vis_type")]] %||% "line"
  )

  if (identical(source_value, "ABS CPI")) {
    selected_series <- input[[series_input_id(index, "text")]]
    if (length(selected_series) == 0) {
      return(NULL)
    }

    spec$text <- selected_series
    spec$region <- input[[series_input_id(index, "region")]] %||% region_list[[1]]
    spec$transform <- input[[series_input_id(index, "transform")]] %||% "y.y"
    spec$rebase_date <- as.Date(input[[series_input_id(index, "rebase_date")]] %||% "2019-12-31")
  }

  if (identical(source_value, "FRED")) {
    spec$fred_series <- trimws(input[[series_input_id(index, "fred_series")]] %||% "")
    if (!nzchar(spec$fred_series)) {
      return(NULL)
    }
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

global_transform_settings_from_input <- function(input) {
  list(
    moving_average = as.numeric(input$moving_avg %||% 1),
    lagged_value = as.numeric(input$lagged_change_val %||% 0),
    lagged_pct = as.numeric(input$lagged_change_pct %||% 0),
    lagged_ann = as.numeric(input$lagged_change_ann %||% 0),
    expression = trimws(input$expression_num %||% "data")
  )
}

style_settings_from_input <- function(input) {
  list(
    title = trimws(input$style_title %||% ""),
    y_axis_label = trimws(input$style_y_axis_label %||% ""),
    note = trimws(input$style_note %||% ""),
    legend = input$style_legend %||% "bottom",
    palette = input$style_palette %||% APP_PALETTES[[1]],
    date_format = input$style_date_format %||% APP_DATE_FORMATS[[2]],
    x_labels = as.numeric(input$style_x_labels %||% 6),
    auto_y_axis = isTRUE(input$style_auto_y_axis),
    y_min = suppressWarnings(as.numeric(input$style_y_min)),
    y_max = suppressWarnings(as.numeric(input$style_y_max)),
    y_breaks = suppressWarnings(as.numeric(input$style_y_breaks)),
    invert_y_axis = isTRUE(input$style_invert_y_axis),
    horizontal_1 = suppressWarnings(as.numeric(input$style_horizontal_1)),
    horizontal_2 = suppressWarnings(as.numeric(input$style_horizontal_2)),
    horizontal_shading = suppressWarnings(as.numeric(input$style_horizontal_shading)),
    vertical_1 = as.Date(input$style_vertical_1),
    vertical_2 = as.Date(input$style_vertical_2),
    recession_shading = input$style_recession_shading %||% "none",
    export_width = as.numeric(input$export_width %||% 8),
    export_height = as.numeric(input$export_height %||% 5)
  )
}

builder_state_from_input <- function(input) {
  list(
    date_range = as.numeric(input$year1 %||% c(2014, lubridate::year(Sys.Date()) + 1)),
    show_table = identical(as.character(input$viewData1), "1"),
    series = lapply(seq_len(MAX_SERIES), function(index) series_spec_from_input(input, index)),
    transforms = global_transform_settings_from_input(input),
    style = style_settings_from_input(input)
  )
}

safe_transform_expression <- function(data, expression, label) {
  expression_text <- trimws(expression %||% "data")

  if (!nzchar(expression_text) || identical(expression_text, "data")) {
    return(data)
  }

  tryCatch(
    {
      data %>%
        mutate(value = eval(parse(text = sub("data", "value", expression_text))))
    },
    error = function(error) {
      stop(sprintf("%s expression is invalid: %s", label, conditionMessage(error)), call. = FALSE)
    }
  )
}

apply_series_metadata <- function(data, spec) {
  data <- safe_transform_expression(data, spec$expression, paste("Series", spec$index))

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

query_series_data <- function(spec, date_range) {
  start_date <- lubridate::ymd(min(date_range), truncated = 2L)
  end_date <- lubridate::ymd(max(date_range), truncated = 2L)

  if (identical(spec$source, "ABS CPI")) {
    return(
      cpi_splits_cust(
        cpi_data = cpi_data_all,
        transformation = spec$transform,
        dates = date_range,
        pick_split_1 = spec$text,
        region_1_split = spec$region,
        rebase_date = spec$rebase_date
      )
    )
  }

  if (identical(spec$source, "FRED")) {
    return(fred_data(series = spec$fred_series, start_date = start_date, end_date = end_date))
  }

  if (identical(spec$source, "dbnomics")) {
    return(db_data(series = spec$dbnomics_series, start_date = start_date, end_date = end_date))
  }

  if (identical(spec$source, "rba")) {
    return(rba_data(series = spec$rba_desc))
  }

  if (identical(spec$source, "abs")) {
    return(abs_data(series = spec$abs_id))
  }

  tibble::tibble()
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

apply_global_transforms <- function(data, transforms) {
  transformed <- data %>%
    arrange(date, name)

  transformed <- apply_moving_average(transformed, transforms$moving_average)
  transformed <- apply_lagged_value(transformed, transforms$lagged_value)
  transformed <- apply_lagged_pct(transformed, transforms$lagged_pct)
  transformed <- apply_lagged_annualised(transformed, transforms$lagged_ann)

  tryCatch(
    safe_transform_expression(transformed, transforms$expression, "Global"),
    error = function(error) {
      attr(transformed, "warning_message") <- conditionMessage(error)
      transformed
    }
  )
}

build_chart_data <- function(chart_state) {
  specs <- Filter(Negate(is.null), chart_state$series)
  messages <- character()

  if (length(specs) == 0) {
    return(list(data = tibble::tibble(), messages = "Choose at least one series to build a chart."))
  }

  series_results <- lapply(specs, function(spec) {
    tryCatch(
      {
        queried_data <- query_series_data(spec, chart_state$date_range)

        if (is.null(queried_data) || nrow(queried_data) == 0) {
          return(list(
            data = tibble::tibble(),
            message = sprintf("Series %s returned no rows.", spec$index)
          ))
        }

        list(
          data = apply_series_metadata(queried_data, spec),
          message = NULL
        )
      },
      error = function(error) {
        list(
          data = tibble::tibble(),
          message = sprintf("Series %s (%s): %s", spec$index, spec$source, conditionMessage(error))
        )
      }
    )
  })

  messages <- c(messages, vapply(series_results, function(result) result$message %||% "", character(1)))
  messages <- messages[nzchar(messages)]

  combined_data <- series_results %>%
    lapply(`[[`, "data") %>%
    bind_rows() %>%
    arrange(date, name)

  if (nrow(combined_data) == 0) {
    return(list(data = combined_data, messages = unique(messages)))
  }

  combined_data <- make_unique_series_names(combined_data)
  combined_data <- apply_global_transforms(combined_data, chart_state$transforms)

  warning_message <- attr(combined_data, "warning_message")
  if (!is.null(warning_message)) {
    messages <- c(messages, warning_message)
    attr(combined_data, "warning_message") <- NULL
  }

  list(data = combined_data, messages = unique(messages))
}

pretty_axis_breaks <- function(data, style) {
  if (nrow(data) == 0) {
    return(list(min = 0, max = 1, by = 0.25))
  }

  if (!style$auto_y_axis &&
      all(is.finite(c(style$y_min, style$y_max, style$y_breaks))) &&
      style$y_max > style$y_min &&
      style$y_breaks > 0) {
    return(list(min = style$y_min, max = style$y_max, by = style$y_breaks))
  }

  data_range <- range(data$value, na.rm = TRUE)

  if (!all(is.finite(data_range))) {
    return(list(min = 0, max = 1, by = 0.25))
  }

  if (diff(data_range) == 0) {
    data_range <- data_range + c(-0.5, 0.5)
  }

  axis_values <- pretty(data_range, n = 6)

  list(
    min = min(axis_values),
    max = max(axis_values),
    by = axis_values[2] - axis_values[1]
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

  if (!is.na(style$vertical_1)) {
    chart_plot <- chart_plot + geom_vline(xintercept = as.numeric(style$vertical_1), linetype = "dashed", colour = "#6b7280")
  }

  if (!is.na(style$vertical_2)) {
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
      x = NULL,
      y = style$y_axis_label,
      caption = style$note,
      colour = NULL,
      fill = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 16, margin = margin(b = 12)),
      plot.caption = element_text(size = 9, colour = "#4b5563"),
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
        breaks = seq(axis_settings$max, axis_settings$min, by = -axis_settings$by),
        expand = expansion(mult = c(0.02, 0.08))
      )
  } else {
    chart_plot <- chart_plot +
      scale_y_continuous(
        limits = c(axis_settings$min, axis_settings$max),
        breaks = seq(axis_settings$min, axis_settings$max, by = axis_settings$by),
        expand = expansion(mult = c(0.02, 0.08))
      )
  }

  chart_plot
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
  updateCheckboxInput(session, series_input_id(index, "enabled"), value = !is.null(spec))

  if (is.null(spec)) {
    return(invisible(NULL))
  }

  updateSelectInput(session, series_input_id(index, "source"), selected = spec$source)
  updateTextInput(session, series_input_id(index, "label"), value = spec$label %||% "")
  updateTextInput(session, series_input_id(index, "expression"), value = spec$expression %||% "data")
  updateSelectInput(session, series_input_id(index, "vis_type"), selected = spec$vis_type %||% "line")

  if (identical(spec$source, "ABS CPI")) {
    updateSelectizeInput(session, series_input_id(index, "text"), selected = spec$text, server = TRUE)
    updateSelectizeInput(session, series_input_id(index, "region"), selected = spec$region, server = TRUE)
    updateSelectInput(session, series_input_id(index, "transform"), selected = spec$transform)
    updateDateInput(session, series_input_id(index, "rebase_date"), value = spec$rebase_date)
  }

  if (identical(spec$source, "FRED")) {
    updateTextInput(session, series_input_id(index, "fred_series"), value = spec$fred_series %||% "")
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

  invisible(NULL)
}

restore_chart_state <- function(session, chart_state) {
  updateSliderInput(session, "year1", value = chart_state$date_range)
  updateSelectInput(session, "viewData1", selected = if (isTRUE(chart_state$show_table)) "1" else "0")

  updateNumericInput(session, "moving_avg", value = chart_state$transforms$moving_average)
  updateNumericInput(session, "lagged_change_val", value = chart_state$transforms$lagged_value)
  updateNumericInput(session, "lagged_change_pct", value = chart_state$transforms$lagged_pct)
  updateNumericInput(session, "lagged_change_ann", value = chart_state$transforms$lagged_ann)
  updateTextInput(session, "expression_num", value = chart_state$transforms$expression)

  updateTextInput(session, "style_title", value = chart_state$style$title)
  updateTextInput(session, "style_y_axis_label", value = chart_state$style$y_axis_label)
  updateTextInput(session, "style_note", value = chart_state$style$note)
  updateSelectInput(session, "style_legend", selected = chart_state$style$legend)
  updateSelectInput(session, "style_palette", selected = chart_state$style$palette)
  updateSelectInput(session, "style_date_format", selected = chart_state$style$date_format)
  updateNumericInput(session, "style_x_labels", value = chart_state$style$x_labels)
  updateCheckboxInput(session, "style_auto_y_axis", value = chart_state$style$auto_y_axis)
  updateNumericInput(session, "style_y_min", value = chart_state$style$y_min)
  updateNumericInput(session, "style_y_max", value = chart_state$style$y_max)
  updateNumericInput(session, "style_y_breaks", value = chart_state$style$y_breaks)
  updateCheckboxInput(session, "style_invert_y_axis", value = chart_state$style$invert_y_axis)
  updateNumericInput(session, "style_horizontal_1", value = chart_state$style$horizontal_1)
  updateNumericInput(session, "style_horizontal_2", value = chart_state$style$horizontal_2)
  updateNumericRangeInput(session, "style_horizontal_shading", value = chart_state$style$horizontal_shading)
  updateDateInput(session, "style_vertical_1", value = chart_state$style$vertical_1)
  updateDateInput(session, "style_vertical_2", value = chart_state$style$vertical_2)
  updateSelectInput(session, "style_recession_shading", selected = chart_state$style$recession_shading)
  updateNumericInput(session, "export_width", value = chart_state$style$export_width)
  updateNumericInput(session, "export_height", value = chart_state$style$export_height)

  for (index in seq_len(MAX_SERIES)) {
    restore_series_spec(session, index, chart_state$series[[index]])
  }

  invisible(NULL)
}

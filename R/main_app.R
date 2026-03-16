build_main_ui <- function() {
  year_min <- as.numeric(lubridate::year(min(cpi_data_all$date)))
  year_max <- as.numeric(lubridate::year(max(cpi_data_all$date))) + 1

  navbarPage(
    title = "Data Explorer",
    id = "main_tabs",
    tabPanel(
      title = "Chart Builder",
      value = "builder",
      div(
        class = "page-shell",
        fluidRow(
          column(
            width = 3,
            chart_card(
              "Data Window",
              tags$p(
                class = "muted-copy",
                "Set the date window and choose whether to show the data table."
              ),
              fluidRow(
                column(
                  width = 6,
                  numericInput("start_year", "Start year", value = max(year_min, year_max - 12), min = year_min, max = year_max)
                ),
                column(
                  width = 6,
                  numericInput("end_year", "End year", value = year_max, min = year_min, max = year_max)
                )
              ),
              radioGroupButtons(
                "viewData1",
                "Underlying data table",
                choices = c("Hide" = "0", "Show" = "1"),
                selected = "0",
                justified = TRUE,
                checkIcon = list(yes = icon("check"))
              )
            ),
            chart_card(
              "Series Setup",
              tags$p(
                class = "muted-copy",
                "Choose the source, series, and chart style. Use the right panel for transforms."
              ),
              do.call(
                tabsetPanel,
                c(
                  list(id = "series_tabs", type = "tabs"),
                  lapply(seq_len(MAX_SERIES), builder_series_ui)
                )
              )
            ),
            chart_card(
              "Save to Library",
              tags$p(
                class = "muted-copy",
                "Save the current chart to the library."
              ),
              textInput("library_title", "Library title", value = ""),
              textAreaInput("library_description", "Library description", value = "", rows = 3, resize = "vertical"),
              actionButton("save_chart", "Save chart to library", class = "btn-primary btn-block")
            )
          ),
          column(
            width = 6,
            chart_card(
              "Primary Chart",
              uiOutput("builder_summary"),
              uiOutput("builder_messages"),
              plotlyOutput("builder_plot", height = "500px"),
              div(
                class = "export-row",
                downloadButton("exportPNG", "Download PNG"),
                downloadButton("exportHTML", "Download HTML"),
                downloadButton("exportData", "Download CSV")
              )
            ),
            conditionalPanel(
              condition = "input.viewData1 == '1'",
              chart_card(
                "Underlying Data",
                DT::dataTableOutput("builder_table")
              )
            ),
            chart_card(
              "Presentation",
              tags$p(
                class = "muted-copy",
                "Set titles, axes, guides, colours, and export options."
              ),
              fluidRow(
                column(
                  width = 4,
                  textInput("style_title", "Chart title shown above the plot", value = "Custom data view"),
                  textInput("style_y_axis_label", "Y-axis label", value = "%"),
                  textInput("style_note", "Source note or caption shown below the chart", value = "Source: custom query")
                ),
                column(
                  width = 4,
                  selectInput("style_palette", "Colour palette", choices = APP_PALETTES, selected = APP_PALETTES[[1]]),
                  radioGroupButtons(
                    "style_legend",
                    "Legend position",
                    choices = c("Bottom" = "bottom", "Right" = "right", "Hide" = "none"),
                    selected = "bottom",
                    justified = TRUE,
                    checkIcon = list(yes = icon("check"))
                  ),
                  selectInput("style_date_format", "Date label format on the x-axis", choices = APP_DATE_FORMATS, selected = APP_DATE_FORMATS[[2]]),
                  numericInput("style_x_labels", "Approximate number of x-axis labels", value = 6, min = 2, step = 1)
                ),
                column(
                  width = 4,
                  radioGroupButtons(
                    "style_auto_y_axis",
                    "Y-axis range mode",
                    choices = c("Automatic" = "auto", "Manual" = "manual"),
                    selected = "auto",
                    justified = TRUE,
                    checkIcon = list(yes = icon("check"))
                  ),
                  conditionalPanel(
                    condition = "input.style_auto_y_axis == 'manual'",
                    numericInput("style_y_min", "Manual y-axis minimum", value = NA),
                    numericInput("style_y_max", "Manual y-axis maximum", value = NA),
                    numericInput("style_y_breaks", "Manual y-axis interval", value = NA, min = 0.01)
                  ),
                  radioGroupButtons(
                    "style_invert_y_axis",
                    "Y-axis direction",
                    choices = c("Standard" = "standard", "Inverted" = "inverted"),
                    selected = "standard",
                    justified = TRUE,
                    checkIcon = list(yes = icon("check"))
                  ),
                  numericInput("export_width", "PNG export width", value = 8, min = 4, step = 0.5),
                  numericInput("export_height", "PNG export height", value = 5, min = 3, step = 0.5)
                )
              ),
              tags$hr(),
              fluidRow(
                column(
                  width = 4,
                  numericInput("style_horizontal_1", "Horizontal reference line 1", value = NA),
                  numericInput("style_horizontal_2", "Horizontal reference line 2", value = NA)
                ),
                column(
                  width = 4,
                  numericInput("style_horizontal_shading_min", "Horizontal shaded band minimum", value = NA),
                  numericInput("style_horizontal_shading_max", "Horizontal shaded band maximum", value = NA),
                  selectInput("style_recession_shading", "Recession shading region", choices = c("none", rec_regions), selected = "none")
                ),
                column(
                  width = 4,
                  dateInput("style_vertical_1", "Vertical marker date 1", value = NULL),
                  dateInput("style_vertical_2", "Vertical marker date 2", value = NULL)
                )
              )
            )
          ),
          column(
            width = 3,
            chart_card(
              "Workspace Tools",
              radioGroupButtons(
                "side_panel_mode",
                "Right-hand workspace",
                choices = c("Transformations" = "transform", "Analysis" = "analysis"),
                selected = "transform",
                justified = TRUE,
                checkIcon = list(yes = icon("check"))
              ),
              conditionalPanel(
                condition = "input.side_panel_mode == 'transform'",
                do.call(
                  tabsetPanel,
                  c(
                    list(id = "transform_tabs", type = "tabs"),
                    list(transform_profile_ui("transform_all", "All series")),
                    lapply(seq_len(MAX_SERIES), function(index) {
                      transform_profile_ui(
                        paste0("transform_", index),
                        paste("Series", c("one", "two", "three", "four")[index])
                      )
                    })
                  )
                )
              ),
              conditionalPanel(
                condition = "input.side_panel_mode == 'analysis'",
                tabsetPanel(
                  id = "analysis_tabs",
                  type = "tabs",
                  tabPanel(
                    "Correlations",
                    tags$p(class = "muted-copy", "Compare two series with a rolling correlation. The chart appears in the main panel."),
                    selectInput("analysis_corr_x", "Series on the left-hand side", choices = character()),
                    selectInput("analysis_corr_y", "Series on the right-hand side", choices = character()),
                    numericInput("analysis_corr_window", "Rolling correlation window length (observations)", value = 4, min = 2, step = 1),
                    uiOutput("analysis_corr_summary")
                  ),
                  tabPanel(
                    "Regression",
                    tags$p(class = "muted-copy", "Run a simple regression with standard or robust errors. The chart appears in the main panel."),
                    selectInput("analysis_reg_y", "Dependent series (y)", choices = character()),
                    selectInput("analysis_reg_x", "Independent series (x)", choices = character()),
                    radioGroupButtons(
                      "analysis_reg_errors",
                      "Standard error treatment",
                      choices = c("Homoskedastic" = "classical", "Heteroskedastic-robust" = "robust"),
                      selected = "classical",
                      justified = TRUE,
                      checkIcon = list(yes = icon("check"))
                    ),
                    uiOutput("analysis_reg_summary")
                  ),
                  tabPanel(
                    "Forecast",
                    tags$p(class = "muted-copy", "Run AR, MA, or ARMA forecasts and compare holdout results. The chart appears in the main panel."),
                    selectInput("analysis_forecast_series", "Series to forecast", choices = character()),
                    radioGroupButtons(
                      "analysis_forecast_family",
                      "Model family",
                      choices = c("AR" = "AR", "MA" = "MA", "ARMA" = "ARMA"),
                      selected = "AR",
                      justified = TRUE,
                      checkIcon = list(yes = icon("check"))
                    ),
                    radioGroupButtons(
                      "analysis_forecast_window_mode",
                      "Estimation strategy",
                      choices = c(
                        "Expanding window" = "expanding",
                        "Fixed window" = "fixed",
                        "Rolling window" = "rolling"
                      ),
                      selected = "expanding",
                      justified = TRUE,
                      checkIcon = list(yes = icon("check"))
                    ),
                    conditionalPanel(
                      condition = "input.analysis_forecast_window_mode != 'expanding'",
                      numericInput(
                        "analysis_forecast_window_size",
                        "Window length used for fixed or rolling estimation (observations)",
                        value = 12,
                        min = 8,
                        step = 1
                      )
                    ),
                    numericInput("analysis_forecast_ar", "AR lag", value = 1, min = 0, step = 1),
                    numericInput("analysis_forecast_ma", "MA lag", value = 0, min = 0, step = 1),
                    numericInput(
                      "analysis_forecast_holdout",
                      "Hold out this many observations for pseudo out-of-sample checking",
                      value = 0,
                      min = 0,
                      step = 1
                    ),
                    numericInput("analysis_forecast_horizon", "Forecast horizon", value = 4, min = 1, step = 1),
                    uiOutput("analysis_forecast_summary")
                  )
                )
              )
            )
          )
        )
      )
    ),
    tabPanel(
      title = "Saved Charts",
      value = "library",
      div(
        class = "page-shell",
        fluidRow(
          column(
            width = 4,
            chart_card(
              "Chart Library",
              textInput("library_search", "Search saved charts", value = "", placeholder = "Search by title, description, or source"),
              DT::dataTableOutput("library_table"),
              div(
                class = "library-actions",
                actionButton("load_chart", "Load into builder"),
                actionButton("update_chart", "Update selected"),
                actionButton("delete_chart", "Delete saved chart")
              ),
              div(
                class = "library-actions",
                downloadButton("export_saved_chart", "Export selected chart"),
                downloadButton("export_chart_presentation", "Export chart presentation")
              )
            )
          ),
          column(
            width = 8,
            chart_card(
              "Selected Preview",
              uiOutput("library_selected_meta"),
              plotlyOutput("library_plot", height = "500px"),
              DT::dataTableOutput("library_preview_table")
            )
          )
        )
      )
    ),
    header = tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
    ),
    collapsible = TRUE
  )
}

build_main_server <- function(input, output, session) {
  ensure_chart_library()

  for (index in seq_len(MAX_SERIES)) {
    register_series_dependencies(input, output, session, index)
  }

  observeEvent(
    {
      list(
        input$transform_all_moving_average,
        input$transform_all_lagged_value,
        input$transform_all_lagged_pct,
        input$transform_all_lagged_ann,
        input$transform_all_expression
      )
    },
    {
    all_profile <- transform_profile_from_input(input, "transform_all")

    for (index in seq_len(MAX_SERIES)) {
      restore_transform_profile(session, paste0("transform_", index), all_profile)
    }
    },
    ignoreInit = TRUE
  )

  restored_state <- reactiveVal(NULL)
  synced_library_title <- reactiveVal("")

  observe({
    current_chart_title <- trimws(input$style_title %||% "")
    current_library_title <- trimws(input$library_title %||% "")
    prior_synced_title <- trimws(synced_library_title() %||% "")

    if (!nzchar(current_library_title) || identical(current_library_title, prior_synced_title)) {
      updateTextInput(session, "library_title", value = current_chart_title)
      synced_library_title(current_chart_title)
    }
  })

  builder_state <- reactive({
    input_state <- normalize_chart_state(builder_state_from_input(input))
    loaded_state <- restored_state()

    if (!is.null(loaded_state)) {
      normalized_loaded_state <- normalize_chart_state(loaded_state)

      if (chart_states_equal(input_state, normalized_loaded_state)) {
        restored_state(NULL)
      } else {
        return(normalized_loaded_state)
      }
    }

    input_state$date_range <- sort(c(max(1900, input_state$date_range[[1]]), max(1900, input_state$date_range[[2]])))
    input_state
  })

  chart_payload <- reactive({
    build_chart_data(builder_state())
  })

  chart_data <- reactive({
    chart_payload()$data
  })

  current_plot <- reactive({
    build_chart_plot(chart_data(), builder_state()$style)
  })

  current_widget <- reactive({
    build_chart_widget(chart_data(), builder_state()$style)
  })

  chart_library_store <- reactiveVal(read_chart_library())

  output$builder_summary <- renderUI({
    metrics <- chart_summary_metrics(chart_data())

    div(
      class = "summary-chip-row",
      summary_chip("Series", metrics$series_count),
      summary_chip("Rows", scales::comma(metrics$rows)),
      summary_chip("Coverage", metrics$range)
    )
  })

  output$builder_messages <- renderUI({
    messages <- chart_payload()$messages

    if (length(messages) == 0) {
      return(NULL)
    }

    div(
      class = "message-stack",
      lapply(messages, function(message) {
        div(class = "message-banner", message)
      })
    )
  })

  output$builder_plot <- renderPlotly({
    main_panel_widget()
  })

  output$builder_table <- DT::renderDataTable({
    chart_table <- wide_chart_data(chart_data())
    validate(need(nrow(chart_table) > 0, "No data is available for the current selection."))

    DT::datatable(chart_table, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$exportPNG <- downloadHandler(
    filename = function() {
      paste0(str_replace_all(builder_state()$style$title %||% "chart", "[^A-Za-z0-9]+", "_"), ".png")
    },
    content = function(file) {
      req(nrow(chart_data()) > 0)
      ggplot2::ggsave(
        file,
        plot = current_plot(),
        width = builder_state()$style$export_width,
        height = builder_state()$style$export_height,
        dpi = 300
      )
    }
  )

  output$exportHTML <- downloadHandler(
    filename = function() {
      paste0(str_replace_all(builder_state()$style$title %||% "chart", "[^A-Za-z0-9]+", "_"), ".html")
    },
    content = function(file) {
      req(nrow(chart_data()) > 0)
      saveWidget(current_widget(), file, selfcontained = TRUE)
    }
  )

  output$exportData <- downloadHandler(
    filename = function() {
      paste0(str_replace_all(builder_state()$style$title %||% "chart_data", "[^A-Za-z0-9]+", "_"), ".csv")
    },
    content = function(file) {
      req(nrow(chart_data()) > 0)
      write.csv(wide_chart_data(chart_data()), file, row.names = FALSE)
    }
  )

  observeEvent(input$save_chart, {
    current_data <- chart_data()
    if (nrow(current_data) == 0) {
      showNotification("Build a chart before saving it.", type = "error")
      return(invisible(NULL))
    }

    chart_state <- builder_state()
    save_title <- trimws(input$library_title %||% "")
    save_description <- trimws(input$library_description %||% "")

    chart_record <- new_chart_record(
      chart_state = chart_state,
      data_snapshot = current_data,
      title = if (nzchar(save_title)) save_title else chart_state$style$title,
      description = save_description
    )

    updated_library <- upsert_chart_record(chart_library_store(), chart_record)
    chart_library_store(updated_library)
    write_chart_library(updated_library)

    showNotification("Chart saved to the library.", type = "message")
  })

  filtered_library <- reactive({
    chart_library_store() %>%
      filter_chart_library(input$library_search)
  })

  output$library_table <- DT::renderDataTable({
    library_rows <- filtered_library() %>%
      transmute(
        Title = title,
        Sources = source_summary,
        Series = series_count,
        Saved = format(saved_at, "%Y-%m-%d %H:%M")
      )

    DT::datatable(
      library_rows,
      selection = "multiple",
      rownames = FALSE,
      options = list(dom = "tip", pageLength = 8, scrollX = TRUE)
    )
  })

  selected_chart_records <- reactive({
    selected_rows <- input$library_table_rows_selected
    library_rows <- filtered_library()

    if (length(selected_rows) == 0 || nrow(library_rows) == 0) {
      return(NULL)
    }

    library_rows[selected_rows, , drop = FALSE]
  })

  selected_chart_record <- reactive({
    selected_records <- selected_chart_records()

    if (is.null(selected_records) || nrow(selected_records) == 0) {
      return(NULL)
    }

    selected_records[1, , drop = FALSE]
  })

  output$library_selected_meta <- renderUI({
    chart_record <- selected_chart_record()
    selected_records <- selected_chart_records()

    if (is.null(chart_record)) {
      return(div(class = "empty-state", "Select a saved chart to preview it."))
    }

    description_text <- chart_record$description[[1]] %||% ""
    if (!nzchar(description_text)) {
      description_text <- "No notes saved."
    }

    div(
      class = "library-meta",
      tags$h3(chart_record$title[[1]]),
      tags$p(class = "muted-copy", description_text),
      if (!is.null(selected_records) && nrow(selected_records) > 1) {
        tags$p(class = "muted-copy", paste("Showing the first of", nrow(selected_records), "selected charts."))
      },
      div(
        class = "summary-chip-row",
        summary_chip("Sources", chart_record$source_summary[[1]]),
        summary_chip("Series", chart_record$series_count[[1]]),
        summary_chip("Saved", format(chart_record$saved_at[[1]], "%d %b %Y %H:%M"))
      )
    )
  })

  output$library_plot <- renderPlotly({
    chart_record <- selected_chart_record()
    if (is.null(chart_record)) {
      return(empty_plotly_widget("Select a saved chart to preview it."))
    }

    build_chart_widget(chart_record$data_snapshot[[1]], chart_record$chart_state[[1]]$style)
  })

  output$library_preview_table <- DT::renderDataTable({
    chart_record <- selected_chart_record()
    validate(need(!is.null(chart_record), "Select a saved chart to inspect its data."))

    DT::datatable(
      wide_chart_data(chart_record$data_snapshot[[1]]),
      options = list(pageLength = 6, scrollX = TRUE)
    )
  })

  observeEvent(input$load_chart, {
    selected_records <- selected_chart_records()
    req(!is.null(selected_records), nrow(selected_records) == 1)
    chart_record <- selected_records[1, , drop = FALSE]

    restored_state(chart_record$chart_state[[1]])
    restore_chart_state(session, chart_record$chart_state[[1]])
    updateTextInput(session, "library_title", value = chart_record$title[[1]])
    updateTextAreaInput(session, "library_description", value = chart_record$description[[1]] %||% "")
    updateNavbarPage(session, "main_tabs", selected = "builder")

    showNotification("Saved chart loaded into the builder.", type = "message")
  })

  observeEvent(input$update_chart, {
    selected_records <- selected_chart_records()
    req(!is.null(selected_records), nrow(selected_records) == 1)

    current_data <- chart_data()
    if (nrow(current_data) == 0) {
      showNotification("Build a chart before updating a saved chart.", type = "error")
      return(invisible(NULL))
    }

    chart_record <- selected_records[1, , drop = FALSE]
    chart_state <- builder_state()
    updated_record <- new_chart_record(
      chart_state = chart_state,
      data_snapshot = current_data,
      title = trimws(input$library_title %||% chart_state$style$title),
      description = trimws(input$library_description %||% "")
    )
    updated_record$chart_id <- chart_record$chart_id

    updated_library <- upsert_chart_record(chart_library_store(), updated_record)
    chart_library_store(updated_library)
    write_chart_library(updated_library)

    showNotification("Saved chart updated.", type = "message")
  })

  observeEvent(input$delete_chart, {
    selected_records <- selected_chart_records()
    req(!is.null(selected_records))

    updated_library <- Reduce(
      f = function(library_data, chart_id) {
        delete_chart_record(library_data, chart_id)
      },
      x = selected_records$chart_id,
      init = chart_library_store()
    )
    chart_library_store(updated_library)
    write_chart_library(updated_library)

    showNotification("Saved chart removed from the library.", type = "warning")
  })

  available_series <- reactive({
    analysis_series_choices(chart_data())
  })

  observe({
    series_choices <- available_series()
    choose_distinct_pair <- function(primary_value, secondary_value) {
      if (length(series_choices) == 0) {
        return(list(primary = character(), secondary = character()))
      }

      selected_primary <- if (length(primary_value) > 0 && primary_value %in% series_choices) primary_value else series_choices[1]
      available_secondary <- setdiff(series_choices, selected_primary)
      selected_secondary <- if (length(secondary_value) > 0 && secondary_value %in% available_secondary) {
        secondary_value
      } else if (length(available_secondary) > 0) {
        available_secondary[1]
      } else {
        selected_primary
      }

      list(primary = selected_primary, secondary = selected_secondary)
    }

    corr_pair <- choose_distinct_pair(input$analysis_corr_x %||% character(), input$analysis_corr_y %||% character())
    reg_pair <- choose_distinct_pair(input$analysis_reg_x %||% character(), input$analysis_reg_y %||% character())

    updateSelectInput(session, "analysis_corr_x", choices = series_choices, selected = corr_pair$primary %||% character())
    updateSelectInput(session, "analysis_corr_y", choices = series_choices, selected = corr_pair$secondary %||% character())
    updateSelectInput(session, "analysis_reg_x", choices = series_choices, selected = reg_pair$primary %||% character())
    updateSelectInput(session, "analysis_reg_y", choices = series_choices, selected = reg_pair$secondary %||% character())
    updateSelectInput(session, "analysis_forecast_series", choices = series_choices, selected = series_choices[1] %||% character())
  })

  correlation_result <- reactive({
    req(length(available_series()) >= 2)
    req(nzchar(input$analysis_corr_x), nzchar(input$analysis_corr_y))
    req(input$analysis_corr_x != input$analysis_corr_y)

    rolling_correlation_data(
      chart_data(),
      input$analysis_corr_x,
      input$analysis_corr_y,
      max(2, as.integer(input$analysis_corr_window))
    )
  })

  selected_analysis_widget <- reactive({
    if (!identical(input$side_panel_mode %||% "transform", "analysis")) {
      return(NULL)
    }

    active_tab <- input$analysis_tabs %||% "Correlations"

    if (identical(active_tab, "Correlations")) {
      if (length(available_series()) < 2) {
        return(empty_plotly_widget("Add at least two chart series to run correlation analysis."))
      }

      result <- tryCatch(correlation_result(), error = function(error) NULL)
      if (is.null(result) || nrow(result) == 0) {
        return(empty_plotly_widget("Unable to compute the requested rolling correlation."))
      }

      return(
        ggplotly(
          build_correlation_plot(result, input$analysis_corr_x, input$analysis_corr_y, input$analysis_corr_window),
          tooltip = c("x", "y")
        )
      )
    }

    if (identical(active_tab, "Regression")) {
      if (length(available_series()) < 2) {
        return(empty_plotly_widget("Add at least two chart series to run regression analysis."))
      }

      result <- tryCatch(regression_result(), error = function(error) NULL)
      if (is.null(result)) {
        return(empty_plotly_widget("Unable to estimate the requested regression."))
      }

      return(
        ggplotly(
          build_regression_plot(result, input$analysis_reg_y, input$analysis_reg_x),
          tooltip = c("x", "y")
        )
      )
    }

    if (length(available_series()) < 1) {
      return(empty_plotly_widget("Add at least one chart series to run forecasting."))
    }

    result <- tryCatch(forecast_result(), error = function(error) NULL)
    if (is.null(result)) {
      return(empty_plotly_widget("Unable to estimate the requested forecast model."))
    }

    ggplotly(
      build_forecast_plot(result, input$analysis_forecast_series),
      tooltip = c("x", "y")
    )
  })

  main_panel_widget <- reactive({
    if (identical(input$side_panel_mode %||% "transform", "analysis")) {
      return(selected_analysis_widget() %||% empty_plotly_widget("Choose an analysis to display on the main panel."))
    }

    if (nrow(chart_data()) == 0) {
      return(empty_plotly_widget("Configure at least one valid series to render the chart."))
    }

    current_widget()
  })

  session$userData$debug <- list(
    builder_state = builder_state,
    chart_payload = chart_payload,
    chart_data = chart_data,
    chart_library = reactive(chart_library_store()),
    main_panel_widget = main_panel_widget
  )

  output$analysis_corr_summary <- renderUI({
    if (length(available_series()) < 2) {
      return(div(class = "empty-state", "Add at least two series to run correlation analysis."))
    }

    result <- tryCatch(correlation_result(), error = function(error) NULL)
    if (is.null(result) || nrow(result) == 0) {
      return(div(class = "empty-state", "Unable to compute the requested rolling correlation."))
    }

    latest_row <- tail(result, 1)

    div(
      class = "summary-chip-row",
      summary_chip("Latest corr.", round(latest_row$correlation[[1]], 3)),
      summary_chip("Window", input$analysis_corr_window),
      summary_chip("Points", nrow(result))
    )
  })

  regression_result <- reactive({
    req(length(available_series()) >= 2)
    req(nzchar(input$analysis_reg_x), nzchar(input$analysis_reg_y))
    req(input$analysis_reg_x != input$analysis_reg_y)

    regression_analysis(
      chart_data(),
      dependent = input$analysis_reg_y,
      independent = input$analysis_reg_x,
      error_assumption = input$analysis_reg_errors
    )
  })

  output$analysis_reg_summary <- renderUI({
    if (length(available_series()) < 2) {
      return(div(class = "empty-state", "Add at least two chart series to run regression analysis."))
    }

    result <- tryCatch(regression_result(), error = function(error) NULL)
    if (is.null(result)) {
      return(div(class = "empty-state", "Unable to estimate the requested regression."))
    }

    div(
      class = "summary-chip-row",
      summary_chip("Observations", result$metrics$observations),
      summary_chip("R-squared", round(result$metrics$r_squared, 3)),
      summary_chip("Adj. R-squared", round(result$metrics$adjusted_r_squared, 3))
    )
  })

  forecast_result <- reactive({
    req(length(available_series()) >= 1)
    req(nzchar(input$analysis_forecast_series))

    forecast_analysis(
      chart_data(),
      series_name = input$analysis_forecast_series,
      model_family = input$analysis_forecast_family,
      ar_lag = max(0, as.integer(input$analysis_forecast_ar)),
      ma_lag = max(0, as.integer(input$analysis_forecast_ma)),
      horizon = max(1, as.integer(input$analysis_forecast_horizon)),
      window_mode = input$analysis_forecast_window_mode %||% "expanding",
      window_size = as.integer(input$analysis_forecast_window_size %||% NA),
      holdout_size = max(0, as.integer(input$analysis_forecast_holdout %||% 0))
    )
  })

  output$analysis_forecast_summary <- renderUI({
    if (length(available_series()) < 1) {
      return(div(class = "empty-state", "Add at least one chart series to run forecasting."))
    }

    result <- tryCatch(forecast_result(), error = function(error) NULL)
    if (is.null(result)) {
      return(div(class = "empty-state", "Unable to estimate the requested forecast configuration."))
    }

    rmse_value <- if (is.na(result$metrics$rmse)) "n/a" else round(result$metrics$rmse, 3)
    mae_value <- if (is.na(result$metrics$mae)) "n/a" else round(result$metrics$mae, 3)

    div(
      class = "summary-chip-row",
      summary_chip("Window", stringr::str_to_title(result$metrics$estimation_strategy)),
      summary_chip("Estimation obs", result$metrics$estimation_window),
      summary_chip("Holdout obs", result$metrics$holdout_observations),
      summary_chip("RMSE", rmse_value),
      summary_chip("MAE", mae_value)
    )
  })

  output$export_saved_chart <- downloadHandler(
    filename = function() {
      selected_records <- selected_chart_records()
      req(!is.null(selected_records), nrow(selected_records) == 1)
      paste0(str_replace_all(selected_records$title[[1]], "[^A-Za-z0-9]+", "_"), ".html")
    },
    content = function(file) {
      selected_records <- selected_chart_records()
      req(!is.null(selected_records), nrow(selected_records) == 1)
      chart_record <- selected_records[1, , drop = FALSE]
      saveWidget(
        build_chart_widget(chart_record$data_snapshot[[1]], chart_record$chart_state[[1]]$style),
        file,
        selfcontained = TRUE
      )
    }
  )

  output$export_chart_presentation <- downloadHandler(
    filename = function() {
      paste0("chart_presentation_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    },
    content = function(file) {
      selected_records <- selected_chart_records()
      req(!is.null(selected_records), nrow(selected_records) > 0)

      presentation_sections <- lapply(seq_len(nrow(selected_records)), function(row_index) {
        chart_record <- selected_records[row_index, , drop = FALSE]

        tags$section(
          style = "page-break-after: always; padding: 24px 0;",
          tags$h1(chart_record$title[[1]]),
          tags$p(chart_record$description[[1]] %||% ""),
          build_chart_widget(chart_record$data_snapshot[[1]], chart_record$chart_state[[1]]$style)
        )
      })

      htmltools::save_html(
        htmltools::tagList(
          tags$html(
            tags$head(
              tags$title("Chart Presentation"),
              tags$style("body { font-family: 'Helvetica Neue', Arial, sans-serif; margin: 32px; } h1 { margin-bottom: 12px; } section:last-child { page-break-after: auto; }")
            ),
            tags$body(
              tags$h1("Saved Charts Presentation"),
              tags$p(paste("Generated on", format(Sys.time(), "%d %b %Y %H:%M"))),
              presentation_sections
            )
          )
        ),
        file = file
      )
    }
  )
}

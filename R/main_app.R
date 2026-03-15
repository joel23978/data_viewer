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
            width = 4,
            chart_card(
              "Workspace",
              tags$p(
                class = "muted-copy",
                "Build a chart from live series, tune the presentation, then save it into your library."
              ),
              sliderInput(
                "year1",
                "Date range",
                min = year_min,
                max = year_max,
                value = c(max(year_min, year_max - 12), year_max),
                sep = ""
              ),
              selectInput(
                "viewData1",
                "Show data table",
                choices = c("No" = "0", "Yes" = "1"),
                selected = "0"
              )
            ),
            chart_card(
              "Series Builder",
              do.call(
                tabsetPanel,
                c(
                  list(id = "series_tabs", type = "tabs"),
                  lapply(seq_len(MAX_SERIES), builder_series_ui)
                )
              )
            ),
            chart_card(
              "Transformations",
              numericInput("moving_avg", "Moving average", value = 1, min = 1, step = 1),
              numericInput("lagged_change_val", "Lagged change: value", value = 0, min = 0, step = 1),
              numericInput("lagged_change_pct", "Lagged change: percent", value = 0, min = 0, step = 1),
              numericInput("lagged_change_ann", "Lagged change: annualised percent", value = 0, min = 0, step = 1),
              textInput("expression_num", "Post-processing expression", value = "data")
            ),
            chart_card(
              "Presentation",
              textInput("style_title", "Chart title", value = "Custom data view"),
              textInput("style_y_axis_label", "Y-axis label", value = "%"),
              textInput("style_note", "Caption / source note", value = "Source: custom query"),
              selectInput("style_legend", "Legend placement", choices = APP_LEGEND_POSITIONS, selected = "bottom"),
              selectInput("style_palette", "Palette", choices = APP_PALETTES, selected = APP_PALETTES[[1]]),
              selectInput("style_date_format", "Date format", choices = APP_DATE_FORMATS, selected = APP_DATE_FORMATS[[2]]),
              numericInput("style_x_labels", "Approx. x-axis labels", value = 6, min = 2, step = 1),
              checkboxInput("style_auto_y_axis", "Auto-fit y-axis", value = TRUE),
              conditionalPanel(
                condition = "!input.style_auto_y_axis",
                numericInput("style_y_min", "Y minimum", value = NA),
                numericInput("style_y_max", "Y maximum", value = NA),
                numericInput("style_y_breaks", "Y-axis interval", value = NA, min = 0.01)
              ),
              checkboxInput("style_invert_y_axis", "Invert y-axis", value = FALSE),
              numericInput("style_horizontal_1", "Reference line 1", value = NA),
              numericInput("style_horizontal_2", "Reference line 2", value = NA),
              numericRangeInput("style_horizontal_shading", "Horizontal shading", value = c(NA, NA)),
              dateInput("style_vertical_1", "Vertical marker 1", value = NULL),
              dateInput("style_vertical_2", "Vertical marker 2", value = NULL),
              selectInput(
                "style_recession_shading",
                "Recession shading",
                choices = c("none", rec_regions),
                selected = "none"
              ),
              numericInput("export_width", "PNG width", value = 8, min = 4, step = 0.5),
              numericInput("export_height", "PNG height", value = 5, min = 3, step = 0.5)
            ),
            chart_card(
              "Save To Library",
              textInput("library_title", "Saved chart title", value = ""),
              textAreaInput("library_description", "Description", value = "", rows = 3, resize = "vertical"),
              actionButton("save_chart", "Save chart", class = "btn-primary btn-block")
            )
          ),
          column(
            width = 8,
            chart_card(
              "Preview",
              uiOutput("builder_summary"),
              uiOutput("builder_messages"),
              plotlyOutput("builder_plot", height = "480px"),
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
                actionButton("delete_chart", "Delete saved chart")
              )
            )
          ),
          column(
            width = 8,
            chart_card(
              "Selected Preview",
              uiOutput("library_selected_meta"),
              plotOutput("library_plot", height = "420px"),
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
    register_series_dependencies(input, session, index)
  }

  chart_library_store <- reactiveVal(read_chart_library())
  debounced_builder_state <- shiny::debounce(reactive(builder_state_from_input(input)), millis = 600)

  chart_payload <- reactive({
    build_chart_data(debounced_builder_state())
  })

  chart_data <- reactive({
    chart_payload()$data
  })

  current_plot <- reactive({
    build_chart_plot(chart_data(), debounced_builder_state()$style)
  })

  current_widget <- reactive({
    ggplotly(current_plot(), tooltip = c("x", "y", "colour")) %>%
      layout(legend = list(orientation = "h"))
  })

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
    current_widget()
  })

  output$builder_table <- DT::renderDataTable({
    chart_table <- wide_chart_data(chart_data())
    validate(need(nrow(chart_table) > 0, "No data is available for the current selection."))

    DT::datatable(chart_table, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$exportPNG <- downloadHandler(
    filename = function() {
      paste0(str_replace_all(debounced_builder_state()$style$title %||% "chart", "[^A-Za-z0-9]+", "_"), ".png")
    },
    content = function(file) {
      req(nrow(chart_data()) > 0)
      ggplot2::ggsave(
        file,
        plot = current_plot(),
        width = debounced_builder_state()$style$export_width,
        height = debounced_builder_state()$style$export_height,
        dpi = 300
      )
    }
  )

  output$exportHTML <- downloadHandler(
    filename = function() {
      paste0(str_replace_all(debounced_builder_state()$style$title %||% "chart", "[^A-Za-z0-9]+", "_"), ".html")
    },
    content = function(file) {
      req(nrow(chart_data()) > 0)
      saveWidget(current_widget(), file, selfcontained = TRUE)
    }
  )

  output$exportData <- downloadHandler(
    filename = function() {
      paste0(str_replace_all(debounced_builder_state()$style$title %||% "chart_data", "[^A-Za-z0-9]+", "_"), ".csv")
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

    chart_state <- debounced_builder_state()
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
      selection = "single",
      rownames = FALSE,
      options = list(dom = "tip", pageLength = 8, scrollX = TRUE)
    )
  })

  selected_chart_record <- reactive({
    selected_row <- input$library_table_rows_selected
    library_rows <- filtered_library()

    if (length(selected_row) == 0 || nrow(library_rows) == 0) {
      return(NULL)
    }

    library_rows[selected_row, , drop = FALSE]
  })

  output$library_selected_meta <- renderUI({
    chart_record <- selected_chart_record()

    if (is.null(chart_record)) {
      return(div(class = "empty-state", "Choose a saved chart to preview it here."))
    }

    div(
      class = "library-meta",
      tags$h3(chart_record$title[[1]]),
      tags$p(class = "muted-copy", chart_record$description[[1]] %||% "No description provided."),
      div(
        class = "summary-chip-row",
        summary_chip("Sources", chart_record$source_summary[[1]]),
        summary_chip("Series", chart_record$series_count[[1]]),
        summary_chip("Saved", format(chart_record$saved_at[[1]], "%d %b %Y %H:%M"))
      )
    )
  })

  output$library_plot <- renderPlot({
    chart_record <- selected_chart_record()
    validate(need(!is.null(chart_record), "Choose a saved chart to preview it here."))

    build_chart_plot(
      chart_record$data_snapshot[[1]],
      chart_record$chart_state[[1]]$style
    )
  })

  output$library_preview_table <- DT::renderDataTable({
    chart_record <- selected_chart_record()
    validate(need(!is.null(chart_record), "Choose a saved chart to inspect the underlying data."))

    DT::datatable(
      wide_chart_data(chart_record$data_snapshot[[1]]),
      options = list(pageLength = 6, scrollX = TRUE)
    )
  })

  observeEvent(input$load_chart, {
    chart_record <- selected_chart_record()
    req(!is.null(chart_record))

    restore_chart_state(session, chart_record$chart_state[[1]])
    updateTabsetPanel(session, "main_tabs", selected = "builder")

    showNotification("Saved chart loaded into the builder.", type = "message")
  })

  observeEvent(input$delete_chart, {
    chart_record <- selected_chart_record()
    req(!is.null(chart_record))

    updated_library <- delete_chart_record(chart_library_store(), chart_record$chart_id[[1]])
    chart_library_store(updated_library)
    write_chart_library(updated_library)

    showNotification("Saved chart removed from the library.", type = "warning")
  })
}

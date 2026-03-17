build_search_tab_ui <- function() {
  div(
    class = "page-shell",
    chart_card(
      "Search",
      class = "search-toolbar-card",
      header_actions = actionButton("open_fred_api_key_modal", "Enter FRED key", class = "app-card__header-chip"),
      div(
        class = "search-toolbar",
        div(
          class = "search-toolbar__hero",
          div(
            class = "search-toolbar__hero-copy",
            tags$p(class = "search-toolbar__eyebrow", "Search local metadata and live FRED"),
            tags$h2(class = "search-toolbar__title", "Find a series")
          ),
          div(
            class = "search-toolbar__primary",
            textInput("search_query", "Search terms", value = "", placeholder = "e.g. industrial AND production")
          )
        ),
        div(
          class = "search-toolbar__filters-grid",
          div(
            class = "search-toolbar__group search-toolbar__group--wide",
            div(
              class = "search-toolbar__group-label",
              "Source"
            ),
            radioGroupButtons(
              "search_source_filter",
              NULL,
              choices = c("All" = "all", "Recent" = "Recent", "CPI" = "ABS CPI", "FRED" = "FRED", "RBA" = "RBA", "ABS" = "ABS"),
              selected = "all",
              justified = FALSE,
              checkIcon = list(yes = icon("check"))
            )
          ),
          div(
            class = "search-toolbar__group search-toolbar__group--compact",
            div(
              class = "search-toolbar__group-label",
              "FRED mode"
            ),
            radioGroupButtons(
              "search_fred_mode",
              NULL,
              choices = c("Text" = "full_text", "ID" = "series_id"),
              selected = "full_text",
              justified = FALSE,
              checkIcon = list(yes = icon("check"))
            )
          ),
          div(
            class = "search-toolbar__group",
            div(
              class = "search-toolbar__group-label",
              "Type"
            ),
            radioGroupButtons(
              "search_type_filter",
              NULL,
              choices = search_type_choices(),
              selected = "all",
              justified = FALSE,
              checkIcon = list(yes = icon("check"))
            )
          ),
          div(
            class = "search-toolbar__group",
            div(
              class = "search-toolbar__group-label",
              "Location"
            ),
            radioGroupButtons(
              "search_location_filter",
              NULL,
              choices = search_location_choices(),
              selected = "all",
              justified = FALSE,
              checkIcon = list(yes = icon("check"))
            )
          ),
          div(
            class = "search-toolbar__group search-toolbar__group--select",
            div(
              class = "search-toolbar__group-label",
              "Frequency"
            ),
            uiOutput("search_frequency_filter")
          )
        )
      ),
      uiOutput("search_status")
    ),
    fluidRow(
      column(
        width = 9,
        chart_card(
          "Search Results",
          DT::dataTableOutput("search_results_table")
        )
      ),
      column(
        width = 3,
        chart_card(
          "Selected Result",
          class = "search-sidebar-card",
          uiOutput("search_selected_meta"),
          div(
            class = "search-preview-plot",
            plotOutput("search_preview_plot", height = "250px")
          ),
          radioGroupButtons(
            "search_target_series",
            "Add to",
            choices = search_result_target_choices(),
            selected = "next",
            justified = FALSE,
            checkIcon = list(yes = icon("check"))
          ),
          actionButton("search_add_series", "Add result to builder", class = "btn-primary btn-block")
        )
      )
    )
  )
}

build_library_tab_ui <- function() {
  div(
    class = "page-shell saved-charts-page",
    fluidRow(
      column(
        width = 6,
        chart_card(
          "Chart Library",
          textInput("library_search", "Search saved charts", value = "", placeholder = "Search by title, description, or source"),
          DT::dataTableOutput("library_table"),
          div(
            class = "library-actions",
            actionButton("load_chart", "Load into builder"),
            actionButton("update_chart", "Update selected"),
            actionButton("delete_chart", "Delete saved chart"),
            downloadButton("export_saved_chart", "Export selected chart")
          )
        ),
        chart_card(
          "Presentations Library",
          class = "presentation-library-card",
          textInput("presentation_search", "Search presentations", value = "", placeholder = "Search presentations"),
          textInput("presentation_title", "Presentation title", value = ""),
          textAreaInput("presentation_description", "Presentation notes", value = "", rows = 2, resize = "vertical"),
          DT::dataTableOutput("presentation_table"),
          div(
            class = "library-actions presentation-library-actions",
            actionButton("create_presentation", "Create presentation"),
            actionButton("delete_presentation", "Delete presentation"),
            actionButton("add_to_presentation", "Add selected charts"),
            actionButton("replace_presentation_charts", "Replace with selected charts")
          )
        )
      ),
      column(
        width = 6,
        chart_card(
          "Selected Preview",
          uiOutput("library_selected_meta"),
          plotlyOutput("library_plot", height = "500px")
        ),
        chart_card(
          "Presentation Details",
          uiOutput("presentation_selected_meta"),
          DT::dataTableOutput("presentation_chart_table"),
          uiOutput("presentation_detail_actions"),
          uiOutput("presentation_chart_actions")
        )
      )
    )
  )
}

build_tab_loading_ui <- function(title, message) {
  div(
    class = "page-shell tab-loading-shell",
    div(
      class = "tab-loading-card",
      div(
        class = "tab-loading-state",
        tags$p(class = "tab-loading-eyebrow", title),
        tags$h2(class = "tab-loading-title", "Loading level"),
        tags$p(class = "tab-loading-subtitle", message),
        div(
          class = "tab-loading-frame",
          div(
            class = "tab-loading-bar",
            div(class = "tab-loading-bar__fill")
          )
        )
      )
    )
  )
}

build_tab_error_ui <- function(title, message) {
  div(
    class = "page-shell",
    chart_card(
      title,
      div(
        class = "message-banner",
        message
      )
    )
  )
}

search_activity_message <- function(source_filter = "all", query_text = "") {
  cleaned_query <- trimws(query_text %||% "")
  targets <- switch(
    source_filter %||% "all",
    "FRED" = "FRED",
    "Recent" = "recent saved series",
    "ABS CPI" = "local CPI metadata",
    "RBA" = "local RBA metadata",
    "ABS" = "local ABS metadata",
    "all" = if (nzchar(cleaned_query)) "recent series, local metadata, and FRED" else "recent series and local metadata",
    "search sources"
  )

  if (nzchar(cleaned_query)) {
    sprintf("Searching %s for \"%s\"...", targets, cleaned_query)
  } else {
    sprintf("Refreshing %s...", targets)
  }
}

build_main_ui <- function() {
  year_bounds <- default_year_bounds()

  navbarPage(
    title = "Data Explorer",
    id = "main_tabs",
    selected = "search",
    tabPanel(
      title = "Data Search",
      value = "search",
      uiOutput("search_tab_ui")
    ),
    tabPanel(
      title = "Chart Builder",
      value = "builder",
      div(
        class = "page-shell",
        fluidRow(
          column(
            width = 3,
            class = "builder-left-rail",
            chart_card(
              "Data Window",
              tags$p(
                class = "muted-copy",
                "Set the date window and choose whether to show the data table."
              ),
              radioGroupButtons(
                "date_window_shortcut",
                "Quick range",
                choices = c("1Y" = "1", "3Y" = "3", "5Y" = "5", "10Y" = "10", "Max" = "max"),
                selected = character(0),
                justified = TRUE,
                checkIcon = list(yes = icon("check"))
              ),
              fluidRow(
                column(
                  width = 6,
                  dateInput("start_date", "Start date", value = year_bounds$start_date, min = year_bounds$min_date, max = year_bounds$max_date)
                ),
                column(
                  width = 6,
                  dateInput("end_date", "End date", value = year_bounds$end_date, min = year_bounds$min_date, max = year_bounds$max_date)
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
              class = "series-setup-card",
              header_actions = actionButton("clear_series_setup", "Clear", class = "app-card__header-chip"),
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
              header_actions = actionButton("reset_builder", "Reset", class = "app-card__header-chip"),
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
                downloadButton("exportSVG", "Download SVG"),
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
              header_actions = actionButton("clear_presentation_panel", "Clear", class = "app-card__header-chip"),
              tags$p(
                class = "muted-copy",
                "Set titles, axes, guides, colours, and export options."
              ),
              fluidRow(
                column(
                  width = 4,
                  textInput("style_title", "Chart title shown above the plot", value = "Custom data view"),
                  textInput("style_subtitle", "Chart subtitle shown below the title", value = ""),
                  textInput("style_y_axis_label", "Y-axis label", value = "%"),
                  textInput("style_note", "Source note or caption shown below the chart", value = default_builder_state()$style$note),
                  selectInput("style_font_family", "Chart font", choices = APP_CHART_FONTS, selected = APP_CHART_FONTS[[1]])
                ),
                column(
                  width = 4,
                  selectInput("style_palette", "Colour palette", choices = APP_PALETTES, selected = APP_PALETTES[[1]]),
                  radioGroupButtons(
                    "style_legend",
                    "Legend position",
                    choices = c("Below" = "bottom", "Right" = "right", "Hide" = "none"),
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
                  numericInput("export_width", "PNG export width", value = 7, min = 4, step = 0.5),
                  numericInput("export_height", "PNG export height", value = 5, min = 3, step = 0.5)
                )
              ),
              tags$hr(),
              tags$p(
                class = "muted-copy",
                "Use these controls to add reference lines, shaded bands, and date markers."
              ),
              fluidRow(
                column(
                  width = 4,
                  numericInput("style_horizontal_1", "Horizontal reference line 1", value = NA),
                  numericInput("style_horizontal_2", "Horizontal reference line 2", value = NA)
                ),
                column(
                  width = 4,
                  tags$div(
                    class = "paired-range-input",
                    tags$label(class = "control-label", `for` = "style_horizontal_shading_min", "Horizontal shaded band"),
                    tags$div(
                      class = "paired-range-input__row",
                      tags$div(
                        class = "paired-range-input__field",
                        numericInput("style_horizontal_shading_min", label = NULL, value = NA)
                      ),
                      tags$div(class = "paired-range-input__separator", "to"),
                      tags$div(
                        class = "paired-range-input__field",
                        numericInput("style_horizontal_shading_max", label = NULL, value = NA)
                      )
                    )
                  ),
                  selectInput("style_recession_shading", "Recession shading region", choices = c("none", rec_regions), selected = "none")
                ),
                column(
                  width = 4,
                  suppressWarnings(dateInput("style_vertical_1", "Vertical marker date 1", value = as.Date(NA))),
                  suppressWarnings(dateInput("style_vertical_2", "Vertical marker date 2", value = as.Date(NA)))
                )
              )
            )
          ),
          column(
            width = 3,
            chart_card(
              "Workspace Tools",
              class = "workspace-tools-card",
              header_actions = actionButton("clear_workspace_tools", "Clear", class = "app-card__header-chip"),
              radioGroupButtons(
                "side_panel_mode",
                NULL,
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
                    list(transform_profile_ui("transform_all", "All")),
                    lapply(seq_len(MAX_SERIES), function(index) {
                      transform_profile_ui(
                        paste0("transform_", index),
                        paste("Series", index)
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
                  ),
                  tabPanel(
                    "Seasonal Adjust",
                    tags$p(class = "muted-copy", "Run X-13ARIMA-SEATS on a monthly or quarterly series. The adjusted chart appears in the main panel."),
                    selectInput("analysis_seasonal_series", "Series to seasonally adjust", choices = character()),
                    radioGroupButtons(
                      "analysis_seasonal_view",
                      "Display",
                      choices = c("Adjusted only" = "adjusted", "Original vs adjusted" = "both"),
                      selected = "both",
                      justified = TRUE,
                      checkIcon = list(yes = icon("check"))
                    ),
                    uiOutput("analysis_seasonal_summary"),
                    radioGroupButtons(
                      "analysis_seasonal_target_series",
                      "Add adjusted result to",
                      choices = c("Series 1" = "1", "Series 2" = "2", "Series 3" = "3", "Series 4" = "4"),
                      selected = "1",
                      justified = TRUE,
                      checkIcon = list(yes = icon("check"))
                    ),
                    actionButton("analysis_add_seasonal_series", "Add to builder", class = "btn-primary btn-block")
                  ),
                  tabPanel(
                    "HP Filter",
                    tags$p(class = "muted-copy", "Estimate a Hodrick-Prescott trend and cycle. The chart appears in the main panel."),
                    selectInput("analysis_hp_series", "Series to filter", choices = character()),
                    radioGroupButtons(
                      "analysis_hp_side",
                      "Filter direction",
                      choices = c("Two-sided" = "two_sided", "One-sided" = "one_sided"),
                      selected = "two_sided",
                      justified = TRUE,
                      checkIcon = list(yes = icon("check"))
                    ),
                    radioGroupButtons(
                      "analysis_hp_view",
                      "Display",
                      choices = filter_display_choices(),
                      selected = "overlay",
                      justified = TRUE,
                      checkIcon = list(yes = icon("check"))
                    ),
                    uiOutput("analysis_hp_summary"),
                    radioGroupButtons(
                      "analysis_hp_target_series",
                      "Add filtered result to",
                      choices = c("Series 1" = "1", "Series 2" = "2", "Series 3" = "3", "Series 4" = "4"),
                      selected = "1",
                      justified = TRUE,
                      checkIcon = list(yes = icon("check"))
                    ),
                    actionButton("analysis_add_hp_series", "Add to builder", class = "btn-primary btn-block")
                  ),
                  tabPanel(
                    "Kalman Filter",
                    tags$p(class = "muted-copy", "Estimate a local linear trend with a Kalman filter. The chart appears in the main panel."),
                    selectInput("analysis_kalman_series", "Series to filter", choices = character()),
                    radioGroupButtons(
                      "analysis_kalman_side",
                      "Filter direction",
                      choices = c("Two-sided" = "two_sided", "One-sided" = "one_sided"),
                      selected = "two_sided",
                      justified = TRUE,
                      checkIcon = list(yes = icon("check"))
                    ),
                    radioGroupButtons(
                      "analysis_kalman_view",
                      "Display",
                      choices = filter_display_choices(),
                      selected = "overlay",
                      justified = TRUE,
                      checkIcon = list(yes = icon("check"))
                    ),
                    uiOutput("analysis_kalman_summary"),
                    radioGroupButtons(
                      "analysis_kalman_target_series",
                      "Add filtered result to",
                      choices = c("Series 1" = "1", "Series 2" = "2", "Series 3" = "3", "Series 4" = "4"),
                      selected = "1",
                      justified = TRUE,
                      checkIcon = list(yes = icon("check"))
                    ),
                    actionButton("analysis_add_kalman_series", "Add to builder", class = "btn-primary btn-block")
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
      uiOutput("library_tab_ui")
    ),
    header = tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
    ),
    collapsible = TRUE
  )
}

build_main_server <- function(input, output, session) {
  ensure_chart_library()
  session$userData$restored_series_specs <- list()
  loaded_main_tabs <- reactiveVal(c("search"))
  tab_load_state <- reactiveValues(search = "idle", library = "idle")
  tab_load_error <- reactiveValues(search = "", library = "")

  fred_key_modal_open <- reactiveVal(FALSE)
  fred_key_prompt_ignored <- reactiveVal(FALSE)

  run_with_status <- function(working_message, success_message, expr, success_type = "message", failure_prefix = NULL) {
    notification_id <- paste0("status-", format(Sys.time(), "%H%M%OS3"), "-", sample.int(9999, 1))
    progress <- shiny::Progress$new(session, min = 0, max = 1)
    progress$set(message = working_message, detail = "Starting...", value = 0.05)

    on.exit(progress$close(), add = TRUE)

    tryCatch(
      {
        progress$set(message = working_message, detail = "Working...", value = 0.35)
        result <- force(expr)
        progress$set(message = success_message, detail = "Done.", value = 1)
        showNotification(
          success_message,
          id = notification_id,
          duration = 4,
          closeButton = TRUE,
          type = success_type
        )
        result
      },
      error = function(error) {
        error_message <- if (is.null(failure_prefix)) {
          conditionMessage(error)
        } else {
          paste(failure_prefix, conditionMessage(error))
        }

        progress$set(message = working_message, detail = "Failed.", value = 1)

        showNotification(
          error_message,
          id = notification_id,
          duration = NULL,
          closeButton = TRUE,
          type = "error"
        )
        invisible(NULL)
      }
    )
  }

  show_fred_key_modal <- function(force = FALSE) {
    if (isTRUE(fred_key_modal_open())) {
      return(invisible(NULL))
    }

    if (!isTRUE(force) && isTRUE(fred_key_prompt_ignored()) && !fred_search_available()) {
      return(invisible(NULL))
    }

    fred_key_modal_open(TRUE)

    showModal(
      modalDialog(
        title = "Enter FRED API Key",
        passwordInput(
          "fred_api_key_value",
          "FRED API key",
          value = current_fred_api_key()
        ),
        tags$p(
          class = "muted-copy",
          "This key is used for the current app session so live FRED search and downloads can run."
        ),
        footer = tagList(
          actionButton("ignore_fred_api_key", "Ignore"),
          actionButton("cancel_fred_api_key", "Cancel"),
          actionButton("save_fred_api_key", "Save key", class = "btn-primary")
        ),
        easyClose = FALSE
      )
    )
  }

  for (index in seq_len(MAX_SERIES)) {
    register_series_dependencies(input, output, session, index)
  }

  observeEvent(input$main_tabs, {
    selected_tab <- input$main_tabs %||% "search"
    loaded_main_tabs(unique(c(loaded_main_tabs(), selected_tab)))
  }, ignoreInit = FALSE)

  output$search_tab_ui <- renderUI({
    req("search" %in% loaded_main_tabs())
    if (identical(tab_load_state$search, "loading")) {
      return(build_tab_loading_ui("Data Search", "Loading search metadata..."))
    }

    if (identical(tab_load_state$search, "error")) {
      return(build_tab_error_ui("Data Search", tab_load_error$search %||% "Unable to load search metadata."))
    }

    build_search_tab_ui()
  })

  output$library_tab_ui <- renderUI({
    req("library" %in% loaded_main_tabs())
    if (identical(tab_load_state$library, "loading")) {
      return(build_tab_loading_ui("Saved Charts", "Loading saved charts and presentations..."))
    }

    if (identical(tab_load_state$library, "error")) {
      return(build_tab_error_ui("Saved Charts", tab_load_error$library %||% "Unable to load saved charts."))
    }

    build_library_tab_ui()
  })

  observeEvent(input$open_fred_api_key_modal, {
    show_fred_key_modal(force = TRUE)
  })

  observeEvent(input$cancel_fred_api_key, {
    fred_key_modal_open(FALSE)
    removeModal()
  })

  observeEvent(input$ignore_fred_api_key, {
    fred_key_prompt_ignored(TRUE)
    fred_key_modal_open(FALSE)
    removeModal()
    showNotification("FRED key prompt ignored for this app session.", type = "message")
  })

  observeEvent(input$save_fred_api_key, {
    entered_key <- trimws(input$fred_api_key_value %||% "")

    if (!nzchar(entered_key)) {
      showNotification("Enter a FRED API key before saving.", type = "warning")
      return(invisible(NULL))
    }

    set_fred_api_key(entered_key)
    fred_key_prompt_ignored(FALSE)
    fred_key_modal_open(FALSE)
    removeModal()
    showNotification("FRED API key saved for this app session.", type = "message")
  })

  observeEvent(input$main_tabs, {
    if (
      identical(input$main_tabs %||% "search", "search") &&
      !fred_search_available() &&
      !isTRUE(fred_key_prompt_ignored())
    ) {
      show_fred_key_modal()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$search_source_filter, {
    if ((input$search_source_filter %||% "all") %in% c("all", "FRED") && !fred_search_available()) {
      show_fred_key_modal()
    }
  }, ignoreInit = TRUE)

  observe({
    series_sources <- vapply(
      seq_len(MAX_SERIES),
      function(index) input[[series_input_id(index, "source")]] %||% "ABS CPI",
      character(1)
    )

    if (any(series_sources == "FRED") && !fred_search_available()) {
      show_fred_key_modal()
    }
  })

  observeEvent(
    {
      list(
        input$transform_all_moving_average,
        input$transform_all_rolling_sum,
        input$transform_all_lagged_value,
        input$transform_all_lagged_pct,
        input$transform_all_lagged_ann,
        input$transform_all_subtract_series,
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
  synced_source_note <- reactiveVal(default_builder_state()$style$note)

  apply_builder_state <- function(chart_state, selected_series_index = NULL, navigate_builder = FALSE) {
    normalized_state <- normalize_chart_state(chart_state)
    restored_state(normalized_state)
    restore_chart_state(session, normalized_state)

    if (!is.null(selected_series_index)) {
      updateTabsetPanel(session, "series_tabs", selected = paste("Series", selected_series_index))
    }

    if (isTRUE(navigate_builder)) {
      updateNavbarPage(session, "main_tabs", selected = "builder")
    }
  }

  observe({
    current_chart_title <- trimws(input$style_title %||% "")
    current_library_title <- trimws(input$library_title %||% "")
    prior_synced_title <- trimws(synced_library_title() %||% "")

    if (!nzchar(current_library_title) || identical(current_library_title, prior_synced_title)) {
      updateTextInput(session, "library_title", value = current_chart_title)
      synced_library_title(current_chart_title)
    }
  })

  observe({
    generated_source_note <- default_source_note(normalize_chart_state(builder_state_from_input(input, session))$series)
    current_source_note <- trimws(input$style_note %||% "")
    prior_synced_note <- trimws(synced_source_note() %||% "")

    if (!nzchar(current_source_note) || identical(current_source_note, prior_synced_note)) {
      updateTextInput(session, "style_note", value = generated_source_note)
      synced_source_note(generated_source_note)
    }
  })

  observeEvent(input$reset_builder, {
    default_state <- normalize_chart_state(default_builder_state())
    apply_builder_state(default_state, selected_series_index = 1, navigate_builder = TRUE)
    updateTextInput(session, "library_title", value = default_state$style$title)
    updateTextAreaInput(session, "library_description", value = "")
    synced_library_title(default_state$style$title)
    synced_source_note(default_state$style$note)
    showNotification("Builder reset to the default view.", type = "message")
  })

  observeEvent(input$clear_series_setup, {
    cleared_state <- builder_state()
    cleared_state$series <- rep(list(NULL), MAX_SERIES)
    apply_builder_state(cleared_state, selected_series_index = 1, navigate_builder = FALSE)
    updateTabsetPanel(session, "series_tabs", selected = "Series 1")
    showNotification("Series setup cleared.", type = "message")
  })

  observeEvent(input$clear_presentation_panel, {
    cleared_state <- builder_state()
    cleared_state$style <- default_style_settings()
    cleared_state$style$note <- default_source_note(cleared_state$series)
    apply_builder_state(cleared_state, navigate_builder = FALSE)
    synced_source_note(cleared_state$style$note)
    showNotification("Presentation inputs cleared.", type = "message")
  })

  observeEvent(input$clear_workspace_tools, {
    cleared_state <- builder_state()
    cleared_state$all_series_transform <- default_transform_profile()
    cleared_state$series <- lapply(cleared_state$series, function(spec) {
      if (is.null(spec)) {
        return(NULL)
      }

      spec$transform_profile <- default_transform_profile()
      spec
    })

    apply_builder_state(cleared_state, navigate_builder = FALSE)
    updateRadioGroupButtons(session, "side_panel_mode", selected = "transform")
    updateTabsetPanel(session, "transform_tabs", selected = "All")
    updateTabsetPanel(session, "analysis_tabs", selected = "Correlations")
    updateNumericInput(session, "analysis_corr_window", value = 4)
    updateRadioGroupButtons(session, "analysis_reg_errors", selected = "classical")
    updateRadioGroupButtons(session, "analysis_forecast_family", selected = "AR")
    updateRadioGroupButtons(session, "analysis_forecast_window_mode", selected = "expanding")
    updateNumericInput(session, "analysis_forecast_window_size", value = 12)
    updateNumericInput(session, "analysis_forecast_ar", value = 1)
    updateNumericInput(session, "analysis_forecast_ma", value = 0)
    updateNumericInput(session, "analysis_forecast_holdout", value = 0)
    updateNumericInput(session, "analysis_forecast_horizon", value = 4)
    updateRadioGroupButtons(session, "analysis_seasonal_view", selected = "both")
    updateRadioGroupButtons(session, "analysis_hp_side", selected = "two_sided")
    updateRadioGroupButtons(session, "analysis_hp_view", selected = "overlay")
    updateRadioGroupButtons(session, "analysis_kalman_side", selected = "two_sided")
    updateRadioGroupButtons(session, "analysis_kalman_view", selected = "overlay")
    showNotification("Workspace tools cleared.", type = "message")
  })

  apply_date_window_shortcut <- function(shortcut_value) {
    shortcut_value <- as.character(shortcut_value %||% "")
    if (!nzchar(shortcut_value)) {
      return(invisible(NULL))
    }

    year_bounds <- default_year_bounds()
    current_end_date <- as.Date(input$end_date %||% year_bounds$end_date)

    shortcut_start_date <- if (identical(shortcut_value, "max")) {
      year_bounds$min_date
    } else {
      max(year_bounds$min_date, current_end_date %m-% years(as.numeric(shortcut_value)))
    }

    updateDateInput(session, "start_date", value = shortcut_start_date)
  }

  observeEvent(input$date_window_shortcut, {
    apply_date_window_shortcut(input$date_window_shortcut)
  }, ignoreInit = TRUE)

  observeEvent(input$end_date, {
    shortcut_value <- input$date_window_shortcut %||% ""
    if (!nzchar(as.character(shortcut_value))) {
      return(invisible(NULL))
    }

    apply_date_window_shortcut(shortcut_value)
  }, ignoreInit = TRUE)

  builder_state <- reactive({
    input_state <- normalize_chart_state(builder_state_from_input(input, session))
    loaded_state <- restored_state()

    if (!is.null(loaded_state)) {
      return(normalize_chart_state(loaded_state))
    }

    input_state$date_range <- sort(c(max(1900, input_state$date_range[[1]]), max(1900, input_state$date_range[[2]])))
    input_state
  })

  observeEvent(
    {
      list(input$start_date, input$end_date)
    },
    {
      loaded_state <- restored_state()
      req(!is.null(loaded_state))

      requested_range <- normalize_date_range(c(
        as.Date(input$start_date %||% loaded_state$date_range[[1]]),
        as.Date(input$end_date %||% loaded_state$date_range[[2]])
      ))
      loaded_range <- normalize_date_range(loaded_state$date_range)

      if (!identical(requested_range, loaded_range)) {
        restored_state(NULL)
      }
    },
    ignoreInit = TRUE
  )

  observeEvent(
    {
      list(
        input$style_title,
        input$style_subtitle,
        input$style_y_axis_label,
        input$style_note,
        input$style_font_family,
        input$style_legend,
        input$style_palette,
        input$style_date_format,
        input$style_x_labels,
        input$style_auto_y_axis,
        input$style_y_min,
        input$style_y_max,
        input$style_y_breaks,
        input$style_invert_y_axis,
        input$style_horizontal_1,
        input$style_horizontal_2,
        input$style_horizontal_shading_min,
        input$style_horizontal_shading_max,
        input$style_vertical_1,
        input$style_vertical_2,
        input$style_recession_shading,
        input$export_width,
        input$export_height
      )
    },
    {
      loaded_state <- restored_state()
      req(!is.null(loaded_state))

      if (!identical(
        normalize_style_settings(style_settings_from_input(input)),
        normalize_style_settings(loaded_state$style)
      )) {
        restored_state(NULL)
      }
    },
    ignoreInit = TRUE
  )

  observeEvent(
    {
      c(
        list(
          input$transform_all_moving_average,
          input$transform_all_rolling_sum,
          input$transform_all_lagged_value,
          input$transform_all_lagged_pct,
          input$transform_all_lagged_ann,
          input$transform_all_subtract_series,
          input$transform_all_expression
        ),
        unlist(lapply(seq_len(MAX_SERIES), function(index) {
          list(
            input[[transform_input_id(paste0("transform_", index), "moving_average")]],
            input[[transform_input_id(paste0("transform_", index), "rolling_sum")]],
            input[[transform_input_id(paste0("transform_", index), "lagged_value")]],
            input[[transform_input_id(paste0("transform_", index), "lagged_pct")]],
            input[[transform_input_id(paste0("transform_", index), "lagged_ann")]],
            input[[transform_input_id(paste0("transform_", index), "subtract_series")]],
            input[[transform_input_id(paste0("transform_", index), "expression")]]
          )
        }), recursive = FALSE)
      )
    },
    {
      loaded_state <- restored_state()
      req(!is.null(loaded_state))

      current_series_profiles <- lapply(
        seq_len(MAX_SERIES),
        function(index) transform_profile_from_input(input, paste0("transform_", index))
      )
      loaded_series_profiles <- lapply(
        seq_len(MAX_SERIES),
        function(index) normalize_transform_profile((loaded_state$series[[index]] %||% list())$transform_profile)
      )

      if (
        !identical(
          normalize_transform_profile(transform_profile_from_input(input, "transform_all")),
          normalize_transform_profile(loaded_state$all_series_transform)
        ) ||
        !identical(current_series_profiles, loaded_series_profiles)
      ) {
        restored_state(NULL)
      }
    },
    ignoreInit = TRUE
  )

  observeEvent(
    {
      unlist(lapply(seq_len(MAX_SERIES), function(index) {
        list(input[[series_input_id(index, "label")]])
      }), recursive = FALSE)
    },
    {
      loaded_state <- restored_state()
      req(!is.null(loaded_state))

      current_labels <- vapply(
        seq_len(MAX_SERIES),
        function(index) trimws(input[[series_input_id(index, "label")]] %||% ""),
        character(1)
      )
      loaded_labels <- vapply(
        seq_len(MAX_SERIES),
        function(index) trimws(((loaded_state$series[[index]] %||% list())$label) %||% ""),
        character(1)
      )

      if (!identical(current_labels, loaded_labels)) {
        restored_state(NULL)
      }
    },
    ignoreInit = TRUE
  )

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

  export_widget <- reactive({
    style <- builder_state()$style
    width_px <- round((style$export_width %||% 8) * 160)
    height_px <- round((style$export_height %||% 5) * 160)

    current_widget() %>%
      plotly::layout(
        autosize = FALSE,
        width = width_px,
        height = height_px
      ) %>%
      plotly::config(
        responsive = FALSE,
        displayModeBar = TRUE
      )
  })

  chart_library_store <- reactiveVal(NULL)
  presentation_library_store <- reactiveVal(NULL)
  search_index_store <- reactiveVal(NULL)
  search_loading_message <- reactiveVal("")
  search_query_notification_id <- reactiveVal(NULL)
  search_query_text <- reactive(input$search_query %||% "")
  search_query_debounced <- debounce(search_query_text, millis = 350)

  ensure_chart_library_loaded <- function() {
    if (is.null(shiny::isolate(chart_library_store()))) {
      chart_library_store(read_chart_library())
    }

    invisible(shiny::isolate(chart_library_store()))
  }

  ensure_presentation_library_loaded <- function() {
    if (is.null(shiny::isolate(presentation_library_store()))) {
      presentation_library_store(read_chart_presentation_library())
    }

    invisible(shiny::isolate(presentation_library_store()))
  }

  ensure_search_index_loaded <- function(force = FALSE) {
    if (force || is.null(shiny::isolate(search_index_store()))) {
      search_loading_message("Loading local metadata index...")
      on.exit(search_loading_message(""), add = TRUE)
      search_index_store(build_search_index(force = force))
    }

    invisible(shiny::isolate(search_index_store()))
  }

  refresh_search_index_after_library_change <- function() {
    invalidate_search_index_cache()

    if (!is.null(shiny::isolate(search_index_store()))) {
      search_index_store(build_search_index(force = TRUE))
    }

    invisible(NULL)
  }

  session$onFlushed(function() {
    showNotification(
      "Opening data search. Other tabs will load when needed.",
      type = "message",
      duration = 5
    )

    later::later(function() {
      if (is.null(shiny::isolate(search_index_store()))) {
        try(ensure_search_index_loaded(), silent = TRUE)
      }
    }, delay = 1.5)
  }, once = TRUE)

  observeEvent(input$main_tabs, {
    selected_tab <- input$main_tabs %||% "search"

    if (
      identical(selected_tab, "search") &&
      is.null(search_index_store()) &&
      !identical(tab_load_state$search, "loading")
    ) {
      tab_load_state$search <- "loading"
      tab_load_error$search <- ""

      session$onFlushed(function() {
        tryCatch(
          {
            ensure_search_index_loaded()
            tab_load_state$search <- "ready"
            showNotification("Search metadata ready.", type = "message", duration = 3)
          },
          error = function(error) {
            tab_load_state$search <- "error"
            tab_load_error$search <- paste("Unable to load the search metadata:", conditionMessage(error))
          }
        )
      }, once = TRUE)
    }

    if (
      identical(selected_tab, "library") &&
      (is.null(chart_library_store()) || is.null(presentation_library_store())) &&
      !identical(tab_load_state$library, "loading")
    ) {
      tab_load_state$library <- "loading"
      tab_load_error$library <- ""

      session$onFlushed(function() {
        tryCatch(
          {
            ensure_chart_library_loaded()
            ensure_presentation_library_loaded()
            tab_load_state$library <- "ready"
            showNotification("Saved charts ready.", type = "message", duration = 3)
          },
          error = function(error) {
            tab_load_state$library <- "error"
            tab_load_error$library <- paste("Unable to load the saved charts:", conditionMessage(error))
          }
        )
      }, once = TRUE)
    }
  }, ignoreInit = TRUE)

  output$search_frequency_filter <- renderUI({
    compact_single_choice_input(
      "search_frequency_filter",
      "Frequency",
      choices = search_frequency_choices(search_index_store() %||% empty_search_index()),
      selected = "all"
    )
  })

  fred_search_response <- reactive({
    if (!(input$search_source_filter %||% "all") %in% c("all", "FRED")) {
      return(empty_search_response())
    }

    search_fred_series(
      query = search_query_debounced(),
      frequency_filter = input$search_frequency_filter %||% "all",
      search_type = input$search_fred_mode %||% "full_text",
      limit = 100
    )
  })

  dbnomics_search_response <- reactive({
    if (!(input$search_source_filter %||% "all") %in% c("all", "DBnomics")) {
      return(empty_search_response())
    }

    if (identical(input$search_source_filter %||% "all", "all")) {
      has_exact_id <- !is.null(parse_dbnomics_series_id(search_query_debounced()))
      has_scope <- nzchar(trimws(input$search_dbnomics_provider %||% "")) &&
        nzchar(trimws(input$search_dbnomics_dataset %||% ""))

      if (!has_exact_id && !has_scope) {
        return(empty_search_response())
      }
    }

    search_dbnomics_series(
      query = search_query_debounced(),
      provider_code = input$search_dbnomics_provider %||% "",
      dataset_code = input$search_dbnomics_dataset %||% "",
      limit = 100
    )
  })

  output$search_status <- renderUI({
    loading_message <- trimws(search_loading_message() %||% "")
    source_filter <- input$search_source_filter %||% "all"
    query_text <- trimws(search_query_debounced())
    include_remote_status <- nzchar(query_text) || source_filter %in% c("FRED", "DBnomics")

    search_messages <- c(
      loading_message,
      if (is.null(search_index_store()) && source_filter %in% c("all", "Recent", "ABS CPI", "RBA", "ABS")) "Local metadata will load when search runs." else "",
      if (isTRUE(include_remote_status)) fred_search_response()$status %||% "" else "",
      if (isTRUE(include_remote_status)) dbnomics_search_response()$status %||% "" else ""
    )
    search_messages <- unique(search_messages[nzchar(search_messages)])

    if (length(search_messages) == 0) {
      return(NULL)
    }

    div(
      class = "search-status-banner",
      lapply(search_messages, tags$p)
    )
  })

  observeEvent(
    {
      list(
        input$search_query,
        input$search_source_filter,
        input$search_type_filter,
        input$search_location_filter,
        input$search_frequency_filter,
        input$search_fred_mode
      )
    },
    {
      req(identical(input$main_tabs %||% "builder", "search"))
      notification_id <- "search-query-status"
      search_query_notification_id(notification_id)
      showNotification(
        search_activity_message(
          source_filter = input$search_source_filter %||% "all",
          query_text = search_query_debounced()
        ),
        id = notification_id,
        duration = NULL,
        closeButton = FALSE,
        type = "message"
      )
    },
    ignoreInit = TRUE
  )

  search_results <- reactive({
    if ((input$search_source_filter %||% "all") %in% c("all", "Recent", "ABS CPI", "RBA", "ABS") && is.null(search_index_store())) {
      ensure_search_index_loaded()
    }

    local_results <- if ((input$search_source_filter %||% "all") %in% c("FRED", "DBnomics")) {
      empty_search_index()
    } else {
      filter_search_index(
        search_index_store(),
        query = "",
        source_filter = input$search_source_filter,
        type_filter = "all",
        location_filter = "all",
        frequency_filter = input$search_frequency_filter %||% "all",
        limit = Inf
      )
    }

    combined_results <- bind_rows(
      local_results,
      fred_search_response()$results,
      dbnomics_search_response()$results
    )

    apply_search_filters(
      combined_results %>% distinct(search_id, .keep_all = TRUE),
      query = search_query_debounced(),
      source_filter = "all",
      type_filter = input$search_type_filter %||% "all",
      location_filter = input$search_location_filter %||% "all",
      frequency_filter = "all",
      limit = Inf
    )
  })

  observe({
    req("search" %in% loaded_main_tabs())
    result_count <- nrow(search_results())
    notification_id <- search_query_notification_id() %||% "search-query-status"

    showNotification(
      sprintf("Search ready: %s result%s.", scales::comma(result_count), if (identical(result_count, 1L)) "" else "s"),
      id = notification_id,
      duration = 2.5,
      closeButton = TRUE,
      type = "message"
    )
  })

  output$search_results_table <- DT::renderDataTable({
    search_rows <- search_results() %>%
      transmute(
        Title = title,
        Source = source,
        Type = type_code_label(type_code),
        Location = location_code_label(location_code),
        Frequency = frequency,
        Start = if_else(is.na(start_date), "", format(start_date, "%Y-%m-%d")),
        End = if_else(is.na(end_date), "", format(end_date, "%Y-%m-%d")),
        Summary = summary
      )

    DT::datatable(
      search_rows,
      selection = "single",
      rownames = FALSE,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(width = "30%", targets = 0),
          list(width = "34%", targets = 7)
        )
      )
    )
  }, server = TRUE)

  selected_search_result <- reactive({
    selected_row <- input$search_results_table_rows_selected
    search_rows <- search_results()

    if (length(selected_row) == 0 || nrow(search_rows) == 0) {
      return(NULL)
    }

    search_rows[selected_row, , drop = FALSE]
  })

  output$search_selected_meta <- renderUI({
    search_row <- selected_search_result()

    if (is.null(search_row)) {
      return(div(class = "empty-state", "Select a result to inspect it."))
    }

    div(
      class = "library-meta",
      tags$h3(search_row$title[[1]]),
      tags$p(class = "muted-copy", search_row$summary[[1]]),
      div(
        class = "summary-chip-row",
        summary_chip("Source", search_row$source[[1]]),
        summary_chip("Type", type_code_label(search_row$type_code[[1]])),
        summary_chip("Location", location_code_label(search_row$location_code[[1]])),
        summary_chip("Frequency", search_row$frequency[[1]]),
        summary_chip("Start", if (is.na(search_row$start_date[[1]])) "Unknown" else format(search_row$start_date[[1]], "%Y-%m-%d"))
      )
    )
  })

  search_preview_plot <- reactive({
    search_row <- selected_search_result()

    if (is.null(search_row)) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Select a result to preview it.", size = 5, colour = "#64748b") +
          xlim(0, 1) +
          ylim(0, 1) +
          theme_void()
      )
    }

    preview_spec <- search_result_series_spec(search_row, 1)
    preview_state <- preview_chart_state(
      preview_spec,
      date_range = c(search_row$start_date[[1]], search_row$end_date[[1]])
    )
    preview_payload <- build_chart_data(preview_state)

    if (nrow(preview_payload$data) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No preview data is available for this result.", size = 5, colour = "#64748b") +
          xlim(0, 1) +
          ylim(0, 1) +
          theme_void()
      )
    }

    preview_style <- preview_state$style
    preview_style$title <- ""
    preview_style$subtitle <- ""
    preview_style$legend <- "none"
    preview_style$note <- ""
    preview_style$y_axis_label <- ""
    preview_style$x_labels <- 3
    preview_style$y_breaks <- pretty(preview_payload$data$value, n = 3)
    preview_style$auto_y_axis <- TRUE

    build_chart_plot(preview_payload$data, preview_style) +
      theme(
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
        axis.text.y.right = element_text(margin = margin(l = 10))
      )
  })

  output$search_preview_plot <- renderPlot({
    search_preview_plot()
  })

  observeEvent(input$search_add_series, {
    search_row <- selected_search_result()
    req(!is.null(search_row))

    target_index <- series_slot_from_search_target(builder_state(), input$search_target_series)
    series_spec <- search_result_series_spec(search_row, target_index)
    updated_state <- builder_state()
    updated_state$series[[target_index]] <- normalize_series_spec(series_spec)
    apply_builder_state(updated_state, selected_series_index = target_index, navigate_builder = FALSE)

    showNotification("Search result added to the builder.", type = "message")
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
      saveWidget(export_widget(), file, selfcontained = TRUE)
    }
  )

  output$exportSVG <- downloadHandler(
    filename = function() {
      paste0(str_replace_all(builder_state()$style$title %||% "chart", "[^A-Za-z0-9]+", "_"), ".svg")
    },
    content = function(file) {
      req(nrow(chart_data()) > 0)
      grDevices::svg(
        filename = file,
        width = builder_state()$style$export_width,
        height = builder_state()$style$export_height,
        onefile = TRUE,
        bg = "white"
      )
      on.exit(grDevices::dev.off(), add = TRUE)
      print(current_plot())
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

  build_analysis_save_context <- function(chart_state, data) {
    active_tab <- input$analysis_tabs %||% "Correlations"

    if (identical(active_tab, "Correlations")) {
      series_x <- input$analysis_corr_x %||% ""
      series_y <- input$analysis_corr_y %||% ""
      window <- max(2L, as.integer(input$analysis_corr_window %||% 4))

      if (!nzchar(series_x) || !nzchar(series_y) || identical(series_x, series_y)) {
        return(NULL)
      }

      result <- tryCatch(
        rolling_correlation_data(data, series_x, series_y, window),
        error = function(error) NULL
      )

      if (is.null(result) || nrow(result) == 0) {
        return(NULL)
      }

      return(list(
        chart_kind = "correlation",
        title = paste("Rolling correlation:", series_x, "vs", series_y),
        chart_state = chart_state,
        data_snapshot = empty_chart_data(),
        analysis_spec = list(
          tab = "Correlations",
          series_x = series_x,
          series_y = series_y,
          window = window
        ),
        analysis_payload = result
      ))
    }

    if (identical(active_tab, "Regression")) {
      dependent <- input$analysis_reg_y %||% ""
      independent <- input$analysis_reg_x %||% ""
      error_assumption <- input$analysis_reg_errors %||% "classical"

      if (!nzchar(dependent) || !nzchar(independent) || identical(dependent, independent)) {
        return(NULL)
      }

      result <- tryCatch(
        regression_analysis(
          data,
          dependent = dependent,
          independent = independent,
          error_assumption = error_assumption
        ),
        error = function(error) NULL
      )

      if (is.null(result)) {
        return(NULL)
      }

      return(list(
        chart_kind = "regression",
        title = paste("Regression:", dependent, "on", independent),
        chart_state = chart_state,
        data_snapshot = empty_chart_data(),
        analysis_spec = list(
          tab = "Regression",
          dependent = dependent,
          independent = independent,
          error_assumption = error_assumption
        ),
        analysis_payload = result
      ))
    }

    if (identical(active_tab, "Forecast")) {
      series_name <- input$analysis_forecast_series %||% ""

      if (!nzchar(series_name)) {
        return(NULL)
      }

      result <- tryCatch(
        forecast_analysis(
          data,
          series_name = series_name,
          model_family = input$analysis_forecast_family,
          ar_lag = max(0, as.integer(input$analysis_forecast_ar)),
          ma_lag = max(0, as.integer(input$analysis_forecast_ma)),
          horizon = max(1, as.integer(input$analysis_forecast_horizon)),
          window_mode = input$analysis_forecast_window_mode %||% "expanding",
          window_size = as.integer(input$analysis_forecast_window_size %||% NA),
          holdout_size = max(0, as.integer(input$analysis_forecast_holdout %||% 0))
        ),
        error = function(error) NULL
      )

      if (is.null(result)) {
        return(NULL)
      }

      return(list(
        chart_kind = "forecast",
        title = paste("Forecast for", series_name),
        chart_state = chart_state,
        data_snapshot = empty_chart_data(),
        analysis_spec = list(
          tab = "Forecast",
          series_name = series_name,
          model_family = input$analysis_forecast_family %||% "AR",
          ar_lag = max(0, as.integer(input$analysis_forecast_ar)),
          ma_lag = max(0, as.integer(input$analysis_forecast_ma)),
          horizon = max(1, as.integer(input$analysis_forecast_horizon)),
          window_mode = input$analysis_forecast_window_mode %||% "expanding",
          window_size = as.integer(input$analysis_forecast_window_size %||% NA),
          holdout_size = max(0, as.integer(input$analysis_forecast_holdout %||% 0))
        ),
        analysis_payload = result
      ))
    }

    if (identical(active_tab, "Seasonal Adjust")) {
      series_name <- input$analysis_seasonal_series %||% ""
      display_mode <- input$analysis_seasonal_view %||% "both"

      if (!nzchar(series_name)) {
        return(NULL)
      }

      result <- tryCatch(
        seasonal_adjustment_analysis(
          data,
          series_name = series_name,
          display_mode = display_mode
        ),
        error = function(error) NULL
      )

      if (is.null(result)) {
        return(NULL)
      }

      return(list(
        chart_kind = "seasonal_adjustment",
        title = if (identical(display_mode, "adjusted")) {
          paste("Seasonally adjusted:", series_name)
        } else {
          paste("Seasonal adjustment:", series_name)
        },
        chart_state = chart_state,
        data_snapshot = empty_chart_data(),
        analysis_spec = list(
          tab = "Seasonal Adjust",
          series_name = series_name,
          display_mode = display_mode
        ),
        analysis_payload = result
      ))
    }

    if (identical(active_tab, "HP Filter")) {
      series_name <- input$analysis_hp_series %||% ""
      side <- input$analysis_hp_side %||% "two_sided"
      display_mode <- input$analysis_hp_view %||% "overlay"

      if (!nzchar(series_name)) {
        return(NULL)
      }

      result <- tryCatch(
        hp_filter_analysis(
          data,
          series_name = series_name,
          side = side,
          display_mode = display_mode
        ),
        error = function(error) NULL
      )

      if (is.null(result)) {
        return(NULL)
      }

      return(list(
        chart_kind = "hp_filter",
        title = paste("HP filter:", series_name),
        chart_state = chart_state,
        data_snapshot = empty_chart_data(),
        analysis_spec = list(
          tab = "HP Filter",
          series_name = series_name,
          side = side,
          display_mode = display_mode
        ),
        analysis_payload = result
      ))
    }

    if (identical(active_tab, "Kalman Filter")) {
      series_name <- input$analysis_kalman_series %||% ""
      side <- input$analysis_kalman_side %||% "two_sided"
      display_mode <- input$analysis_kalman_view %||% "overlay"

      if (!nzchar(series_name)) {
        return(NULL)
      }

      result <- tryCatch(
        kalman_filter_analysis(
          data,
          series_name = series_name,
          side = side,
          display_mode = display_mode
        ),
        error = function(error) NULL
      )

      if (is.null(result)) {
        return(NULL)
      }

      return(list(
        chart_kind = "kalman_filter",
        title = paste("Kalman filter:", series_name),
        chart_state = chart_state,
        data_snapshot = empty_chart_data(),
        analysis_spec = list(
          tab = "Kalman Filter",
          series_name = series_name,
          side = side,
          display_mode = display_mode
        ),
        analysis_payload = result
      ))
    }

    NULL
  }

  build_current_save_context <- function(chart_state, data) {
    if (identical(input$side_panel_mode %||% "transform", "analysis")) {
      return(build_analysis_save_context(chart_state, data))
    }

    if (nrow(data) == 0) {
      return(NULL)
    }

    list(
      chart_kind = "builder",
      title = trimws(chart_state$style$title %||% ""),
      chart_state = chart_state,
      data_snapshot = data,
      analysis_spec = NULL,
      analysis_payload = NULL
    )
  }

  build_saved_chart_widget <- function(chart_record, width = NULL, height = NULL) {
    if (is.null(chart_record) || nrow(chart_record) == 0) {
      return(empty_plotly_widget("Select a saved chart to preview it."))
    }

    chart_kind <- chart_record$chart_kind[[1]] %||% "builder"
    chart_state <- chart_record$chart_state[[1]]
    analysis_spec <- chart_record$analysis_spec[[1]] %||% list()
    analysis_payload <- chart_record$analysis_payload[[1]] %||% NULL

    widget <- if (identical(chart_kind, "builder") || is.null(analysis_payload)) {
      build_chart_widget(chart_record$data_snapshot[[1]], chart_state$style)
    } else if (identical(chart_kind, "correlation")) {
      ggplotly(
        build_correlation_plot(
          analysis_payload,
          analysis_spec$series_x %||% "",
          analysis_spec$series_y %||% "",
          analysis_spec$window %||% 4
        ),
        tooltip = c("x", "y")
      )
    } else if (identical(chart_kind, "regression")) {
      ggplotly(
        build_regression_plot(
          analysis_payload,
          analysis_spec$dependent %||% "",
          analysis_spec$independent %||% ""
        ),
        tooltip = c("x", "y")
      )
    } else if (identical(chart_kind, "forecast")) {
      ggplotly(
        build_forecast_plot(
          analysis_payload,
          analysis_spec$series_name %||% "Series"
        ),
        tooltip = c("x", "y")
      )
    } else if (identical(chart_kind, "seasonal_adjustment")) {
      ggplotly(
        build_seasonal_adjustment_plot(
          analysis_payload,
          analysis_spec$series_name %||% "Series",
          analysis_spec$display_mode %||% "both"
        ),
        tooltip = c("x", "y", "colour")
      )
    } else if (identical(chart_kind, "hp_filter")) {
      ggplotly(
        build_hp_filter_plot(
          analysis_payload,
          analysis_spec$series_name %||% "Series",
          analysis_spec$side %||% "two_sided",
          analysis_spec$display_mode %||% "overlay"
        ),
        tooltip = c("x", "y", "colour")
      )
    } else if (identical(chart_kind, "kalman_filter")) {
      ggplotly(
        build_kalman_filter_plot(
          analysis_payload,
          analysis_spec$series_name %||% "Series",
          analysis_spec$side %||% "two_sided",
          analysis_spec$display_mode %||% "overlay"
        ),
        tooltip = c("x", "y", "colour")
      )
    } else {
      build_chart_widget(chart_record$data_snapshot[[1]], chart_state$style)
    }

    if (!is.null(width) || !is.null(height)) {
      widget <- widget %>%
        plotly::layout(
          autosize = FALSE,
          width = width,
          height = height
        ) %>%
        plotly::config(
          responsive = FALSE,
          displayModeBar = TRUE
        )
    }

    widget
  }

  restore_saved_analysis_state <- function(chart_record) {
    chart_kind <- chart_record$chart_kind[[1]] %||% "builder"
    analysis_spec <- chart_record$analysis_spec[[1]] %||% list()

    if (identical(chart_kind, "builder")) {
      updateRadioGroupButtons(session, "side_panel_mode", selected = "transform")
      return(invisible(NULL))
    }

    updateRadioGroupButtons(session, "side_panel_mode", selected = "analysis")
    updateTabsetPanel(session, "analysis_tabs", selected = analysis_spec$tab %||% "Correlations")

    session$onFlushed(function() {
      if (identical(chart_kind, "correlation")) {
        updateSelectInput(session, "analysis_corr_x", selected = analysis_spec$series_x %||% "")
        updateSelectInput(session, "analysis_corr_y", selected = analysis_spec$series_y %||% "")
        updateNumericInput(session, "analysis_corr_window", value = analysis_spec$window %||% 4)
      } else if (identical(chart_kind, "regression")) {
        updateSelectInput(session, "analysis_reg_y", selected = analysis_spec$dependent %||% "")
        updateSelectInput(session, "analysis_reg_x", selected = analysis_spec$independent %||% "")
        updateRadioGroupButtons(session, "analysis_reg_errors", selected = analysis_spec$error_assumption %||% "classical")
      } else if (identical(chart_kind, "forecast")) {
        updateSelectInput(session, "analysis_forecast_series", selected = analysis_spec$series_name %||% "")
        updateRadioGroupButtons(session, "analysis_forecast_family", selected = analysis_spec$model_family %||% "AR")
        updateRadioGroupButtons(session, "analysis_forecast_window_mode", selected = analysis_spec$window_mode %||% "expanding")
        updateNumericInput(session, "analysis_forecast_window_size", value = analysis_spec$window_size %||% 12)
        updateNumericInput(session, "analysis_forecast_ar", value = analysis_spec$ar_lag %||% 1)
        updateNumericInput(session, "analysis_forecast_ma", value = analysis_spec$ma_lag %||% 0)
        updateNumericInput(session, "analysis_forecast_holdout", value = analysis_spec$holdout_size %||% 0)
        updateNumericInput(session, "analysis_forecast_horizon", value = analysis_spec$horizon %||% 4)
      } else if (identical(chart_kind, "seasonal_adjustment")) {
        updateSelectInput(session, "analysis_seasonal_series", selected = analysis_spec$series_name %||% "")
        updateRadioGroupButtons(session, "analysis_seasonal_view", selected = analysis_spec$display_mode %||% "both")
      } else if (identical(chart_kind, "hp_filter")) {
        updateSelectInput(session, "analysis_hp_series", selected = analysis_spec$series_name %||% "")
        updateRadioGroupButtons(session, "analysis_hp_side", selected = analysis_spec$side %||% "two_sided")
        updateRadioGroupButtons(session, "analysis_hp_view", selected = analysis_spec$display_mode %||% "overlay")
      } else if (identical(chart_kind, "kalman_filter")) {
        updateSelectInput(session, "analysis_kalman_series", selected = analysis_spec$series_name %||% "")
        updateRadioGroupButtons(session, "analysis_kalman_side", selected = analysis_spec$side %||% "two_sided")
        updateRadioGroupButtons(session, "analysis_kalman_view", selected = analysis_spec$display_mode %||% "overlay")
      }
    }, once = TRUE)

    invisible(NULL)
  }

  observeEvent(input$save_chart, {
    ensure_chart_library_loaded()
    save_context <- build_current_save_context(builder_state(), chart_data())

    if (is.null(save_context)) {
      showNotification("Build a valid chart before saving it.", type = "error")
      return(invisible(NULL))
    }

    run_with_status(
      "Saving chart to the library...",
      "Chart saved to the library.",
      {
        save_title <- trimws(input$library_title %||% "")
        save_description <- trimws(input$library_description %||% "")

        chart_record <- new_chart_record(
          chart_state = save_context$chart_state,
          data_snapshot = save_context$data_snapshot,
          title = if (nzchar(save_title)) save_title else save_context$title,
          description = save_description,
          chart_kind = save_context$chart_kind,
          analysis_spec = save_context$analysis_spec,
          analysis_payload = save_context$analysis_payload
        )

        updated_library <- upsert_chart_record(chart_library_store(), chart_record)
        chart_library_store(updated_library)
        write_chart_library(updated_library)
        refresh_search_index_after_library_change()
      },
      failure_prefix = "Unable to save the chart:"
    )
  })

  filtered_library <- reactive({
    (chart_library_store() %||% empty_chart_library()) %>%
      filter_chart_library(input$library_search)
  })

  filtered_presentation_library <- reactive({
    (presentation_library_store() %||% empty_chart_presentation_library()) %>%
      filter_chart_presentation_library(input$presentation_search)
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

  output$presentation_table <- DT::renderDataTable({
    presentation_rows <- filtered_presentation_library() %>%
      transmute(
        Title = title,
        Charts = chart_count,
        Updated = format(updated_at, "%Y-%m-%d %H:%M")
      )

    DT::datatable(
      presentation_rows,
      selection = "single",
      rownames = FALSE,
      options = list(dom = "tip", pageLength = 6, scrollX = TRUE)
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

  selected_presentation_record <- reactive({
    selected_rows <- input$presentation_table_rows_selected
    presentation_rows <- filtered_presentation_library()

    if (length(selected_rows) == 0 || nrow(presentation_rows) == 0) {
      return(NULL)
    }

    presentation_rows[selected_rows[1], , drop = FALSE]
  })

  observe({
    presentation_record <- selected_presentation_record()

    if (is.null(presentation_record)) {
      return(invisible(NULL))
    }

    updateTextInput(session, "presentation_title", value = presentation_record$title[[1]])
    updateTextAreaInput(session, "presentation_description", value = presentation_record$description[[1]] %||% "")
  })

  selected_presentation_charts <- reactive({
    presentation_chart_records(selected_presentation_record(), chart_library_store() %||% empty_chart_library())
  })

  output$presentation_selected_meta <- renderUI({
    presentation_record <- selected_presentation_record()

    if (is.null(presentation_record)) {
      return(div(class = "empty-state", "Select or create a presentation to manage its charts."))
    }

    description_text <- presentation_record$description[[1]] %||% ""
    if (!nzchar(description_text)) {
      description_text <- "No notes saved."
    }

    missing_count <- length(setdiff(
      as.character(presentation_record$chart_ids[[1]] %||% character()),
      selected_presentation_charts()$chart_id
    ))

    div(
      class = "library-meta",
      tags$h3(presentation_record$title[[1]]),
      tags$p(class = "muted-copy", description_text),
      div(
        class = "summary-chip-row",
        summary_chip("Charts", presentation_record$chart_count[[1]]),
        summary_chip("Updated", format(presentation_record$updated_at[[1]], "%d %b %Y %H:%M")),
        if (missing_count > 0) summary_chip("Missing", missing_count)
      )
    )
  })

  output$presentation_chart_table <- DT::renderDataTable({
    presentation_rows <- selected_presentation_charts()
    validate(need(nrow(presentation_rows) > 0, "No charts are currently saved in this presentation."))

    presentation_rows <- presentation_rows %>%
      mutate(Order = seq_len(n())) %>%
      transmute(
        Order = Order,
        Title = title,
        Sources = source_summary,
        Saved = format(saved_at, "%Y-%m-%d %H:%M")
      )

    DT::datatable(
      presentation_rows,
      selection = "single",
      rownames = FALSE,
      options = list(dom = "tip", pageLength = 6, scrollX = TRUE)
      )
  })

  output$presentation_chart_actions <- renderUI({
    presentation_rows <- selected_presentation_charts()
    if (nrow(presentation_rows) == 0) {
      return(NULL)
    }

    div(
      class = "library-actions",
      actionButton("presentation_chart_up", "Move up"),
      actionButton("presentation_chart_down", "Move down"),
      actionButton("presentation_chart_remove", "Remove chart")
    )
  })

  output$presentation_detail_actions <- renderUI({
    presentation_record <- selected_presentation_record()
    if (is.null(presentation_record)) {
      return(NULL)
    }

    div(
      class = "library-actions",
      actionButton("update_presentation", "Update presentation"),
      downloadButton("export_selected_presentation", "Export presentation")
    )
  })

  selected_presentation_chart_id <- reactive({
    selected_row <- input$presentation_chart_table_rows_selected
    presentation_rows <- selected_presentation_charts()

    if (length(selected_row) == 0 || nrow(presentation_rows) == 0) {
      return(NULL)
    }

    presentation_rows$chart_id[[selected_row[1]]]
  })

  selected_presentation_chart_record <- reactive({
    selected_row <- input$presentation_chart_table_rows_selected
    presentation_rows <- selected_presentation_charts()

    if (length(selected_row) == 0 || nrow(presentation_rows) == 0) {
      return(NULL)
    }

    presentation_rows[selected_row[1], , drop = FALSE]
  })

  selected_preview_chart_record <- reactive({
    selected_presentation_record <- selected_presentation_chart_record()
    if (!is.null(selected_presentation_record)) {
      return(selected_presentation_record)
    }

    selected_chart_record()
  })

  output$library_selected_meta <- renderUI({
    chart_record <- selected_preview_chart_record()
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
    chart_record <- selected_preview_chart_record()
    if (is.null(chart_record)) {
      return(empty_plotly_widget("Select a saved chart to preview it."))
    }

    build_saved_chart_widget(chart_record)
  })

  observeEvent(input$load_chart, {
    ensure_chart_library_loaded()
    selected_records <- selected_chart_records()
    req(!is.null(selected_records), nrow(selected_records) == 1)
    chart_record <- selected_records[1, , drop = FALSE]

    run_with_status(
      paste("Loading", chart_record$title[[1]], "into the builder..."),
      "Saved chart loaded into the builder.",
      {
        apply_builder_state(chart_record$chart_state[[1]], navigate_builder = TRUE)
        restore_saved_analysis_state(chart_record)
        updateTextInput(session, "library_title", value = chart_record$title[[1]])
        updateTextAreaInput(session, "library_description", value = chart_record$description[[1]] %||% "")
      },
      failure_prefix = "Unable to load the saved chart:"
    )
  })

  observeEvent(input$update_chart, {
    ensure_chart_library_loaded()
    selected_records <- selected_chart_records()
    if (is.null(selected_records) || nrow(selected_records) == 0) {
      fallback_records <- filtered_library()
      if (nrow(fallback_records) == 1) {
        selected_records <- fallback_records[1, , drop = FALSE]
      }
    }
    req(!is.null(selected_records), nrow(selected_records) == 1)

    chart_record <- selected_records[1, , drop = FALSE]

    run_with_status(
      paste("Updating saved chart", chart_record$title[[1]], "..."),
      "Saved chart updated.",
      {
        chart_state <- builder_state()
        latest_date <- latest_chart_observation_date(chart_state)
        if (!is.na(latest_date)) {
          chart_state$date_range[[2]] <- latest_date
        }

        refreshed_payload <- build_chart_data(chart_state)
        save_context <- build_current_save_context(chart_state, refreshed_payload$data)
        if (is.null(save_context)) {
          stop("Build a valid chart before updating the saved chart.", call. = FALSE)
        }

        updated_record <- new_chart_record(
          chart_state = save_context$chart_state,
          data_snapshot = save_context$data_snapshot,
          title = trimws(input$library_title %||% save_context$title),
          description = trimws(input$library_description %||% ""),
          chart_kind = save_context$chart_kind,
          analysis_spec = save_context$analysis_spec,
          analysis_payload = save_context$analysis_payload
        )
        updated_record$chart_id <- chart_record$chart_id

        updated_library <- upsert_chart_record(chart_library_store(), updated_record)
        chart_library_store(updated_library)
        write_chart_library(updated_library)
        refresh_search_index_after_library_change()
        apply_builder_state(chart_state)
      },
      failure_prefix = "Unable to update the saved chart:"
    )
  })

  observeEvent(input$delete_chart, {
    ensure_chart_library_loaded()
    ensure_presentation_library_loaded()
    selected_records <- selected_chart_records()
    req(!is.null(selected_records))

    run_with_status(
      sprintf("Removing %s saved chart%s...", nrow(selected_records), if (nrow(selected_records) == 1) "" else "s"),
      "Saved chart removed from the library.",
      {
        updated_library <- Reduce(
          f = function(library_data, chart_id) {
            delete_chart_record(library_data, chart_id)
          },
          x = selected_records$chart_id,
          init = chart_library_store()
        )
        chart_library_store(updated_library)
        write_chart_library(updated_library)
        refresh_search_index_after_library_change()

        updated_presentations <- remove_chart_ids_from_presentations(
          presentation_library_store(),
          selected_records$chart_id
        )
        presentation_library_store(updated_presentations)
        write_chart_presentation_library(updated_presentations)
      },
      success_type = "warning",
      failure_prefix = "Unable to delete the saved chart:"
    )
  })

  observeEvent(input$create_presentation, {
    ensure_presentation_library_loaded()
    selected_records <- selected_chart_records()
    selected_chart_ids <- if (is.null(selected_records)) character() else selected_records$chart_id

    run_with_status(
      "Creating presentation...",
      "Presentation saved to the library.",
      {
        new_record <- new_chart_presentation_record(
          title = input$presentation_title,
          description = input$presentation_description,
          chart_ids = selected_chart_ids
        )

        updated_presentations <- upsert_chart_presentation_record(presentation_library_store(), new_record)
        presentation_library_store(updated_presentations)
        write_chart_presentation_library(updated_presentations)
      },
      failure_prefix = "Unable to create the presentation:"
    )
  })

  observeEvent(input$update_presentation, {
    ensure_presentation_library_loaded()
    presentation_record <- selected_presentation_record()
    req(!is.null(presentation_record))

    run_with_status(
      paste("Updating presentation", presentation_record$title[[1]], "..."),
      "Presentation details updated.",
      {
        updated_record <- update_presentation_charts(
          presentation_record,
          presentation_record$chart_ids[[1]]
        )
        updated_record$title[[1]] <- trimws(input$presentation_title %||% updated_record$title[[1]])
        updated_record$description[[1]] <- trimws(input$presentation_description %||% "")

        updated_presentations <- upsert_chart_presentation_record(presentation_library_store(), updated_record)
        presentation_library_store(updated_presentations)
        write_chart_presentation_library(updated_presentations)
      },
      failure_prefix = "Unable to update the presentation:"
    )
  })

  observeEvent(input$delete_presentation, {
    ensure_presentation_library_loaded()
    presentation_record <- selected_presentation_record()
    req(!is.null(presentation_record))

    run_with_status(
      paste("Deleting presentation", presentation_record$title[[1]], "..."),
      "Presentation removed from the library.",
      {
        updated_presentations <- delete_chart_presentation_record(
          presentation_library_store(),
          presentation_record$presentation_id[[1]]
        )
        presentation_library_store(updated_presentations)
        write_chart_presentation_library(updated_presentations)
      },
      success_type = "warning",
      failure_prefix = "Unable to delete the presentation:"
    )
  })

  observeEvent(input$add_to_presentation, {
    ensure_presentation_library_loaded()
    presentation_record <- selected_presentation_record()
    selected_records <- selected_chart_records()
    req(!is.null(presentation_record), !is.null(selected_records), nrow(selected_records) > 0)

    run_with_status(
      sprintf("Adding %s selected chart%s to %s...", nrow(selected_records), if (nrow(selected_records) == 1) "" else "s", presentation_record$title[[1]]),
      "Selected charts added to the presentation.",
      {
        updated_chart_ids <- c(
          presentation_record$chart_ids[[1]] %||% character(),
          selected_records$chart_id
        )
        updated_record <- update_presentation_charts(presentation_record, updated_chart_ids)

        updated_presentations <- upsert_chart_presentation_record(presentation_library_store(), updated_record)
        presentation_library_store(updated_presentations)
        write_chart_presentation_library(updated_presentations)
      },
      failure_prefix = "Unable to add charts to the presentation:"
    )
  })

  observeEvent(input$replace_presentation_charts, {
    ensure_presentation_library_loaded()
    presentation_record <- selected_presentation_record()
    selected_records <- selected_chart_records()
    req(!is.null(presentation_record), !is.null(selected_records), nrow(selected_records) > 0)

    run_with_status(
      sprintf("Replacing the charts in %s with %s selected chart%s...", presentation_record$title[[1]], nrow(selected_records), if (nrow(selected_records) == 1) "" else "s"),
      "Presentation charts replaced from the current selection.",
      {
        updated_record <- update_presentation_charts(presentation_record, selected_records$chart_id)

        updated_presentations <- upsert_chart_presentation_record(presentation_library_store(), updated_record)
        presentation_library_store(updated_presentations)
        write_chart_presentation_library(updated_presentations)
      },
      failure_prefix = "Unable to replace the presentation charts:"
    )
  })

  observeEvent(input$presentation_chart_up, {
    ensure_presentation_library_loaded()
    presentation_record <- selected_presentation_record()
    chart_id <- selected_presentation_chart_id()
    req(!is.null(presentation_record), !is.null(chart_id))

    run_with_status(
      "Moving chart up in the presentation...",
      "Chart order updated.",
      {
        updated_record <- update_presentation_charts(
          presentation_record,
          move_presentation_chart_ids(presentation_record$chart_ids[[1]], chart_id, "up")
        )

        updated_presentations <- upsert_chart_presentation_record(presentation_library_store(), updated_record)
        presentation_library_store(updated_presentations)
        write_chart_presentation_library(updated_presentations)
      },
      failure_prefix = "Unable to move the chart:"
    )
  })

  observeEvent(input$presentation_chart_down, {
    ensure_presentation_library_loaded()
    presentation_record <- selected_presentation_record()
    chart_id <- selected_presentation_chart_id()
    req(!is.null(presentation_record), !is.null(chart_id))

    run_with_status(
      "Moving chart down in the presentation...",
      "Chart order updated.",
      {
        updated_record <- update_presentation_charts(
          presentation_record,
          move_presentation_chart_ids(presentation_record$chart_ids[[1]], chart_id, "down")
        )

        updated_presentations <- upsert_chart_presentation_record(presentation_library_store(), updated_record)
        presentation_library_store(updated_presentations)
        write_chart_presentation_library(updated_presentations)
      },
      failure_prefix = "Unable to move the chart:"
    )
  })

  observeEvent(input$presentation_chart_remove, {
    ensure_presentation_library_loaded()
    presentation_record <- selected_presentation_record()
    chart_id <- selected_presentation_chart_id()
    req(!is.null(presentation_record), !is.null(chart_id))

    run_with_status(
      "Removing chart from the presentation...",
      "Chart removed from the presentation.",
      {
        updated_record <- update_presentation_charts(
          presentation_record,
          setdiff(presentation_record$chart_ids[[1]] %||% character(), chart_id)
        )

        updated_presentations <- upsert_chart_presentation_record(presentation_library_store(), updated_record)
        presentation_library_store(updated_presentations)
        write_chart_presentation_library(updated_presentations)
      },
      failure_prefix = "Unable to remove the chart from the presentation:"
    )
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
    updateSelectInput(session, "analysis_seasonal_series", choices = series_choices, selected = series_choices[1] %||% character())
    updateSelectInput(session, "analysis_hp_series", choices = series_choices, selected = series_choices[1] %||% character())
    updateSelectInput(session, "analysis_kalman_series", choices = series_choices, selected = series_choices[1] %||% character())
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

    if (identical(active_tab, "Seasonal Adjust")) {
      if (length(available_series()) < 1) {
        return(empty_plotly_widget("Add at least one chart series to run seasonal adjustment."))
      }

      result <- tryCatch(seasonal_adjustment_result(), error = function(error) NULL)
      if (is.null(result)) {
        return(empty_plotly_widget("Unable to run X-13 seasonal adjustment for the selected series."))
      }

      return(
        ggplotly(
          build_seasonal_adjustment_plot(
            result,
            input$analysis_seasonal_series,
            input$analysis_seasonal_view %||% "both"
          ),
          tooltip = c("x", "y", "colour")
        )
      )
    }

    if (identical(active_tab, "HP Filter")) {
      if (length(available_series()) < 1) {
        return(empty_plotly_widget("Add at least one chart series to run an HP filter."))
      }

      result <- tryCatch(hp_filter_result(), error = function(error) NULL)
      if (is.null(result)) {
        return(empty_plotly_widget("Unable to estimate the requested HP filter."))
      }

      return(
        ggplotly(
          build_hp_filter_plot(
            result,
            input$analysis_hp_series,
            input$analysis_hp_side %||% "two_sided",
            input$analysis_hp_view %||% "overlay"
          ),
          tooltip = c("x", "y", "colour")
        )
      )
    }

    if (identical(active_tab, "Kalman Filter")) {
      if (length(available_series()) < 1) {
        return(empty_plotly_widget("Add at least one chart series to run a Kalman filter."))
      }

      result <- tryCatch(kalman_filter_result(), error = function(error) NULL)
      if (is.null(result)) {
        return(empty_plotly_widget("Unable to estimate the requested Kalman filter."))
      }

      return(
        ggplotly(
          build_kalman_filter_plot(
            result,
            input$analysis_kalman_series,
            input$analysis_kalman_side %||% "two_sided",
            input$analysis_kalman_view %||% "overlay"
          ),
          tooltip = c("x", "y", "colour")
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
      summary_chip("Model", result$metrics$model_label),
      summary_chip("Observations", result$metrics$observations),
      summary_chip("R-squared", round(result$metrics$r_squared, 3)),
      summary_chip("Adj. R-squared", round(result$metrics$adjusted_r_squared, 3)),
      summary_chip(result$metrics$statistic_label, round(result$metrics$statistic_value, 3))
    )
  })

  seasonal_adjustment_result <- reactive({
    req(length(available_series()) >= 1)
    req(nzchar(input$analysis_seasonal_series))

    seasonal_adjustment_analysis(
      chart_data(),
      series_name = input$analysis_seasonal_series,
      display_mode = input$analysis_seasonal_view %||% "both"
    )
  })

  output$analysis_seasonal_summary <- renderUI({
    if (length(available_series()) < 1) {
      return(div(class = "empty-state", "Add at least one chart series to run seasonal adjustment."))
    }

    result <- tryCatch(seasonal_adjustment_result(), error = function(error) NULL)
    if (is.null(result)) {
      return(div(class = "empty-state", "Unable to run X-13 seasonal adjustment for the selected series."))
    }

    div(
      class = "summary-chip-row",
      summary_chip("Method", result$metrics$method),
      summary_chip("Frequency", result$metrics$frequency),
      summary_chip("Points", result$metrics$observations),
      summary_chip("Start", format(result$metrics$start_date, "%Y-%m-%d")),
      summary_chip("End", format(result$metrics$end_date, "%Y-%m-%d"))
    )
  })

  hp_filter_result <- reactive({
    req(length(available_series()) >= 1)
    req(nzchar(input$analysis_hp_series))

    hp_filter_analysis(
      chart_data(),
      series_name = input$analysis_hp_series,
      side = input$analysis_hp_side %||% "two_sided",
      display_mode = input$analysis_hp_view %||% "overlay"
    )
  })

  output$analysis_hp_summary <- renderUI({
    if (length(available_series()) < 1) {
      return(div(class = "empty-state", "Add at least one chart series to run an HP filter."))
    }

    result <- tryCatch(hp_filter_result(), error = function(error) NULL)
    if (is.null(result)) {
      return(div(class = "empty-state", "Unable to estimate the requested HP filter."))
    }

    div(
      class = "summary-chip-row",
      summary_chip("Method", result$metrics$method),
      summary_chip("Side", result$metrics$side),
      summary_chip("Lambda", format(round(result$metrics$lambda, 2), trim = TRUE)),
      summary_chip("Points", result$metrics$observations),
      summary_chip("Frequency", result$metrics$frequency)
    )
  })

  kalman_filter_result <- reactive({
    req(length(available_series()) >= 1)
    req(nzchar(input$analysis_kalman_series))

    kalman_filter_analysis(
      chart_data(),
      series_name = input$analysis_kalman_series,
      side = input$analysis_kalman_side %||% "two_sided",
      display_mode = input$analysis_kalman_view %||% "overlay"
    )
  })

  output$analysis_kalman_summary <- renderUI({
    if (length(available_series()) < 1) {
      return(div(class = "empty-state", "Add at least one chart series to run a Kalman filter."))
    }

    result <- tryCatch(kalman_filter_result(), error = function(error) NULL)
    if (is.null(result)) {
      return(div(class = "empty-state", "Unable to estimate the requested Kalman filter."))
    }

    div(
      class = "summary-chip-row",
      summary_chip("Method", result$metrics$method),
      summary_chip("Side", result$metrics$side),
      summary_chip("Points", result$metrics$observations),
      summary_chip("Frequency", result$metrics$frequency),
      summary_chip("Range", paste(format(result$metrics$start_date, "%Y"), "-", format(result$metrics$end_date, "%Y")))
    )
  })

  observeEvent(input$analysis_add_seasonal_series, {
    result <- tryCatch(seasonal_adjustment_result(), error = function(error) NULL)
    if (is.null(result)) {
      showNotification("Run a valid seasonal adjustment before adding it to the builder.", type = "error")
      return(invisible(NULL))
    }

    target_index <- max(1L, min(MAX_SERIES, as.integer(input$analysis_seasonal_target_series %||% "1")))
    series_name <- trimws(input$analysis_seasonal_series %||% "Seasonal adjustment")
    display_mode <- input$analysis_seasonal_view %||% "both"
    result_key <- paste0(
      "seasonal_adjustment::",
      digest::digest(list(series_name, display_mode, result$data))
    )

    analysis_spec <- normalize_series_spec(list(
      index = target_index,
      source = "analysis_result",
      label = if (identical(display_mode, "adjusted")) paste(series_name, "(SA)") else paste(series_name, "(X-13)"),
      transform_profile = default_transform_profile(),
      vis_type = "line",
      analysis_result_key = result_key,
      analysis_result_name = paste(series_name, "seasonal adjustment"),
      analysis_data = result$data %>%
        transmute(date = as.Date(date), value = as.numeric(value), name = as.character(series))
    ))

    updated_state <- builder_state()
    updated_state$series[[target_index]] <- analysis_spec
    apply_builder_state(updated_state, selected_series_index = target_index, navigate_builder = FALSE)

    showNotification(sprintf("Seasonally adjusted result added to Series %s.", target_index), type = "message")
  })

  observeEvent(input$analysis_add_hp_series, {
    result <- tryCatch(hp_filter_result(), error = function(error) NULL)
    if (is.null(result)) {
      showNotification("Run a valid HP filter before adding it to the builder.", type = "error")
      return(invisible(NULL))
    }

    target_index <- max(1L, min(MAX_SERIES, as.integer(input$analysis_hp_target_series %||% "1")))
    series_name <- trimws(input$analysis_hp_series %||% "HP filter")
    view_mode <- input$analysis_hp_view %||% "overlay"
    side <- input$analysis_hp_side %||% "two_sided"
    result_key <- paste0("hp_filter::", digest::digest(list(series_name, side, view_mode, result$data)))

    analysis_spec <- normalize_series_spec(list(
      index = target_index,
      source = "analysis_result",
      label = if (identical(view_mode, "trend")) paste(series_name, "(HP trend)") else paste(series_name, "(HP)"),
      transform_profile = default_transform_profile(),
      vis_type = "line",
      analysis_result_key = result_key,
      analysis_result_name = paste(series_name, "HP filter"),
      analysis_data = result$data %>%
        transmute(date = as.Date(date), value = as.numeric(value), name = as.character(series))
    ))

    updated_state <- builder_state()
    updated_state$series[[target_index]] <- analysis_spec
    apply_builder_state(updated_state, selected_series_index = target_index, navigate_builder = FALSE)

    showNotification(sprintf("HP-filtered result added to Series %s.", target_index), type = "message")
  })

  observeEvent(input$analysis_add_kalman_series, {
    result <- tryCatch(kalman_filter_result(), error = function(error) NULL)
    if (is.null(result)) {
      showNotification("Run a valid Kalman filter before adding it to the builder.", type = "error")
      return(invisible(NULL))
    }

    target_index <- max(1L, min(MAX_SERIES, as.integer(input$analysis_kalman_target_series %||% "1")))
    series_name <- trimws(input$analysis_kalman_series %||% "Kalman filter")
    view_mode <- input$analysis_kalman_view %||% "overlay"
    side <- input$analysis_kalman_side %||% "two_sided"
    result_key <- paste0("kalman_filter::", digest::digest(list(series_name, side, view_mode, result$data)))

    analysis_spec <- normalize_series_spec(list(
      index = target_index,
      source = "analysis_result",
      label = if (identical(view_mode, "trend")) paste(series_name, "(Kalman trend)") else paste(series_name, "(Kalman)"),
      transform_profile = default_transform_profile(),
      vis_type = "line",
      analysis_result_key = result_key,
      analysis_result_name = paste(series_name, "Kalman filter"),
      analysis_data = result$data %>%
        transmute(date = as.Date(date), value = as.numeric(value), name = as.character(series))
    ))

    updated_state <- builder_state()
    updated_state$series[[target_index]] <- analysis_spec
    apply_builder_state(updated_state, selected_series_index = target_index, navigate_builder = FALSE)

    showNotification(sprintf("Kalman-filtered result added to Series %s.", target_index), type = "message")
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

  build_presentation_export_widget <- function(chart_record) {
    build_saved_chart_widget(chart_record, width = 960, height = 540)
  }

  save_presentation_document <- function(title, subtitle = NULL, sections, file) {
    document <- tags$html(
      tags$head(
        tags$title(title),
        tags$style(
          "body { font-family: 'Helvetica Neue', Arial, sans-serif; margin: 32px; }
           h1 { margin-bottom: 12px; }
           section:last-child { page-break-after: auto; }
           .deck-meta { color: #475569; margin-bottom: 24px; }
           .deck-chart { margin-top: 18px; width: 960px; max-width: 960px; }"
        )
      ),
      tags$body(
        tags$h1(title),
        if (!is.null(subtitle) && nzchar(trimws(subtitle))) tags$p(class = "deck-meta", subtitle),
        sections
      )
    )

    htmlwidgets::saveWidget(
      widget = htmltools::browsable(document),
      file = file,
      selfcontained = TRUE
    )
  }

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
        build_saved_chart_widget(chart_record),
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
          div(
            class = "deck-chart",
            htmltools::as.tags(build_presentation_export_widget(chart_record))
          )
        )
      })

      save_presentation_document(
        title = "Saved Charts Presentation",
        subtitle = paste("Generated on", format(Sys.time(), "%d %b %Y %H:%M")),
        sections = presentation_sections,
        file = file
      )
    }
  )

  output$export_selected_presentation <- downloadHandler(
    filename = function() {
      presentation_record <- selected_presentation_record()
      req(!is.null(presentation_record))
      paste0(str_replace_all(presentation_record$title[[1]], "[^A-Za-z0-9]+", "_"), ".html")
    },
    content = function(file) {
      presentation_record <- selected_presentation_record()
      presentation_records <- selected_presentation_charts()
      req(!is.null(presentation_record), nrow(presentation_records) > 0)

      presentation_sections <- lapply(seq_len(nrow(presentation_records)), function(row_index) {
        chart_record <- presentation_records[row_index, , drop = FALSE]

        tags$section(
          style = "page-break-after: always; padding: 24px 0;",
          tags$h1(chart_record$title[[1]]),
          tags$p(chart_record$description[[1]] %||% ""),
          div(
            class = "deck-chart",
            htmltools::as.tags(build_presentation_export_widget(chart_record))
          )
        )
      })

      save_presentation_document(
        title = presentation_record$title[[1]],
        subtitle = paste(
          c(
            trimws(presentation_record$description[[1]] %||% ""),
            paste("Exported on", format(Sys.time(), "%d %b %Y %H:%M"))
          )[nzchar(c(
            trimws(presentation_record$description[[1]] %||% ""),
            paste("Exported on", format(Sys.time(), "%d %b %Y %H:%M"))
          ))],
          collapse = " | "
        ),
        sections = presentation_sections,
        file = file
      )
    }
  )
}

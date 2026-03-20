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
            class = "search-toolbar__group search-toolbar__group--segmented search-toolbar__group--wide",
            style = "padding-bottom: 0;",
            div(
              class = "search-toolbar__group-label",
              "Source"
            ),
            div(
              class = "search-segmented-control",
              radioGroupButtons(
                "search_source_filter",
                NULL,
                choices = search_source_filter_choices(),
                selected = "all",
                justified = FALSE,
                checkIcon = list(yes = icon("check"))
              )
            )
          ),
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
          ),
          div(
            class = "search-toolbar__group search-toolbar__group--segmented",
            style = "padding-bottom: 0;",
            div(
              class = "search-toolbar__group-label",
              "Type"
            ),
            div(
              class = "search-segmented-control",
              radioGroupButtons(
                "search_type_filter",
                NULL,
                choices = search_type_choices(),
                selected = "all",
                justified = FALSE,
                checkIcon = list(yes = icon("check"))
              )
            )
          ),
          div(
            class = "search-toolbar__group search-toolbar__group--segmented",
            style = "padding-bottom: 0;",
            div(
              class = "search-toolbar__group-label",
              "Location"
            ),
            div(
              class = "search-segmented-control",
              radioGroupButtons(
                "search_location_filter",
                NULL,
                choices = search_location_choices(),
                selected = "all",
                justified = FALSE,
                checkIcon = list(yes = icon("check"))
              )
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
          uiOutput("search_results_panel")
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
            plotOutput("search_preview_plot", height = "256px")
          ),
          div(
            class = "search-segmented-control search-segmented-control--target",
            radioGroupButtons(
              "search_target_series",
              "Add to",
              choices = search_result_target_choices(),
              selected = "next",
              justified = FALSE,
              checkIcon = list(yes = icon("check"))
            )
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
          header_actions = actionButton("clear_library_selection", "Clear", class = "app-card__header-chip"),
          textInput("library_search", "Search saved charts", value = "", placeholder = "Search by title, description, or source"),
          DT::dataTableOutput("library_table"),
          div(
            class = "library-actions",
            actionButton("update_chart", "Update selected"),
            actionButton("delete_chart", "Delete saved chart")
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
          header_actions = div(
            class = "card-renderer-toggle",
            radioGroupButtons(
              "library_renderer_toggle",
              NULL,
              choices = c("Plotly" = "plotly", "Static" = "static"),
              selected = "plotly",
              justified = FALSE,
              checkIcon = list(yes = icon("check"))
            )
          ),
          uiOutput("library_selected_meta"),
          uiOutput("library_plot_container"),
          div(
            class = "library-actions",
            actionButton("load_chart", "Load into builder"),
            downloadButton("export_preview_png", "Download PNG"),
            downloadButton("export_preview_svg", "Download SVG"),
            downloadButton("export_preview_pptx", "Download PPTX")
          )
        ),
        chart_card(
          "Presentation Details",
          header_actions = actionButton("clear_presentation_chart_selection", "Clear", class = "app-card__header-chip"),
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

build_panel_loading_ui <- function(message) {
  div(
    class = "search-results-loading",
    div(class = "search-results-loading__label", message),
    div(
      class = "search-results-loading__track",
      div(class = "search-results-loading__bar")
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

build_about_tab_ui <- function() {
  div(
    class = "page-shell",
    fluidRow(
      column(
        width = 8,
        chart_card(
          "About Chartwell",
          tags$p(
            class = "muted-copy",
            "Chartwell helps you search, transform, analyse, save, and present macro and financial charts."
          ),
          tags$h4("How it works"),
          tags$p(
            "Start in Data Search to find a series from the local metadata index or live FRED search. You can preview a result and add it directly into the chart builder."
          ),
          tags$p(
            "In Chart Builder, choose up to four series, set the date window, and apply transformations such as rolling averages, percent changes, custom formulas, and cross-series comparisons."
          ),
          tags$p(
            "The Analysis workspace adds tools for correlations, regressions, forecasts, seasonal adjustment, HP filtering, and Kalman filtering. Results can be shown on the main chart and saved to the library like any other chart."
          ),
          tags$p(
            "Saved Charts stores both regular charts and analysis charts. Presentations let you collect charts, reorder them, and export a chart deck for sharing."
          )
        )
      ),
      column(
        width = 4,
        chart_card(
          "Created By",
          tags$p(
            class = "muted-copy",
            "Chartwell was created by Joel Findlay."
          ),
          tags$p(
            "The app is designed to make macro and financial data easier to search, chart, compare, and present in a clean, repeatable workflow."
          ),
          tags$h4("What You Can Do"),
          tags$ul(
            tags$li("Search series across supported sources"),
            tags$li("Build and style charts"),
            tags$li("Run time-series analysis tools"),
            tags$li("Save charts and presentations for later use")
          )
        )
      )
    )
  )
}

search_activity_message <- function(source_filter = "all", query_text = "") {
  cleaned_query <- trimws(query_text %||% "")
  targets <- search_activity_target(source_filter, cleaned_query)

  if (nzchar(cleaned_query)) {
    sprintf("Searching %s for \"%s\"...", targets, cleaned_query)
  } else {
    sprintf("Refreshing %s...", targets)
  }
}

build_chartwell_wordmark <- function() {
  div(
    class = "chartwell-wordmark",
    tags$span(class = "chartwell-wordmark__name", "Chartwell"),
    tags$span(class = "chartwell-wordmark__sub", "Macro & Financial Charts")
  )
}

build_main_ui <- function() {
  year_bounds <- default_year_bounds()

  tagList(
    tags$head(
      tags$title("Chartwell — Macro & Financial Charts"),
      tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
      tags$link(rel = "icon", type = "image/svg+xml", href = "assets/chartwell-icon.svg"),
      tags$meta(name = "theme-color", content = "#f5f8f4")
    ),
    navbarPage(
      title = build_chartwell_wordmark(),
      windowTitle = "Chartwell — Macro & Financial Charts",
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
                header_actions = actionButton("reset_builder", "Reset", class = "app-card__header-chip"),
                tags$p(
                  class = "muted-copy",
                  "Set the date window and choose whether to show the data table."
                ),
                div(
                  class = "segmented-control",
                  radioGroupButtons(
                    "date_window_shortcut",
                    "Quick range",
                    choices = c("1Y" = "1", "3Y" = "3", "5Y" = "5", "10Y" = "10", "20Y" = "20", "Max" = "max"),
                    selected = character(0),
                    justified = TRUE,
                    checkIcon = list(yes = icon("check"))
                  )
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
                div(
                  class = "segmented-control",
                  radioGroupButtons(
                    "viewData1",
                    "Underlying data table",
                    choices = c("Hide" = "0", "Show" = "1"),
                    selected = "0",
                    justified = TRUE,
                    checkIcon = list(yes = icon("check"))
                  )
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
              )
            ),
            column(
              width = 6,
              chart_card(
                "Primary Chart",
                header_actions = div(
                  class = "card-renderer-toggle",
                  radioGroupButtons(
                    "style_renderer",
                    NULL,
                    choices = c("Plotly" = "plotly", "Static" = "static"),
                    selected = "plotly",
                    justified = FALSE,
                    checkIcon = list(yes = icon("check"))
                  )
                ),
                uiOutput("builder_summary"),
                uiOutput("builder_messages"),
                uiOutput("builder_plot_container"),
                div(
                  class = "export-row",
                  downloadButton("exportPNG", "Download PNG"),
                  downloadButton("exportSVG", "Download SVG"),
                  downloadButton("exportPPTX", "Download PPTX"),
                  downloadButton("exportData", "Download CSV")
                ),
                tags$p(
                  class = "muted-copy export-note",
                  "Chart exports use the static chart renderer with dimensions specified in the panel below."
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
                  width = 8,
                  textInput("style_title", "Chart title shown above the plot", value = "Custom data view", width = "100%"),
                  textInput("style_subtitle", "Chart subtitle shown below the title", value = "", width = "100%"),
                  textInput("style_note", "Source note or caption shown below the chart", value = default_builder_state()$style$note, width = "100%")
                ),
                column(
                  width = 4,
                  selectInput("style_font_family", "Chart font", choices = APP_CHART_FONTS, selected = APP_CHART_FONTS[[1]]),
                  selectInput("style_palette", "Colour palette", choices = APP_PALETTES, selected = APP_PALETTES[[1]]),
                  textInput("style_y_axis_label", "Y-axis label", value = "%")
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  div(
                    class = "segmented-control",
                    radioGroupButtons(
                      "style_invert_y_axis",
                      "Y-axis direction",
                      choices = c("Standard" = "standard", "Inverted" = "inverted"),
                      selected = "standard",
                      justified = TRUE,
                      checkIcon = list(yes = icon("check"))
                    )
                  ),
                  div(
                    class = "segmented-control",
                    radioGroupButtons(
                      "style_auto_y_axis",
                      "Y-axis range mode",
                      choices = c("Automatic" = "auto", "Manual" = "manual"),
                      selected = "auto",
                      justified = TRUE,
                      checkIcon = list(yes = icon("check"))
                    )
                  ),
                  conditionalPanel(
                    condition = "input.style_auto_y_axis == 'manual'",
                    tags$div(
                      class = "paired-range-input",
                      tags$label(class = "control-label", `for` = "style_y_min", "Manual y-axis range"),
                      tags$div(
                        class = "paired-range-input__row",
                        tags$div(
                          class = "paired-range-input__field",
                          numericInput("style_y_min", label = NULL, value = NA)
                        ),
                        tags$div(class = "paired-range-input__separator", "to"),
                        tags$div(
                          class = "paired-range-input__field",
                          numericInput("style_y_max", label = NULL, value = NA)
                        )
                      )
                    ),
                    numericInput("style_y_breaks", "Manual y-axis interval", value = NA, min = 0.01)
                  )
                ),
                column(
                  width = 4,
                  selectInput("style_date_format", "Date format on the x-axis", choices = APP_DATE_FORMATS, selected = APP_DATE_FORMATS[[2]]),
                  numericInput("style_x_labels", "X-axis labels", value = 6, min = 2, step = 1)
                ),
                column(
                  width = 4,
                  div(
                    class = "segmented-control",
                    radioGroupButtons(
                      "style_legend",
                      "Legend position",
                      choices = c("Below" = "bottom", "Right" = "right", "Hide" = "none"),
                      selected = "bottom",
                      justified = TRUE,
                      checkIcon = list(yes = icon("check"))
                    )
                  ),
                  tags$div(
                    class = "paired-range-input",
                    tags$label(class = "control-label", `for` = "export_width", "PNG export size"),
                    tags$div(
                      class = "paired-range-input__row",
                      tags$div(
                        class = "paired-range-input__field",
                        numericInput("export_width", label = NULL, value = 8, min = 4, step = 0.5)
                      ),
                      tags$div(class = "paired-range-input__separator", "x"),
                      tags$div(
                        class = "paired-range-input__field",
                        numericInput("export_height", label = NULL, value = 7, min = 3, step = 0.5)
                      )
                    )
                  )
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
              div(
                class = "segmented-control",
                radioGroupButtons(
                  "side_panel_mode",
                  NULL,
                  choices = c("Transformations" = "transform", "Analysis" = "analysis"),
                  selected = "transform",
                  justified = TRUE,
                  checkIcon = list(yes = icon("check"))
                )
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
                    div(
                      class = "segmented-control",
                      radioGroupButtons(
                        "analysis_reg_errors",
                        "Standard error treatment",
                        choices = c("Homoskedastic" = "classical", "Heteroskedastic-robust" = "robust"),
                        selected = "classical",
                        justified = TRUE,
                        checkIcon = list(yes = icon("check"))
                      )
                    ),
                    uiOutput("analysis_reg_summary")
                  ),
                  tabPanel(
                    "Forecast",
                    tags$p(class = "muted-copy", "Run AR, MA, or ARMA forecasts and compare holdout results. The chart appears in the main panel."),
                    selectInput("analysis_forecast_series", "Series to forecast", choices = character()),
                    div(
                      class = "segmented-control",
                      radioGroupButtons(
                        "analysis_forecast_family",
                        "Model family",
                        choices = c("AR" = "AR", "MA" = "MA", "ARMA" = "ARMA"),
                        selected = "AR",
                        justified = TRUE,
                        checkIcon = list(yes = icon("check"))
                      )
                    ),
                    div(
                      class = "segmented-control",
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
                      )
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
                    div(
                      class = "segmented-control",
                      radioGroupButtons(
                        "analysis_seasonal_view",
                        "Display",
                        choices = c("Adjusted only" = "adjusted", "Original vs adjusted" = "both"),
                        selected = "both",
                        justified = TRUE,
                        checkIcon = list(yes = icon("check"))
                      )
                    ),
                    uiOutput("analysis_seasonal_summary"),
                    div(
                      class = "segmented-control",
                      radioGroupButtons(
                        "analysis_seasonal_target_series",
                        "Add adjusted result to",
                        choices = c("Series 1" = "1", "Series 2" = "2", "Series 3" = "3", "Series 4" = "4"),
                        selected = "1",
                        justified = TRUE,
                        checkIcon = list(yes = icon("check"))
                      )
                    ),
                    actionButton("analysis_add_seasonal_series", "Add to builder", class = "btn-primary btn-block")
                  ),
                  tabPanel(
                    "HP Filter",
                    tags$p(class = "muted-copy", "Estimate a Hodrick-Prescott trend and cycle. The chart appears in the main panel."),
                    selectInput("analysis_hp_series", "Series to filter", choices = character()),
                    div(
                      class = "segmented-control",
                      radioGroupButtons(
                        "analysis_hp_side",
                        "Filter direction",
                        choices = c("Two-sided" = "two_sided", "One-sided" = "one_sided"),
                        selected = "two_sided",
                        justified = TRUE,
                        checkIcon = list(yes = icon("check"))
                      )
                    ),
                    div(
                      class = "segmented-control",
                      radioGroupButtons(
                        "analysis_hp_view",
                        "Display",
                        choices = filter_display_choices(),
                        selected = "overlay",
                        justified = TRUE,
                        checkIcon = list(yes = icon("check"))
                      )
                    ),
                    uiOutput("analysis_hp_summary"),
                    div(
                      class = "segmented-control",
                      radioGroupButtons(
                        "analysis_hp_target_series",
                        "Add filtered result to",
                        choices = c("Series 1" = "1", "Series 2" = "2", "Series 3" = "3", "Series 4" = "4"),
                        selected = "1",
                        justified = TRUE,
                        checkIcon = list(yes = icon("check"))
                      )
                    ),
                    actionButton("analysis_add_hp_series", "Add to builder", class = "btn-primary btn-block")
                  ),
                  tabPanel(
                    "Kalman Filter",
                    tags$p(class = "muted-copy", "Estimate a local linear trend with a Kalman filter. The chart appears in the main panel."),
                    selectInput("analysis_kalman_series", "Series to filter", choices = character()),
                    div(
                      class = "segmented-control",
                      radioGroupButtons(
                        "analysis_kalman_side",
                        "Filter direction",
                        choices = c("Two-sided" = "two_sided", "One-sided" = "one_sided"),
                        selected = "two_sided",
                        justified = TRUE,
                        checkIcon = list(yes = icon("check"))
                      )
                    ),
                    div(
                      class = "segmented-control",
                      radioGroupButtons(
                        "analysis_kalman_view",
                        "Display",
                        choices = filter_display_choices(),
                        selected = "overlay",
                        justified = TRUE,
                        checkIcon = list(yes = icon("check"))
                      )
                    ),
                    uiOutput("analysis_kalman_summary"),
                    div(
                      class = "segmented-control",
                      radioGroupButtons(
                        "analysis_kalman_target_series",
                        "Add filtered result to",
                        choices = c("Series 1" = "1", "Series 2" = "2", "Series 3" = "3", "Series 4" = "4"),
                        selected = "1",
                        justified = TRUE,
                        checkIcon = list(yes = icon("check"))
                      )
                    ),
                    actionButton("analysis_add_kalman_series", "Add to builder", class = "btn-primary btn-block")
                  )
                )
              )
            ),
            chart_card(
              "Save to Library",
              textInput("library_title", "Chart title", value = ""),
              textAreaInput("library_description", "Library description", value = "", rows = 3, resize = "vertical"),
              actionButton("save_chart", "Save chart to library", class = "btn-primary btn-block")
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
    tabPanel(
      title = "About",
      value = "about",
      build_about_tab_ui()
    ),
    collapsible = TRUE
  )
  )
}

default_builder_restore_context <- function() {
  list(
    mode = "idle",
    token = 0L,
    scope = "none",
    selected_series_index = NULL
  )
}

init_builder_restore_state <- function(session) {
  restored_state <- reactiveVal(NULL)
  builder_restore_context <- reactiveVal(default_builder_restore_context())
  session$userData$builder_restore_token <- 0L

  builder_restore_mode <- function() {
    builder_restore_context()$mode %||% "idle"
  }

  clear_builder_restore_state <- function(clear_series_specs = TRUE) {
    restored_state(NULL)
    builder_restore_context(default_builder_restore_context())
    session$userData$builder_restore_token <- 0L
    if (isTRUE(clear_series_specs)) {
      session$userData$restored_series_specs <- list()
    }

    invisible(NULL)
  }

  settle_builder_restore_state <- function(expected_token = NULL) {
    current_context <- shiny::isolate(builder_restore_context())
    current_token <- current_context$token %||% 0L
    target_token <- expected_token %||% current_token

    if (!identical(current_context$mode %||% "idle", "applying")) {
      return(invisible(NULL))
    }

    if (!identical(current_token, target_token)) {
      return(invisible(NULL))
    }

    builder_restore_context(list(
      mode = "settled",
      token = current_token,
      scope = current_context$scope %||% "full",
      selected_series_index = current_context$selected_series_index
    ))

    invisible(NULL)
  }

  apply_builder_state <- function(chart_state, selected_series_index = NULL, navigate_builder = FALSE, restore_scope = "full") {
    normalized_state <- normalize_chart_state(chart_state)
    next_token <- (shiny::isolate(builder_restore_context())$token %||% 0L) + 1L

    restored_state(normalized_state)
    builder_restore_context(list(
      mode = "applying",
      token = next_token,
      scope = restore_scope %||% "full",
      selected_series_index = selected_series_index
    ))
    session$userData$builder_restore_token <- next_token
    session$userData$restored_series_specs <- list()
    restore_chart_state(session, normalized_state, restore_token = next_token)

    if (!is.null(selected_series_index)) {
      updateTabsetPanel(session, "series_tabs", selected = paste("Series", selected_series_index))
    }

    if (isTRUE(navigate_builder)) {
      updateNavbarPage(session, "main_tabs", selected = "builder")
    }

    invisible(NULL)
  }

  list(
    restored_state = restored_state,
    builder_restore_context = builder_restore_context,
    builder_restore_mode = builder_restore_mode,
    clear_builder_restore_state = clear_builder_restore_state,
    settle_builder_restore_state = settle_builder_restore_state,
    apply_builder_state = apply_builder_state
  )
}

init_builder_sync_handlers <- function(
    input,
    session,
    builder_state,
    restored_state,
    builder_restore_mode,
    clear_builder_restore_state,
    settle_builder_restore_state,
    apply_builder_state,
    synced_library_title,
    synced_source_note
) {
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
    if (identical(builder_restore_mode(), "applying")) {
      return(invisible(NULL))
    }

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

  observeEvent(
    {
      c(
        list(
          input$start_date,
          input$end_date,
          input$style_title,
          input$style_subtitle,
          input$style_y_axis_label,
          input$style_note,
          input$style_renderer,
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
          input$export_height,
          input$viewData1,
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
            input[[series_input_id(index, "enabled")]],
            input[[series_input_id(index, "source")]],
            input[[series_input_id(index, "label")]],
            input[[series_input_id(index, "vis_type")]],
            input[[series_input_id(index, "text")]],
            input[[series_input_id(index, "region")]],
            input[[series_input_id(index, "transform")]],
            input[[series_input_id(index, "rebase_date")]],
            input[[series_input_id(index, "fred_series")]],
            input[[series_input_id(index, "fred_vintage_mode")]],
            input[[series_input_id(index, "fred_vintage_date")]],
            input[[series_input_id(index, "dbnomics_series")]],
            input[[series_input_id(index, "rba_table")]],
            input[[series_input_id(index, "rba_desc")]],
            input[[series_input_id(index, "abs_catalogue")]],
            input[[series_input_id(index, "abs_desc")]],
            input[[series_input_id(index, "abs_series_type")]],
            input[[series_input_id(index, "abs_table")]],
            input[[series_input_id(index, "abs_id")]],
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

      current_state <- normalize_chart_state(builder_state_from_input(input, session))
      if (identical(builder_restore_mode(), "applying")) {
        if (identical(current_state, loaded_state)) {
          settle_builder_restore_state()
        }
        return(invisible(NULL))
      }

      if (!identical(current_state, loaded_state)) {
        clear_builder_restore_state()
      }
    },
    ignoreInit = TRUE
  )

  invisible(NULL)
}

init_search_builder_handlers <- function(
    input,
    output,
    session,
    loaded_main_tabs,
    search_results,
    search_query_notification_id,
    builder_state,
    apply_builder_state,
    run_with_status
) {
  build_search_preview_style <- function(preview_state, preview_payload) {
    preview_style <- preview_state$style
    preview_style$title <- ""
    preview_style$subtitle <- ""
    preview_style$legend <- "none"
    preview_style$note <- ""
    preview_style$y_axis_label <- ""
    preview_style$x_labels <- 3
    preview_style$y_breaks <- pretty(preview_payload$data$value, n = 3)
    preview_style$auto_y_axis <- TRUE
    preview_style
  }

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
    search_result_rows <- search_results()
    series_ids <- vapply(
      seq_len(nrow(search_result_rows)),
      function(index) provider_registry_search_result_series_id(search_result_rows[index, , drop = FALSE]),
      character(1)
    )

    search_rows <- search_result_rows %>%
      transmute(
        Title = title,
        Source = source,
        `Series ID` = series_ids,
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
          list(width = "28%", targets = 0),
          list(width = "12%", targets = 2),
          list(width = "30%", targets = 8)
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

  search_preview_context <- reactive({
    search_row <- selected_search_result()

    if (is.null(search_row)) {
      return(NULL)
    }

    preview_spec <- search_result_series_spec(search_row, 1)
    preview_state <- preview_chart_state(
      preview_spec,
      date_range = c(search_row$start_date[[1]], search_row$end_date[[1]])
    )
    preview_payload <- build_chart_data(preview_state)

    if (nrow(preview_payload$data) == 0) {
      return(list(search_row = search_row, payload = preview_payload, style = NULL))
    }

    list(
      search_row = search_row,
      payload = preview_payload,
      style = build_search_preview_style(preview_state, preview_payload)
    )
  })

  search_preview_widget <- reactive({
    preview_context <- search_preview_context()

    if (is.null(preview_context)) {
      return(empty_plotly_widget("Select a result to preview it."))
    }

    if (nrow(preview_context$payload$data) == 0) {
      return(empty_plotly_widget("No preview data is available for this result."))
    }

    build_chart_widget(preview_context$payload$data, preview_context$style)
  })

  search_preview_plot <- reactive({
    preview_context <- search_preview_context()

    if (is.null(preview_context)) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Select a result to preview it.", size = 5, colour = "#64748b") +
          xlim(0, 1) +
          ylim(0, 1) +
          theme_void()
      )
    }

    if (nrow(preview_context$payload$data) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No preview data is available for this result.", size = 5, colour = "#64748b") +
          xlim(0, 1) +
          ylim(0, 1) +
          theme_void()
      )
    }

    build_chart_plot(preview_context$payload$data, preview_context$style) +
      theme(
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        plot.tag = element_blank(),
        plot.margin = margin(t = -10, r = 4, b = 12, l = 0),
        axis.text.y.right = element_text(margin = margin(l = 10))
      )
  })

  output$search_preview_plot <- renderPlot({
    search_preview_plot()
  })

  observeEvent(input$search_add_series, {
    search_row <- selected_search_result()
    req(!is.null(search_row))

    run_with_status(
      "Adding the selected search result to the builder...",
      "Search result added to the builder.",
      {
        target_index <- series_slot_from_search_target(builder_state(), input$search_target_series)
        series_spec <- search_result_series_spec(search_row, target_index)
        updated_state <- builder_state()
        updated_state$series[[target_index]] <- normalize_series_spec(series_spec)
        apply_builder_state(
          updated_state,
          selected_series_index = target_index,
          navigate_builder = FALSE,
          restore_scope = "single_series"
        )
      },
      failure_prefix = "Unable to add the search result:"
    )
  })

  list(
    selected_search_result = selected_search_result,
    search_preview_widget = search_preview_widget
  )
}

init_saved_analysis_restore_handlers <- function(session) {
  restore_saved_analysis_state <- function(chart_record) {
    chart_kind <- chart_record$chart_kind[[1]] %||% "builder"
    analysis_spec <- chart_record$analysis_spec[[1]] %||% list()
    expected_restore_token <- session$userData$builder_restore_token %||% 0L

    if (identical(chart_kind, "builder")) {
      updateRadioGroupButtons(session, "side_panel_mode", selected = "transform")
      return(invisible(NULL))
    }

    updateRadioGroupButtons(session, "side_panel_mode", selected = "analysis")
    updateTabsetPanel(session, "analysis_tabs", selected = analysis_spec$tab %||% "Correlations")

    session$onFlushed(function() {
      current_restore_token <- session$userData$builder_restore_token %||% 0L
      if (!identical(current_restore_token, expected_restore_token)) {
        return(invisible(NULL))
      }

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

  list(
    restore_saved_analysis_state = restore_saved_analysis_state
  )
}

init_library_preview_handlers <- function(
    input,
    output,
    session,
    ensure_chart_library_loaded,
    selected_chart_record,
    selected_chart_records,
    selected_presentation_charts,
    library_renderer,
    build_saved_chart_widget,
    build_saved_chart_plot,
    run_with_status,
    apply_builder_state,
    restore_saved_analysis_state
) {
  selected_presentation_chart_record <- reactive({
    selected_row <- input$presentation_chart_table_rows_selected
    presentation_rows <- selected_presentation_charts()

    if (length(selected_row) == 0 || nrow(presentation_rows) == 0) {
      return(NULL)
    }

    presentation_rows[selected_row[1], , drop = FALSE]
  })

  selected_preview_chart_record <- reactive({
    presentation_chart_record <- selected_presentation_chart_record()
    if (!is.null(presentation_chart_record)) {
      return(presentation_chart_record)
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

  library_plot_height <- reactive({
    chart_record <- selected_preview_chart_record()
    if (is.null(chart_record) || nrow(chart_record) == 0) {
      return(500L)
    }

    chart_kind <- chart_record$chart_kind[[1]] %||% "builder"
    chart_state <- chart_record$chart_state[[1]] %||% list(style = default_style_settings())
    chart_style <- chart_state$style %||% default_style_settings()

    if (!identical(chart_kind, "builder")) {
      return(500L)
    }

    plotly_widget_height(chart_record$data_snapshot[[1]], chart_style, base_height = 500)
  })

  output$library_plot_container <- renderUI({
    if (identical(library_renderer(), "plotly")) {
      return(plotlyOutput("library_plot", height = paste0(library_plot_height(), "px")))
    }

    plotOutput("library_plot_static", height = paste0(library_plot_height(), "px"))
  })

  output$library_plot_static <- renderPlot({
    chart_record <- selected_preview_chart_record()
    if (is.null(chart_record)) {
      return(empty_static_plot("Select a saved chart to preview it."))
    }

    build_saved_chart_plot(chart_record)
  })

  observeEvent(input$load_chart, {
    ensure_chart_library_loaded()
    chart_record <- selected_preview_chart_record()
    req(!is.null(chart_record), nrow(chart_record) == 1)

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

  list(
    selected_preview_chart_record = selected_preview_chart_record
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
    if (search_filter_includes(input$search_source_filter, "FRED") && !fred_search_available()) {
      show_fred_key_modal()
    }
  }, ignoreInit = TRUE)

  observe({
    series_sources <- vapply(
      seq_len(MAX_SERIES),
      function(index) input[[series_input_id(index, "source")]] %||% "abs",
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

  restore_manager <- init_builder_restore_state(session)
  restored_state <- restore_manager$restored_state
  builder_restore_mode <- restore_manager$builder_restore_mode
  clear_builder_restore_state <- restore_manager$clear_builder_restore_state
  apply_builder_state <- restore_manager$apply_builder_state
  synced_library_title <- reactiveVal("")
  synced_source_note <- reactiveVal(default_builder_state()$style$note)

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

    min_builder_date <- as.Date("1900-01-01")
    input_state$date_range <- as.Date(
      sort(pmax(as.numeric(min_builder_date), as.numeric(input_state$date_range))),
      origin = "1970-01-01"
    )
    input_state
  })

  init_builder_sync_handlers(
    input = input,
    session = session,
    builder_state = builder_state,
    restored_state = restored_state,
    builder_restore_mode = builder_restore_mode,
    clear_builder_restore_state = clear_builder_restore_state,
    settle_builder_restore_state = restore_manager$settle_builder_restore_state,
    apply_builder_state = apply_builder_state,
    synced_library_title = synced_library_title,
    synced_source_note = synced_source_note
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

  current_renderer <- reactive({
    builder_state()$style$renderer %||% "plotly"
  })

  current_widget <- reactive({
    build_chart_widget(chart_data(), builder_state()$style)
  })

  export_widget <- reactive({
    style <- builder_state()$style
    width_px <- round((style$export_width %||% 8) * 160)
    height_px <- round((style$export_height %||% 7) * 160)

    current_widget() %>%
      plotly::layout(
        autosize = FALSE,
        width = width_px,
        height = height_px
      ) %>%
      plotly::config(
        responsive = FALSE,
        displayModeBar = FALSE
      )
  })

  empty_static_plot <- function(message) {
    ggplot() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = message, size = 6, colour = "#64748b") +
      coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE)
  }

  chart_export_python_path <- function() {
    configured_path <- trimws(Sys.getenv("RETICULATE_PYTHON", unset = ""))
    discovered_path <- trimws(Sys.which("python3"))
    python_path <- c(configured_path, discovered_path)
    python_path <- python_path[nzchar(python_path)][1] %||% ""

    if (!nzchar(python_path) || !file.exists(python_path)) {
      return("")
    }

    python_path
  }

  save_plotly_image_with_python <- function(widget, file, width_px, height_px, format = c("png", "svg")) {
    format <- match.arg(format)
    python_path <- chart_export_python_path()

    if (!nzchar(python_path)) {
      stop("Python is not available for Plotly image export.", call. = FALSE)
    }

    widget_json <- plotly::plotly_json(widget, jsonedit = FALSE, pretty = FALSE)
    json_file <- tempfile(fileext = ".json")
    script_file <- tempfile(fileext = ".py")

    writeLines(as.character(widget_json), con = json_file, useBytes = TRUE)
    writeLines(
      c(
        "import pathlib",
        "import sys",
        "import plotly.io as pio",
        "",
        "json_path = pathlib.Path(sys.argv[1])",
        "output_path = pathlib.Path(sys.argv[2])",
        "width = int(sys.argv[3])",
        "height = int(sys.argv[4])",
        "fmt = sys.argv[5]",
        "",
        "figure = pio.from_json(json_path.read_text(encoding='utf-8'))",
        "figure.write_image(output_path, format=fmt, width=width, height=height, scale=2 if fmt == 'png' else 1)"
      ),
      con = script_file
    )

    command_output <- system2(
      python_path,
      c(script_file, json_file, file, as.integer(width_px), as.integer(height_px), format),
      stdout = TRUE,
      stderr = TRUE
    )
    command_status <- as.integer(attr(command_output, "status") %||% 0L)

    if (!isTRUE(command_status == 0L)) {
      stop(paste(command_output, collapse = "\n"), call. = FALSE)
    }
  }

  chartwell_title_slide_template_path <- function() {
    candidate_paths <- c(
      here::here("brand", "chartwell-title-slide.pptx"),
      here::here("brand", "chartwell-title-slide_widescreen.pptx"),
      here::here("chartwell-title-slide.pptx"),
      here::here("chartwell-title-slide_widescreen.pptx")
    )

    existing_path <- candidate_paths[file.exists(candidate_paths)][1] %||% NA_character_
    if (is.na(existing_path) || !nzchar(existing_path)) {
      return("")
    }

    existing_path
  }

  save_chart_pptx_with_python <- function(image_files, pptx_file, slide_title = NULL) {
    python_path <- chart_export_python_path()

    if (!nzchar(python_path)) {
      stop("Python 3 is not available for PPTX export.", call. = FALSE)
    }

    image_files <- normalizePath(image_files[file.exists(image_files)], winslash = "/", mustWork = FALSE)

    if (length(image_files) == 0) {
      stop("No chart image is available for PPTX export.", call. = FALSE)
    }

    template_path <- chartwell_title_slide_template_path()
    export_title <- trimws(slide_title %||% "Chart")
    export_date <- format(Sys.Date(), "%B %Y")
    export_author <- "Joel Findlay"
    script_file <- tempfile(fileext = ".py")
    args_file <- tempfile(fileext = ".txt")
    script_lines <- c(
      "import sys",
      "import pathlib",
      "from pptx import Presentation",
      "from pptx.util import Inches, Pt",
      "from pptx.enum.shapes import MSO_AUTO_SHAPE_TYPE",
      "from pptx.enum.text import PP_ALIGN",
      "from PIL import Image",
      "",
      "args_path = pathlib.Path(sys.argv[1])",
      "args = args_path.read_text(encoding='utf-8').splitlines()",
      "template_file, pptx_file, slide_title, author_name, export_date, *image_files = args",
      "",
      "RGBColor = __import__('pptx.dml.color', fromlist=['RGBColor']).RGBColor",
      "BG_COLOR = RGBColor(0xF5, 0xF8, 0xF4)",
      "TITLE_BG_COLOR = RGBColor(0x0D, 0x14, 0x20)",
      "ACCENT_COLOR = RGBColor(0x4F, 0x7E, 0xC9)",
      "TEXT_COLOR = RGBColor(0x0D, 0x14, 0x20)",
      "LIGHT_TEXT_COLOR = RGBColor(0xFF, 0xFF, 0xFF)",
      "COPYRIGHT_TEXT = '© CHARTWELL 2026'",
      "",
      "def set_text(shape, text, font_size=None, bold=None, color=None):",
      "    if not getattr(shape, 'has_text_frame', False):",
      "        return",
      "    shape.text_frame.clear()",
      "    paragraph = shape.text_frame.paragraphs[0]",
      "    run = paragraph.add_run()",
      "    run.text = text",
      "    if font_size is not None:",
      "        run.font.size = font_size",
      "    if bold is not None:",
      "        run.font.bold = bold",
      "    if color is not None:",
      "        run.font.color.rgb = RGBColor(*color)",
      "",
      "def add_slide_branding(slide, prs):",
      "    bg_fill = slide.background.fill",
      "    bg_fill.solid()",
      "    bg_fill.fore_color.rgb = BG_COLOR",
      "    bar = slide.shapes.add_shape(MSO_AUTO_SHAPE_TYPE.RECTANGLE, 0, 0, Inches(0.055), prs.slide_height)",
      "    bar.fill.solid()",
      "    bar.fill.fore_color.rgb = ACCENT_COLOR",
      "    bar.line.fill.background()",
      "    copyright_box = slide.shapes.add_textbox(prs.slide_width - Inches(2.8), prs.slide_height - Inches(0.42), Inches(2.55), Inches(0.2))",
      "    paragraph = copyright_box.text_frame.paragraphs[0]",
      "    paragraph.alignment = PP_ALIGN.RIGHT",
      "    run = paragraph.add_run()",
      "    run.text = COPYRIGHT_TEXT",
      "    run.font.size = Pt(10)",
      "    run.font.bold = False",
      "    run.font.color.rgb = TEXT_COLOR",
      "",
      "prs = Presentation(template_file) if template_file else Presentation()",
      "prs.slide_width = Inches(10)",
      "prs.slide_height = Inches(7.5)",
      "",
      "default_layout = prs.slide_layouts[0]",
      "",
      "if len(prs.slides) == 0:",
      "    title_slide = prs.slides.add_slide(default_layout)",
      "    title_slide.background.fill.solid()",
      "    title_slide.background.fill.fore_color.rgb = TITLE_BG_COLOR",
      "else:",
      "    title_slide = prs.slides[0]",
      "",
      "title_slide.background.fill.solid()",
      "title_slide.background.fill.fore_color.rgb = TITLE_BG_COLOR",
      "",
      "title_shape = None",
      "author_shape = None",
      "author_value_shape = None",
      "date_shape = None",
      "date_value_shape = None",
      "copyright_shape = None",
      "for shape in title_slide.shapes:",
      "    if getattr(shape, 'has_text_frame', False):",
      "        text = (shape.text or '').strip()",
      "        if text == 'Your Presentation Title':",
      "            title_shape = shape",
      "        elif text.startswith('AUTHOR'):",
      "            author_shape = shape",
      "        elif text == 'Your Name':",
      "            author_value_shape = shape",
      "        elif text.startswith('DATE'):",
      "            date_shape = shape",
      "        elif text == 'March 2026':",
      "            date_value_shape = shape",
      "        elif 'CHARTWELL 2026' in text:",
      "            copyright_shape = shape",
      "",
      "if title_shape is not None:",
      "    set_text(title_shape, slide_title, font_size=Pt(30), bold=True, color=(0xFF, 0xFF, 0xFF))",
      "if author_shape is not None:",
      "    set_text(author_shape, 'AUTHOR', font_size=Pt(13), bold=False, color=(0xFF, 0xFF, 0xFF))",
      "if author_value_shape is not None:",
      "    set_text(author_value_shape, author_name, font_size=Pt(13), bold=False, color=(0xFF, 0xFF, 0xFF))",
      "if date_shape is not None:",
      "    set_text(date_shape, 'DATE', font_size=Pt(13), bold=False, color=(0xFF, 0xFF, 0xFF))",
      "if date_value_shape is not None:",
      "    set_text(date_value_shape, export_date, font_size=Pt(13), bold=False, color=(0xFF, 0xFF, 0xFF))",
      "if copyright_shape is not None:",
      "    set_text(copyright_shape, COPYRIGHT_TEXT, font_size=Pt(10), bold=False, color=(0xFF, 0xFF, 0xFF))",
      "",
      "for image_file in image_files:",
      "    slide = prs.slides.add_slide(default_layout)",
      "    add_slide_branding(slide, prs)",
      "    with Image.open(image_file) as img:",
      "        img_width, img_height = img.size",
      "    content_left = Inches(0.28)",
      "    content_top = Inches(0.18)",
      "    content_width = prs.slide_width - Inches(0.42)",
      "    content_height = prs.slide_height - Inches(0.42)",
      "    scale = min(content_width / img_width, content_height / img_height)",
      "    pic_width = int(img_width * scale)",
      "    pic_height = int(img_height * scale)",
      "    pic_left = int(content_left + (content_width - pic_width) / 2)",
      "    pic_top = int(content_top + (content_height - pic_height) / 2)",
      "    slide.shapes.add_picture(image_file, pic_left, pic_top, width=pic_width, height=pic_height)",
      "",
      "prs.save(pptx_file)"
    )
    writeLines(script_lines, script_file)
    writeLines(
      c(
        template_path,
        pptx_file,
        export_title,
        export_author,
        export_date,
        image_files
      ),
      args_file
    )

    command_output <- tryCatch(
      system2(
        python_path,
        c(
          script_file,
          args_file
        ),
        stdout = TRUE,
        stderr = TRUE
      ),
      error = function(error) paste(error$message)
    )
    command_status <- as.integer(attr(command_output, "status") %||% 0L)

    if (!isTRUE(command_status == 0L)) {
      stop(paste(command_output, collapse = "\n"), call. = FALSE)
    }
  }

  save_static_chart_html <- function(file, plot_object, style) {
    temp_png <- tempfile(fileext = ".png")
    ggplot2::ggsave(
      temp_png,
      plot = plot_object,
      width = style$export_width,
      height = style$export_height,
      dpi = 300
    )

    image_uri <- base64enc::dataURI(file = temp_png, mime = "image/png")
    chart_title <- trimws(style$title %||% "Chart")
    chart_doc <- htmltools::tags$html(
      htmltools::tags<head(
        htmltools::tags$meta(charset = "utf-8"),
        htmltools::tags$title(chart_title),
        htmltools::tags$style(htmltools::HTML(
          "
          body { margin: 0; font-family: 'Plus Jakarta Sans', sans-serif; background: #edf2ec; }
          .export-shell { max-width: 960px; margin: 0 auto; padding: 24px; }
          .export-frame { background: #f5f8f4; border: 1px solid #d7e2d6; border-radius: 24px; padding: 24px; }
          .export-frame img { width: 100%; height: auto; display: block; }
          "
        ))
      ),
      htmltools::tags$body(
        htmltools::tags$div(
          class = "export-shell",
          htmltools::tags$div(
            class = "export-frame",
            htmltools::tags$img(src = image_uri, alt = chart_title)
          )
        )
      )
    )

    htmltools::save_html(chart_doc, file = file)
  }

  save_plotly_chart_html <- function(file, widget, style) {
    width_px <- round((style$export_width %||% 8) * 96)
    height_px <- round((style$export_height %||% 7) * 96)
    chart_title <- trimws(style$title %||% "Chart")

    export_widget_tag <- widget %>%
      plotly::layout(
        autosize = FALSE,
        width = width_px,
        height = height_px
      ) %>%
      plotly::config(
        responsive = FALSE,
        displayModeBar = FALSE
      ) %>%
      htmltools::as.tags()

    chart_doc <- htmltools::tags$html(
      htmltools::tags$head(
        htmltools::tags$meta(charset = "utf-8"),
        htmltools::tags$title(chart_title),
        htmltools::tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
        htmltools::tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = ""),
        htmltools::tags$link(
          rel = "stylesheet",
          href = "https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@400;500;600;700;800&display=swap"
        ),
        htmltools::tags$style(htmltools::HTML(
          paste0(
            "body { margin: 0; font-family: 'Plus Jakarta Sans', sans-serif; background: #edf2ec; color: #111827; }",
            ".export-shell { max-width: ", width_px + 64L, "px; margin: 0 auto; padding: 24px; }",
            ".export-frame { background: #f5f8f4; border: 1px solid #d7e2d6; border-radius: 24px; padding: 24px; }",
            ".export-chart { width: ", width_px, "px; max-width: 100%; margin: 0 auto; }",
            ".export-chart .plotly.html-widget { margin: 0 auto; }"
          )
        ))
      ),
      htmltools::tags$body(
        htmltools::tags$div(
          class = "export-shell",
          htmltools::tags$div(
            class = "export-frame",
            htmltools::tags$div(
              class = "export-chart",
              export_widget_tag
            )
          )
        )
      )
    )

    htmltools::save_html(htmltools::browsable(chart_doc), file = file)
  }

  chart_library_store <- reactiveVal(NULL)
  presentation_library_store <- reactiveVal(NULL)
  search_index_store <- reactiveVal(NULL)
  search_loading_message <- reactiveVal("")
  search_runtime_status <- reactiveVal("")
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
      load_started_at <- Sys.time()
      search_loading_message("Loading local metadata index...")
      on.exit(search_loading_message(""), add = TRUE)
      search_index_store(build_search_index(force = force))
      load_elapsed_ms <- round(as.numeric(difftime(Sys.time(), load_started_at, units = "secs")) * 1000)
      message(sprintf(
        "Local search metadata loaded: %s rows in %sms.",
        scales::comma(nrow(shiny::isolate(search_index_store()))),
        load_elapsed_ms
      ))
      search_runtime_status(sprintf(
        "Local metadata ready: %s rows loaded in %sms.",
        scales::comma(nrow(shiny::isolate(search_index_store()))),
        load_elapsed_ms
      ))
    }

    invisible(shiny::isolate(search_index_store()))
  }

  refresh_search_index_after_library_change <- function() {
    invalidate_search_index_cache()

    if (!is.null(shiny::isolate(search_index_store()))) {
      refresh_started_at <- Sys.time()
      search_index_store(build_search_index(force = FALSE))
      refresh_elapsed_ms <- round(as.numeric(difftime(Sys.time(), refresh_started_at, units = "secs")) * 1000)
      message(sprintf("Recent search overlay refreshed in %sms.", refresh_elapsed_ms))
      search_runtime_status(sprintf("Recent series refreshed in %sms.", refresh_elapsed_ms))
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
        tab_load_state$search <- "loading"
        tab_load_error$search <- ""

        tryCatch(
          {
            ensure_search_index_loaded()
            tab_load_state$search <- "ready"
          },
          error = function(error) {
            tab_load_state$search <- "error"
            tab_load_error$search <- paste("Unable to load the search metadata:", conditionMessage(error))
          }
        )
      }
    }, delay = 0.15)
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

  output$search_results_panel <- renderUI({
    local_source_selected <- search_filter_includes(input$search_source_filter, local_search_source_values())

    if (identical(tab_load_state$search, "error")) {
      return(div(class = "message-banner", tab_load_error$search %||% "Unable to load local search metadata."))
    }

    if (local_source_selected && is.null(search_index_store())) {
      return(build_panel_loading_ui(search_loading_message() %||% "Loading local metadata..."))
    }

    DT::dataTableOutput("search_results_table")
  })

  fred_search_response <- reactive({
    if (!search_filter_includes(input$search_source_filter, "FRED")) {
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
    if (!search_filter_includes(input$search_source_filter, "DBnomics")) {
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
    include_remote_status <- nzchar(query_text) || search_filter_includes(source_filter, remote_search_source_values())

    search_messages <- c(
      loading_message,
      if (is.null(search_index_store()) && search_filter_includes(source_filter, local_search_source_values())) "Local metadata will load when search runs." else "",
      search_runtime_status() %||% "",
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
    source_filter <- input$search_source_filter %||% "all"
    type_filter <- input$search_type_filter %||% "all"
    location_filter <- input$search_location_filter %||% "all"
    frequency_filter <- input$search_frequency_filter %||% "all"
    query_text <- search_query_debounced()
    local_source_selected <- search_filter_includes(source_filter, local_search_source_values())
    search_started_at <- Sys.time()

    local_results <- if (!local_source_selected || is.null(search_index_store())) {
      empty_search_index()
    } else {
      filter_search_index(
        search_index_store(),
        query = query_text,
        source_filter = source_filter,
        type_filter = type_filter,
        location_filter = location_filter,
        frequency_filter = frequency_filter,
        limit = Inf,
        token_index = current_search_token_index()
      )
    }

    remote_results <- bind_rows(
      fred_search_response()$results,
      dbnomics_search_response()$results
    ) %>%
      distinct(search_id, .keep_all = TRUE)

    remote_results <- filter_search_index(
      remote_results,
      query = "",
      source_filter = "all",
      type_filter = type_filter,
      location_filter = location_filter,
      frequency_filter = "all",
      limit = Inf
    )

    combined_results <- bind_rows(local_results, remote_results) %>%
      distinct(search_id, .keep_all = TRUE)

    search_elapsed_ms <- round(as.numeric(difftime(Sys.time(), search_started_at, units = "secs")) * 1000)

    if (local_source_selected && !is.null(search_index_store())) {
      message(sprintf(
        "Local search query \"%s\" returned %s rows in %sms.",
        query_text,
        scales::comma(nrow(local_results)),
        search_elapsed_ms
      ))
      search_runtime_status(sprintf(
        "Local search: %s result%s in %sms.",
        scales::comma(nrow(local_results)),
        if (identical(nrow(local_results), 1L)) "" else "s",
        search_elapsed_ms
      ))
    }

    combined_results
  })

  search_builder_handlers <- init_search_builder_handlers(
    input = input,
    output = output,
    session = session,
    loaded_main_tabs = loaded_main_tabs,
    search_results = search_results,
    search_query_notification_id = search_query_notification_id,
    builder_state = builder_state,
    apply_builder_state = apply_builder_state,
    run_with_status = run_with_status
  )
  selected_search_result <- search_builder_handlers$selected_search_result
  search_preview_widget <- search_builder_handlers$search_preview_widget

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

  builder_plot_height <- reactive({
    plotly_widget_height(chart_data(), builder_state()$style, base_height = 500)
  })

  output$builder_plot_container <- renderUI({
    if (identical(current_renderer(), "plotly")) {
      return(plotlyOutput("builder_plot_plotly", height = paste0(builder_plot_height(), "px")))
    }

    plotOutput("builder_plot_static", height = paste0(builder_plot_height(), "px"))
  })

  output$builder_plot_plotly <- renderPlotly({
    main_panel_widget()
  })

  output$builder_plot_static <- renderPlot({
    if (identical(input$side_panel_mode %||% "transform", "analysis")) {
      return(selected_analysis_plot())
    }

    if (nrow(chart_data()) == 0) {
      return(empty_static_plot("Configure at least one valid series to render the chart."))
    }

    current_plot()
  })

  library_renderer <- reactive({
    input$library_renderer_toggle %||% "plotly"
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
      style <- builder_state()$style

      if (identical(current_renderer(), "plotly")) {
        width_px <- round((style$export_width %||% 8) * 160)
        height_px <- round((style$export_height %||% 7) * 160)
        tryCatch(
          save_plotly_image_with_python(export_widget(), file, width_px, height_px, format = "png"),
          error = function(error) {
            ggplot2::ggsave(
              file,
              plot = current_plot(),
              width = style$export_width,
              height = style$export_height,
              dpi = 300
            )
          }
        )
      } else {
        ggplot2::ggsave(
          file,
          plot = current_plot(),
          width = style$export_width,
          height = style$export_height,
          dpi = 300
        )
      }
    }
  )

  output$exportPPTX <- downloadHandler(
    filename = function() {
      paste0(str_replace_all(builder_state()$style$title %||% "chart", "[^A-Za-z0-9]+", "_"), ".pptx")
    },
    contentType = "application/vnd.openxmlformats-officedocument.presentationml.presentation",
    content = function(file) {
      req(nrow(chart_data()) > 0)
      style <- builder_state()$style
      temp_png <- tempfile(fileext = ".png")

      ggplot2::ggsave(
        temp_png,
        plot = current_plot(),
        width = style$export_width,
        height = style$export_height,
        dpi = 300
      )

      save_chart_pptx_with_python(temp_png, file, style$title %||% "Chart")
    }
  )

  output$exportSVG <- downloadHandler(
    filename = function() {
      paste0(str_replace_all(builder_state()$style$title %||% "chart", "[^A-Za-z0-9]+", "_"), ".svg")
    },
    content = function(file) {
      req(nrow(chart_data()) > 0)
      style <- builder_state()$style

      if (identical(current_renderer(), "plotly")) {
        width_px <- round((style$export_width %||% 8) * 160)
        height_px <- round((style$export_height %||% 7) * 160)
        tryCatch(
          save_plotly_image_with_python(export_widget(), file, width_px, height_px, format = "svg"),
          error = function(error) {
            grDevices::svg(
              filename = file,
              width = style$export_width,
              height = style$export_height,
              onefile = TRUE,
              bg = "white"
            )
            on.exit(grDevices::dev.off(), add = TRUE)
            print(current_plot())
          }
        )
      } else {
        grDevices::svg(
          filename = file,
          width = style$export_width,
          height = style$export_height,
          onefile = TRUE,
          bg = "white"
        )
        on.exit(grDevices::dev.off(), add = TRUE)
        print(current_plot())
      }
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
      corr_pair <- analysis_corr_pair()
      series_x <- corr_pair$primary %||% ""
      series_y <- corr_pair$secondary %||% ""
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
      reg_pair <- analysis_reg_pair()
      dependent <- reg_pair$secondary %||% ""
      independent <- reg_pair$primary %||% ""
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
      series_name <- selected_forecast_series() %||% ""

      if (!nzchar(series_name)) {
        return(NULL)
      }

      result <- tryCatch(
        forecast_analysis(
          data,
          series_name = series_name,
          model_family = input$analysis_forecast_family %||% "AR",
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
      series_name <- selected_seasonal_series() %||% ""
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
      series_name <- selected_hp_series() %||% ""
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
      series_name <- selected_kalman_series() %||% ""
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
      build_analysis_widget(
        build_correlation_plot(
          analysis_payload,
          analysis_spec$series_x %||% "",
          analysis_spec$series_y %||% "",
          analysis_spec$window %||% 4,
          chart_state$style
        ),
        chart_state$style,
        tooltip = c("x", "y")
      )
    } else if (identical(chart_kind, "regression")) {
      build_analysis_widget(
        build_regression_plot(
          analysis_payload,
          analysis_spec$dependent %||% "",
          analysis_spec$independent %||% "",
          chart_state$style
        ),
        chart_state$style,
        tooltip = c("x", "y")
      )
    } else if (identical(chart_kind, "forecast")) {
      build_analysis_widget(
        build_forecast_plot(
          analysis_payload,
          analysis_spec$series_name %||% "Series",
          chart_state$style
        ),
        chart_state$style,
        tooltip = c("x", "y")
      )
    } else if (identical(chart_kind, "seasonal_adjustment")) {
      build_analysis_widget(
        build_seasonal_adjustment_plot(
          analysis_payload,
          analysis_spec$series_name %||% "Series",
          analysis_spec$display_mode %||% "both",
          chart_state$style
        ),
        chart_state$style,
        tooltip = c("x", "y", "colour")
      )
    } else if (identical(chart_kind, "hp_filter")) {
      build_analysis_widget(
        build_hp_filter_plot(
          analysis_payload,
          analysis_spec$series_name %||% "Series",
          analysis_spec$side %||% "two_sided",
          analysis_spec$display_mode %||% "overlay",
          chart_state$style
        ),
        chart_state$style,
        tooltip = c("x", "y", "colour")
      )
    } else if (identical(chart_kind, "kalman_filter")) {
      build_analysis_widget(
        build_kalman_filter_plot(
          analysis_payload,
          analysis_spec$series_name %||% "Series",
          analysis_spec$side %||% "two_sided",
          analysis_spec$display_mode %||% "overlay",
          chart_state$style
        ),
        chart_state$style,
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

  build_saved_chart_plot <- function(chart_record) {
    if (is.null(chart_record) || nrow(chart_record) == 0) {
      return(empty_static_plot("Select a saved chart to preview it."))
    }

    chart_kind <- chart_record$chart_kind[[1]] %||% "builder"
    chart_state <- chart_record$chart_state[[1]] %||% list(style = default_style_settings())
    chart_style <- chart_state$style %||% default_style_settings()
    analysis_spec <- chart_record$analysis_spec[[1]] %||% list()
    analysis_payload <- chart_record$analysis_payload[[1]] %||% NULL

    if (identical(chart_kind, "builder") || is.null(analysis_payload)) {
      return(build_chart_plot(chart_record$data_snapshot[[1]], chart_style))
    }

    if (identical(chart_kind, "correlation")) {
      return(build_correlation_plot(
        analysis_payload,
        analysis_spec$series_x %||% "",
        analysis_spec$series_y %||% "",
        analysis_spec$window %||% 4,
        chart_style
      ))
    }

    if (identical(chart_kind, "regression")) {
      return(build_regression_plot(
        analysis_payload,
        analysis_spec$dependent %||% "",
        analysis_spec$independent %||% "",
        chart_style
      ))
    }

    if (identical(chart_kind, "forecast")) {
      return(build_forecast_plot(
        analysis_payload,
        analysis_spec$series_name %||% "Series",
        chart_style
      ))
    }

    if (identical(chart_kind, "seasonal_adjustment")) {
      return(build_seasonal_adjustment_plot(
        analysis_payload,
        analysis_spec$series_name %||% "Series",
        analysis_spec$display_mode %||% "both",
        chart_style
      ))
    }

    if (identical(chart_kind, "hp_filter")) {
      return(build_hp_filter_plot(
        analysis_payload,
        analysis_spec$series_name %||% "Series",
        analysis_spec$side %||% "two_sided",
        analysis_spec$display_mode %||% "overlay",
        chart_style
      ))
    }

    if (identical(chart_kind, "kalman_filter")) {
      return(build_kalman_filter_plot(
        analysis_payload,
        analysis_spec$series_name %||% "Series",
        analysis_spec$side %||% "two_sided",
        analysis_spec$display_mode %||% "overlay",
        chart_style
      ))
    }

    build_chart_plot(chart_record$data_snapshot[[1]], chart_style)
  }

  saved_chart_export_data <- function(chart_record) {
    if (is.null(chart_record) || nrow(chart_record) == 0) {
      return(empty_chart_data())
    }

    chart_kind <- chart_record$chart_kind[[1]] %||% "builder"
    analysis_payload <- chart_record$analysis_payload[[1]] %||% NULL

    if (identical(chart_kind, "builder")) {
      return(chart_record$data_snapshot[[1]] %||% empty_chart_data())
    }

    if (!is.null(analysis_payload$data) && tibble::is_tibble(analysis_payload$data)) {
      return(analysis_payload$data)
    }

    if (identical(chart_kind, "forecast")) {
      history_data <- (analysis_payload$history %||% tibble::tibble()) %>%
        mutate(series = "Observed")
      holdout_data <- (analysis_payload$holdout %||% tibble::tibble()) %>%
        mutate(series = "Holdout")
      forecast_data <- (analysis_payload$forecast %||% tibble::tibble()) %>%
        mutate(series = "Forecast")

      return(bind_rows(history_data, holdout_data, forecast_data))
    }

    chart_record$data_snapshot[[1]] %||% empty_chart_data()
  }

  saved_analysis_restore_handlers <- init_saved_analysis_restore_handlers(session)
  restore_saved_analysis_state <- saved_analysis_restore_handlers$restore_saved_analysis_state

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

  observeEvent(input$clear_library_selection, {
    DT::selectRows(DT::dataTableProxy("library_table"), NULL)
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

  observeEvent(input$clear_presentation_chart_selection, {
    DT::selectRows(DT::dataTableProxy("presentation_chart_table"), NULL)
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
      downloadButton("export_selected_presentation", "Download PPTX")
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

  library_preview_handlers <- init_library_preview_handlers(
    input = input,
    output = output,
    session = session,
    ensure_chart_library_loaded = ensure_chart_library_loaded,
    selected_chart_record = selected_chart_record,
    selected_chart_records = selected_chart_records,
    selected_presentation_charts = selected_presentation_charts,
    library_renderer = library_renderer,
    build_saved_chart_widget = build_saved_chart_widget,
    build_saved_chart_plot = build_saved_chart_plot,
    run_with_status = run_with_status,
    apply_builder_state = apply_builder_state,
    restore_saved_analysis_state = restore_saved_analysis_state
  )
  selected_preview_chart_record <- library_preview_handlers$selected_preview_chart_record

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

  analysis_corr_pair <- reactive({
    resolve_distinct_choice_pair(input$analysis_corr_x %||% character(), input$analysis_corr_y %||% character(), available_series())
  })

  analysis_reg_pair <- reactive({
    resolve_distinct_choice_pair(input$analysis_reg_x %||% character(), input$analysis_reg_y %||% character(), available_series())
  })

  selected_analysis_series <- function(current_value) {
    resolve_valid_single_choice(current_value, NULL, available_series())
  }

  selected_forecast_series <- reactive({
    selected_analysis_series(input$analysis_forecast_series)
  })

  selected_seasonal_series <- reactive({
    selected_analysis_series(input$analysis_seasonal_series)
  })

  selected_hp_series <- reactive({
    selected_analysis_series(input$analysis_hp_series)
  })

  selected_kalman_series <- reactive({
    selected_analysis_series(input$analysis_kalman_series)
  })

  update_analysis_select <- function(input_id, choices, selected) {
    updateSelectInput(session, input_id, choices = choices, selected = selected %||% character())
  }

  observeEvent(available_series(), {
    series_choices <- available_series()
    corr_pair <- resolve_distinct_choice_pair(isolate(input$analysis_corr_x), isolate(input$analysis_corr_y), series_choices)
    reg_pair <- resolve_distinct_choice_pair(isolate(input$analysis_reg_x), isolate(input$analysis_reg_y), series_choices)

    update_analysis_select("analysis_corr_x", series_choices, corr_pair$primary)
    update_analysis_select("analysis_corr_y", series_choices, corr_pair$secondary)
    update_analysis_select("analysis_reg_x", series_choices, reg_pair$primary)
    update_analysis_select("analysis_reg_y", series_choices, reg_pair$secondary)
    update_analysis_select("analysis_forecast_series", series_choices, resolve_valid_single_choice(isolate(input$analysis_forecast_series), NULL, series_choices))
    update_analysis_select("analysis_seasonal_series", series_choices, resolve_valid_single_choice(isolate(input$analysis_seasonal_series), NULL, series_choices))
    update_analysis_select("analysis_hp_series", series_choices, resolve_valid_single_choice(isolate(input$analysis_hp_series), NULL, series_choices))
    update_analysis_select("analysis_kalman_series", series_choices, resolve_valid_single_choice(isolate(input$analysis_kalman_series), NULL, series_choices))
  }, ignoreInit = FALSE)

  correlation_result <- reactive({
    if (length(available_series()) < 2) {
      return(NULL)
    }
    pair <- analysis_corr_pair()
    if (length(pair$primary) == 0 || length(pair$secondary) == 0) {
      return(NULL)
    }
    if (identical(pair$primary, pair$secondary)) {
      return(NULL)
    }

    rolling_correlation_data(
      chart_data(),
      pair$primary,
      pair$secondary,
      max(2, as.integer(input$analysis_corr_window))
    )
  })

  selected_analysis_plot <- reactive({
    if (!identical(input$side_panel_mode %||% "transform", "analysis")) {
      return(empty_static_plot("Choose an analysis to display on the main panel."))
    }

    safe_analysis_plot <- function(expr, failure_message) {
      tryCatch(
        expr,
        error = function(error) {
          detail <- trimws(conditionMessage(error) %||% "")
          if (nzchar(detail)) {
            empty_static_plot(paste(failure_message, detail))
          } else {
            empty_static_plot(failure_message)
          }
        }
      )
    }

    active_tab <- input$analysis_tabs %||% "Correlations"

    if (identical(active_tab, "Correlations")) {
      if (length(available_series()) < 2) {
        return(empty_static_plot("Add at least two chart series to run correlation analysis."))
      }

      corr_pair <- analysis_corr_pair()
      result <- tryCatch(correlation_result(), error = function(error) NULL)
      if (is.null(result) || nrow(result) == 0) {
        return(empty_static_plot("Unable to compute the requested rolling correlation."))
      }

      return(safe_analysis_plot(
        build_correlation_plot(result, corr_pair$primary, corr_pair$secondary, input$analysis_corr_window, builder_state()$style),
        "Unable to render the rolling correlation chart."
      ))
    }

    if (identical(active_tab, "Regression")) {
      if (length(available_series()) < 2) {
        return(empty_static_plot("Add at least two chart series to run regression analysis."))
      }

      reg_pair <- analysis_reg_pair()
      result <- tryCatch(regression_result(), error = function(error) NULL)
      if (is.null(result)) {
        return(empty_static_plot("Unable to estimate the requested regression."))
      }

      return(safe_analysis_plot(
        build_regression_plot(result, reg_pair$secondary, reg_pair$primary, builder_state()$style),
        "Unable to render the regression chart."
      ))
    }

    if (identical(active_tab, "Seasonal Adjust")) {
      if (length(available_series()) < 1) {
        return(empty_static_plot("Add at least one chart series to run seasonal adjustment."))
      }

      series_name <- selected_seasonal_series()
      result <- tryCatch(seasonal_adjustment_result(), error = function(error) NULL)
      if (is.null(result)) {
        return(empty_static_plot("Unable to run X-13 seasonal adjustment for the selected series."))
      }

      return(safe_analysis_plot(
        build_seasonal_adjustment_plot(
          result,
          series_name,
          input$analysis_seasonal_view %||% "both",
          builder_state()$style
        ),
        "Unable to render the seasonal-adjustment chart."
      ))
    }

    if (identical(active_tab, "HP Filter")) {
      if (length(available_series()) < 1) {
        return(empty_static_plot("Add at least one chart series to run an HP filter."))
      }

      series_name <- selected_hp_series()
      result <- tryCatch(hp_filter_result(), error = function(error) NULL)
      if (is.null(result)) {
        return(empty_static_plot("Unable to estimate the requested HP filter."))
      }

      return(safe_analysis_plot(
        build_hp_filter_plot(
          result,
          series_name,
          input$analysis_hp_side %||% "two_sided",
          input$analysis_hp_view %||% "overlay",
          builder_state()$style
        ),
        "Unable to render the HP-filter chart."
      ))
    }

    if (identical(active_tab, "Kalman Filter")) {
      if (length(available_series()) < 1) {
        return(empty_static_plot("Add at least one chart series to run a Kalman filter."))
      }

      series_name <- selected_kalman_series()
      result <- tryCatch(kalman_filter_result(), error = function(error) NULL)
      if (is.null(result)) {
        return(empty_static_plot("Unable to estimate the requested Kalman filter."))
      }

      return(safe_analysis_plot(
        build_kalman_filter_plot(
          result,
          series_name,
          input$analysis_kalman_side %||% "two_sided",
          input$analysis_kalman_view %||% "overlay",
          builder_state()$style
        ),
        "Unable to render the Kalman-filter chart."
      ))
    }

    if (length(available_series()) < 1) {
      return(empty_static_plot("Add at least one chart series to run forecasting."))
    }

    series_name <- selected_forecast_series()
    result <- tryCatch(forecast_result(), error = function(error) NULL)
    if (is.null(result)) {
      return(empty_static_plot("Unable to estimate the requested forecast model."))
    }

    safe_analysis_plot(
      build_forecast_plot(result, series_name, builder_state()$style),
      "Unable to render the forecast chart."
    )
  })

  selected_analysis_widget <- reactive({
    if (!identical(input$side_panel_mode %||% "transform", "analysis")) {
      return(NULL)
    }

    safe_analysis_widget <- function(expr, failure_message) {
      tryCatch(
        expr,
        error = function(error) {
          detail <- trimws(conditionMessage(error) %||% "")
          if (nzchar(detail)) {
            empty_plotly_widget(paste(failure_message, detail))
          } else {
            empty_plotly_widget(failure_message)
          }
        }
      )
    }

    active_tab <- input$analysis_tabs %||% "Correlations"

    if (identical(active_tab, "Correlations")) {
      if (length(available_series()) < 2) {
        return(empty_plotly_widget("Add at least two chart series to run correlation analysis."))
      }

      corr_pair <- analysis_corr_pair()
      result <- tryCatch(correlation_result(), error = function(error) NULL)
      if (is.null(result) || nrow(result) == 0) {
        return(empty_plotly_widget("Unable to compute the requested rolling correlation."))
      }

      return(safe_analysis_widget(
        build_analysis_widget(
          build_correlation_plot(result, corr_pair$primary, corr_pair$secondary, input$analysis_corr_window, builder_state()$style),
          builder_state()$style,
          tooltip = c("x", "y")
        ),
        "Unable to render the rolling correlation chart."
      ))
    }

    if (identical(active_tab, "Regression")) {
      if (length(available_series()) < 2) {
        return(empty_plotly_widget("Add at least two chart series to run regression analysis."))
      }

      reg_pair <- analysis_reg_pair()
      result <- tryCatch(regression_result(), error = function(error) NULL)
      if (is.null(result)) {
        return(empty_plotly_widget("Unable to estimate the requested regression."))
      }

      return(safe_analysis_widget(
        build_analysis_widget(
          build_regression_plot(result, reg_pair$secondary, reg_pair$primary, builder_state()$style),
          builder_state()$style,
          tooltip = c("x", "y")
        ),
        "Unable to render the regression chart."
      ))
    }

    if (identical(active_tab, "Seasonal Adjust")) {
      if (length(available_series()) < 1) {
        return(empty_plotly_widget("Add at least one chart series to run seasonal adjustment."))
      }

      series_name <- selected_seasonal_series()
      result <- tryCatch(seasonal_adjustment_result(), error = function(error) NULL)
      if (is.null(result)) {
        return(empty_plotly_widget("Unable to run X-13 seasonal adjustment for the selected series."))
      }

      return(safe_analysis_widget(
        build_analysis_widget(
          build_seasonal_adjustment_plot(
            result,
            series_name,
            input$analysis_seasonal_view %||% "both",
            builder_state()$style
          ),
          builder_state()$style,
          tooltip = c("x", "y", "colour")
        ),
        "Unable to render the seasonal-adjustment chart."
      ))
    }

    if (identical(active_tab, "HP Filter")) {
      if (length(available_series()) < 1) {
        return(empty_plotly_widget("Add at least one chart series to run an HP filter."))
      }

      series_name <- selected_hp_series()
      result <- tryCatch(hp_filter_result(), error = function(error) NULL)
      if (is.null(result)) {
        return(empty_plotly_widget("Unable to estimate the requested HP filter."))
      }

      return(safe_analysis_widget(
        build_analysis_widget(
          build_hp_filter_plot(
            result,
            series_name,
            input$analysis_hp_side %||% "two_sided",
            input$analysis_hp_view %||% "overlay",
            builder_state()$style
          ),
          builder_state()$style,
          tooltip = c("x", "y", "colour")
        ),
        "Unable to render the HP-filter chart."
      ))
    }

    if (identical(active_tab, "Kalman Filter")) {
      if (length(available_series()) < 1) {
        return(empty_plotly_widget("Add at least one chart series to run a Kalman filter."))
      }

      series_name <- selected_kalman_series()
      result <- tryCatch(kalman_filter_result(), error = function(error) NULL)
      if (is.null(result)) {
        return(empty_plotly_widget("Unable to estimate the requested Kalman filter."))
      }

      return(safe_analysis_widget(
        build_analysis_widget(
          build_kalman_filter_plot(
            result,
            series_name,
            input$analysis_kalman_side %||% "two_sided",
            input$analysis_kalman_view %||% "overlay",
            builder_state()$style
          ),
          builder_state()$style,
          tooltip = c("x", "y", "colour")
        ),
        "Unable to render the Kalman-filter chart."
      ))
    }

    if (length(available_series()) < 1) {
      return(empty_plotly_widget("Add at least one chart series to run forecasting."))
    }

    series_name <- selected_forecast_series()
    result <- tryCatch(forecast_result(), error = function(error) NULL)
    if (is.null(result)) {
      return(empty_plotly_widget("Unable to estimate the requested forecast model."))
    }

    safe_analysis_widget(
      build_analysis_widget(
        build_forecast_plot(result, series_name, builder_state()$style),
        builder_state()$style,
        tooltip = c("x", "y")
      ),
      "Unable to render the forecast chart."
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
    if (length(available_series()) < 2) {
      return(NULL)
    }
    pair <- analysis_reg_pair()
    if (length(pair$primary) == 0 || length(pair$secondary) == 0) {
      return(NULL)
    }
    if (identical(pair$primary, pair$secondary)) {
      return(NULL)
    }

    regression_analysis(
      chart_data(),
      dependent = pair$secondary,
      independent = pair$primary,
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
    if (length(available_series()) < 1) {
      return(NULL)
    }
    series_name <- selected_seasonal_series()
    if (length(series_name) == 0 || !nzchar(series_name)) {
      return(NULL)
    }

    seasonal_adjustment_analysis(
      chart_data(),
      series_name = series_name,
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
    if (length(available_series()) < 1) {
      return(NULL)
    }
    series_name <- selected_hp_series()
    if (length(series_name) == 0 || !nzchar(series_name)) {
      return(NULL)
    }

    hp_filter_analysis(
      chart_data(),
      series_name = series_name,
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
    if (length(available_series()) < 1) {
      return(NULL)
    }
    series_name <- selected_kalman_series()
    if (length(series_name) == 0 || !nzchar(series_name)) {
      return(NULL)
    }

    kalman_filter_analysis(
      chart_data(),
      series_name = series_name,
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
    series_name <- trimws(selected_seasonal_series() %||% "Seasonal adjustment")
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
    apply_builder_state(
      updated_state,
      selected_series_index = target_index,
      navigate_builder = FALSE,
      restore_scope = "single_series"
    )

    showNotification(sprintf("Seasonally adjusted result added to Series %s.", target_index), type = "message")
  })

  observeEvent(input$analysis_add_hp_series, {
    result <- tryCatch(hp_filter_result(), error = function(error) NULL)
    if (is.null(result)) {
      showNotification("Run a valid HP filter before adding it to the builder.", type = "error")
      return(invisible(NULL))
    }

    target_index <- max(1L, min(MAX_SERIES, as.integer(input$analysis_hp_target_series %||% "1")))
    series_name <- trimws(selected_hp_series() %||% "HP filter")
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
    apply_builder_state(
      updated_state,
      selected_series_index = target_index,
      navigate_builder = FALSE,
      restore_scope = "single_series"
    )

    showNotification(sprintf("HP-filtered result added to Series %s.", target_index), type = "message")
  })

  observeEvent(input$analysis_add_kalman_series, {
    result <- tryCatch(kalman_filter_result(), error = function(error) NULL)
    if (is.null(result)) {
      showNotification("Run a valid Kalman filter before adding it to the builder.", type = "error")
      return(invisible(NULL))
    }

    target_index <- max(1L, min(MAX_SERIES, as.integer(input$analysis_kalman_target_series %||% "1")))
    series_name <- trimws(selected_kalman_series() %||% "Kalman filter")
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
    if (length(available_series()) < 1) {
      return(NULL)
    }
    series_name <- selected_forecast_series()
    if (length(series_name) == 0 || !nzchar(series_name)) {
      return(NULL)
    }

    forecast_analysis(
      chart_data(),
      series_name = series_name,
      model_family = input$analysis_forecast_family %||% "AR",
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

  output$export_preview_png <- downloadHandler(
    filename = function() {
      chart_record <- selected_preview_chart_record()
      req(!is.null(chart_record), nrow(chart_record) == 1)
      paste0(str_replace_all(chart_record$title[[1]] %||% "chart", "[^A-Za-z0-9]+", "_"), ".png")
    },
    content = function(file) {
      chart_record <- selected_preview_chart_record()
      req(!is.null(chart_record), nrow(chart_record) == 1)
      chart_state <- chart_record$chart_state[[1]] %||% list(style = default_style_settings())
      chart_style <- chart_state$style %||% default_style_settings()

      ggplot2::ggsave(
        filename = file,
        plot = build_saved_chart_plot(chart_record),
        width = chart_style$export_width %||% 8,
        height = chart_style$export_height %||% 7,
        dpi = 300
      )
    }
  )

  output$export_preview_pptx <- downloadHandler(
    filename = function() {
      chart_record <- selected_preview_chart_record()
      req(!is.null(chart_record), nrow(chart_record) == 1)
      paste0(str_replace_all(chart_record$title[[1]] %||% "chart", "[^A-Za-z0-9]+", "_"), ".pptx")
    },
    contentType = "application/vnd.openxmlformats-officedocument.presentationml.presentation",
    content = function(file) {
      chart_record <- selected_preview_chart_record()
      req(!is.null(chart_record), nrow(chart_record) == 1)
      chart_state <- chart_record$chart_state[[1]] %||% list(style = default_style_settings())
      chart_style <- chart_state$style %||% default_style_settings()
      temp_png <- tempfile(fileext = ".png")

      ggplot2::ggsave(
        filename = temp_png,
        plot = build_saved_chart_plot(chart_record),
        width = chart_style$export_width %||% 8,
        height = chart_style$export_height %||% 7,
        dpi = 300
      )

      save_chart_pptx_with_python(
        image_files = temp_png,
        pptx_file = file,
        slide_title = chart_record$title[[1]] %||% "Chart"
      )
    }
  )

  output$export_preview_svg <- downloadHandler(
    filename = function() {
      chart_record <- selected_preview_chart_record()
      req(!is.null(chart_record), nrow(chart_record) == 1)
      paste0(str_replace_all(chart_record$title[[1]] %||% "chart", "[^A-Za-z0-9]+", "_"), ".svg")
    },
    content = function(file) {
      chart_record <- selected_preview_chart_record()
      req(!is.null(chart_record), nrow(chart_record) == 1)
      chart_state <- chart_record$chart_state[[1]] %||% list(style = default_style_settings())
      chart_style <- chart_state$style %||% default_style_settings()

      grDevices::svg(
        filename = file,
        width = chart_style$export_width %||% 8,
        height = chart_style$export_height %||% 7,
        onefile = TRUE,
        bg = "white"
      )
      on.exit(grDevices::dev.off(), add = TRUE)
      print(build_saved_chart_plot(chart_record))
    }
  )

  output$export_preview_data <- downloadHandler(
    filename = function() {
      chart_record <- selected_preview_chart_record()
      req(!is.null(chart_record), nrow(chart_record) == 1)
      paste0(str_replace_all(chart_record$title[[1]] %||% "chart_data", "[^A-Za-z0-9]+", "_"), ".csv")
    },
    content = function(file) {
      chart_record <- selected_preview_chart_record()
      req(!is.null(chart_record), nrow(chart_record) == 1)
      export_data <- saved_chart_export_data(chart_record)
      readr::write_csv(export_data, file, na = "")
    }
  )

  output$export_chart_presentation <- downloadHandler(
    filename = function() {
      paste0("chart_presentation_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pptx")
    },
    contentType = "application/vnd.openxmlformats-officedocument.presentationml.presentation",
    content = function(file) {
      selected_records <- selected_chart_records()
      req(!is.null(selected_records), nrow(selected_records) > 0)
      temp_dir <- tempfile("chart-presentation-")
      dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
      on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)

      image_files <- lapply(seq_len(nrow(selected_records)), function(row_index) {
        chart_record <- selected_records[row_index, , drop = FALSE]
        chart_state <- chart_record$chart_state[[1]] %||% list(style = default_style_settings())
        chart_style <- chart_state$style %||% default_style_settings()
        image_path <- file.path(temp_dir, sprintf("slide-%02d.png", row_index))

        ggplot2::ggsave(
          filename = image_path,
          plot = build_saved_chart_plot(chart_record),
          width = chart_style$export_width %||% 8,
          height = chart_style$export_height %||% 7,
          dpi = 300
        )

        image_path
      }) %>%
        unlist(use.names = FALSE)

      save_chart_pptx_with_python(
        image_files = image_files,
        pptx_file = file,
        slide_title = "Saved Charts Presentation"
      )
    }
  )

  output$export_selected_presentation <- downloadHandler(
    filename = function() {
      presentation_record <- selected_presentation_record()
      req(!is.null(presentation_record))
      paste0(str_replace_all(presentation_record$title[[1]], "[^A-Za-z0-9]+", "_"), ".pptx")
    },
    contentType = "application/vnd.openxmlformats-officedocument.presentationml.presentation",
    content = function(file) {
      presentation_record <- selected_presentation_record()
      presentation_records <- selected_presentation_charts()
      req(!is.null(presentation_record), nrow(presentation_records) > 0)
      temp_dir <- tempfile("presentation-export-")
      dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
      on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)

      image_files <- lapply(seq_len(nrow(presentation_records)), function(row_index) {
        chart_record <- presentation_records[row_index, , drop = FALSE]
        chart_state <- chart_record$chart_state[[1]] %||% list(style = default_style_settings())
        chart_style <- chart_state$style %||% default_style_settings()
        image_path <- file.path(temp_dir, sprintf("presentation-slide-%02d.png", row_index))

        ggplot2::ggsave(
          filename = image_path,
          plot = build_saved_chart_plot(chart_record),
          width = chart_style$export_width %||% 8,
          height = chart_style$export_height %||% 7,
          dpi = 300
        )

        image_path
      }) %>%
        unlist(use.names = FALSE)

      save_chart_pptx_with_python(
        image_files = image_files,
        pptx_file = file,
        slide_title = presentation_record$title[[1]] %||% "Presentation"
      )
    }
  )
}

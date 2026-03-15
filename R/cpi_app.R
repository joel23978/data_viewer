build_cpi_ui <- function() {
  year_min <- as.numeric(lubridate::year(min(cpi_data_all$date)))
  year_max <- as.numeric(lubridate::year(max(cpi_data_all$date)))

  fluidPage(
    titlePanel("CPI Explorer"),
    sidebarLayout(
      sidebarPanel(
        h3("Variables"),
        sliderInput(
          "year",
          "Date range",
          min = year_min,
          max = year_max,
          value = c(max(year_min, year_max - 10), year_max),
          sep = ""
        ),
        selectInput(
          "dataset",
          "Data",
          choices = c("cpi" = 0, "cpi_monthly" = 1),
          selected = 0
        ),
        conditionalPanel(
          condition = "input.dataset == 0",
          selectInput(
            "splits",
            "Splits",
            choices = c("class_0", "class_1", "class_2", "class_3"),
            selected = "class_0"
          ),
          selectInput("sub_split", "Category", names(name_list), selected = names(name_list)[1]),
          selectInput("sub_sub_split", "Sub-category", name_list[[1]], selected = name_list[[1]][1]),
          selectInput("splits_region", "Region", choices = region_list, selected = region_list[[1]])
        ),
        selectInput(
          "trnsfrm",
          "Transformation",
          choices = c("index", "y.y", "q.q"),
          selected = "y.y"
        ),
        selectInput(
          "viewData",
          "Display Tables",
          choices = c("No" = 0, "Yes" = 1),
          selected = 0
        ),
        downloadButton("downloadData", "Download")
      ),
      mainPanel(
        h2("CPI"),
        plotlyOutput("p_plot"),
        tableOutput("p_table"),
        h2("Detail"),
        plotlyOutput("p_sub_plot"),
        tableOutput("p_table_sub"),
        h2("More Detail"),
        plotlyOutput("p_sub_sub_plot"),
        tableOutput("p_table_sub_sub")
      )
    )
  )
}

build_cpi_server <- function(input, output, session) {
  observe({
    updateSelectInput(session, "sub_sub_split", choices = name_list[[input$sub_split]])
  })

  p_data <- reactive({
    cpi_splits(
      cpi_data = cpi_data_all,
      pick_split = input$splits,
      region_split = input$splits_region,
      transformation = input$trnsfrm,
      dates = as.numeric(input$year)
    )
  })

  p_data_sub <- reactive({
    cpi_sub_splits(
      cpi_data = cpi_data_all,
      pick_sub_split = input$sub_split,
      region_split = input$splits_region,
      transformation = input$trnsfrm,
      dates = as.numeric(input$year)
    )
  })

  p_data_sub_sub <- reactive({
    cpi_sub_sub_splits(
      cpi_data = cpi_data_all,
      pick_sub_sub_split = input$sub_sub_split,
      region_split = input$splits_region,
      transformation = input$trnsfrm,
      dates = as.numeric(input$year)
    )
  })

  output$p_plot <- renderPlotly({
    ggplotly(
      ggplot(p_data(), aes(x = date, y = value, colour = name)) +
        geom_line() +
        theme_minimal()
    )
  })

  output$p_sub_plot <- renderPlotly({
    ggplotly(
      ggplot(p_data_sub(), aes(x = date, y = value, colour = name)) +
        geom_line() +
        theme_minimal()
    )
  })

  output$p_sub_sub_plot <- renderPlotly({
    ggplotly(
      ggplot(p_data_sub_sub(), aes(x = date, y = value, colour = name)) +
        geom_line() +
        theme_minimal()
    )
  })

  output$p_table <- renderTable({
    req(input$viewData == 1)
    p_data() %>%
      select(name, value, date) %>%
      arrange(date) %>%
      pivot_wider(names_from = date, values_from = value)
  })

  output$p_table_sub <- renderTable({
    req(input$viewData == 1)
    p_data_sub() %>%
      select(name, value, date) %>%
      arrange(date) %>%
      pivot_wider(names_from = date, values_from = value)
  })

  output$p_table_sub_sub <- renderTable({
    req(input$viewData == 1)
    p_data_sub_sub() %>%
      select(name, value, date) %>%
      arrange(date) %>%
      pivot_wider(names_from = date, values_from = value)
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste(
        "cpi",
        input$splits_region,
        input$trnsfrm,
        min(input$year),
        max(input$year),
        ".csv",
        sep = "-"
      )
    },
    content = function(file) {
      write.csv(
        p_data() %>%
          pivot_wider(id_cols = c(name, region, series), names_from = date, values_from = value) %>%
          arrange(name) %>%
          mutate(group = 1) %>%
          bind_rows(
            p_data_sub() %>%
              pivot_wider(id_cols = c(name, region, series), names_from = date, values_from = value) %>%
              arrange(name) %>%
              mutate(group = 2)
          ) %>%
          bind_rows(
            p_data_sub_sub() %>%
              pivot_wider(id_cols = c(name, region, series), names_from = date, values_from = value) %>%
              arrange(name) %>%
              mutate(group = 3)
          ),
        file,
        row.names = FALSE
      )
    }
  )
}

# app.R

# SETUP ----
'%!in%' <- function(x,y)!('%in%'(x,y))



library(shiny)
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(stringr)
library(scales)
library(htmlwidgets)
library(shinyWidgets)
library(DT)
library(shinyToastify)
library(spsComps)


source(here::here("cpi_annual.R"))
source(here::here("external_data.R"))

source(here::here("plot_functions.R"))
source(here::here("modular_functions.R"))

set_default_values()


js <- '
async function getImageBlobFromUrl(url) {
  const fetchedImageData = await fetch(url);
  const blob = await fetchedImageData.blob();
  return blob;
}
$(document).ready(function () {
  $("#copybtn").on("click", async () => {
    const src = $("#p_cust_static>img").attr("src");
    try {
      const blob = await getImageBlobFromUrl(src);
      await navigator.clipboard.write([
        new ClipboardItem({
          [blob.type]: blob
        })
      ]);
      Shiny.setInputValue("success", true, {priority: "event"});
    } catch (err) {
      console.error(err.name, err.message);
      Shiny.setInputValue("failure", true, {priority: "event"});
    }
  });
});
'


# UI ----
ui <- navbarPage(
  
  
  "Data Explorer",
  
  # Panel 1 ----
  tabPanel(
    "External",
    
    ## inputs ----
    fluidPage(
      tags$head(
        tags$style(HTML("hr {border-top: 1px solid #000000;}")),
        tags$script(HTML(js))
      ),
      
      h5("Note: Select a data source and a data series to display a chart"),
      
      sidebarLayout(
        sidebarPanel(
          h4("Data Inputs")
          , sliderInput("year1"
                        , "Date range"
                        , min = as.numeric(year(min(cpi_data_all$date)))
                        , max = as.numeric(year(max(cpi_data_all$date)))+1
                        , value = c(2014, as.numeric(year(max(cpi_data_all$date)))+1),
                        sep = "")
          , selectInput("viewData1"
                        , label = "Display Table"
                        , choices = list("No"=0
                                         , "Yes"=1
                        )
          )
          
          ## Series (mod) ----
          , hr()
          , h4("Series 1")
          , seriesUI(1)

          , hr()
          , h4("Series 2")
          , seriesUI(2)

          , hr()
          , h4("Series 3")
          , seriesUI(3)

          , hr()
          , h4("Series 4")
          , seriesUI(4)
 
          
        )
        ## outputs ----
        , mainPanel(
          uiOutput("tab")
          # , conditionalPanel(
          #   condition = "input.source_1 == `FRED` || input.source_2 == `FRED` || input.source_3 == `FRED` || input.source_4 == `FRED`"
          , actionButton("reset", "Enter FRED API Key (required to query data from FRED)", style="simple", size="sm", color = "warning")
          , verbatimTextOutput(outputId = "text")
          #)


          ## Chart
          , h4("Interactive Chart:")
          , plotlyOutput("p_cust")
          , hr()
          
          
          ## Data
          , h4("Data:")
          , DT::dataTableOutput("p_table_cust") 
          , hr()
          
          
          ## Inputs
          , fluidRow(
            column(5,
                   h4("More Inputs:")
                   , numericInput("moving_avg", "Moving Average (no. obs):", value = 1)
                   , numericInput("lagged_change_val", "Lagged Change - Value (no. obs):", value = 0)
                   , numericInput("lagged_change_pct", "Lagged Change - % (no. obs):", value = 0)
                   , numericInput("lagged_change_ann", "Lagged Change - %ann (no. obs):", value = 0)
                   # , selectizeInput("data_freq", "x Data Frequency:", choices = c("Day", "Week", "Month", "Quarter", "Annual"))
            ),
            column(4
                   , selectInput("recession_shading", "Recession Shading:"
                                 , choices = append(rec_regions, "none"), selected = "none")
                   , numericRangeInput("horizontal_shading", "Horizontal Shading", value = c(NA,NA))
                   # Input for the mathematical expression
                   , textInput("expression_num", "Enter an expression (use 'data' as variable):", value = "data * 2")
                   , actionButton("calculate_num", "Calculate")
            ),
            column(3
                   , numericInput("horizontal_1", "Horizontal Line (1)", value = NULL)
                   , numericInput("horizontal_2", "Horizontal Line (2)", value = NULL)
                   , dateInput("vertical_1", "Vertical Line (1)", value = as.Date(NA))
                   , dateInput("vertical_2", "Vertical Line (2)", value = as.Date(NA))
                   
            )
          ),
          hr()
          
          ## Static Chart
          , h4("Static Chart:")
          , plotOutput("p_cust_static")
          , h6("Note: Copy to clipboard only works in Chrome.")
          , fluidRow(
           column(width = 11, 
              useShinyToastify(),
              div(style="display:inline-block",actionButton("copybtn", "Copy", icon = icon("copy"), class = "btn-primary")),
              div(style="display:inline-block",downloadButton("exportPNG", "Static Chart (.png)")),
              div(style="display:inline-block",downloadButton("exportHTML", "Interactive Chart (.html)")),
              div(style="display:inline-block",downloadButton("exportData", "Data (.csv)")),
            ))

          
          
          , hr()
          , fluidRow(
            column(width = 3,
                   h4("Chart Inputs")
                   # , selectizeInput("cht_type", "x Chart Type:", choices =  "simple") #list()
                   , selectizeInput("cht_legend", "Legend:", choices = c("none", "bottom"), selected = "bottom")
                   , selectizeInput("cht_colour_palette", "Colour palette:", choices = palette.pals()) #list()
                   
                   , textInput("cht_title",  "Chart title:", value = "please_add_title")
                   , textInput("cht_y_axes_unit",  "y-axes (unit-label):", value = "%")
                   , textInput("cht_note",  "Note/Source:", value = "c. Joel Findlay")
                   
            ),
            column(width = 3, offset = 1
                   , h4("Manual Override")
                   , numericInput("cht_title_y_placement", "Title placement:", value = 17)
                   , numericInput("cht_y_min", "y-axes (min):", value = 17)
                   , numericInput("cht_y_max", "y-axes (max):", value = 5)
                   , numericInput("cht_y_increment", "y-axes (increment):", value = 2)
                   , selectInput("cht_y_invert", "y-axes (invert):", choices =c(F, T), selected = F) 
                   
                   , selectizeInput("cht_x_date_format", "x-axis (date format)", choices = c("%d-%b", "%b-%y", "%b-%Y", "%m-%Y", "%d-%m-%Y", "%Y")
                                    , options = list(create = TRUE), selected = "%b-%y") #list()
                   , numericInput("cht_x_num_labels", "x-axis (no. labels):", value = 6)
            ),
            column(width = 3, offset = 1
                   , h4("Font Size")
                   , numericInput("cht_title_size", "Title:", value = 8)
                   , numericInput("cht_axes_font_size", "Axes:", value = 15)
                   , numericInput("cht_label_size", "Series labels:", value = 7)
                   , h4(".png Export")
                   , numericInput("cht_height", "Height:", value = 5) #list()
                   , numericInput("cht_width", "Width:", value = 7) #list()
                   
            )
          )
          , hr()
          
          
      
          
        )
      )
    )
  )
  
  
  
  # Panel 2 ----
  , tabPanel(
    "CPI",
    
    ## inputs ----
    fluidPage(
      sidebarPanel(
        h3("Variables")
        , sliderInput("year"
                      , "Date range"
                      , min = as.numeric(year(min(cpi_data_all$date)))
                      , max = as.numeric(year(max(cpi_data_all$date)))
                      , value = c(2014, as.numeric(year(max(cpi_data_all$date)))),
                    sep = "")
        , selectInput("dataset"
                      , label = "Data"
                      , choices = list("cpi" = 0
                                       , "cpi_monthly" = 1)
                      , selected = 0)
        
        , conditionalPanel(
          condition = "input.dataset == 0"
          , selectInput("splits"
                        , label = "Splits"
                        , choices = list("class_0"
                                         , "class_1"
                                         , "class_2"
                                         , "class_3")
                        , selected = "class_0"
          )
          
          , selectInput("sub_split"
                        , label = "Category"
                        , names(name_list)
                        )
          , selectInput("sub_sub_split"
                        , label = "Sub-category"
                        , name_list[[1]]
          )
          , selectInput("splits_region"
                        , label = "Region"
                        , choices = region_list
                        , selected = region_list[[1]]
          )
        )
        , selectInput("trnsfrm"
                      , label = "Transformation"
                      , choices = list("index" 
                                       , "y.y" 
                                       , "q.q" 
                                       #, "Contribution (Proportion)" = 3
                      )
                      , selected = "y.y")
        , selectInput("viewData"
                      , label = "Display Tables"
                      , choices = list("No"=0
                                       , "Yes"=1
                      )
        )
        # Button
        , downloadButton("downloadData", "Download")
      )
      
      
      
      ## outputs ----
      , mainPanel(
        
        ## Chart
        h2("CPI")
        , plotlyOutput("p_plot")
        , helpText("Data:")
        
        , tableOutput("p_table")
        
        ## Sub chart
        , h2("Detail")
        , plotlyOutput("p_sub_plot")
        , helpText("Data:")
        , tableOutput("p_table_sub")
        
        ## sub sub charts
        , h2("More Detail")
        , plotlyOutput("p_sub_sub_plot")
        , helpText("Data:")
        , tableOutput("p_table_sub_sub")        
        

      )
      
      
    )
  )
  
 
  , collapsible = TRUE
)









# SERVER ----
server <- function(input, output, session) {


  
  ### weblinks----
  
  fred_link <- a("FRED", href="https://fred.stlouisfed.org/tags/series")
  dbnomics_link <- a("dbnomics", href="https://db.nomics.world")
  rba_link <- a("RBA Statistical Tables", href="https://www.rba.gov.au/statistics/tables/")
  abs_link <- a("ABS", href="https://www.abs.gov.au")
  fred_api_link <- a("FRED API Keys", href="https://fred.stlouisfed.org/docs/api/api_key.html")
  
  output$tab <- renderUI({
    tagList("Data Sources:", fred_link,  dbnomics_link, rba_link, abs_link, fred_api_link)
  })
  
  
  
  ### copy/paste notification ----
  
  observeEvent(input[["success"]], {
    showToast(
      session,
      input,
      text = tags$span(
        style = "color: white; font-size: 20px;", "Image copied!"
      ),
      type = "success",
      position = "top-center",
      autoClose = 3000,
      pauseOnFocusLoss = FALSE,
      draggable = FALSE,
      style = list(
        border = "4px solid crimson",
        boxShadow = "rgba(0, 0, 0, 0.56) 0px 22px 30px 4px"
      )
    )
  })
  
  observeEvent(input[["failure"]], {
    showToast(
      session,
      input,
      text = tags$span(
        style = "color: white; font-size: 20px;", "Failed to copy image!"
      ),
      type = "error",
      position = "top-center",
      autoClose = 3000,
      pauseOnFocusLoss = FALSE,
      draggable = FALSE,
      style = list(
        border = "4px solid crimson",
        boxShadow = "rgba(0, 0, 0, 0.56) 0px 22px 30px 4px"
      )
    )
  })
  
  
  
  
  ### FRED API key ----
  l <- reactiveValues()
  observeEvent(input$reset, {
    # display a modal dialog with a header, textinput and action buttons
    showModal(modalDialog(
      tags$h2('Please enter your FRED API key'),
      textInput('fred_key', 'Key:'),
      footer=tagList(
        actionButton('submit', 'Submit'),
        modalButton('cancel')
      )
    ))
  })
  
  # only store the information if the user clicks submit
  observeEvent(input$submit, {
    removeModal()
    l$fred_key <- input$fred_key
    fredr_set_key(l$fred_key)
  })
  
  # display whatever is listed in l
  output$text <- renderPrint({
    if (is.null(l$fred_key)) return(NULL)
    paste('FRED API Key:', l$fred_key)
  })

  
  
  
  
  ### update inputs ----
  observe({
    ## chart formatting
    if(input$cht_y_invert == F){
      y_vals <- pretty(c(min(data_for_plotting()$value), max(data_for_plotting()$value)+(max(data_for_plotting()$value) - min(data_for_plotting()$value))/10)
                       , n = n_ticks) 
      title_placement <- max(y_vals) -1/20*(max(y_vals)-min(y_vals))
    } else {
      y_vals <- pretty(c(min(data_for_plotting()$value) - (max(data_for_plotting()$value) - min(data_for_plotting()$value))/10, max(data_for_plotting()$value))
                       , n = n_ticks) 
      title_placement <- min(y_vals) +1/20*(max(y_vals)-min(y_vals))
    }
    updateNumericInput(session, "cht_y_min", value = min(y_vals))
    updateNumericInput(session, "cht_y_max", value = max(y_vals))
    updateNumericInput(session, "cht_y_increment", value = y_vals[2] - y_vals[1])
    updateNumericInput(session, "cht_title_y_placement", value = title_placement)
    
    # data choices 
    updateSelectInput(session, "sub_sub_split", choices = name_list[[input$sub_split]])
    
  })
  
  session_store <- reactiveValues()
  
  
  
  ### query series ----
  
  callModule(rba_update, 1)
  callModule(abs_update, 1)
  callModule(bloomy_update, 1)
  data_queried1 <- reactive({callModule(data_query, "1", input_dates = input$year1)})
  data_transformed1 <- reactive({callModule(data_transform, "1", data_source = data_queried1)})
  
  callModule(rba_update, 2)
  callModule(abs_update, 2)
  callModule(bloomy_update, 2)
  data_queried2 <- reactive({callModule(data_query, "2", input_dates = input$year1)})
  data_transformed2 <- reactive({callModule(data_transform, "2", data_source = data_queried2)})
  
  
  callModule(rba_update, 3)
  callModule(abs_update, 3)
  callModule(bloomy_update, 3)
  data_queried3 <- reactive({callModule(data_query, "3", input_dates = input$year1)})
  data_transformed3 <- reactive({callModule(data_transform, "3", data_source = data_queried3)})
  
  
  callModule(rba_update, 4)
  callModule(abs_update, 4)
  callModule(bloomy_update, 4)
  data_queried4 <- reactive({callModule(data_query, "4", input_dates = input$year1)})
  data_transformed4 <- reactive({callModule(data_transform, "4", data_source = data_queried4)})
  
  
  
  
  ### combine series ----
  data_combined <- reactive({
    tmp <- data_transformed1() %>%
      rbind(data_transformed2()) %>%
      rbind(data_transformed3()) %>%
      rbind(data_transformed4()) %>%
      filter(date >=  lubridate::ymd(min(input$year1), truncated = 2L)
             , date <= lubridate::ymd(max(input$year1), truncated = 2L)
      )
    
    if(is.integer(input$moving_avg) == T){
      tmp <- calc_moving_avg(input_data = tmp
                             , moving_avg = input$moving_avg) 
    } 
    
    if(is.integer(input$lagged_change_val) == T & input$lagged_change_val != 0){
      tmp <- calc_lagged_change_val(input_data = tmp
                                    , input_lag = input$lagged_change_val) 
    } 
    
    if(is.integer(input$lagged_change_pct) == T & input$lagged_change_pct != 0){
      tmp <- calc_lagged_change_pct(input_data = tmp
                                    , input_lag = input$lagged_change_pct) 
    } 
    
    if(is.integer(input$lagged_change_ann) == T & input$lagged_change_ann != 0){
      tmp <- calc_lagged_change_ann(input_data = tmp
                                    , input_lag = input$lagged_change_ann) 
    }
    
    return(tmp)
    
  })
  
  
  

  data_for_plotting <- reactiveVal()
  observe({
    data_for_plotting(data_combined())
  })
  observeEvent(input$calculate_num, {
    expression_text <- sub("data", "value", input$expression_num)
    tmp <- data_for_plotting() %>%  # use the latest stored data
      mutate(value = eval(parse(text = expression_text)))
    data_for_plotting(tmp)
  })
  
  
  

  # Plot (interactive) ---------------
  output$p_cust <- renderPlotly({
    session_store$plt <- ggplotly(
      data_for_plotting() %>%
      ggplot() +
      geom_line(aes(x=date, y=value, colour = name)) +
      theme_minimal() +
      xlab("Date") +
      ylab("Percent")
    )
    
    session_store$plt
  })
  
  # Plot (static) ---------------
  p_plot_settings <- reactive({
    chart_formatting(data_for_plotting())
    
    set_chart_defaults(input_data = data_for_plotting()
                       , cht_y_min = input$cht_y_min
                       , cht_y_max = input$cht_y_max
                       , cht_y_increment = input$cht_y_increment
                       , cht_y_invert = input$cht_y_invert
                       , cht_y_axes_unit = input$cht_y_axes_unit
                       #, cht_y_axes_unit_size = input$cht_y_axes_unit_size
                       , cht_start_date# = input$cht_start_date
                       , cht_end_date# = input$cht_end_date
                       , cht_x_date_format = input$cht_x_date_format
                       , cht_x_num_labels = pretty_breaks(input$cht_x_num_labels)
                       , cht_title = input$cht_title
                       , cht_title_size = input$cht_title_size
                       , cht_title_x_placement #= input$cht_title_x_placement
                       , cht_title_y_placement = input$cht_title_y_placement
                       , cht_width = input$cht_width
                       , cht_height = input$cht_height
                       , cht_axes_font_size = input$cht_axes_font_size
                       , cht_label_size = input$cht_label_size
                       , cht_legend = input$cht_legend
                       , cht_colour_palette = palette.colors(palette = input$cht_colour_palette)
                       , cht_note = input$cht_note
                       , cht_type# = input$cht_type
                       , horizontal_1 = input$horizontal_1
                       , horizontal_2 = input$horizontal_2
                       , horizontal_shading = as.numeric(input$horizontal_shading)
                       , vertical_1 = as.Date(input$vertical_1)
                       , vertical_2 = as.Date(input$vertical_2)
                       , recession_shading = input$recession_shading
                       )

    return(chart_defaults)
  })

  p_cust_static <- reactive({
    data_for_plotting() %>%
      ggplot(aes(x=date, y=value, colour = name, fill = name)) +
      p_plot_settings() 
  })
  
  
  output$p_cust_static <- renderPlot({
    req(p_cust_static())
    p_cust_static()
  })
  
  # png download ----
  output$exportPNG <-  downloadHandler(
    filename = function(){paste0(input$cht_title, ".png")}, 
    content = function(file){
      req(p_cust_static())
      ggsave(file
             ,plot=p_cust_static()
             , width = input$cht_width
             , height = input$cht_height)}
  )
  
  
  # html download ----
  output$exportHTML <- downloadHandler(
    filename = function(){paste0(input$cht_title, ".html")}, 
    content = function(file) {
      # export plotly html widget as a temp file to download.
      saveWidget(as_widget(session_store$plt), file, selfcontained = TRUE)
    }
  )


  
  # Custom table ----
  output$p_table_cust <- DT::renderDataTable(
    
    if(input$viewData1 == 1){
      tmp <- data_for_plotting()  %>%
        select(c("name", "value", "date")) %>%
        arrange(date) %>%
        pivot_wider(names_from = name, values_from = value)
      
      DT::datatable(tmp, options = list(pageLength = 10))
      
    } else{
      cat("Change selection to display table.")
    }

  )
  
  # csv download ---------------
  output$exportData <- downloadHandler(
    filename = function(){paste0(input$cht_title, ".csv")}, 
    content = function(file) {
      write.csv(data_for_plotting()  %>%
                  select(c("name", "value", "date")) %>%
                  arrange(date) %>%
                  pivot_wider(names_from = name, values_from = value)
                , file
                , row.names = FALSE)
    }
  )
  
  
  
  
  
  



  
  #CPI TAB  ----
  
  
  p_data <- reactive({
    cpi_splits(cpi_data = cpi_data_all
               , pick_split = input$splits
               , region_split = input$splits_region
               , transformation = input$trnsfrm
               , dates = as.numeric(input$year)
    ) 
  })
  
  p_data_sub <- reactive({
    cpi_sub_splits(cpi_data = cpi_data_all
                   , pick_sub_split = input$sub_split
                   , region_split = input$splits_region
                   , transformation = input$trnsfrm
                   , dates = as.numeric(input$year)
    )
  })
  
  
  p_data_sub_sub <- reactive({
    cpi_sub_sub_splits(cpi_data = cpi_data_all
                       , pick_sub_sub_split = input$sub_sub_split
                       , region_split = input$splits_region
                       , transformation = input$trnsfrm
                       , dates = as.numeric(input$year)
    )
  })
  
  
  
  # Plot (main) ---------------
  output$p_plot <- renderPlotly({ggplotly(
    p_data() %>%
      ggplot() +
      geom_line(aes(x=date, y=value, colour = name)) +
      theme_minimal() +
      xlab("Date") +
      ylab("Percent")
  )})
  
  
  # Plot (sub) ---------------
  output$p_sub_plot <- renderPlotly({ggplotly(
    p_data_sub() %>%
      ggplot() +
      geom_line(aes(x=date, y=value, colour = name)) +
      theme_minimal() +
      xlab("Date") +
      ylab("Percent")
  )})
  
  
  # Plot (sub-sub) ---------------
  output$p_sub_sub_plot <- renderPlotly({ggplotly(
    p_data_sub_sub() %>%
      ggplot() +
      geom_line(aes(x=date, y=value, colour = name)) +
      theme_minimal() +
      xlab("Date") +
      ylab("Percent")
  )})
  
  
  # Table ---------------
  output$p_table <- renderTable({
    if(input$viewData == 1){
      p_data()  %>%
        select(c("name", "value", "date")) %>%
        arrange(date) %>%
        pivot_wider(names_from = date, values_from = value)
    } else{
      cat("Change selection to display table.")
    }
    
    
    
  })
  
  output$p_table_sub <- renderTable({
    if(input$viewData == 1){
      p_data_sub()  %>%
        select(c("name", "value", "date")) %>%
        arrange(date) %>%
        pivot_wider(names_from = date, values_from = value)
    } else{
      cat("Change selection to display table.")
    }
    
  })
  
  
  output$p_table_sub_sub <- renderTable({
    if(input$viewData == 1){
      p_data_sub_sub()  %>%
        select(c("name", "value", "date")) %>%
        arrange(date) %>%
        pivot_wider(names_from = date, values_from = value)
    } else{
      cat("Change selection to display table.")
    }
    
    
  })
  
 
  
  # csv download ---------------
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("cpi"
            , input$splits_region
            , input$trnsfrm
            , min(input$year)
            , max(input$year)
            , ".csv", sep = "-")
    },
    content = function(file) {
      write.csv(p_data() %>%
                  pivot_wider(id_cols = c(name, region, series), names_from = date, values_from = value) %>%
                  arrange(name) %>%
                  mutate(group = 1) %>%
                  rbind(
                    p_data_sub() %>%
                      pivot_wider(id_cols = c(name, region, series), names_from = date, values_from = value) %>%
                      arrange(name) %>%
                      mutate(group = 2)
                  ) %>%
                  rbind(
                    p_data_sub_sub() %>%
                      pivot_wider(id_cols = c(name, region, series), names_from = date, values_from = value) %>%
                      arrange(name) %>%
                      mutate(group = 3)
                  ) 
                , file
                , row.names = FALSE)
    }
  )
  
}



# CREATE APP ----
shinyApp(ui, server)






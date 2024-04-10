# app.R

# SETUP ----

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

source(here::here("cpi_annual.R"))
source(here::here("plot_functions.R"))
source(here::here("external_data.R"))

set_default_values()



# UI ----
ui <- navbarPage(
  
  "Data Explorer",
  
  # Panel 1 ----
  tabPanel(
    "CPI Viewer",
    
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
        
        # , selectInput("trnsfrm"
        #               , label = "Transformation"
        #               , choices = list("Index" = 0
        #                                , "Percent Change" = 1
        #                                , "Contribution" = 2
        #                                #, "Contribution (Proportion)" = 3
        #                                )
        #               , selected = 0)
        
        # , conditionalPanel(
        #   condition = "input.trnsfrm != 0"
        #   , selectInput("period"
        #                 , label = "Period"
        #                 , choices = list("12-month" = 12
        #                                  , "9-month" = 9
        #                                  , "6-month" = 6
        #                                  , "3-month" = 3
        #                                  , "1-month" = 1)
        #                 , selected = 12
        #   )
        # )
        # 
        # , conditionalPanel(
        #   condition = "input.period != 0"
        #   , selectInput("annualise"
        #                 , label = "Annualised Change"
        #                 , choices = list("no" = F
        #                                  , "yes" = T
        #                 )
        #                 , selected = 0
        #   )
        # )
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
  
  # Panel 2 ----
  , tabPanel(
    "CPI Explorer",
    
    ## inputs ----
    fluidPage(
      tags$head(
        tags$style(HTML("hr {border-top: 1px solid #000000;}"))
      ),
      
      sidebarLayout(
        sidebarPanel(
          h4("Data Inputs")
          , sliderInput("year1"
                        , "Date range"
                        , min = as.numeric(year(min(cpi_data_all$date)))
                        , max = as.numeric(year(max(cpi_data_all$date)))
                        , value = c(2014, as.numeric(year(max(cpi_data_all$date)))),
                        sep = "")
          , selectInput("viewData1"
                        , label = "Display Table"
                        , choices = list("No"=0
                                         , "Yes"=1
                        )
          )
          
          , selectInput("trnsfrm1"
                        , label = "Transformation"
                        , choices = list("index" 
                                         , "y.y" 
                                         , "q.q" 
                                         , "rebased index"
                                         #, "Contribution (Proportion)" = 3 
                        )
                        , selected = "y.y")
          , conditionalPanel(
            condition = "input.trnsfrm1 == `rebased index`"
            , dateInput("rebase_date"
                        , label = "Date (rebase)"
                        , startview = "year"
                        , value = "2019-12-31"
                        
            )
            
          )
          
          , hr()
          , h4("Series 1")
          , selectInput("source_1"
                        , label = "Data Source"
                        , choices = data_sources
                        , selected = "local")
          
          , conditionalPanel(
            condition = "input.source_1 == `FRED`"
            , textInput("fred_series_1"
                        , label  = "FRED Series ID"
                        , value = "UNRATE"
            )
          )
          , conditionalPanel(
            condition = "input.source_1 == `dbnomics`"
            , textInput("dbnomics_series_1"
                        , label  = "dbnomics Series ID"
                        , value = "AMECO/ZUTN/EA19.1.0.0.0.ZUTN"
            )
          )
          
          
          , conditionalPanel(
            condition = "input.source_1 == `local`"
            # , selectizeInput("local_1"
            #                  , "Local Series"
            #                  , choices = list("CPI")
            #                  , selected = "CPI")
            , selectizeInput("text_1"
                             , "Search"
                             , choices = list(
                               Category_1 = cat1,
                               Category_2 = cat2,
                               Category_3 = cat3,
                               Category_4 = cat4
                             )
                             , selected = cat1[[1]]
                             , multiple = TRUE)
            , selectizeInput("region_1"
                             , label = "Region"
                             , choices = region_list
                             , selected = region_list[[1]]
                             , multiple = TRUE
            )
          )
          
          , selectizeInput("vis_type1", "Series plot:"
                           , choices = c("line", "bar", "scatter"))
          , hr()
          , h4("Series 2")
          , selectInput("source_2"
                        , label = "Data Source"
                        , choices = data_sources
                        , selected = "local")
          
          , conditionalPanel(
            condition = "input.source_2 == `FRED`"
            , textInput("fred_series_2"
                        , label  = "FRED Series ID"
                        , value = "DFF"
            )
          )
          , conditionalPanel(
            condition = "input.source_2 == `dbnomics`"
            , textInput("dbnomics_series_2"
                        , label  = "dbnomics Series ID"
                        , value = "AMECO/ZUTN/EA19.1.0.0.0.ZUTN"
            )
          )
          
          
          , conditionalPanel(
            condition = "input.source_2 == `local`"
            , selectizeInput("text_2"
                             , "Search"
                             , choices = list(
                               Category_1 = cat1,
                               Category_2 = cat2,
                               Category_3 = cat3,
                               Category_4 = cat4
                             )
                             , selected = NULL
                             , multiple = TRUE)
            , selectizeInput("region_2"
                             , label = "Region"
                             , choices = region_list
                             , selected = region_list[[1]]
                             , multiple = TRUE
            )
          )
          
          
          , hr()
          , h4("Series 3")
          , selectInput("source_3"
                        , label = "Data Source"
                        , choices = data_sources
                        , selected = "local")
          
          , conditionalPanel(
            condition = "input.source_3 == `FRED`"
            , textInput("fred_series_3"
                        , label  = "FRED Series ID"
                        , value = "PCE"
            )
          )
          , conditionalPanel(
            condition = "input.source_3 == `dbnomics`"
            , textInput("dbnomics_series_3"
                        , label  = "dbnomics Series ID"
                        , value = "AMECO/ZUTN/EA19.1.0.0.0.ZUTN"
            )
          )
          
          , conditionalPanel(
            condition = "input.source_3 == `local`"
            , selectizeInput("text_3"
                             , "Search"
                             , choices = list(
                               Category_1 = cat1,
                               Category_2 = cat2,
                               Category_3 = cat3,
                               Category_4 = cat4
                             )
                             , selected = NULL
                             , multiple = TRUE)
            , selectizeInput("region_3"
                             , label = "Region"
                             , choices = region_list
                             , selected = region_list[[1]]
                             , multiple = TRUE
            )
          )
          
          
          , hr()
          , h4("Series 4")
          , selectInput("source_4"
                        , label = "Data Source"
                        , choices = data_sources
                        , selected = "local")
          
          , conditionalPanel(
            condition = "input.source_4 == `FRED`"
            , textInput("fred_series_4"
                        , label  = "FRED Series ID"
                        , value = "GDP"
            )
          )
          , conditionalPanel(
            condition = "input.source_4 == `dbnomics`"
            , textInput("dbnomics_series_4"
                        , label  = "dbnomics Series ID"
                        , value = "AMECO/ZUTN/EA19.1.0.0.0.ZUTN"
            )
          )
          
          , conditionalPanel(
            condition = "input.source_4 == `local`"
            , selectizeInput("text_4"
                             , "Search"
                             , choices = list(
                               Category_1 = cat1,
                               Category_2 = cat2,
                               Category_3 = cat3,
                               Category_4 = cat4
                             )
                             , selected = NULL
                             , multiple = TRUE)
            , selectizeInput("region_4"
                             , label = "Region"
                             , choices = region_list
                             , selected = region_list[[1]]
                             , multiple = TRUE
            )
          )
          
          
        )
        ## outputs ----
        , mainPanel(

          ## Chart
          h4("Interactive Chart:")
          , plotlyOutput("p_cust")
          , hr()
          
          
          ## Data
          , h4("Data:")
          , DT::dataTableOutput("p_table_cust") 
          , hr()
          
          
          ## Inputs
          , fluidRow(
            column(3,
                   h4("More Inputs:")
                   , selectizeInput("data_freq", "Data Frequency:", choices = c("Day", "Week", "Month", "Quarter", "Annual"))
                   , numericInput("moving_avg", "Moving Average (no. obs):", value = 1)
            ),
            column(4, offset = 1,
                   numericInput("horizontal_1", "Horizontal Line (1)", value = NULL)
                   , numericInput("horizontal_2", "Horizontal Line (2)", value = NULL)
                   , numericRangeInput("horizontal_shading", "Horizontal Shading", value = c(NA,NA))
            ),
            column(4,
                   dateInput("vertical_1", "Vertical Line (1)", value = as.Date(NA))
                   , dateInput("vertical_2", "Vertical Line (2)", value = as.Date(NA))
                   , selectizeInput("recession_shading", "Recession Shading:"
                                    , choices = c("AU", "US", "none"), selected = "none")
            )
          ),
          hr()
          
          ## Static Chart
          , h4("Static Chart:")
          , plotOutput("p_cust_static")
          , hr()
          , fluidRow(
            column(width = 3,
                   h4("Chart Inputs")
                   , selectizeInput("cht_type", "Chart Type:", choices =  "simple") #list()
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
                   , numericInput("cht_height", "Height:", value = 5) #list()
                   , numericInput("cht_width", "Width:", value = 7) #list()
                   
            )
          )
          , hr()
          
          
          
          ## More inputs
          , fluidRow(
            h4("Exports:")
            , downloadButton("exportData", "Data (.csv)")
            , downloadButton("exportHTML", "Chart (.html)")
            , downloadButton("exportPNG", "Chart (.png)")
            , downloadButton("exportPPTX", "Chart (.pptx)")
            , downloadButton("exportR", "Chart (.R)")
            
          )
         , hr()

        )
      )
    )
  )
  , collapsible = TRUE
)









# SERVER ----
server <- function(input, output, session) {

  observe({
    if(input$cht_y_invert == F){
      y_vals <- pretty(c(min(p_data_cust()$value), max(p_data_cust()$value)+(max(p_data_cust()$value) - min(p_data_cust()$value))/10)
                       , n = n_ticks) 
      title_placement <- max(y_vals) -1/20*(max(y_vals)-min(y_vals))
    } else {
      y_vals <- pretty(c(min(p_data_cust()$value) - (max(p_data_cust()$value) - min(p_data_cust()$value))/10, max(p_data_cust()$value))
                       , n = n_ticks) 
      title_placement <- min(y_vals) +1/20*(max(y_vals)-min(y_vals))
    }
    
    updateSelectInput(session, "sub_sub_split", choices = name_list[[input$sub_split]])
    updateNumericInput(session, "cht_y_min", value = min(y_vals))
    updateNumericInput(session, "cht_y_max", value = max(y_vals))
    updateNumericInput(session, "cht_y_increment", value = y_vals[2] - y_vals[1])
    updateNumericInput(session, "cht_title_y_placement", value = title_placement)
    
  })
  
  session_store <- reactiveValues()
  
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
  
  
  # Data query ----
  # p_data_edit <- reactive({
  #   
  #   if (input$source_1 == "local"){
  #     return_data <- cpi_splits_cust(cpi_data = cpi_data_all
  #                                    , transformation = input$trnsfrm1
  #                                    , dates = as.numeric(input$year1)
  #                                    
  #                                    , pick_split_1 = unlist(input$text_1)
  #                                    , pick_split_2 = unlist(input$text_2)
  #                                    , pick_split_3 = unlist(input$text_3)
  #                                    , pick_split_4 = unlist(input$text_4)
  #                                    
  #                                    , region_1_split = input$region_1
  #                                    , region_2_split = input$region_2
  #                                    , region_3_split = input$region_3
  #                                    , region_4_split = input$region_4
  #                                    , rebase_date = as.Date(input$rebase_date)
  #     ) 
  #     
  #   } else if (input$source_1 == "FRED"){
  #     
  #     return_data <- fred_data(series = input$fred_series
  #                              , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
  #                              , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
  #     )
  #   }
  #   return(return_data)
  # })
  # 
  # 
  # 
  p_data_edit <- reactive({
    
    if (input$source_1 == "local"){
      return_data_1 <- cpi_splits_cust(cpi_data = cpi_data_all
                                  , transformation = input$trnsfrm1
                                  , dates = as.numeric(input$year1)
                                  
                                  , pick_split_1 = unlist(input$text_1)
                                  , region_1_split = input$region_1
                                  , rebase_date = as.Date(input$rebase_date)
      ) 
    } else if (input$source_1 == "FRED"){
      return_data_1 <- fred_data(series = input$fred_series_1
                                , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
                                , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
                               )
    } else if (input$source_1 == "dbnomics"){
      return_data_1 <- db_data(series = input$dbnomics_series_1
                                 , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
                                 , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
      )
    }
    
    
    if (input$source_2 == "local"){
      return_data_2 <- cpi_splits_cust(cpi_data = cpi_data_all
                                     , transformation = input$trnsfrm1
                                     , dates = as.numeric(input$year1)
                                     
                                     , pick_split_1 = unlist(input$text_2)
                                     , region_1_split = input$region_2
                                     , rebase_date = as.Date(input$rebase_date)
      ) 
    } else if (input$source_2 == "FRED"){
      return_data_2 <- fred_data(series = input$fred_series_2
                               , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
                               , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
      )
    } else if (input$source_2 == "dbnomics"){
      return_data_2 <- db_data(series = input$dbnomics_series_2
                                 , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
                                 , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
      )
    }
    
    
    if (input$source_3 == "local"){
      return_data_3 <- cpi_splits_cust(cpi_data = cpi_data_all
                                       , transformation = input$trnsfrm1
                                       , dates = as.numeric(input$year1)
                                       
                                       , pick_split_1 = unlist(input$text_3)
                                       , region_1_split = input$region_3
                                       , rebase_date = as.Date(input$rebase_date)
      ) 
    } else if (input$source_3 == "FRED"){
      return_data_3 <- fred_data(series = input$fred_series_3
                                 , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
                                 , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
      )
    } else if (input$source_3 == "dbnomics"){
      return_data_3 <- db_data(series = input$dbnomics_series_3
                                 , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
                                 , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
      )
    }
    
    
    
    if (input$source_4 == "local"){
      return_data_4 <- cpi_splits_cust(cpi_data = cpi_data_all
                                       , transformation = input$trnsfrm1
                                       , dates = as.numeric(input$year1)
                                       
                                       , pick_split_1 = unlist(input$text_4)
                                       , region_1_split = input$region_4
                                       , rebase_date = as.Date(input$rebase_date)
      ) 
      
    } else if (input$source_4 == "FRED"){
      
      return_data_4 <- fred_data(series = input$fred_series_4
                                 , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
                                 , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
      )
    } else if (input$source_4 == "dbnomics"){
      return_data_4 <- db_data(series = input$dbnomics_series_4
                                 , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
                                 , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
      )
    }
  
    

    
    return(return_data_1 %>%
             rbind(return_data_2) %>%
             rbind(return_data_3) %>%
             rbind(return_data_4))
  })
  
  
  
  # Data transformation ----
  p_data_cust <- reactive({
    
    return_data <- p_data_edit()
    
    if(is.integer(input$moving_avg) == T){
      return_data <- calc_moving_avg(input_data = p_data_edit()
                                  , moving_avg = input$moving_avg) 

    } 

    return(return_data)
  })
  

  
  

  # Plot (interactive) ---------------
  output$p_cust <- renderPlotly({
    session_store$plt <- ggplotly(
      p_data_cust() %>%
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
    chart_formatting(p_data_cust())
    
    set_chart_defaults(input_data = p_data_cust()
                       , cht_y_min = input$cht_y_min
                       , cht_y_max = input$cht_y_max
                       , cht_y_increment = input$cht_y_increment
                       , cht_y_invert = input$cht_y_invert
                       , cht_y_axes_unit = input$cht_y_axes_unit
                       , cht_y_axes_unit_size = input$cht_y_axes_unit_size
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
                       #, recession_shading = input$recession_shading
                       )

    return(chart_defaults)
  })

  p_cust_static <- reactive({
    p_data_cust() %>%
      ggplot(aes(x=date, y=value, colour = name)) +
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
    filename = function() {
      paste("data-", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      # export plotly html widget as a temp file to download.
      saveWidget(as_widget(session_store$plt), file, selfcontained = TRUE)
    }
  )


  
  # Custom table ----
  output$p_table_cust <- DT::renderDataTable(
    
    if(input$viewData1 == 1){
      tmp <- p_data_cust()  %>%
        select(c("name", "value", "date")) %>%
        arrange(date) %>%
        pivot_wider(names_from = name, values_from = value)
      
      DT::datatable(tmp, options = list(pageLength = 10))
      
    } else{
      cat("Change selection to display table.")
    }

  )




  
  
  
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
  

#   p_data_0 <- reactive({
#     cpi_splits(cpi_data = cpi_long
#                , pick_split = input$splits
#                , region_split = input$splits_region
#                #, pick_sub_split = input$sub_split
#                , dates = as.numeric(input$year)
#     ) 
#   })
#   
#   p_data <- reactive({
#     cpi_transformation(cpi_data = p_data_0
#                      #  , pick_split = input$splits
#                      #  , transformation = as.numeric(input$trnsfrm)
#                        , transformation = input$trnsfrm
#                        , period_change = as.numeric(input$period)
#                        , annualise = as.numeric(input$annualise)
#                        , dates = as.numeric(input$year)
#     )
#   })
# 
# 
#   
#   #browser()
#   
#   
#   
#   # Plot ---------------
#   output$p_plot <- renderPlotly({ggplotly(
#     p_data() %>%
#       ggplot() +
#       geom_col(aes(x=date, y=change_pct_cont, fill = pick_split1)) +
#       geom_line(data = p_data() %>%
#                   group_by(date) %>%
#                   summarise(change_pct_cont = sum(change_pct_cont))
#                 , aes(x=date, y=change_pct_cont)) +
#       theme_minimal() +
#       xlab("Date") +
#       ylab("Percent")
#   )})
#   
#   
#   # Table ---------------
#   output$p_table_change <- renderTable({
#     p_data() %>%
#       select(c("date", "change_pct", "pick_split1")) %>%
#       pivot_wider(names_from = date, values_from = change_pct) 
#     
#   })
#   
#   output$p_table_change_cont <- renderTable({
# 
#     p_data() #%>%
#       # select(c("date", "change_pct_cont", "pick_split1")) %>%
#       # pivot_wider(names_from = date, values_from = change_pct_cont) %>%
#       # adorn_totals("row")
#     
#   })
#   
  
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






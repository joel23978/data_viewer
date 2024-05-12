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
source(here::here("plot_functions.R"))
source(here::here("external_data.R"))
source(here::here("mod_ui.R"))




set_default_values()


ui <- fluidPage(
  
  
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
  
  , mainPanel(
    dataTableOutput("results")
    ))
)

server <- function(input, output, session) {
  # Server logic for handling inputs and outputs
  callModule(rba_update, 1)
  callModule(abs_update, 1)
  callModule(bloomy_update, 1)
  data_queried1 <- reactive({callModule(data_query, "1", input_dates = input$year1)})
  data_transformed1 <- reactive({callModule(data_transform, "1", data_source = data_queried1)})
  
  
  # 
  # Render the transformed data as a table in the UI
  output$results <- renderDataTable({
    req(data_transformed1())  # Ensure the data is available before rendering
    data_transformed1()       # Render the data frame
  })


}

shinyApp(ui, server)

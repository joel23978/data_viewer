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

source(here::here("cpi_annual.R"))




# UI ----
ui <- navbarPage(
  
  "Data Explorer",
  tabPanel(
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
                        , choices = list("Australia"
                                         , "Sydney"		
                                         , "Melbourne"			
                                         , "Brisbane"		
                                         , "Adelaide"			
                                         , "Perth"		
                                         , "Hobart"			
                                         , "Darwin"		
                                         , "Canberra"			
                                         #, "Weighted average of eight capital cities"
                        )
                        , selected = "Australia"
          )
        )
        , selectInput("trnsfrm"
                      , label = "Transformation"
                      , choices = list("index" 
                                       , "y.y" 
                                       , "q.q" 
                                       #, "Contribution (Proportion)" = 3
                      )
                      , selected = 0)
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
  , collapsible = TRUE
)









# SERVER ----
server <- function(input, output, session) {

  observe({
    updateSelectInput(session, "sub_sub_split", choices = name_list[[input$sub_split]])
  })
  
  
  
  
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
        pivot_wider(names_from = date, values_from = value)
      } else{
      cat("Change selection to display table.")
    }
    


  })
  
  output$p_table_sub <- renderTable({
    if(input$viewData == 1){
      p_data_sub()  %>%
        select(c("name", "value", "date")) %>%
        pivot_wider(names_from = date, values_from = value)
    } else{
      cat("Change selection to display table.")
    }

  })
  
  
  output$p_table_sub_sub <- renderTable({
    if(input$viewData == 1){
      p_data_sub_sub()  %>%
        select(c("name", "value", "date")) %>%
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







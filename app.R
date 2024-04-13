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
library(shinyToastify)


source(here::here("cpi_annual.R"))
source(here::here("plot_functions.R"))
source(here::here("external_data.R"))

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
          
          
          ## Series 1 ----
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
            , selectInput("trnsfrm_1"
                          , label = "Transformation"
                          , choices = transformation_choices
                          , selected = "y.y")
            , conditionalPanel(
              condition = "input.trnsfrm_1 == `rebased index`"
              , dateInput("rebase_date_1"
                          , label = "Date (rebase)"
                          , startview = "year"
                          , value = "2019-12-31"
              ))
          )
          
          , conditionalPanel(
            condition = "input.source_1 == `rba`"
            , selectInput("rba_table_1"
                          , "RBA Table"
                          , choices = names(rba_series)
                          , selected = "A1"
            )
            , selectInput("rba_desc_1"
                          , label = "RBA Series"
                          , choices = rba_series[["A1"]]
                          , selected = "" #rba_series[["A1"]][1]
            )
          )
          
          , conditionalPanel(
            condition = "input.source_1 == `bloomberg`"
            , selectInput("bloomberg_category_1"
                          , "Category"
                          , choices = bbg_categories
                          , selected = bbg_categories[1]
            )
            , selectizeInput("bloomberg_desc_1"
                          , label = "Series"
                          , choices = bbg_series[[1]]
                          , selected = "" 
                          , multiple = T
            )
            , selectizeInput("bloomberg_ticker_1"
                          , label = "Ticker (for query)"
                          , choices = bbg_tickers[[1]]
                          , selected = "" 
                          , options = list(create = TRUE)
                          , multiple = T
            )
          )
          
          
          , textInput("label_1",  "Label:", value = "")
          
          # Input for the mathematical expression
          ,textInput("expression_1", "Enter an expression (use 'data' as variable):", value = "data * 2")
          ,actionButton("calculate_1", "Calculate")
        

          , selectizeInput("vis_type_1", "Series plot:"
                           , choices = c("line", "bar", "scatter"))
          
          
          
      
          
          ## Series 2 ----
        
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
            , selectInput("trnsfrm_2"
                          , label = "Transformation"
                          , choices = transformation_choices
                          , selected = "y.y")
            , conditionalPanel(
              condition = "input.trnsfrm_2 == `rebased index`"
              , dateInput("rebase_date_2"
                          , label = "Date (rebase)"
                          , startview = "year"
                          , value = "2019-12-31"
              ))
          )
          , conditionalPanel(
            condition = "input.source_2 == `rba`"
            , selectizeInput("rba_table_2"
                             , "RBA Table"
                             , choices = names(rba_series)
                             , selected = "F16")
            , selectizeInput("rba_desc_2"
                             , label = "RBA Series"
                             , choices = rba_series[[1]]
            )
          )
          , textInput("label_2",  "Label:", value = "")
          ,textInput("expression_2", "Enter an expression (use 'data' as variable):", value = "data * 2")
          ,actionButton("calculate_2", "Calculate")
          , selectizeInput("vis_type_2", "Series plot:"
                           , choices = c("line", "bar", "scatter"))
          
          
          
          
          
          
          ## Series 3 ----
        
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
            , selectInput("trnsfrm_3"
                          , label = "Transformation"
                          , choices = transformation_choices
                          , selected = "y.y")
            , conditionalPanel(
              condition = "input.trnsfrm_3 == `rebased index`"
              , dateInput("rebase_date_3"
                          , label = "Date (rebase)"
                          , startview = "year"
                          , value = "2019-12-31"
              ))
          )
          , conditionalPanel(
            condition = "input.source_3 == `rba`"
            , selectizeInput("rba_table_3"
                             , "RBA Table"
                             , choices = names(rba_series)
                             , selected = "F16")
            , selectizeInput("rba_desc_3"
                             , label = "RBA Series"
                             , choices = rba_series[[1]]
            )
          )
          , textInput("label_3",  "Label:", value = "")
          ,textInput("expression_3", "Enter an expression (use 'data' as variable):", value = "data * 2")
          ,actionButton("calculate_3", "Calculate")
          , selectizeInput("vis_type_3", "Series plot:"
                           , choices = c("line", "bar", "scatter"))
          
          
          
          
          
          
          
          
          
          ## Series 4 ----
        
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
            , selectInput("trnsfrm_4"
                          , label = "Transformation"
                          , choices = transformation_choices
                          , selected = "y.y")
            , conditionalPanel(
              condition = "input.trnsfrm_4 == `rebased index`"
              , dateInput("rebase_date_4"
                          , label = "Date (rebase)"
                          , startview = "year"
                          , value = "2019-12-31"
              ))
          )
          , conditionalPanel(
            condition = "input.source_4 == `rba`"
            , selectizeInput("rba_table_4"
                             , "RBA Table"
                             , choices = names(rba_series)
                             , selected = "F16")
            , selectizeInput("rba_desc_4"
                             , label = "RBA Series"
                             , choices = rba_series[[1]]
            )
          )
          , textInput("label_4",  "Label:", value = "")
          
          ,textInput("expression_4", "Enter an expression (use 'data' as variable):", value = "data * 2")
          ,actionButton("calculate_4", "Calculate")
          , selectizeInput("vis_type_4", "Series plot:"
                           , choices = c("line", "bar", "scatter"))
          
          
          
        )
        ## outputs ----
        , mainPanel(
          uiOutput("tab")
          
          
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
            column(3,
                   h4("More Inputs:")
                   , selectizeInput("data_freq", "x Data Frequency:", choices = c("Day", "Week", "Month", "Quarter", "Annual"))
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
                   , selectizeInput("recession_shading", "x Recession Shading:"
                                    , choices = c("AU", "US", "none"), selected = "none")
            )
          ),
          hr()
          
          ## Static Chart
          , h4("Static Chart:")
          , plotOutput("p_cust_static")
          , fluidRow(
            useShinyToastify()
            , actionButton("copybtn", "Copy", icon = icon("copy"), class = "btn-primary")        
            
          )
          
          
          
          
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
            , downloadButton("exportPPTX", "x Chart (.pptx)")
            , downloadButton("exportR", "x Chart (.R)")

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


  
  #weblinks
  
  fred_link <- a("FRED", href="https://fred.stlouisfed.org/tags/series")
  dbnomics_link <- a("dbnomics", href="https://db.nomics.world")
  rba_link <- a("RBA Statistical Tables", href="https://www.rba.gov.au/statistics/tables/")
  
  output$tab <- renderUI({
    tagList("Data Sources:", fred_link, dbnomics_link, rba_link)
  })
  
  
  observe({
    ## chart formatting
    if(input$cht_y_invert == F){
      y_vals <- pretty(c(min(p_data_cust()$value), max(p_data_cust()$value)+(max(p_data_cust()$value) - min(p_data_cust()$value))/10)
                       , n = n_ticks) 
      title_placement <- max(y_vals) -1/20*(max(y_vals)-min(y_vals))
    } else {
      y_vals <- pretty(c(min(p_data_cust()$value) - (max(p_data_cust()$value) - min(p_data_cust()$value))/10, max(p_data_cust()$value))
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
  

  # Observes for updating session_store independently for each rba_table input
  lapply(1:4, function(i) {
    observe({
      table_input <- input[[paste0("rba_table_", i)]]
      if (!is.null(table_input)) {
        session_store[[paste0("rba_desc_", i)]] <- rba_series[[table_input]] %>% na.omit()
      }
    })
  })
  
  # Observes for updating UI elements independently based on session_store
  lapply(1:4, function(i) {
    observe({
      desc_data <- session_store[[paste0("rba_desc_", i)]]
      if (length(desc_data) > 0) {
        # Use the first available option as the default selection if it exists
        default_selection <- ifelse(length(desc_data) >= 1, desc_data[1], NULL)
        updateSelectInput(session, paste0("rba_desc_", i), choices = desc_data, selected = default_selection)
      }
    })
  })
  
  session_store <- reactiveValues()
  
  # Observe changes in each category separately to update series
  lapply(1:4, function(i) {
    observe({
      cat_input <- input[[paste0("bloomberg_category_", i)]]
      if (!is.null(cat_input)) {
        series_choices <- bbg_series[[cat_input]] %>% na.omit()  # Ensure no NA values
        if (length(series_choices) > 0) {
          updateSelectInput(session, paste0("bloomberg_desc_", i), choices = series_choices, selected = series_choices[1])
        }
      }
    })
  })
  
  # Observe changes in each series separately to update ticker
  lapply(1:4, function(i) {
    observe({
      desc_input <- input[[paste0("bloomberg_desc_", i)]]
      if (!is.null(desc_input)) {
        valid_tickers <- bbg_ref %>% filter(Description %in% desc_input) %>% pull(Security) %>% na.omit()
        if (length(valid_tickers) > 0) {
          updateSelectInput(session, paste0("bloomberg_ticker_", i), choices = valid_tickers, selected = valid_tickers)
        }
      }
    })
  })
  
  
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
  
  # Data inputs ----
  p_data_1 <- reactive({
    if (input$source_1 == "local"){
      tmp <- cpi_splits_cust(cpi_data = cpi_data_all
                                  , transformation = input$trnsfrm_1
                                  , dates = as.numeric(input$year1)
                                  
                                  , pick_split_1 = unlist(input$text_1)
                                  , region_1_split = input$region_1
                                  , rebase_date = as.Date(input$rebase_date_1)
      ) 
    } else if (input$source_1 == "FRED"){
      tmp <- fred_data(series = input$fred_series_1
                                , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
                                , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
                               )
    } else if (input$source_1 == "dbnomics"){
      tmp <- db_data(series = input$dbnomics_series_1
                                 , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
                                 , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
      )
    } else if (input$source_1 == "rba"){
      tmp <- rba_data(table = input$rba_table_1
                                , series = input$rba_desc_1
                               , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
                               , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
      )
    } else if (input$source_1 == "bloomberg"){
      tmp <- bbg_data(series = input$bloomberg_ticker_1
                                , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
                                , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
      )
    }
  
    if(nzchar(input$label_1)){
      tmp <- tmp %>%
        mutate(name = input$label_1)
    } 
    tmp <- tmp %>%
      mutate(plotting = input$vis_type_1)
    return(tmp)

  })
  
  p_data_2 <- reactive({
    if (input$source_2 == "local"){
      tmp <- cpi_splits_cust(cpi_data = cpi_data_all
                                     , transformation = input$trnsfrm_2
                                     , dates = as.numeric(input$year1)
                                     
                                     , pick_split_1 = unlist(input$text_2)
                                     , region_1_split = input$region_2
                                     , rebase_date = as.Date(input$rebase_date_2)
      ) 
    } else if (input$source_2 == "FRED"){
      tmp <- fred_data(series = input$fred_series_2
                               , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
                               , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
      )
    } else if (input$source_2 == "dbnomics"){
      tmp <- db_data(series = input$dbnomics_series_2
                                 , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
                                 , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
      )
    } else if (input$source_2 == "rba"){
      tmp <- rba_data(table = input$rba_table_2
                                , series = input$rba_desc_2
                               , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
                               , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
      )
    } else if (input$source_2 == "bloomberg"){
      tmp <- bbg_data(series = input$bloomberg_ticker_2
                                , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
                                , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
      )
    }
    if(nzchar(input$label_2)){
      tmp <- tmp %>%
        mutate(name = input$label_2)
    } 
    tmp <- tmp %>%
      mutate(plotting = input$vis_type_2)
    return(tmp)
  })
    
    
  p_data_3 <- reactive({
    if (input$source_3 == "local"){
      tmp <- cpi_splits_cust(cpi_data = cpi_data_all
                                       , transformation = input$trnsfrm_3
                                       , dates = as.numeric(input$year1)
                                       
                                       , pick_split_1 = unlist(input$text_3)
                                       , region_1_split = input$region_3
                                       , rebase_date = as.Date(input$rebase_date_3)
      ) 
    } else if (input$source_3 == "FRED"){
      tmp <- fred_data(series = input$fred_series_3
                                 , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
                                 , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
      )
    } else if (input$source_3 == "dbnomics"){
      tmp <- db_data(series = input$dbnomics_series_3
                                 , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
                                 , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
      )
    }  else if (input$source_3 == "rba"){
      tmp <- rba_data(table = input$rba_table_3
                                , series = input$rba_desc_3
                               , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
                               , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
      )
    } else if (input$source_3 == "bloomberg"){
      tmp <- bbg_data(series = input$bloomberg_ticker_3
                                , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
                                , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
      )
    }
    if(nzchar(input$label_3)){
      tmp <- tmp %>%
        mutate(name = input$label_3)
    } 
    tmp <- tmp %>%
      mutate(plotting = input$vis_type_3)
    return(tmp)
  })
    
    
  p_data_4 <- reactive({
    if (input$source_4 == "local"){
      tmp <- cpi_splits_cust(cpi_data = cpi_data_all
                                       , transformation = input$trnsfrm_4
                                       , dates = as.numeric(input$year1)
                                       
                                       , pick_split_1 = unlist(input$text_4)
                                       , region_1_split = input$region_4
                                       , rebase_date = as.Date(input$rebase_date_4)
      ) 
      
    } else if (input$source_4 == "FRED"){
      
      tmp <- fred_data(series = input$fred_series_4
                                 , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
                                 , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
      )
    } else if (input$source_4 == "dbnomics"){
      tmp <- db_data(series = input$dbnomics_series_4
                                 , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
                                 , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
      )
    } else if (input$source_4 == "rba"){
      tmp <- rba_data(table = input$rba_table_4
                                , series = input$rba_desc_4
                               , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
                               , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
      )
    }  else if (input$source_4 == "bloomberg"){
      tmp <- bbg_data(series = input$bloomberg_ticker_4
                                , start_date = lubridate::ymd(min(input$year1), truncated = 2L)
                                , end_date = lubridate::ymd(max(input$year1), truncated = 2L)
      )
    }
    if(nzchar(input$label_4)){
      tmp <- tmp %>%
        mutate(name = input$label_4)
    } 
    
    tmp <- tmp %>%
      mutate(plotting = input$vis_type_4)
    return(tmp)
  })
   
  # Initialize reactive value to store modified data
  p_data_1.1 <- reactiveVal()
  
  # Initialize p_data_1.1 with p_data_1
  observe({
    p_data_1.1(p_data_1())
  })
  
  observeEvent(input$calculate_1, {
    # Retrieve the user's expression and replace 'data' with 'value'
    expression_text <- sub("data", "value", input$expression_1)
    
    # Update the data using the provided expression
    tmp <- p_data_1.1() %>%  # use the latest stored data
      mutate(value = eval(parse(text = expression_text)))
    
    # Update the reactive value with new data
    p_data_1.1(tmp)
  })
  
  # 2.1
  p_data_2.1 <- reactiveVal()
  observe({
    p_data_2.1(p_data_2())
  })
  observeEvent(input$calculate_2, {
    expression_text <- sub("data", "value", input$expression_2)
    tmp <- p_data_2.1() %>%  # use the latest stored data
      mutate(value = eval(parse(text = expression_text)))
    p_data_2.1(tmp)
  })
  
  # 3.1
  p_data_3.1 <- reactiveVal()
  observe({
    p_data_3.1(p_data_3())
  })
  observeEvent(input$calculate_3, {
    expression_text <- sub("data", "value", input$expression_3)
    tmp <- p_data_3.1() %>%  # use the latest stored data
      mutate(value = eval(parse(text = expression_text)))
    p_data_3.1(tmp)
  })
  
  # 4.1
  p_data_4.1 <- reactiveVal()
  observe({
    p_data_4.1(p_data_4())
  })
  observeEvent(input$calculate_4, {
    expression_text <- sub("data", "value", input$expression_4)
    tmp <- p_data_4.1() %>%  # use the latest stored data
      mutate(value = eval(parse(text = expression_text)))
    p_data_4.1(tmp)
  })
  
  p_data_edit <- reactive({
    return(p_data_1.1() %>%
             rbind(p_data_2.1()) %>%
             rbind(p_data_3.1()) %>%
             rbind(p_data_4())
    )
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



  
  #CPI  ----
  
  
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
  #                                    , transformation = input$trnsfrm_1
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






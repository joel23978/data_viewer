# Function to generate UI for each series


seriesUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("source"), "Data Source", choices = data_sources, selected = "local")
  
    # FRED Series ID input
    , conditionalPanel(
      condition = "input.source == 'FRED'",
      textInput(ns("fred_series"), "FRED Series ID", value = "UNRATE"),
      ns=NS(id)  
    )
    
    # dbnomics Series ID input
    , conditionalPanel(
      condition = "input.source == 'dbnomics'",
      textInput(ns("dbnomics_series"), "dbnomics Series ID", value = "AMECO/ZUTN/EA19.1.0.0.0.ZUTN"),
      ns=NS(id)  
    )
    
    # Local source inputs
    , conditionalPanel(
      condition = "input.source == 'local'",
      selectizeInput(ns("text"), "Search", choices = list(
        Category_1 = cat1,
        Category_2 = cat2,
        Category_3 = cat3,
        Category_4 = cat4
      ), multiple = TRUE),
      selectizeInput(ns("region"), "Region", choices = region_list, multiple = TRUE, selected = region_list[[1]]),
      selectInput(ns("transform"), "Transformation", choices = transformation_choices, selected = transformation_choices[[1]]),
      conditionalPanel(
        condition = "input.transform== 'rebased index'",
        dateInput(ns("rebase_date"), "Date (rebase)", startview = "year", value = "2019-12-31"),
        ns=NS(id)  
      ),
      ns=NS(id)  
    )
    
    # RBA options
    , conditionalPanel(
      condition = "input.source == 'rba'",
      selectInput(ns("rba_table"), "RBA Table", choices = rba_tables),
      selectizeInput(ns("rba_desc"), "RBA Series", choices = rba_series[[1]], multiple = TRUE),
      ns=NS(id)
    )
    
    # Bloomberg options
    , conditionalPanel(
      condition = "input.source == 'bloomberg'",
      selectInput(ns("bloomberg_category"), "Category", choices = bbg_categories),
      selectizeInput(ns("bloomberg_desc"), "Series", choices = bbg_series[[1]], multiple = TRUE),
      selectizeInput(ns("bloomberg_ticker"), "Ticker (for query)", choices = bbg_tickers[[1]], options = list(create = TRUE), multiple = TRUE),
      ns=NS(id)
    )
    
    # ABS options
    , conditionalPanel(
      condition = "input.source == 'abs'",
      selectInput(ns("abs_catalogue"), "Catalogue", choices = abs_cat),
      selectInput(ns("abs_desc"), "Series", choices = abs_ref[[1]]$series),
      selectInput(ns("abs_series_type"), "Series Type", choices = unique(abs_ref[[1]]$series_type)),
      selectInput(ns("abs_table"), "Table", choices = unique(abs_ref[[1]]$table_title)),
      selectizeInput(ns("abs_id"), "Series ID (for query)", choices = abs_ref[[1]]$series_id, options = list(create = TRUE)),
      ns=NS(id)
    )
    
    # common inputs
    , textInput(ns("label"), "Label:", value = "")
    , textInput(ns("expression"), "Enter an expression (use 'data' as variable):", value = "data")
    , actionButton(ns("calculate"), "Calculate")
    , selectizeInput(ns("vis_type"), "Series plot:", choices = c("line", "bar", "scatter"))

  )
}




rba_update <- function(input, output, session){
  observe({ 
    # req(input$rba_table)
    
    table_input <- input$rba_table
    if (!is.null(table_input)) {
      series_choices <- rba_series[[table_input]] %>% na.omit()  # Ensure no NA values
      if (length(series_choices) > 0) {
        updateSelectInput(session, 'rba_desc', choices = series_choices, selected = series_choices[1])
      }
    }
    })
}



abs_update <- function(input, output, session){
  #catalogue
  observe({
    # req(input$abs_catalogue)
    
    cat_input <- input$abs_catalogue
    desc_choices <- abs_ref[[cat_input]] %>% pull(series) %>% unique() %>% na.omit()  
    if (length(desc_choices) > 0) {
      desc_input <- desc_choices[1]
      updateSelectInput(session, 'abs_desc', choices = desc_choices, selected = desc_choices[1])
    }
  })
  
  #desc
  observe({
    # req(input$abs_catalogue)
    # req(input$abs_desc)
    
    cat_input <- input$abs_catalogue
    desc_input <- input$abs_desc
    series_type_choices <- abs_ref[[cat_input]] %>% filter(series == desc_input) %>% pull(series_type) %>% unique() %>% na.omit() 
    if (length(series_type_choices) > 0) {
      series_type_input <- series_type_choices[1]
      updateSelectInput(session, 'abs_series_type', choices = series_type_choices, selected = series_type_choices[1])
    }
  })
  
  
  #series type
  observe({
    # req(input$abs_catalogue)
    # req(input$abs_desc)
    # req(input$abs_series_type)
    
    cat_input <- input$abs_catalogue
    desc_input <- input$abs_desc
    series_type_input <- input$abs_series_type
    table_choices <- abs_ref[[cat_input]] %>% filter(series == desc_input
                                                     , series_type == series_type_input) %>% pull(table_title) %>% na.omit() 
    if (length(table_choices) > 0) {
      updateSelectInput(session, 'abs_table', choices = table_choices, selected = table_choices[1])
    }
  })
  
  #table input
  observe({
    # req(input$abs_catalogue)
    # req(input$abs_desc)
    # req(input$abs_series_type)
    # req(input$abs_table)
    
    cat_input <- input$abs_catalogue
    desc_input <- input$abs_desc
    series_type_input <- input$abs_series_type
    table_input <- input$abs_table
    id_choices <- abs_ref[[cat_input]] %>% filter(series == desc_input
                                                  , series_type == series_type_input
                                                  , table_title == table_input) %>% pull(series_id) %>% na.omit() 
    if (length(id_choices) > 0) {
      updateSelectInput(session, 'abs_id', choices = id_choices, selected = id_choices[1])
    }
  })
}



bloomy_update <- function(input, output, session){
  observe({
    cat_input <- input$bloomberg_category
    if (!is.null(cat_input)) {
      series_choices <- bbg_series[[cat_input]] %>% na.omit()  # Ensure no NA values
      if (length(series_choices) > 0) {
        updateSelectInput(session, 'bloomberg_desc', choices = series_choices, selected = series_choices[1])
      }
    }
  })
  
  observe({
    desc_input <- input$bloomberg_desc
    if (!is.null(desc_input)) {
      valid_tickers <- bbg_ref %>% filter(Description %in% desc_input) %>% pull(Security) %>% na.omit()
      if (length(valid_tickers) > 0) {
        updateSelectInput(session, 'bloomberg_ticker', choices = valid_tickers, selected = valid_tickers)
      }
    }
  })
}



bloomy_update <- function(input, output, session){
  observe({
    cat_input <- input$bloomberg_category
    if (!is.null(cat_input)) {
      series_choices <- bbg_series[[cat_input]] %>% na.omit()  # Ensure no NA values
      if (length(series_choices) > 0) {
        updateSelectInput(session, 'bloomberg_desc', choices = series_choices, selected = series_choices[1])
      }
    }
  })
  
  observe({
    desc_input <- input$bloomberg_desc
    if (!is.null(desc_input)) {
      valid_tickers <- bbg_ref %>% filter(Description %in% desc_input) %>% pull(Security) %>% na.omit()
      if (length(valid_tickers) > 0) {
        updateSelectInput(session, 'bloomberg_ticker', choices = valid_tickers, selected = valid_tickers)
      }
    }
  })
}



data_query <- function(input, output, session, input_dates){

 start_date <- lubridate::ymd(min(input_dates), truncated = 2L)
 end_date <- lubridate::ymd(max(input_dates), truncated = 2L)
 
    if (input$source == "local"){
      tmp <- cpi_splits_cust(cpi_data = cpi_data_all
                             , transformation = input$transform
                             , dates = as.numeric(input_dates)
                             
                             , pick_split = unlist(input$text)
                             , region_1_split = input$region
                             , rebase_date = as.Date(input$rebase_date)
      ) 
    } else if (input$source == "FRED"){
      tmp <- fred_data(series = input$fred_series
                       , start_date 
                       , end_date 
      )
    } else if (input$source == "dbnomics"){
      tmp <- db_data(series = input$dbnomics_series
                     , start_date
                     , end_date
      )
    } else if (input$source == "rba"){
      tmp <- rba_data(series = input$rba_desc)
    } else if (input$source == "bloomberg"){
      tmp <- bbg_data(series = input$bloomberg_ticker
                      , start_date
                      , end_date
      )
    } else if (input$source == "abs"){
      tmp <- abs_data(series = input$abs_id)
    }
    tmp
}


data_transform <- function(input, output, session, data_source){
  
  tmp <- data_source()
 
  if (input$calculate > 0) {
    expression_text <- sub("data", "value", input$expression)
    tmp <- tmp %>%
      dplyr::mutate(value = eval(parse(text = expression_text)))
  } 
    
    # Apply label and visualization type transformations if applicable
    if (nzchar(input$label)) {
      tmp <- tmp %>%
        dplyr::mutate(name = input$label)
    }
    
    # Apply visualization type and series transformations
    tmp <- tmp %>%
      dplyr::mutate(plotting = input$vis_type)
  
  tmp
}
  













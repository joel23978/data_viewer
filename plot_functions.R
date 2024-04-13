data_sources <- list("local" 
                    , "FRED"
                    , "dbnomics"
                    , "rba"
                    , "bloomberg"
)


# Helper function to create reactive data based on input index
create_p_data <- function(index) {
  reactive({
    source <- input[[paste0("source_", index)]]
    year <- as.numeric(input[[paste0("year", index)]])
    label <- input[[paste0("label_", index)]]
    
    tmp <- if (source == "local") {
      cpi_splits_cust(cpi_data = cpi_data_all,
                      transformation = input[[paste0("trnsfrm", index)]],
                      dates = year,
                      pick_split_1 = unlist(input[[paste0("text_", index)]]),
                      region_1_split = input[[paste0("region_", index)]],
                      rebase_date = as.Date(input[[paste0("rebase_date")]]))
    } else if (source == "FRED") {
      fred_data(series = input[[paste0("fred_series_", index)]],
                start_date = ymd(min(year), truncated = 2L),
                end_date = ymd(max(year), truncated = 2L))
    } else if (source == "dbnomics") {
      db_data(series = input[[paste0("dbnomics_series_", index)]],
              start_date = ymd(min(year), truncated = 2L),
              end_date = ymd(max(year), truncated = 2L))
    } else if (source == "rba") {
      rba_data(table = input[[paste0("rba_table_", index)]],
               series = input[[paste0("rba_desc_", index)]],
               start_date = ymd(min(year), truncated = 2L),
               end_date = ymd(max(year), truncated = 2L))
    } else if (source == "bloomberg") {
      bbg_data(series = input[[paste0("bloomberg_ticker_", index)]],
               start_date = ymd(min(year), truncated = 2L),
               end_date = ymd(max(year), truncated = 2L))
    }
    
    if(nzchar(label)) {
      tmp <- tmp %>% mutate(name = label)
    }
    return(tmp)
  })
}


theme_jf <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(panel.border = element_blank(),
                   panel.grid = element_blank(),
                   axis.line.y.left = element_line(size = 0.5, colour = "grey70"),
                   axis.line.y.right = element_line(size = 0.5, colour = "grey70"),
                   axis.title = element_blank(),
                   axis.ticks = element_line(size = 0.5, colour = "grey70"),
                   axis.ticks.length.x.bottom = unit(-0.15, "cm"),
                   axis.ticks.length.y.left = unit(-0.15, "cm"),
                   axis.ticks.length.y.right = unit(-0.15, "cm"),
                   axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
                   axis.text.y.left = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
                   axis.text.y.right = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
                   axis.line.x = element_line(size = 0.5, colour = "grey70"),
                   plot.title = element_text(hjust = 1, size = 10, vjust = -7),
                   strip.background = element_blank(),
                   strip.text = element_text(hjust = 0),
    )
}

set_y_axis <- function(y_min, y_max, invert = F, n_ticks = 6) {

  range <- y_max - y_min
  
  y_axis_mod <- pretty(c(y_min, y_max+range/10)
                       , n = n_ticks)
  
  cht_y_min <<- min(y_axis_mod)
  cht_y_max <<- max(y_axis_mod)
  cht_y_increment <<- y_axis_mod[2] - y_axis_mod[1]
  cht_y_range <<- cht_y_max - cht_y_min
  cht_title_y_placement <<- cht_y_max -1/20*cht_y_range

  
  # 
  # list(y_axis_mod = y_axis_mod
  #      , y_min = y_min
  #      , y_max = y_max
  #      , y_increment = y_increment
  #      , y_range = y_range
  #      , y_title = y_title)
}

set_x_axis <- function(chart_start_date
                       , chart_end_date
                       ){
  cht_start_date <<- chart_start_date
  cht_end_date <<- chart_end_date + (chart_end_date - chart_start_date)/20
  cht_title_x_placement <<- cht_start_date + (cht_end_date - cht_start_date)/2
}


calc_moving_avg <- function(input_data, moving_avg){
  input_data <- input_data %>%
    group_by(name) %>%
    arrange(date) %>%
    mutate(value = rollmean(value, moving_avg ,fill = NA, align='right')) %>%
    ungroup() %>%
    drop_na()
  
  return(input_data)
}
    


set_default_values <- function(
){
  cht_width <<- 8
  cht_height <<- 5 
  cht_axes_font_size <<- 14
  cht_y_axes_unit_size <<- 14
  cht_y_invert <<- F
  cht_title_size <<- 8
  cht_label_size <<- 7 
  cht_legend <<- "none"  
  cht_x_num_labels <<- pretty_breaks(n = 6)
  cht_x_date_format <<- "%b-%y"
  n_ticks <<- 6 
  
  cht_y_axes_unit <<- "add unit"
  cht_title <<- "please add a chart title"
  
  cht_colour_palette <<- as.vector(palette.colors(palette = "Okabe-Ito"))
  cht_note <<- "c. Joel Findlay"
  cht_type <<- "simple"
}

chart_formatting <- function(input_data){
  set_default_values()
  set_y_axis(min(input_data$value), max(input_data$value))
  set_x_axis(min(input_data$date), max(input_data$date))
}



set_chart_defaults <- function(
    input_data = cpi_data
    , cht_y_min = min(input_data$value)
    , cht_y_max = max(input_data$value)
    , cht_y_increment = 2
    , cht_y_invert = F
    , cht_y_axes_unit = "%"
    , cht_y_axes_unit_size = 7
    
    , cht_start_date = min(input_data$value)
    , cht_end_date = max(input_data$value)
    , cht_x_date_format = "%b-%y"
    , cht_x_num_labels = pretty_breaks(n = 6)
    
    , cht_title = "cht_title"
    , cht_title_size = 10
    , cht_title_x_placement
    , cht_title_y_placement 
    
    , cht_width = 8
    , cht_height = 5
    
    , cht_axes_font_size = 7
    , cht_label_size = 7
    
    , cht_legend = "none"
    , cht_colour_palette = as.vector(palette.colors(palette = "Okabe-Ito"))
    
    , cht_note = "c. Joel Findlay"
    , cht_type = "simple"
    
    , horizontal_1 = NULL
    , horizontal_2 = NULL
    , horizontal_shading = NULL
    , vertical_1 = NULL
    , vertical_2 = NULL
) {
  chart_defaults <<- list(
    geom_col(data = input_data %>% filter(plotting == "bar"), aes()),
    geom_line(data = input_data %>% filter(plotting == "line"), aes()),
    geom_point(data = input_data %>% filter(plotting == "scatter"), aes()),
    theme_jf(),
    scale_colour_manual(values = cht_colour_palette),
    scale_fill_manual(values = cht_colour_palette),
    theme(
      legend.position = cht_legend,
      text = element_text(size = cht_axes_font_size),
      plot.title = element_text(size = cht_y_axes_unit_size),
      plot.subtitle = element_text(size = cht_y_axes_unit_size)
    ),
    scale_x_date(
      expand = c(0, 0),
      date_labels = cht_x_date_format,
      breaks = cht_x_num_labels,
      limits = c(cht_start_date, cht_end_date)
    ),
    geom_text(
      aes(x = cht_title_x_placement
          , y = cht_title_y_placement
          , fontface = 2
          , label = cht_title
          ),
      size = cht_title_size,
      colour = "black"
    ),
    labs(title = cht_y_axes_unit
         , subtitle = cht_y_axes_unit
         , caption = cht_note)
    , guides(colour=guide_legend(title="")
             , fill=guide_legend(title=""))
  )
  
  if (cht_y_invert == T){
    chart_defaults <<- c(chart_defaults, 
                         scale_y_reverse(
                           expand = c(0, 0),
                           sec.axis = dup_axis(),
                           breaks = seq(cht_y_max, cht_y_min, by = -cht_y_increment),
                           limits = c(cht_y_max, cht_y_min)
                         )
    )
  } else {
    chart_defaults <<- c(chart_defaults, 
                         scale_y_continuous(
                           expand = c(0, 0),
                           sec.axis = dup_axis(),
                           breaks = seq(cht_y_min, cht_y_max, by = cht_y_increment),
                           limits = c(cht_y_min, cht_y_max)
                         )
    )
  }
  
  if(is.numeric(horizontal_1)==T){
    chart_defaults <<- c(chart_defaults, 
                        geom_hline(yintercept = horizontal_1, color = "grey", linetype = "dashed") 
    )
  }
  
  if(is.numeric(horizontal_2)==T){
    chart_defaults <<- c(chart_defaults, 
                         geom_hline(yintercept = horizontal_2, color = "grey", linetype = "dashed") 
    )
  }
  
  if(length(vertical_1)!=0){
    chart_defaults <<- c(chart_defaults, 
                         geom_segment(aes(x =vertical_1, y = cht_y_min, xend = vertical_1
                                          , yend = cht_title_y_placement - (cht_y_max-cht_title_y_placement)*1.5)
                                      , linetype="dashed", colour = "black") 
    )
  }
  
  if(length(vertical_2)!=0){
    chart_defaults <<- c(chart_defaults, 
                         geom_segment(aes(x =vertical_2, y = cht_y_min, xend = vertical_2
                                          , yend = cht_title_y_placement - (cht_y_max-cht_title_y_placement)*1.5)
                                      , linetype="dashed", colour = "black") 
    )
  }
  
  if(length(horizontal_shading)==2){
    chart_defaults <<- c(chart_defaults, 
                         annotate("rect", xmin = cht_start_date, xmax = cht_end_date
                                  , ymin = horizontal_shading[1], ymax = horizontal_shading[2],
                                  alpha = .1,fill = "blue")
    )
  }
  

}




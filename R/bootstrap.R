`%!in%` <- function(x, y) {
  !("%in%"(x, y))
}

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
library(htmltools)
library(shinyWidgets)
library(DT)

MAX_SERIES <- 4L
APP_DATE_FORMATS <- c("%d-%b", "%b-%y", "%b-%Y", "%m-%Y", "%d-%m-%Y", "%Y")
APP_PALETTES <- c("Okabe-Ito", "Tableau 10", "Set 2", "Dark 3", "R4")
APP_CHART_FONTS <- c(
  "Avenir Next",
  "Helvetica Neue",
  "Segoe UI",
  "Inter",
  "Source Sans 3",
  "IBM Plex Sans",
  "Public Sans",
  "Work Sans",
  "Nunito Sans",
  "Verdana"
)
APP_LEGEND_POSITIONS <- c("bottom", "right", "none")

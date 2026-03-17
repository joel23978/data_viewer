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
  "Plus Jakarta Sans",
  "Inter",
  "DM Sans",
  "Source Sans 3",
  "IBM Plex Sans",
  "Roboto",
  "Libre Franklin",
  "Public Sans",
  "Barlow",
  "Atkinson Hyperlegible",
  "Noto Sans"
)
APP_LEGEND_POSITIONS <- c("bottom", "right", "none")

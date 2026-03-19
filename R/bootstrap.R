`%!in%` <- function(x, y) {
  !("%in%"(x, y))
}

library(shiny)
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)

if (requireNamespace("reticulate", quietly = TRUE)) {
  python_path <- Sys.which("python3")
  if (nzchar(python_path)) {
    Sys.setenv(RETICULATE_PYTHON = python_path)
    try(reticulate::use_python(python_path, required = FALSE), silent = TRUE)
  }
}

if (requireNamespace("ragg", quietly = TRUE)) {
  options(shiny.useragg = TRUE)
}

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
APP_PALETTES <- c("Chartwell", "Okabe-Ito", "Tableau 10", "Set 2", "Dark 3", "R4")
APP_CHART_FONTS <- c(
  "Plus Jakarta Sans",
  "Avenir Next",
  "Avenir",
  "Helvetica Neue",
  "Arial",
  "Verdana",
  "Gill Sans",
  "PT Sans",
  "Trebuchet MS",
  "PT Sans Narrow"
)
APP_LEGEND_POSITIONS <- c("bottom", "right", "none")

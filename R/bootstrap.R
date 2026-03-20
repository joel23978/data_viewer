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

DATA_VIEWER_LAZY_CACHE <- new.env(parent = emptyenv())

data_viewer_timing_enabled <- function() {
  option_flag <- isTRUE(getOption("data_viewer.profile_timing", FALSE))
  env_flag <- tolower(Sys.getenv("DATA_VIEWER_PROFILE_TIMING", "false")) %in% c("1", "true", "yes", "on")
  isTRUE(option_flag || env_flag)
}

data_viewer_timed <- function(label, expr) {
  start_time <- proc.time()[["elapsed"]]
  value <- force(expr)
  elapsed <- proc.time()[["elapsed"]] - start_time

  if (data_viewer_timing_enabled()) {
    message(sprintf("%s: %.3fs", label, elapsed))
  }

  value
}

data_viewer_cache_get <- function(key, value_fun, label = key) {
  if (exists(key, envir = DATA_VIEWER_LAZY_CACHE, inherits = FALSE)) {
    return(get(key, envir = DATA_VIEWER_LAZY_CACHE, inherits = FALSE))
  }

  value <- data_viewer_timed(label, value_fun())
  assign(key, value, envir = DATA_VIEWER_LAZY_CACHE)
  value
}

data_viewer_load_rda <- function(path, object_name = NULL) {
  load_env <- new.env(parent = emptyenv())
  object_names <- load(path, envir = load_env)
  target_name <- object_name

  if (is.null(target_name) || !nzchar(target_name)) {
    target_name <- object_names[[1]]
  }

  get(target_name, envir = load_env, inherits = FALSE)
}

data_viewer_register_active_binding <- function(name, fun, env = .GlobalEnv) {
  if (exists(name, envir = env, inherits = FALSE)) {
    rm(list = name, envir = env)
  }

  makeActiveBinding(name, fun, env)
}

data_viewer_empty_series <- function() {
  tibble::tibble(
    name = character(),
    date = as.Date(character()),
    value = numeric()
  )
}

data_viewer_safe_fetch <- function(provider_label, expr) {
  tryCatch(
    force(expr),
    error = function(error) {
      message(sprintf("%s fetch failed: %s", provider_label, conditionMessage(error)))
      data_viewer_empty_series()
    }
  )
}

data_viewer_has_text <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(FALSE)
  }

  cleaned <- trimws(as.character(value))
  cleaned <- cleaned[!is.na(cleaned)]

  if (length(cleaned) == 0) {
    return(FALSE)
  }

  any(nzchar(cleaned))
}

data_viewer_source <- function(path, label = basename(path)) {
  data_viewer_timed(sprintf("source %s", label), source(path))
}

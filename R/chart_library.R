chart_library_path <- function() {
  getOption("data_viewer.chart_library_path", here::here("data", "chart_library.rds"))
}

empty_chart_library <- function() {
  tibble::tibble(
    chart_id = character(),
    title = character(),
    description = character(),
    source_summary = character(),
    series_count = integer(),
    saved_at = as.POSIXct(character()),
    chart_state = list(),
    data_snapshot = list()
  )
}

ensure_chart_library <- function() {
  path <- chart_library_path()

  if (!file.exists(path)) {
    saveRDS(empty_chart_library(), path)
  }

  invisible(path)
}

read_chart_library <- function() {
  ensure_chart_library()

  library_data <- readRDS(chart_library_path())

  if (!inherits(library_data$saved_at, "POSIXct")) {
    library_data <- library_data %>%
      mutate(saved_at = as.POSIXct(saved_at, tz = Sys.timezone()))
  }

  library_data
}

write_chart_library <- function(chart_library) {
  saveRDS(chart_library, chart_library_path())
  invisible(chart_library)
}

summarise_series_sources <- function(series_specs) {
  active_specs <- Filter(Negate(is.null), series_specs)

  if (length(active_specs) == 0) {
    return("No sources configured")
  }

  active_specs %>%
    vapply(function(spec) spec$source, character(1)) %>%
    unique() %>%
    paste(collapse = ", ")
}

new_chart_record <- function(chart_state, data_snapshot, title = NULL, description = "") {
  chart_title <- title %||% chart_state$style$title
  chart_title <- trimws(chart_title)

  if (!nzchar(chart_title)) {
    chart_title <- paste("Untitled chart", format(Sys.time(), "%Y-%m-%d %H:%M"))
  }

  tibble::tibble(
    chart_id = paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-", sample.int(9999, 1)),
    title = chart_title,
    description = trimws(description %||% ""),
    source_summary = summarise_series_sources(chart_state$series),
    series_count = length(Filter(Negate(is.null), chart_state$series)),
    saved_at = Sys.time(),
    chart_state = list(chart_state),
    data_snapshot = list(data_snapshot)
  )
}

upsert_chart_record <- function(chart_library, chart_record) {
  bind_rows(chart_record, chart_library) %>%
    distinct(chart_id, .keep_all = TRUE) %>%
    arrange(desc(saved_at))
}

delete_chart_record <- function(chart_library, chart_id) {
  chart_library %>%
    filter(chart_id != !!chart_id)
}

filter_chart_library <- function(chart_library, query = "") {
  search_text <- trimws(query %||% "")

  if (!nzchar(search_text)) {
    return(chart_library)
  }

  chart_library %>%
    filter(
      str_detect(str_to_lower(coalesce(title, "")), str_to_lower(search_text)) |
        str_detect(str_to_lower(coalesce(description, "")), str_to_lower(search_text)) |
        str_detect(str_to_lower(coalesce(source_summary, "")), str_to_lower(search_text))
    )
}

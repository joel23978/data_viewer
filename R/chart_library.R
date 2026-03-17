chart_library_path <- function() {
  getOption("data_viewer.chart_library_path", here::here("data", "chart_library.rds"))
}

chart_presentation_library_path <- function() {
  getOption("data_viewer.chart_presentation_library_path", here::here("data", "chart_presentations.rds"))
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

empty_chart_presentation_library <- function() {
  tibble::tibble(
    presentation_id = character(),
    title = character(),
    description = character(),
    chart_ids = list(),
    chart_count = integer(),
    updated_at = as.POSIXct(character())
  )
}

compact_chart_snapshot <- function(data_snapshot) {
  if (is.null(data_snapshot) || !nrow(data_snapshot)) {
    return(tibble::tibble(
      date = integer(),
      value = numeric(),
      name = factor(),
      plotting = factor()
    ))
  }

  data_snapshot %>%
    transmute(
      date = as.integer(as.Date(date)),
      value = as.numeric(value),
      name = factor(as.character(name)),
      plotting = factor(as.character(plotting))
    )
}

restore_chart_snapshot <- function(data_snapshot) {
  if (is.null(data_snapshot) || !nrow(data_snapshot)) {
    return(empty_chart_data())
  }

  date_values <- data_snapshot$date
  if (inherits(date_values, "Date")) {
    restored_dates <- as.Date(date_values)
  } else {
    restored_dates <- as.Date(as.numeric(date_values), origin = "1970-01-01")
  }

  tibble::tibble(
    date = restored_dates,
    value = as.numeric(data_snapshot$value),
    name = as.character(data_snapshot$name),
    plotting = as.character(data_snapshot$plotting)
  ) %>%
    filter(!is.na(date), is.finite(value), nzchar(name), nzchar(plotting))
}

ensure_chart_library <- function() {
  path <- chart_library_path()

  if (!file.exists(path)) {
    saveRDS(empty_chart_library(), path)
  }

  presentation_path <- chart_presentation_library_path()
  if (!file.exists(presentation_path)) {
    saveRDS(empty_chart_presentation_library(), presentation_path)
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

  library_data %>%
    mutate(
      chart_state = lapply(chart_state, normalize_chart_state),
      data_snapshot = lapply(data_snapshot, restore_chart_snapshot)
    )
}

write_chart_library <- function(chart_library) {
  saveRDS(chart_library, chart_library_path())
  invisible(chart_library)
}

read_chart_presentation_library <- function() {
  ensure_chart_library()

  library_data <- readRDS(chart_presentation_library_path())

  if (!inherits(library_data$updated_at, "POSIXct")) {
    library_data <- library_data %>%
      mutate(updated_at = as.POSIXct(updated_at, tz = Sys.timezone()))
  }

  library_data %>%
    mutate(
      chart_ids = lapply(chart_ids, function(ids) as.character(ids %||% character())),
      chart_count = as.integer(chart_count %||% lengths(chart_ids))
    )
}

write_chart_presentation_library <- function(presentation_library) {
  saveRDS(presentation_library, chart_presentation_library_path())
  invisible(presentation_library)
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
  chart_state <- normalize_chart_state(chart_state)
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
    data_snapshot = list(compact_chart_snapshot(data_snapshot))
  )
}

new_chart_presentation_record <- function(title, description = "", chart_ids = character()) {
  presentation_title <- trimws(title %||% "")

  if (!nzchar(presentation_title)) {
    presentation_title <- paste("Untitled presentation", format(Sys.time(), "%Y-%m-%d %H:%M"))
  }

  chart_ids <- unique(as.character(chart_ids %||% character()))

  tibble::tibble(
    presentation_id = paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-p-", sample.int(9999, 1)),
    title = presentation_title,
    description = trimws(description %||% ""),
    chart_ids = list(chart_ids),
    chart_count = length(chart_ids),
    updated_at = Sys.time()
  )
}

upsert_chart_record <- function(chart_library, chart_record) {
  bind_rows(chart_record, chart_library) %>%
    distinct(chart_id, .keep_all = TRUE) %>%
    arrange(desc(saved_at))
}

upsert_chart_presentation_record <- function(presentation_library, presentation_record) {
  bind_rows(presentation_record, presentation_library) %>%
    distinct(presentation_id, .keep_all = TRUE) %>%
    arrange(desc(updated_at))
}

delete_chart_record <- function(chart_library, chart_id) {
  chart_library %>%
    filter(chart_id != !!chart_id)
}

delete_chart_presentation_record <- function(presentation_library, presentation_id) {
  presentation_library %>%
    filter(presentation_id != !!presentation_id)
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

filter_chart_presentation_library <- function(presentation_library, query = "") {
  search_text <- trimws(query %||% "")

  if (!nzchar(search_text)) {
    return(presentation_library)
  }

  presentation_library %>%
    filter(
      str_detect(str_to_lower(coalesce(title, "")), str_to_lower(search_text)) |
        str_detect(str_to_lower(coalesce(description, "")), str_to_lower(search_text))
    )
}

presentation_chart_records <- function(presentation_record, chart_library) {
  if (is.null(presentation_record) || nrow(presentation_record) == 0) {
    return(empty_chart_library())
  }

  ordered_ids <- as.character(presentation_record$chart_ids[[1]] %||% character())

  if (length(ordered_ids) == 0) {
    return(empty_chart_library())
  }

  matched_records <- chart_library %>%
    filter(chart_id %in% ordered_ids)

  ordered_indices <- match(ordered_ids, matched_records$chart_id)
  ordered_indices <- ordered_indices[!is.na(ordered_indices)]

  if (length(ordered_indices) == 0) {
    return(empty_chart_library())
  }

  matched_records[ordered_indices, , drop = FALSE]
}

move_presentation_chart_ids <- function(chart_ids, chart_id, direction = c("up", "down")) {
  direction <- match.arg(direction)
  chart_ids <- as.character(chart_ids %||% character())

  if (length(chart_ids) < 2 || !chart_id %in% chart_ids) {
    return(chart_ids)
  }

  current_index <- match(chart_id, chart_ids)
  swap_index <- if (identical(direction, "up")) current_index - 1 else current_index + 1

  if (swap_index < 1 || swap_index > length(chart_ids)) {
    return(chart_ids)
  }

  swapped_ids <- chart_ids
  swapped_ids[c(current_index, swap_index)] <- swapped_ids[c(swap_index, current_index)]
  swapped_ids
}

update_presentation_charts <- function(presentation_record, chart_ids) {
  updated_record <- presentation_record
  updated_chart_ids <- unique(as.character(chart_ids %||% character()))

  updated_record$chart_ids[[1]] <- updated_chart_ids
  updated_record$chart_count[[1]] <- length(updated_chart_ids)
  updated_record$updated_at[[1]] <- Sys.time()
  updated_record
}

remove_chart_ids_from_presentations <- function(presentation_library, chart_ids) {
  chart_ids <- unique(as.character(chart_ids %||% character()))

  if (length(chart_ids) == 0 || nrow(presentation_library) == 0) {
    return(presentation_library)
  }

  presentation_library %>%
    rowwise() %>%
    mutate(
      chart_ids = list(setdiff(as.character(chart_ids[[1]] %||% character()), !!chart_ids)),
      chart_count = length(chart_ids[[1]]),
      updated_at = Sys.time()
    ) %>%
    ungroup()
}

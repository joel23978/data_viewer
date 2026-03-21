source(here::here("R", "bootstrap.R"))
source(here::here("external_data.R"))
source(here::here("R", "providers.R"))
source(here::here("R", "chart_helpers.R"))
source(here::here("R", "chart_library.R"))

chart_pack_prefix <- "RBA Chart Pack \u2014 "

chart_pack_description <- function(page, detail = NULL) {
  description <- paste(
    "Recreated from the RBA Chart Pack, March 2026 (page", page, ").",
    "Built with app-supported public data sources available in this repo."
  )

  if (!is.null(detail) && nzchar(trimws(detail))) {
    description <- paste(description, trimws(detail))
  }

  description
}

chart_transform <- function(...) {
  modifyList(default_transform_profile(), list(...))
}

rba_series_spec <- function(index, series_id, label, transform_profile = default_transform_profile(), vis_type = "line") {
  normalize_series_spec(list(
    index = index,
    source = "rba",
    rba_series_id = series_id,
    label = label,
    transform_profile = transform_profile,
    vis_type = vis_type
  ))
}

abs_series_spec <- function(index, series_id, label, transform_profile = default_transform_profile(), vis_type = "line") {
  normalize_series_spec(list(
    index = index,
    source = "abs",
    abs_id = series_id,
    label = label,
    transform_profile = transform_profile,
    vis_type = vis_type
  ))
}

analysis_series_spec <- function(index, data, label, cache_key) {
  normalize_series_spec(list(
    index = index,
    source = "analysis_result",
    label = label,
    transform_profile = default_transform_profile(),
    vis_type = "line",
    analysis_result_key = cache_key,
    analysis_result_name = label,
    analysis_data = data
  ))
}

build_chart_state_from_definition <- function(definition) {
  state <- default_builder_state()
  state$date_range <- c(as.Date(definition$date_start), Sys.Date())
  state$series <- rep(list(NULL), MAX_SERIES)

  for (series_index in seq_along(definition$series)) {
    state$series[[series_index]] <- definition$series[[series_index]]
  }

  style <- default_style_settings()
  style$title <- definition$title
  style$subtitle <- definition$subtitle %||% ""
  style$y_axis_label <- definition$y_axis_label %||% "%"
  style$note <- definition$note %||% default_source_note(definition$series)
  style$date_format <- definition$date_format %||% style$date_format
  style$legend <- definition$legend %||% style$legend
  style$export_height <- definition$export_height %||% style$export_height
  state$style <- style

  normalize_chart_state(state)
}

build_record_from_definition <- function(definition) {
  chart_state <- build_chart_state_from_definition(definition)
  payload <- build_chart_data(chart_state)

  if (length(payload$messages) > 0) {
    warning(sprintf("%s: %s", definition$title, paste(unique(payload$messages), collapse = " | ")), call. = FALSE)
  }

  if (nrow(payload$data) == 0) {
    stop(sprintf("%s returned no chart data.", definition$title), call. = FALSE)
  }

  new_chart_record(
    chart_state = chart_state,
    data_snapshot = payload$data,
    title = definition$title,
    description = definition$description,
    chart_kind = "builder"
  )
}

derive_spread_series <- function(label, minuend_spec, subtrahend_spec) {
  minuend <- query_series_data(minuend_spec) %>%
    transmute(date = as.Date(date), lhs = as.numeric(value))
  subtrahend <- query_series_data(subtrahend_spec) %>%
    transmute(date = as.Date(date), rhs = as.numeric(value))

  spread_data <- inner_join(minuend, subtrahend, by = "date") %>%
    transmute(
      date = date,
      value = lhs - rhs,
      name = label
    ) %>%
    arrange(date)

  if (nrow(spread_data) == 0) {
    stop(sprintf("Derived spread series %s has no overlapping data.", label), call. = FALSE)
  }

  spread_data
}

definitions <- local({
  cash_rate_spec <- rba_series_spec(1, "FIRMMCRT", "Cash rate target")
  bond_yield_spec <- rba_series_spec(1, "FCMYGBAG10", "10-year AGS yield")

  list(
    list(
      title = paste0(chart_pack_prefix, "GDP Growth"),
      subtitle = "Quarterly and year-ended growth",
      y_axis_label = "%",
      date_start = "1995-01-01",
      description = chart_pack_description(
        7,
        "Quarterly and year-ended growth are derived from the public RBA GDP level series."
      ),
      series = list(
        rba_series_spec(1, "GGDPCVGDP", "Year-ended", chart_transform(lagged_pct = 4)),
        rba_series_spec(2, "GGDPCVGDP", "Quarterly", chart_transform(lagged_pct = 1))
      )
    ),
    list(
      title = paste0(chart_pack_prefix, "Consumer Price Inflation"),
      subtitle = "Year-ended and quarterly",
      y_axis_label = "%",
      date_start = "1990-01-01",
      description = chart_pack_description(8),
      series = list(
        rba_series_spec(1, "GCPIEITCYP", "Year-ended"),
        rba_series_spec(2, "GCPIEITCQP", "Quarterly")
      )
    ),
    list(
      title = paste0(chart_pack_prefix, "Measures of Underlying Inflation"),
      subtitle = "Year-ended",
      y_axis_label = "%",
      date_start = "1990-01-01",
      description = chart_pack_description(8),
      series = list(
        rba_series_spec(1, "GCPIXVIYP", "CPI excl. volatile items"),
        rba_series_spec(2, "GCPIOCPMWMYP", "Weighted median"),
        rba_series_spec(3, "GCPIOCPMTMYP", "Trimmed mean")
      )
    ),
    list(
      title = paste0(chart_pack_prefix, "Tradable and Non-tradable Inflation"),
      subtitle = "Quarterly collection; adjusted public-source approximation",
      y_axis_label = "%",
      date_start = "2000-01-01",
      description = chart_pack_description(8),
      series = list(
        rba_series_spec(1, "GCPINTXDLYP", "Non-tradables year-ended"),
        rba_series_spec(2, "GCPINTXDLQP", "Non-tradables quarterly"),
        rba_series_spec(3, "GCPITXVIYP", "Tradables excl. volatiles year-ended"),
        rba_series_spec(4, "GCPITXVIQP", "Tradables excl. volatiles quarterly")
      )
    ),
    list(
      title = paste0(chart_pack_prefix, "Household Income and Consumption"),
      subtitle = "Real, year-ended growth",
      y_axis_label = "%",
      date_start = "1990-01-01",
      description = chart_pack_description(
        9,
        "Saving ratio is plotted on the same axis as growth rates because the current builder has no secondary-axis support."
      ),
      series = list(
        rba_series_spec(1, "GGDPECCVPSHY", "Consumption"),
        rba_series_spec(2, "GGDPICHRDIY", "Disposable income"),
        rba_series_spec(3, "GGDPICHDISR", "Saving ratio")
      )
    ),
    list(
      title = paste0(chart_pack_prefix, "Private Dwelling Investment"),
      subtitle = "Chain volume",
      y_axis_label = "$b",
      date_start = "1990-01-01",
      description = chart_pack_description(9),
      series = list(
        rba_series_spec(1, "GGDPECCVPSD", "Total private dwelling investment")
      )
    ),
    list(
      title = paste0(chart_pack_prefix, "Private Dwelling Approvals"),
      subtitle = "Trend",
      y_axis_label = "No.",
      date_start = "1990-01-01",
      description = chart_pack_description(
        9,
        "This library entry recreates the total private dwelling approvals line only; the current local metadata does not expose the detached and higher-density split as cleanly."
      ),
      series = list(
        rba_series_spec(1, "GISDWPRITR", "Private dwelling approvals")
      )
    ),
    list(
      title = paste0(chart_pack_prefix, "Business Investment"),
      subtitle = "Share of nominal GDP",
      y_axis_label = "%",
      date_start = "1990-01-01",
      description = chart_pack_description(11),
      series = list(
        rba_series_spec(1, "GGDPECCVPSBS", "Business investment")
      )
    ),
    list(
      title = paste0(chart_pack_prefix, "Wage Price Index Growth"),
      subtitle = "Quarterly and year-ended",
      y_axis_label = "%",
      date_start = "1998-01-01",
      description = chart_pack_description(15),
      series = list(
        rba_series_spec(1, "GWPIYP", "Year-ended"),
        rba_series_spec(2, "GWPIQP", "Quarterly")
      )
    ),
    list(
      title = paste0(chart_pack_prefix, "Unit Labour Costs Growth"),
      subtitle = "Non-farm, year-ended",
      y_axis_label = "%",
      date_start = "1990-01-01",
      description = chart_pack_description(15),
      series = list(
        rba_series_spec(1, "GNFULCYP", "Unit labour costs"),
        rba_series_spec(2, "GNFAEYP", "Average earnings per hour"),
        rba_series_spec(3, "GNFPROSQP", "Labour productivity per hour")
      )
    ),
    list(
      title = paste0(chart_pack_prefix, "Employment and Participation Rates"),
      subtitle = "Trend",
      y_axis_label = "%",
      date_start = "2000-01-01",
      description = chart_pack_description(16),
      series = list(
        abs_series_spec(1, "A84423135L", "Participation rate"),
        abs_series_spec(2, "A84423138V", "Employment-to-population ratio")
      )
    ),
    list(
      title = paste0(chart_pack_prefix, "Credit Growth"),
      subtitle = "Year-ended public-series approximation",
      y_axis_label = "%",
      date_start = "1990-01-01",
      description = chart_pack_description(
        14,
        "This uses the public year-ended D1 credit growth series rather than the chart pack's six-month annualised presentation."
      ),
      series = list(
        rba_series_spec(1, "DGFACOH12", "Owner-occupier housing"),
        rba_series_spec(2, "DGFACIH12", "Investor housing"),
        rba_series_spec(3, "DGFACB12", "Business")
      )
    ),
    list(
      title = paste0(chart_pack_prefix, "Credit and Broad Money Growth"),
      subtitle = "Year-ended public-series approximation",
      y_axis_label = "%",
      date_start = "1990-01-01",
      description = chart_pack_description(
        14,
        "This uses the public year-ended D1 series rather than the chart pack's six-month annualised credit presentation."
      ),
      series = list(
        rba_series_spec(1, "DGFABM12", "Broad money"),
        rba_series_spec(2, "DGFAC12", "Credit")
      )
    ),
    list(
      title = paste0(chart_pack_prefix, "Current Account Balance"),
      subtitle = "Per cent of nominal GDP",
      y_axis_label = "% GDP",
      date_start = "1990-01-01",
      description = chart_pack_description(
        22,
        "This recreation includes the public trade balance and current account balance series. The net income balance line is not exposed as cleanly in the current local metadata."
      ),
      series = list(
        rba_series_spec(1, "HTBGSCPGDP", "Trade balance"),
        rba_series_spec(2, "HCAGSCPGDP", "Current account balance")
      )
    ),
    list(
      title = paste0(chart_pack_prefix, "RBA Index of Commodity Prices"),
      subtitle = "SDR",
      y_axis_label = "Index",
      date_start = "1990-01-01",
      description = chart_pack_description(20),
      series = list(
        rba_series_spec(1, "GRCPAISSDR", "RBA ICP")
      )
    ),
    list(
      title = paste0(chart_pack_prefix, "Terms of Trade"),
      subtitle = "Goods and services",
      y_axis_label = "Index",
      date_start = "1990-01-01",
      description = chart_pack_description(
        20,
        "This uses the public RBA H1 terms-of-trade series rather than rebasing to a chart-pack-specific reference year."
      ),
      series = list(
        rba_series_spec(1, "GOPITT", "Terms of trade")
      )
    ),
    list(
      title = paste0(chart_pack_prefix, "Australian Cash Rate Target"),
      subtitle = "Monthly average",
      y_axis_label = "%",
      date_start = "1990-01-01",
      description = chart_pack_description(24),
      series = list(
        rba_series_spec(1, "FIRMMCRT", "Cash rate target")
      )
    ),
    list(
      title = paste0(chart_pack_prefix, "Spread between Australian 10-year Bond Yield and the Cash Rate Target"),
      subtitle = "Derived from public RBA monthly series",
      y_axis_label = "ppt",
      date_start = "1990-01-01",
      description = chart_pack_description(25),
      note = "Source: RBA - FCMYGBAG10, FIRMMCRT",
      series = list(
        analysis_series_spec(
          1,
          derive_spread_series(
            "10-year bond yield minus cash rate",
            bond_yield_spec,
            cash_rate_spec
          ),
          "10-year bond yield minus cash rate",
          "rba_chart_pack::spread_10y_minus_cash"
        )
      )
    ),
    list(
      title = paste0(chart_pack_prefix, "Housing Interest Rates"),
      subtitle = "Owner-occupier public-source approximation",
      y_axis_label = "%",
      date_start = "2000-01-01",
      description = chart_pack_description(
        27,
        "This uses public RBA owner-occupier series to approximate the chart pack's new, advertised and outstanding housing loan rates."
      ),
      series = list(
        rba_series_spec(1, "FILRHLBVD", "New variable"),
        rba_series_spec(2, "FILRHLBVS", "Advertised variable"),
        rba_series_spec(3, "FILRSAVR", "Outstanding variable")
      )
    ),
    list(
      title = paste0(chart_pack_prefix, "Australian Business Lending Rates"),
      subtitle = "Average interest rate on credit outstanding",
      y_axis_label = "%",
      date_start = "2019-01-01",
      description = chart_pack_description(28),
      series = list(
        rba_series_spec(1, "FLRBFOSBT", "Small business"),
        rba_series_spec(2, "FLRBFOMBT", "Medium business"),
        rba_series_spec(3, "FLRBFOLBT", "Large business")
      )
    ),
    list(
      title = paste0(chart_pack_prefix, "Australian Dollar Trade-weighted Index"),
      subtitle = "Nominal",
      y_axis_label = "Index",
      date_start = "1990-01-01",
      description = chart_pack_description(
        32,
        "This recreation includes the public nominal trade-weighted index. The real TWI overlay in the chart pack is not exposed as cleanly in the current metadata set."
      ),
      series = list(
        rba_series_spec(1, "FXRTWI", "Nominal TWI")
      )
    ),
    list(
      title = paste0(chart_pack_prefix, "Official Reserve Assets"),
      subtitle = "Public-source approximation",
      y_axis_label = "$b",
      date_start = "2000-01-01",
      description = chart_pack_description(
        32,
        "This uses public A4/A5 series to approximate official reserve assets, foreign currency liquidity and intervention activity."
      ),
      series = list(
        rba_series_spec(1, "ARFXORORA", "Official reserve assets"),
        rba_series_spec(2, "ARFXORNR", "Foreign currency liquidity"),
        rba_series_spec(3, "ARBANFXM", "Intervention transactions")
      )
    )
  )
})

generated_titles <- vapply(definitions, `[[`, character(1), "title")
existing_library <- read_chart_library()
existing_library <- existing_library[!existing_library$title %in% generated_titles, , drop = FALSE]

records <- list()
failures <- character()

for (definition in definitions) {
  message("Building: ", definition$title)
  record <- tryCatch(
    build_record_from_definition(definition),
    error = function(error) {
      failures <<- c(failures, sprintf("%s: %s", definition$title, conditionMessage(error)))
      NULL
    }
  )

  if (!is.null(record)) {
    records[[length(records) + 1L]] <- record
  }
}

updated_library <- Reduce(
  f = upsert_chart_record,
  x = records,
  init = existing_library
)

write_chart_library(updated_library)

message("Saved ", length(records), " chart-pack recreations into ", chart_library_path())

if (length(failures) > 0) {
  message("Failures:")
  for (failure in failures) {
    message(" - ", failure)
  }
}

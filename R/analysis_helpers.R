analysis_series_choices <- function(data) {
  data %>%
    pull(name) %>%
    unique() %>%
    sort()
}

analysis_single_series_data <- function(data, series_name) {
  data %>%
    filter(name == series_name) %>%
    select(date, name, value) %>%
    group_by(date, name) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(date)
}

analysis_pair_data <- function(data, series_x, series_y) {
  data %>%
    filter(name %in% c(series_x, series_y)) %>%
    select(date, name, value) %>%
    group_by(date, name) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = name, values_from = value) %>%
    rename(x = all_of(series_x), y = all_of(series_y)) %>%
    drop_na(x, y) %>%
    arrange(date)
}

rolling_correlation_data <- function(data, series_x, series_y, window = 4) {
  pair_data <- analysis_pair_data(data, series_x, series_y)

  if (nrow(pair_data) < max(3, window)) {
    stop("Not enough overlapping observations for the requested rolling window.", call. = FALSE)
  }

  pair_data %>%
    mutate(
      correlation = zoo::rollapply(
        data = cbind(x, y),
        width = window,
        FUN = function(values) stats::cor(values[, 1], values[, 2], use = "complete.obs"),
        by.column = FALSE,
        fill = NA_real_,
        align = "right"
      )
    ) %>%
    drop_na(correlation)
}

build_correlation_plot <- function(correlation_data, series_x, series_y, window) {
  ggplot(correlation_data, aes(x = date, y = correlation)) +
    geom_hline(yintercept = 0, colour = "#94a3b8", linetype = "dashed") +
    geom_line(linewidth = 0.9, colour = "#2563eb") +
    labs(
      title = paste("Rolling correlation:", series_x, "vs", series_y),
      subtitle = paste("Window:", window, "observations"),
      x = NULL,
      y = "Correlation"
    ) +
    theme_minimal(base_size = 12)
}

robust_vcov <- function(model) {
  sandwich::vcovHC(model, type = "HC3")
}

weighted_regression_model <- function(pair_data) {
  initial_model <- stats::lm(y ~ x, data = pair_data)
  variance_model <- stats::lm(log(pmax(initial_model$residuals ^ 2, 1e-8)) ~ initial_model$fitted.values)
  variance_estimate <- exp(stats::fitted(variance_model))
  stats::lm(y ~ x, data = pair_data, weights = 1 / pmax(variance_estimate, 1e-8))
}

regression_analysis <- function(data, dependent, independent, error_assumption = "classical") {
  pair_data <- analysis_pair_data(data, independent, dependent)

  if (nrow(pair_data) < 5) {
    stop("Not enough overlapping observations to run a regression.", call. = FALSE)
  }

  model <- if (identical(error_assumption, "robust")) {
    weighted_regression_model(pair_data)
  } else {
    stats::lm(y ~ x, data = pair_data)
  }

  coefficient_table <- if (identical(error_assumption, "robust")) {
    lmtest::coeftest(model, vcov. = robust_vcov(model))
  } else {
    lmtest::coeftest(model)
  }

  model_summary <- summary(model)
  model_statistic <- if (identical(error_assumption, "robust")) {
    lmtest::waldtest(model, vcov = robust_vcov(model), test = "Chisq")[["Chisq"]][2]
  } else {
    unname(model_summary$fstatistic[["value"]])
  }

  list(
    model = model,
    data = pair_data,
    coefficients = unclass(coefficient_table) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("term") %>%
      tibble::as_tibble(),
    metrics = list(
      r_squared = model_summary$r.squared,
      adjusted_r_squared = model_summary$adj.r.squared,
      observations = nrow(pair_data),
      model_label = if (identical(error_assumption, "robust")) "Heteroskedastic-adjusted" else "Homoskedastic OLS",
      statistic_label = if (identical(error_assumption, "robust")) "Wald chi-squared" else "F-statistic",
      statistic_value = as.numeric(model_statistic)
    )
  )
}

build_regression_plot <- function(regression_result, dependent, independent) {
  ggplot(regression_result$data, aes(x = x, y = y)) +
    geom_point(size = 2, colour = "#1d4ed8", alpha = 0.8) +
    geom_smooth(method = "lm", se = TRUE, colour = "#0f172a", fill = "#bfdbfe") +
    labs(
      title = paste("Regression:", dependent, "on", independent),
      x = independent,
      y = dependent
    ) +
    theme_minimal(base_size = 12)
}

infer_date_step <- function(dates) {
  if (length(dates) < 2) {
    return("month")
  }

  day_step <- median(as.numeric(diff(sort(dates))), na.rm = TRUE)

  if (day_step >= 360) {
    return("year")
  }

  if (day_step >= 80) {
    return("quarter")
  }

  if (day_step >= 27) {
    return("month")
  }

  if (day_step >= 6) {
    return("week")
  }

  "day"
}

future_dates <- function(dates, horizon) {
  step <- infer_date_step(dates)
  last_date <- max(dates)

  if (identical(step, "quarter")) {
    return(seq(last_date %m+% months(3), by = "3 months", length.out = horizon))
  }

  if (identical(step, "year")) {
    return(seq(last_date %m+% years(1), by = "1 year", length.out = horizon))
  }

  if (identical(step, "month")) {
    return(seq(last_date %m+% months(1), by = "1 month", length.out = horizon))
  }

  if (identical(step, "week")) {
    return(seq(last_date + 7, by = "1 week", length.out = horizon))
  }

  seq(last_date + 1, by = step, length.out = horizon)
}

forecast_order <- function(model_family = "AR", ar_lag = 1, ma_lag = 0) {
  switch(
    model_family,
    "AR" = c(ar_lag, 0, 0),
    "MA" = c(0, 0, ma_lag),
    "ARMA" = c(ar_lag, 0, ma_lag),
    c(ar_lag, 0, ma_lag)
  )
}

minimum_forecast_training_obs <- function(ar_lag = 1, ma_lag = 0) {
  max(8, ar_lag + ma_lag + 4)
}

resolve_forecast_window_size <- function(window_mode, requested_window, available_obs, minimum_obs) {
  if (identical(window_mode, "expanding")) {
    return(available_obs)
  }

  candidate <- suppressWarnings(as.integer(requested_window))

  if (is.na(candidate) || candidate <= 0) {
    candidate <- available_obs
  }

  candidate <- min(candidate, available_obs)

  if (candidate < minimum_obs) {
    stop("The requested estimation window is too short for the selected lag structure.", call. = FALSE)
  }

  candidate
}

forecast_estimation_sample <- function(series_data, window_mode, window_size, history_end) {
  if (history_end < 1) {
    stop("No estimation sample is available.", call. = FALSE)
  }

  if (identical(window_mode, "fixed")) {
    return(series_data[seq_len(window_size), , drop = FALSE])
  }

  if (identical(window_mode, "rolling")) {
    start_index <- max(1, history_end - window_size + 1)
    return(series_data[start_index:history_end, , drop = FALSE])
  }

  series_data[seq_len(history_end), , drop = FALSE]
}

fit_forecast_model <- function(values, model_order) {
  stats::arima(values, order = model_order)
}

single_step_forecast <- function(series_data, holdout_size, window_mode, window_size, model_order) {
  if (holdout_size <= 0) {
    return(tibble::tibble())
  }

  total_obs <- nrow(series_data)
  forecast_rows <- lapply(seq_len(holdout_size), function(step_index) {
    target_index <- total_obs - holdout_size + step_index
    history_end <- target_index - 1
    estimation_sample <- forecast_estimation_sample(series_data, window_mode, window_size, history_end)
    model <- fit_forecast_model(estimation_sample$value, model_order)
    prediction <- stats::predict(model, n.ahead = 1)

    tibble::tibble(
      date = series_data$date[[target_index]],
      actual = series_data$value[[target_index]],
      forecast = as.numeric(prediction$pred[[1]]),
      lower_95 = as.numeric(prediction$pred[[1]] - (1.96 * prediction$se[[1]])),
      upper_95 = as.numeric(prediction$pred[[1]] + (1.96 * prediction$se[[1]])),
      estimation_observations = nrow(estimation_sample)
    )
  })

  bind_rows(forecast_rows) %>%
    mutate(error = actual - forecast)
}

forecast_accuracy_metrics <- function(holdout_data) {
  if (nrow(holdout_data) == 0) {
    return(list(rmse = NA_real_, mae = NA_real_))
  }

  list(
    rmse = sqrt(mean((holdout_data$error) ^ 2, na.rm = TRUE)),
    mae = mean(abs(holdout_data$error), na.rm = TRUE)
  )
}

forecast_analysis <- function(
  data,
  series_name,
  model_family = "AR",
  ar_lag = 1,
  ma_lag = 0,
  horizon = 4,
  window_mode = "expanding",
  window_size = NULL,
  holdout_size = 0
) {
  series_data <- data %>%
    filter(name == series_name) %>%
    arrange(date)

  minimum_obs <- minimum_forecast_training_obs(ar_lag, ma_lag)

  if (nrow(series_data) < minimum_obs) {
    stop("Not enough observations to estimate the requested forecast model.", call. = FALSE)
  }

  holdout_size <- max(0L, min(as.integer(holdout_size %||% 0), nrow(series_data) - minimum_obs))
  available_training_obs <- nrow(series_data) - holdout_size
  if (available_training_obs < minimum_obs) {
    stop("The holdout period leaves too few observations to estimate the model.", call. = FALSE)
  }

  model_order <- forecast_order(model_family, ar_lag, ma_lag)
  estimation_window <- resolve_forecast_window_size(window_mode, window_size, available_training_obs, minimum_obs)

  holdout_forecast <- single_step_forecast(series_data, holdout_size, window_mode, estimation_window, model_order)
  final_window <- if (identical(window_mode, "expanding")) nrow(series_data) else min(estimation_window, nrow(series_data))
  final_sample <- if (identical(window_mode, "expanding")) {
    series_data
  } else {
    tail(series_data, final_window)
  }

  model <- fit_forecast_model(final_sample$value, model_order)
  predictions <- stats::predict(model, n.ahead = horizon)
  forecast_dates <- future_dates(final_sample$date, horizon)

  forecast_table <- tibble::tibble(
    date = forecast_dates,
    forecast = as.numeric(predictions$pred),
    lower_95 = as.numeric(predictions$pred - (1.96 * predictions$se)),
    upper_95 = as.numeric(predictions$pred + (1.96 * predictions$se))
  )

  accuracy_metrics <- forecast_accuracy_metrics(holdout_forecast)

  list(
    model = model,
    history = series_data,
    holdout = holdout_forecast,
    forecast = forecast_table,
    metrics = list(
      estimation_strategy = window_mode,
      estimation_window = final_window,
      holdout_observations = holdout_size,
      training_observations = available_training_obs,
      rmse = accuracy_metrics$rmse,
      mae = accuracy_metrics$mae
    )
  )
}

build_forecast_plot <- function(forecast_result, series_name) {
  history_data <- forecast_result$history %>%
    transmute(date = date, value = value, type = "Observed")

  forecast_data <- forecast_result$forecast %>%
    transmute(date = date, value = forecast, lower_95 = lower_95, upper_95 = upper_95, type = "Forecast")

  holdout_data <- forecast_result$holdout %||% tibble::tibble()
  holdout_actual <- holdout_data %>%
    transmute(date = date, value = actual)
  holdout_forecast <- holdout_data %>%
    transmute(date = date, value = forecast, lower_95 = lower_95, upper_95 = upper_95)

  ggplot() +
    geom_line(data = history_data, aes(x = date, y = value), linewidth = 0.9, colour = "#0f172a") +
    {
      if (nrow(holdout_actual) > 0) {
        list(
          geom_line(data = holdout_actual, aes(x = date, y = value), linewidth = 1, colour = "#059669"),
          geom_line(data = holdout_forecast, aes(x = date, y = value), linewidth = 0.95, colour = "#f97316", linetype = "dashed"),
          geom_point(data = holdout_actual, aes(x = date, y = value), size = 1.8, colour = "#059669")
        )
      }
    } +
    geom_ribbon(
      data = forecast_data,
      aes(x = date, ymin = lower_95, ymax = upper_95),
      alpha = 0.18,
      fill = "#60a5fa"
    ) +
    geom_line(data = forecast_data, aes(x = date, y = value), linewidth = 0.9, colour = "#2563eb") +
    labs(
      title = paste("Forecast for", series_name),
      subtitle = paste(
        stringr::str_to_title(forecast_result$metrics$estimation_strategy),
        "window",
        if (forecast_result$metrics$holdout_observations > 0) {
          paste0("| Holdout: ", forecast_result$metrics$holdout_observations, " observations")
        } else {
          "| No holdout"
        }
      ),
      x = NULL,
      y = "Value"
    ) +
    theme_minimal(base_size = 12)
}

seasonal_adjustment_frequency <- function(dates) {
  step <- infer_date_step(dates)

  if (identical(step, "month")) {
    return(list(
      step = "month",
      frequency = 12L,
      label = "Monthly",
      sequence_by = "1 month",
      minimum_observations = 36L
    ))
  }

  if (identical(step, "quarter")) {
    return(list(
      step = "quarter",
      frequency = 4L,
      label = "Quarterly",
      sequence_by = "3 months",
      minimum_observations = 20L
    ))
  }

  stop("X-13 seasonal adjustment currently supports monthly and quarterly series only.", call. = FALSE)
}

seasonal_adjustment_series <- function(series_data) {
  frequency_info <- seasonal_adjustment_frequency(series_data$date)

  if (nrow(series_data) < frequency_info$minimum_observations) {
    stop("Not enough observations to run X-13 seasonal adjustment for this series.", call. = FALSE)
  }

  period_unit <- if (identical(frequency_info$step, "quarter")) "quarter" else "month"
  normalized_series <- series_data %>%
    mutate(date = lubridate::floor_date(date, unit = period_unit)) %>%
    group_by(date) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(date)

  full_dates <- seq(min(normalized_series$date), max(normalized_series$date), by = frequency_info$sequence_by)
  regular_series <- tibble::tibble(date = full_dates) %>%
    left_join(normalized_series, by = "date")

  if (anyNA(regular_series$value)) {
    stop("The selected series has gaps. X-13 seasonal adjustment needs a continuous monthly or quarterly history.", call. = FALSE)
  }

  start_date <- min(regular_series$date)
  series_start <- if (identical(frequency_info$step, "quarter")) {
    c(lubridate::year(start_date), lubridate::quarter(start_date))
  } else {
    c(lubridate::year(start_date), lubridate::month(start_date))
  }

  ts_data <- stats::ts(
    regular_series$value,
    start = series_start,
    frequency = frequency_info$frequency
  )

  list(
    regular_series = regular_series,
    ts_data = ts_data,
    frequency = frequency_info
  )
}

seasonal_adjustment_analysis <- function(data, series_name, display_mode = "both") {
  if (!requireNamespace("seasonal", quietly = TRUE) || !requireNamespace("x13binary", quietly = TRUE)) {
    stop("The X-13 seasonal-adjustment packages are not installed in this R environment.", call. = FALSE)
  }

  series_data <- analysis_single_series_data(data, series_name)
  if (nrow(series_data) == 0) {
    stop("No data is available for the selected series.", call. = FALSE)
  }

  prepared_series <- seasonal_adjustment_series(series_data)
  seasonal_attempts <- list(
    function(series) seasonal::seas(series),
    function(series) seasonal::seas(series, transform.function = "none"),
    function(series) seasonal::seas(series, x11 = "")
  )

  model <- NULL
  attempt_errors <- character()

  for (attempt in seasonal_attempts) {
    model <- tryCatch(
      attempt(prepared_series$ts_data),
      error = function(error) {
        attempt_errors <<- c(attempt_errors, conditionMessage(error))
        NULL
      }
    )

    if (!is.null(model)) {
      break
    }
  }

  if (is.null(model)) {
    stop(
      paste(
        c(
          "X-13 seasonal adjustment could not be estimated for the selected series.",
          unique(attempt_errors)
        ),
        collapse = " "
      ),
      call. = FALSE
    )
  }

  adjusted_values <- as.numeric(seasonal::final(model))

  adjusted_data <- prepared_series$regular_series %>%
    mutate(
      original = value,
      adjusted = adjusted_values
    )

  plotted_data <- if (identical(display_mode, "adjusted")) {
    adjusted_data %>%
      transmute(date = date, value = adjusted, series = "Seasonally adjusted")
  } else {
    bind_rows(
      adjusted_data %>% transmute(date = date, value = original, series = "Original"),
      adjusted_data %>% transmute(date = date, value = adjusted, series = "Seasonally adjusted")
    )
  }

  list(
    model = model,
    data = plotted_data,
    adjusted_data = adjusted_data,
    metrics = list(
      method = "X-13ARIMA-SEATS",
      frequency = prepared_series$frequency$label,
      observations = nrow(adjusted_data),
      start_date = min(adjusted_data$date, na.rm = TRUE),
      end_date = max(adjusted_data$date, na.rm = TRUE),
      display_mode = display_mode
    )
  )
}

build_seasonal_adjustment_plot <- function(seasonal_result, series_name, display_mode = "both") {
  plotted_data <- seasonal_result$data

  if (identical(display_mode, "adjusted")) {
    return(
      ggplot(plotted_data, aes(x = date, y = value)) +
        geom_line(linewidth = 0.95, colour = "#1d4ed8") +
        labs(
          title = paste("Seasonally adjusted:", series_name),
          subtitle = "X-13ARIMA-SEATS",
          x = NULL,
          y = "Value"
        ) +
        theme_minimal(base_size = 12)
    )
  }

  ggplot(plotted_data, aes(x = date, y = value, colour = series)) +
    geom_line(linewidth = 0.95) +
    scale_colour_manual(
      values = c("Original" = "#94a3b8", "Seasonally adjusted" = "#1d4ed8")
    ) +
    labs(
      title = paste("Seasonal adjustment:", series_name),
      subtitle = "X-13ARIMA-SEATS",
      x = NULL,
      y = "Value",
      colour = NULL
    ) +
    theme_minimal(base_size = 12)
}

filter_display_choices <- function() {
  c(
    "Original vs trend" = "overlay",
    "Trend only" = "trend",
    "Cycle only" = "cycle",
    "Components" = "components"
  )
}

analysis_series_frequency <- function(dates) {
  step <- infer_date_step(dates)

  if (identical(step, "year")) {
    return(list(step = "year", frequency = 1, label = "Annual"))
  }

  if (identical(step, "quarter")) {
    return(list(step = "quarter", frequency = 4, label = "Quarterly"))
  }

  if (identical(step, "month")) {
    return(list(step = "month", frequency = 12, label = "Monthly"))
  }

  if (identical(step, "week")) {
    return(list(step = "week", frequency = 52, label = "Weekly"))
  }

  list(step = step, frequency = 1, label = stringr::str_to_title(step))
}

default_hp_lambda <- function(dates) {
  step <- analysis_series_frequency(dates)$step

  if (identical(step, "year")) {
    return(6.25)
  }

  if (identical(step, "quarter")) {
    return(1600)
  }

  if (identical(step, "month")) {
    return(129600)
  }

  if (identical(step, "week")) {
    return(270400)
  }

  1600
}

hp_trend_two_sided <- function(values, lambda) {
  observation_count <- length(values)

  if (observation_count < 4) {
    stop("Not enough observations to estimate an HP filter.", call. = FALSE)
  }

  difference_matrix <- matrix(0, nrow = observation_count - 2, ncol = observation_count)
  for (row_index in seq_len(observation_count - 2)) {
    difference_matrix[row_index, row_index:(row_index + 2)] <- c(1, -2, 1)
  }

  penalty_matrix <- crossprod(difference_matrix)
  as.numeric(solve(diag(observation_count) + (lambda * penalty_matrix), values))
}

hp_trend_one_sided <- function(values, lambda, minimum_window = 8L) {
  observation_count <- length(values)
  trend_values <- rep(NA_real_, observation_count)

  if (observation_count < minimum_window) {
    stop("Not enough observations to estimate a one-sided HP filter.", call. = FALSE)
  }

  for (end_index in seq.int(minimum_window, observation_count)) {
    trend_values[[end_index]] <- tail(hp_trend_two_sided(values[seq_len(end_index)], lambda), 1)
  }

  trend_values
}

filter_plot_data <- function(component_data, display_mode = "overlay") {
  if (identical(display_mode, "trend")) {
    return(component_data %>%
      transmute(date = date, value = trend, series = "Trend"))
  }

  if (identical(display_mode, "cycle")) {
    return(component_data %>%
      transmute(date = date, value = cycle, series = "Cycle"))
  }

  if (identical(display_mode, "components")) {
    return(bind_rows(
      component_data %>% transmute(date = date, value = trend, series = "Trend", component = "Trend"),
      component_data %>% transmute(date = date, value = cycle, series = "Cycle", component = "Cycle")
    ))
  }

  bind_rows(
    component_data %>% transmute(date = date, value = original, series = "Original"),
    component_data %>% transmute(date = date, value = trend, series = "Trend")
  )
}

hp_filter_analysis <- function(data, series_name, side = "two_sided", display_mode = "overlay", lambda = NULL) {
  series_data <- analysis_single_series_data(data, series_name)

  if (nrow(series_data) < 8) {
    stop("Not enough observations to estimate the HP filter.", call. = FALSE)
  }

  lambda_value <- suppressWarnings(as.numeric(lambda %||% NA_real_))
  if (!is.finite(lambda_value) || lambda_value <= 0) {
    lambda_value <- default_hp_lambda(series_data$date)
  }

  trend_values <- if (identical(side, "one_sided")) {
    hp_trend_one_sided(series_data$value, lambda_value)
  } else {
    hp_trend_two_sided(series_data$value, lambda_value)
  }

  component_data <- series_data %>%
    transmute(
      date = date,
      original = value,
      trend = trend_values,
      cycle = value - trend_values
    ) %>%
    filter(!is.na(trend))

  if (nrow(component_data) == 0) {
    stop("Unable to estimate the HP filter for the selected series.", call. = FALSE)
  }

  list(
    data = filter_plot_data(component_data, display_mode),
    components = component_data,
    metrics = list(
      method = "HP filter",
      side = if (identical(side, "one_sided")) "One-sided" else "Two-sided",
      lambda = lambda_value,
      frequency = analysis_series_frequency(component_data$date)$label,
      observations = nrow(component_data),
      start_date = min(component_data$date, na.rm = TRUE),
      end_date = max(component_data$date, na.rm = TRUE),
      display_mode = display_mode
    )
  )
}

build_hp_filter_plot <- function(filter_result, series_name, side = "two_sided", display_mode = "overlay") {
  plotted_data <- filter_result$data
  subtitle_text <- paste(
    if (identical(side, "one_sided")) "One-sided" else "Two-sided",
    "| lambda:",
    format(round(filter_result$metrics$lambda, 2), trim = TRUE)
  )

  if (identical(display_mode, "components")) {
    return(
      ggplot(plotted_data, aes(x = date, y = value)) +
        geom_line(linewidth = 0.95, colour = "#1d4ed8") +
        facet_wrap(~component, ncol = 1, scales = "free_y") +
        labs(
          title = paste("HP filter:", series_name),
          subtitle = subtitle_text,
          x = NULL,
          y = NULL
        ) +
        theme_minimal(base_size = 12)
    )
  }

  plot_object <- ggplot(plotted_data, aes(x = date, y = value))

  if (identical(display_mode, "overlay")) {
    plot_object <- plot_object +
      geom_line(aes(colour = series), linewidth = 0.95) +
      scale_colour_manual(values = c("Original" = "#94a3b8", "Trend" = "#1d4ed8"))
  } else if (identical(display_mode, "cycle")) {
    plot_object <- plot_object +
      geom_hline(yintercept = 0, colour = "#94a3b8", linetype = "dashed") +
      geom_line(linewidth = 0.95, colour = "#1d4ed8")
  } else {
    plot_object <- plot_object +
      geom_line(linewidth = 0.95, colour = "#1d4ed8")
  }

  plot_object +
    labs(
      title = paste("HP filter:", series_name),
      subtitle = subtitle_text,
      x = NULL,
      y = if (identical(display_mode, "cycle")) "Cycle" else "Value",
      colour = NULL
    ) +
    theme_minimal(base_size = 12)
}

kalman_filter_analysis <- function(data, series_name, side = "two_sided", display_mode = "overlay") {
  series_data <- analysis_single_series_data(data, series_name)

  if (nrow(series_data) < 8) {
    stop("Not enough observations to estimate the Kalman filter.", call. = FALSE)
  }

  frequency_info <- analysis_series_frequency(series_data$date)
  ts_data <- stats::ts(series_data$value, frequency = frequency_info$frequency)

  build_local_trend_model <- function(values) {
    first_difference_variance <- stats::var(as.numeric(diff(values)), na.rm = TRUE)
    second_difference_variance <- stats::var(as.numeric(diff(diff(values))), na.rm = TRUE)
    base_variance <- max(first_difference_variance, stats::var(as.numeric(values), na.rm = TRUE) / 10, 1e-4)
    observation_variance <- max(base_variance * 0.25, 1e-5)
    level_variance <- max(base_variance * 0.05, 1e-6)
    slope_variance <- max(second_difference_variance * 0.025, 1e-7)
    diff_scale <- stats::sd(diff(values), na.rm = TRUE)
    state_scale <- max(diff_scale, stats::sd(values, na.rm = TRUE) / 4, 1e-3)

    list(
      Z = c(1, 0),
      a = c(values[[1]], 0),
      P = diag(state_scale ^ 2, 2),
      T = matrix(c(1, 0, 1, 1), nrow = 2),
      V = diag(c(level_variance, slope_variance), 2),
      h = observation_variance,
      Pn = diag(1e7, 2)
    )
  }
  model_fit <- build_local_trend_model(ts_data)

  trend_values <- if (identical(side, "one_sided")) {
    as.numeric(stats::KalmanRun(ts_data, model_fit)$states[, 1])
  } else {
    as.numeric(stats::KalmanSmooth(ts_data, model_fit)$smooth[, 1])
  }

  component_data <- series_data %>%
    transmute(
      date = date,
      original = value,
      trend = trend_values,
      cycle = value - trend_values
    ) %>%
    filter(!is.na(trend))

  if (nrow(component_data) == 0) {
    stop("Unable to estimate the Kalman filter for the selected series.", call. = FALSE)
  }

  list(
    model = model_fit,
    data = filter_plot_data(component_data, display_mode),
    components = component_data,
    metrics = list(
      method = "Local linear trend",
      side = if (identical(side, "one_sided")) "One-sided" else "Two-sided",
      frequency = frequency_info$label,
      observations = nrow(component_data),
      start_date = min(component_data$date, na.rm = TRUE),
      end_date = max(component_data$date, na.rm = TRUE),
      display_mode = display_mode
    )
  )
}

build_kalman_filter_plot <- function(filter_result, series_name, side = "two_sided", display_mode = "overlay") {
  plotted_data <- filter_result$data
  subtitle_text <- paste(if (identical(side, "one_sided")) "One-sided" else "Two-sided", "| Local linear trend")

  if (identical(display_mode, "components")) {
    return(
      ggplot(plotted_data, aes(x = date, y = value)) +
        geom_line(linewidth = 0.95, colour = "#1d4ed8") +
        facet_wrap(~component, ncol = 1, scales = "free_y") +
        labs(
          title = paste("Kalman filter:", series_name),
          subtitle = subtitle_text,
          x = NULL,
          y = NULL
        ) +
        theme_minimal(base_size = 12)
    )
  }

  plot_object <- ggplot(plotted_data, aes(x = date, y = value))

  if (identical(display_mode, "overlay")) {
    plot_object <- plot_object +
      geom_line(aes(colour = series), linewidth = 0.95) +
      scale_colour_manual(values = c("Original" = "#94a3b8", "Trend" = "#1d4ed8"))
  } else if (identical(display_mode, "cycle")) {
    plot_object <- plot_object +
      geom_hline(yintercept = 0, colour = "#94a3b8", linetype = "dashed") +
      geom_line(linewidth = 0.95, colour = "#1d4ed8")
  } else {
    plot_object <- plot_object +
      geom_line(linewidth = 0.95, colour = "#1d4ed8")
  }

  plot_object +
    labs(
      title = paste("Kalman filter:", series_name),
      subtitle = subtitle_text,
      x = NULL,
      y = if (identical(display_mode, "cycle")) "Cycle" else "Value",
      colour = NULL
    ) +
    theme_minimal(base_size = 12)
}

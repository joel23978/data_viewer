analysis_series_choices <- function(data) {
  data %>%
    pull(name) %>%
    unique() %>%
    sort()
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

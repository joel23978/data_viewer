source(here::here("R", "bootstrap.R"))
source(here::here("external_data.R"))
source(here::here("R", "providers.R"))
source(here::here("R", "chart_helpers.R"))
source(here::here("R", "data_search.R"))
source(here::here("R", "chart_library.R"))
source(here::here("R", "analysis_helpers.R"))
source(here::here("R", "main_app.R"))
source(here::here("tests", "testthat", "helper_app_fixtures.R"))

test_that("analysis helpers produce rolling correlations, regressions, and forecasts", {
  payload <- build_chart_data(build_test_state())
  chart_data <- payload$data
  series_names <- unique(chart_data$name)

  expect_length(series_names, 2)

  rolling <- rolling_correlation_data(chart_data, series_names[[1]], series_names[[2]], window = 4)
  expect_gt(nrow(rolling), 0)
  expect_true(all(c("date", "correlation") %in% colnames(rolling)))

  regression <- suppressWarnings(
    regression_analysis(chart_data, dependent = series_names[[1]], independent = series_names[[2]], error_assumption = "robust")
  )
  expect_true(all(c("term", "Estimate") %in% colnames(regression$coefficients)))
  expect_true(any(grepl("Std", colnames(regression$coefficients))))
  expect_gt(regression$metrics$observations, 5)

  regression_classical <- suppressWarnings(
    regression_analysis(chart_data, dependent = series_names[[1]], independent = series_names[[2]], error_assumption = "classical")
  )
  expect_gt(regression_classical$metrics$observations, 5)
  expect_equal(regression$metrics$model_label, "Heteroskedastic-adjusted")
  expect_equal(regression_classical$metrics$model_label, "Homoskedastic OLS")

  set.seed(123)
  synthetic_data <- tibble::tibble(
    date = rep(seq(as.Date("2020-01-01"), by = "month", length.out = 40), 2),
    name = rep(c("X", "Y"), each = 40),
    value = c(
      seq_len(40),
      2 + (1.5 * seq_len(40)) + stats::rnorm(40, sd = seq(0.5, 6, length.out = 40))
    ),
    plotting = "line"
  )
  synthetic_robust <- regression_analysis(synthetic_data, dependent = "Y", independent = "X", error_assumption = "robust")
  synthetic_classical <- regression_analysis(synthetic_data, dependent = "Y", independent = "X", error_assumption = "classical")
  expect_false(isTRUE(all.equal(synthetic_robust$metrics$r_squared, synthetic_classical$metrics$r_squared)))
  expect_false(isTRUE(all.equal(synthetic_robust$metrics$adjusted_r_squared, synthetic_classical$metrics$adjusted_r_squared)))

  forecast_input <- tibble::tibble(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 48),
    name = "Forecast series",
    value = seq_len(48) + sin(seq_len(48) / 3),
    plotting = "line"
  )

  forecast <- suppressWarnings(
    forecast_analysis(
      forecast_input,
      series_name = "Forecast series",
      model_family = "AR",
      ar_lag = 1,
      ma_lag = 0,
      horizon = 4,
      window_mode = "expanding",
      holdout_size = 4
    )
  )
  expect_equal(nrow(forecast$forecast), 4)
  expect_true(all(c("forecast", "lower_95", "upper_95") %in% colnames(forecast$forecast)))
  expect_equal(nrow(forecast$holdout), 4)
  expect_false(is.na(forecast$metrics$rmse))

  forecast_ma <- suppressWarnings(
    forecast_analysis(
      forecast_input,
      series_name = "Forecast series",
      model_family = "MA",
      ar_lag = 0,
      ma_lag = 2,
      horizon = 3,
      window_mode = "fixed",
      window_size = 24,
      holdout_size = 3
    )
  )
  expect_equal(nrow(forecast_ma$forecast), 3)
  expect_equal(nrow(forecast_ma$holdout), 3)

})

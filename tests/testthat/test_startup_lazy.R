source(here::here("R", "bootstrap.R"))

test_that("provider metadata loads lazily and is cached", {
  withr::local_envvar(DATA_VIEWER_PROFILE_TIMING = "false")

  load_calls <- 0L
  browse_calls <- 0L
  original_load <- base::load

  assign(
    "load",
    function(...) {
      load_calls <<- load_calls + 1L
      original_load(...)
    },
    envir = .GlobalEnv
  )

  assign(
    "browse_rba_series",
    function(...) {
      browse_calls <<- browse_calls + 1L
      tibble::tibble(
        table_no = "A1",
        table_title = "Test table",
        description = "Test description",
        series_id = "TEST_SERIES"
      )
    },
    envir = .GlobalEnv
  )

  on.exit({
    if (exists("load", envir = .GlobalEnv, inherits = FALSE)) {
      rm(list = "load", envir = .GlobalEnv)
    }
    if (exists("browse_rba_series", envir = .GlobalEnv, inherits = FALSE)) {
      rm(list = "browse_rba_series", envir = .GlobalEnv)
    }
  }, add = TRUE)

  data_viewer_timed("source cpi_annual.R", source(here::here("cpi_annual.R")))
  data_viewer_timed("source external_data.R", source(here::here("external_data.R")))

  expect_equal(load_calls, 0L)
  expect_equal(browse_calls, 0L)

  first_rba_browse <- rba_browse_data
  expect_equal(browse_calls, 1L)
  expect_equal(first_rba_browse, rba_browse_data)
  expect_equal(load_calls, 0L)
  expect_equal(browse_calls, 1L)

  first_cpi <- cpi_data_all
  expect_equal(load_calls, 1L)
  expect_equal(browse_calls, 1L)
  expect_identical(first_cpi, cpi_data_all)
  expect_true(all(c("date", "value") %in% names(first_cpi)))
  expect_gt(nrow(first_cpi), 0L)
})

test_that("provider fetch helpers accept explicit empty inputs", {
  empty_helpers <- list(fred_data(), db_data(), rba_data(), abs_data())

  expect_true(all(vapply(empty_helpers, function(x) {
    nrow(x) == 0L && identical(names(x), c("name", "date", "value"))
  }, logical(1))))
})

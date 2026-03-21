source(here::here("R", "bootstrap.R"))
source(here::here("external_data.R"))
source(here::here("R", "providers.R"))
source(here::here("R", "chart_helpers.R"))
source(here::here("R", "data_search.R"))
source(here::here("R", "chart_library.R"))
source(here::here("R", "analysis_helpers.R"))
source(here::here("R", "main_app.R"))
source(here::here("tests", "testthat", "helper_app_fixtures.R"))

apply_test_builder_state <- function(session, apply_builder_state, state) {
  apply_builder_state(normalize_chart_state(state), selected_series_index = 1, navigate_builder = FALSE)
  session$flushReact()
  session$flushReact()
}

render_html <- function(x) {
  paste(as.character(x), collapse = " ")
}

make_executable_script <- function(lines) {
  script_path <- tempfile(pattern = "fake-python-", fileext = ".sh")
  writeLines(lines, script_path, useBytes = TRUE)
  Sys.chmod(script_path, mode = "0755")
  script_path
}

with_mocked_probe_export_python <- function(mock_result, expr) {
  app_env <- environment(pptx_export_control_ui)
  original_probe <- get("probe_export_python", envir = app_env, inherits = FALSE)
  on.exit(assign("probe_export_python", original_probe, envir = app_env), add = TRUE)

  assign(
    "probe_export_python",
    function(required_modules, candidate_paths = NULL) {
      mock_result
    },
    envir = app_env
  )

  force(expr)
}

setup_saved_chart_and_presentation <- function(session, apply_builder_state, state) {
  apply_test_builder_state(session, apply_builder_state, state)
  session$setInputs(
    start_date = as.Date("2024-01-01"),
    end_date = as.Date("2025-12-31"),
    library_title = "Saved chart",
    library_description = "Regression test"
  )
  session$flushReact()
  session$setInputs(save_chart = 1)
  session$flushReact()

  session$setInputs(
    presentation_title = "Saved presentation",
    presentation_description = "Regression test",
    library_table_rows_selected = 1
  )
  session$flushReact()
  session$setInputs(create_presentation = 1)
  session$flushReact()
  session$setInputs(presentation_table_rows_selected = 1)
  session$flushReact()
}

test_that("export python resolver falls back from a stale RETICULATE_PYTHON", {
  fake_python <- make_executable_script(c("#!/bin/sh", "exit 1"))
  fake_python <- normalizePath(fake_python, winslash = "/", mustWork = FALSE)
  real_python <- normalizePath(Sys.which("python3"), winslash = "/", mustWork = FALSE)

  skip_if_not(nzchar(real_python))

  withr::local_envvar(RETICULATE_PYTHON = fake_python)

  result <- suppressWarnings(probe_export_python(c("pptx", "PIL")))

  expect_true(result$available)
  expect_equal(unname(result$python_path), unname(real_python))
  expect_equal(unname(result$tried_paths), c(fake_python, unname(real_python)))
  expect_length(result$missing_modules, 0)
})

test_that("export python resolver reports a failure when no interpreter works", {
  bad_one <- make_executable_script(c("#!/bin/sh", "exit 1"))
  bad_two <- make_executable_script(c("#!/bin/sh", "exit 1"))

  result <- suppressWarnings(probe_export_python(c("pptx", "PIL"), candidate_paths = c(bad_one, bad_two)))

  expect_false(result$available)
  expect_equal(result$python_path, "")
  expect_equal(result$tried_paths, c(bad_one, bad_two))
  expect_equal(sort(result$missing_modules), c("PIL", "pptx"))
  expect_match(result$message, "Python export requires modules:", fixed = TRUE)
  expect_match(result$message, bad_one, fixed = TRUE)
  expect_match(result$message, bad_two, fixed = TRUE)
})

test_that("pptx controls hide when the exporter is unavailable", {
  unavailable_result <- list(
    available = FALSE,
    python_path = "",
    tried_paths = c("/missing/python"),
    missing_modules = c("pptx", "PIL"),
    message = "Python export requires modules: pptx, PIL. Tried: /missing/python."
  )

  with_mocked_probe_export_python(unavailable_result, {
    temp_library <- tempfile(fileext = ".rds")
    temp_presentation_library <- tempfile(fileext = ".rds")
    old_option <- getOption("data_viewer.chart_library_path")
    old_presentation_option <- getOption("data_viewer.chart_presentation_library_path")
    options(data_viewer.chart_library_path = temp_library)
    options(data_viewer.chart_presentation_library_path = temp_presentation_library)
    on.exit(options(data_viewer.chart_library_path = old_option), add = TRUE)
    on.exit(options(data_viewer.chart_presentation_library_path = old_presentation_option), add = TRUE)

    suppressWarnings(shiny::testServer(build_main_server, {
      setup_saved_chart_and_presentation(session, apply_builder_state, build_test_state())

      builder_html <- render_html(output$builder_pptx_control)
      preview_html <- render_html(output$export_preview_pptx_control)
      presentation_html <- render_html(output$presentation_detail_actions)

      expect_match(builder_html, "Python export requires modules:", fixed = TRUE)
      expect_match(preview_html, "Python export requires modules:", fixed = TRUE)
      expect_match(presentation_html, "Python export requires modules:", fixed = TRUE)
      expect_false(grepl("Download PPTX", builder_html, fixed = TRUE))
      expect_false(grepl("Download PPTX", preview_html, fixed = TRUE))
      expect_false(grepl("Download PPTX", presentation_html, fixed = TRUE))
    }))
  })
})

test_that("pptx controls show download buttons when the exporter is available", {
  available_result <- list(
    available = TRUE,
    python_path = unname(Sys.which("python3")),
    tried_paths = unname(Sys.which("python3")),
    missing_modules = character(),
    message = ""
  )

  with_mocked_probe_export_python(available_result, {
    temp_library <- tempfile(fileext = ".rds")
    temp_presentation_library <- tempfile(fileext = ".rds")
    old_option <- getOption("data_viewer.chart_library_path")
    old_presentation_option <- getOption("data_viewer.chart_presentation_library_path")
    options(data_viewer.chart_library_path = temp_library)
    options(data_viewer.chart_presentation_library_path = temp_presentation_library)
    on.exit(options(data_viewer.chart_library_path = old_option), add = TRUE)
    on.exit(options(data_viewer.chart_presentation_library_path = old_presentation_option), add = TRUE)

    suppressWarnings(shiny::testServer(build_main_server, {
      setup_saved_chart_and_presentation(session, apply_builder_state, build_test_state())

      builder_html <- render_html(output$builder_pptx_control)
      preview_html <- render_html(output$export_preview_pptx_control)
      presentation_html <- render_html(output$presentation_detail_actions)

      expect_match(builder_html, "Download PPTX", fixed = TRUE)
      expect_match(preview_html, "Download PPTX", fixed = TRUE)
      expect_match(presentation_html, "Download PPTX", fixed = TRUE)
    }))
  })
})

test_that("save_chart_pptx_with_python writes a real pptx", {
  export_ready <- probe_export_python(c("pptx", "PIL"))
  skip_if_not(export_ready$available)

  temp_png <- tempfile(fileext = ".png")
  temp_pptx <- tempfile(fileext = ".pptx")
  on.exit(unlink(c(temp_png, temp_pptx)), add = TRUE)

  ggplot2::ggsave(
    filename = temp_png,
    plot = ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = wt)) +
      ggplot2::geom_point(),
    width = 6,
    height = 4,
    dpi = 150
  )

  save_chart_pptx_with_python(
    image_files = temp_png,
    pptx_file = temp_pptx,
    slide_title = "Regression test"
  )

  expect_true(file.exists(temp_pptx))
  expect_gt(file.info(temp_pptx)$size, 0)

  pptx_entries <- unzip(temp_pptx, list = TRUE)$Name
  slide_entries <- grep("^ppt/slides/slide[0-9]+\\.xml$", pptx_entries, value = TRUE)

  expect_true("ppt/presentation.xml" %in% pptx_entries)
  expect_equal(length(slide_entries), 2L)
})

# Plan: Session-Scoped FRED API Key

## Problem

`set_fred_api_key()` calls `Sys.setenv()`, which is process-wide. On a public
Shiny Server with concurrent users, user A's key overwrites user B's key. Any
session can read or clobber any other session's key via `Sys.getenv()`.

Additionally, `fred_search_remote()` calls `fredr::fredr_set_key()`, which
also sets a package-level global — same problem.

## Goal

Each Shiny session holds its own FRED key in `session$userData`. Plain
(non-reactive) functions receive the key as an explicit string argument.
No global/process-wide state is written.

---

## Affected Files

| File | Change |
|------|--------|
| `R/data_search.R` | `set_fred_api_key`, `current_fred_api_key`, `fred_search_available`, `fred_search_remote`, `search_fred_series` |
| `R/chart_helpers.R` | `fred_vintage_choice_values`, `fred_vintage_dates`, `fred_data` |
| `R/main_app.R` | Save observer, `show_fred_key_modal`, `fred_search_available` call sites |
| `R/providers.R` | `provider_fred_query_series_history`, `provider_fred_search_remote`, `provider_fred_controls_ui` (via `fred_vintage_choice_values`) |
| `external_data.R` | `fred_data`, `fred_vintage_dates` (if duplicated here — reconcile with chart_helpers.R) |

---

## Approach

### 1. Session storage in main_app.R

In `build_main_server`, initialise the key from the env var (so a server-level
key still works as a pre-loaded default):

```r
session$userData$fred_api_key <- trimws(Sys.getenv("FRED_API_KEY"))
```

### 2. Rewrite set_fred_api_key / current_fred_api_key (R/data_search.R)

```r
# Before
current_fred_api_key <- function() {
  trimws(Sys.getenv("FRED_API_KEY"))
}

set_fred_api_key <- function(api_key) {
  cleaned_key <- trimws(api_key %||% "")
  Sys.setenv(FRED_API_KEY = cleaned_key)
  invisible(cleaned_key)
}

fred_search_available <- function() {
  nzchar(current_fred_api_key())
}
```

```r
# After — session-scoped
current_fred_api_key <- function(session) {
  trimws(session$userData$fred_api_key %||% "")
}

set_fred_api_key <- function(api_key, session) {
  cleaned_key <- trimws(api_key %||% "")
  session$userData$fred_api_key <- cleaned_key
  invisible(cleaned_key)
}

fred_search_available <- function(session) {
  nzchar(current_fred_api_key(session))
}
```

### 3. Thread key as explicit string into pure functions

Functions outside the reactive graph are plain R functions — they can't hold a
`session` reference. Pass the key as a `fred_key` string argument, extracted
from `session$userData` at the Shiny call site.

#### R/data_search.R

```r
# fred_search_remote: stop calling fredr_set_key() globally;
# pass api_key directly to fredr functions instead
fred_search_remote <- function(query, type = "text", fred_key = "") {
  if (!nzchar(fred_key)) return(tibble())
  # fredr functions accept api_key argument directly
  if (type == "text") {
    fredr::fredr_series_search_text(query, api_key = fred_key, ...)
  } else {
    fredr::fredr_series_search_id(query, api_key = fred_key, ...)
  }
}

search_fred_series <- function(query, fred_key = "") {
  if (!nzchar(fred_key)) return(tibble())
  fred_search_remote(query, fred_key = fred_key)
}
```

#### R/chart_helpers.R (and external_data.R if duplicated)

```r
fred_data <- function(series_id, ..., fred_key = "") {
  if (!nzchar(fred_key)) stop("FRED API key required")
  fredr::fredr_series_observations(series_id, ..., api_key = fred_key)
}

fred_vintage_dates <- function(series_id, fred_key = "") {
  if (!nzchar(fred_key)) return(character(0))
  fredr::fredr_series_vintagedates(series_id = series_id, api_key = fred_key)
}

fred_vintage_choice_values <- function(series, fred_key = "") {
  # Replace direct Sys.getenv("FRED_API_KEY") check with fred_key arg
  if (!nzchar(trimws(series)) || !nzchar(fred_key)) return(list())
  available_dates <- fred_vintage_dates(series, fred_key = fred_key)
  ...
}
```

#### R/providers.R

```r
provider_fred_query_series_history <- function(spec, ..., fred_key = "") {
  # Pass fred_key to all four fred_data() call sites
  fred_data(spec$series_id, ..., fred_key = fred_key)
}

provider_fred_search_remote <- function(query, ..., fred_key = "") {
  search_fred_series(query, fred_key = fred_key)
}
```

### 4. Update all call sites in main_app.R

Every place that calls a FRED function needs to extract the key first:

```r
# Pattern: extract once at observer/reactive boundary
fred_key <- current_fred_api_key(session)

# Then pass down:
search_fred_series(query, fred_key = fred_key)
provider_fred_query_series_history(spec, fred_key = fred_key)
fred_vintage_choice_values(series_id, fred_key = fred_key)
```

Key call sites to update (line numbers approximate):

| Site | Current call | Updated call |
|------|-------------|--------------|
| `show_fred_key_modal()` (~1587) | `current_fred_api_key()` | `current_fred_api_key(session)` |
| `observeEvent(input$save_fred_api_key)` (~1654) | `set_fred_api_key(entered_key)` | `set_fred_api_key(entered_key, session)` |
| `fred_search_available()` checks (~1664, 1672, 1684) | `fred_search_available()` | `fred_search_available(session)` |
| FRED search reactive | `search_fred_series(query)` | `search_fred_series(query, fred_key = current_fred_api_key(session))` |
| Provider query call | `provider_fred_query_series_history(spec)` | same + `fred_key = current_fred_api_key(session)` |
| Vintage choice values | `fred_vintage_choice_values(series)` | same + `fred_key = current_fred_api_key(session)` |

### 5. fredr package key: stop using fredr_set_key()

`fredr::fredr_set_key()` writes to `.GlobalEnv` — process-wide. Remove all
calls to it. Instead, pass `api_key` directly to each `fredr::fredr_*()`
call. The fredr package supports this on every function.

```r
# Remove:
fredr::fredr_set_key(fred_key)

# Replace with inline api_key= argument on every fredr call:
fredr::fredr_series_observations(series_id, ..., api_key = fred_key)
fredr::fredr_series_search_text(query, ..., api_key = fred_key)
fredr::fredr_series_vintagedates(series_id = series_id, api_key = fred_key)
```

---

## Migration Notes

- **Server-level default**: If the server has `FRED_API_KEY` in its
  environment (e.g. `/etc/environment`), it is read once on session init
  (`session$userData$fred_api_key <- Sys.getenv("FRED_API_KEY")`). Users
  on a private/internal deployment don't need to enter a key.
- **No behavioural change for single-user local use**: The modal UX is
  identical. Key is still pre-filled from session userData on re-open.
- **Backward compatibility**: Default `fred_key = ""` on all pure functions
  means call sites not yet updated fail gracefully (no data, not a crash).

---

## Test Plan

1. **Isolation test**: Open two browser sessions simultaneously. Enter key K1
   in session 1 and key K2 in session 2. Confirm each session fetches data
   with its own key and neither overwrites the other.
2. **No key path**: Open a fresh session, skip the FRED modal. Confirm FRED
   search returns empty results (not an error) and non-FRED sources still work.
3. **Server default**: Set `FRED_API_KEY=test_key` in the R process environment
   before starting. Confirm `current_fred_api_key(session)` returns it on
   session init without any user input.
4. **Existing tests**: Run `tests/testthat/` suite — no regressions on
   non-FRED functionality.
5. **fredr global state**: After a FRED search, confirm
   `fredr::fredr_get_key()` returns `""` (or whatever it was before the
   search) — i.e. no global side-effect remains.

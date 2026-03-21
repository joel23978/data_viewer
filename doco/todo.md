# data_viewer — Development To-Do

## 1. Data Search

### Performance
- [ ] Profile and optimise ABS search function — currently too slow for interactive use
- [ ] Improve overall table loading speed (lazy load, pagination, or index pre-warming)

### Data quality
- [ ] Ensure all ABS series expose start and end dates in search results and series detail

### UI
- [ ] Rethink type and location filter selectors — evaluate whether current approach scales

---

## 2. Chart Library & Presentations

### Chart library
- [ ] Expand column count in the chart library table (e.g. date range, source, last updated, tags)
- [ ] Audit all existing saved charts — verify they render correctly after recent changes

### Presentations
- [ ] Expand presentation management functionality (reorder slides, rename, duplicate packs)
- [ ] Create default presentation packs for major Australian economic data releases:
  - [ ] CPI (quarterly)
  - [ ] Labour Force (monthly)
  - [ ] National Accounts / GDP (quarterly)
  - [ ] Wage Price Index (quarterly)
  - [ ] Business indicators (quarterly)
  - [ ] RBA cash rate decisions
- [ ] Ensure PowerPoint export works correctly in deployed (server) version of the app
  - Depends on Python virtualenv with `python-pptx` and `kaleido` (see `doco/public_deployment_plan.md` §1.5)

---

## 3. Analysis Tools

### Diagnostic statistics
- [ ] Implement Breusch-Godfrey test for serial correlation
- [ ] Implement White's test for heteroscedasticity
- [ ] Surface test results clearly in the analysis panel (p-value, interpretation)

### Data transformations
- [ ] Enable users to winsorize series (set percentile thresholds, preview effect)

### Advanced analysis
- [ ] Enable Monte Carlo simulation on user-selected series
- [ ] Implement VAR (Vector Autoregression) modelling and IRF visualisation

---

## 4. Visualisation

- [ ] Enable colour-coding of data points by year in scatter/regression views
  - Useful for identifying structural breaks and year-over-year clusters
- [ ] Enable index rebase to a user-selected date
- [ ] Enable stacked bar charts with an optional totals line overlay

---

## 5. Deployment & Infrastructure

- [ ] Implement FRED API key session scoping (see `doco/public_deployment_plan.md` §1.1)
  - Replace `Sys.setenv` / `Sys.getenv` with `session$userData` storage
  - Thread key explicitly through `fred_data()`, `fred_search_remote()`, and all provider calls
  - Seed from `FRED_API_KEY` env var at session start as default

---

## 6. Bug Fixes

- [ ] Chart legend labels apply immediately on each keystroke — debounce input so it only commits on blur or after a pause, preventing the label from getting stuck mid-edit

---

## 7. UI & Content

- [ ] Rewrite About tab

---

## 8. Data Sources

- [ ] Add financial market data via `quantmod` package:
  - [ ] Yahoo Finance (equities, indices, FX, commodities)
  - [ ] Google Finance (where still available)
  - [ ] Alpha Vantage (requires free API key — add key modal similar to FRED)
  - [ ] Integrate into existing source picker and search index

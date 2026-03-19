# Codex first-pass app review

## 1. Concise codebase map

- [/Users/joelfindlay/dev/data_viewer/app.R](/Users/joelfindlay/dev/data_viewer/app.R)
  - Thin entrypoint that `source()`s the full app stack and launches `shinyApp(...)`.
- [/Users/joelfindlay/dev/data_viewer/R/bootstrap.R](/Users/joelfindlay/dev/data_viewer/R/bootstrap.R)
  - Global package loading, constants, font/palette lists, Python setup, and runtime options.
- [/Users/joelfindlay/dev/data_viewer/R/main_app.R](/Users/joelfindlay/dev/data_viewer/R/main_app.R)
  - Main UI and a very large server function.
  - Owns top-level tabs: search, builder, library, about.
  - Owns most reactive coordination, exports, saved-chart/presentation flow, and tab lazy-loading.
- [/Users/joelfindlay/dev/data_viewer/R/chart_helpers.R](/Users/joelfindlay/dev/data_viewer/R/chart_helpers.R)
  - Core builder state model, source-specific series UI, transform logic, chart data building, static chart renderer, Plotly widget renderer, and chart-state restore helpers.
- [/Users/joelfindlay/dev/data_viewer/R/data_search.R](/Users/joelfindlay/dev/data_viewer/R/data_search.R)
  - Search index schema, local search asset loading, token/inverted index logic, recent-series overlay, live FRED/DBnomics search integration, and search result -> series-spec mapping.
- [/Users/joelfindlay/dev/data_viewer/R/analysis_helpers.R](/Users/joelfindlay/dev/data_viewer/R/analysis_helpers.R)
  - Analysis calculations and plotting for correlations, regression, forecast, seasonal adjustment, HP filter, and Kalman filter.
- [/Users/joelfindlay/dev/data_viewer/R/chart_library.R](/Users/joelfindlay/dev/data_viewer/R/chart_library.R)
  - Local chart/presentation library persistence and record shaping.
- [/Users/joelfindlay/dev/data_viewer/external_data.R](/Users/joelfindlay/dev/data_viewer/external_data.R)
  - External provider access and provider metadata objects for Bloomberg, FRED, DBnomics, RBA, and ABS.
- [/Users/joelfindlay/dev/data_viewer/cpi_annual.R](/Users/joelfindlay/dev/data_viewer/cpi_annual.R)
  - Domain-specific CPI transformation logic and CPI data shaping.
- [/Users/joelfindlay/dev/data_viewer/scripts/generate_search_index.R](/Users/joelfindlay/dev/data_viewer/scripts/generate_search_index.R)
  - Offline build step for local search assets.
- [/Users/joelfindlay/dev/data_viewer/www/app.css](/Users/joelfindlay/dev/data_viewer/www/app.css)
  - App-wide styling and most layout/theming behavior.
- [/Users/joelfindlay/dev/data_viewer/tests/testthat/test_main_app.R](/Users/joelfindlay/dev/data_viewer/tests/testthat/test_main_app.R)
  - Broad regression coverage for server flows and helper behavior.

## 2. How the app appears to work

At startup, [app.R](/Users/joelfindlay/dev/data_viewer/app.R) sources a set of script-style modules into the global environment. The UI in [R/main_app.R](/Users/joelfindlay/dev/data_viewer/R/main_app.R) provides four top-level experiences:

- `Data Search`
  - Loads local metadata search assets and combines them with live provider search, mainly FRED.
  - A result can be previewed and converted into a normalized series spec.
- `Chart Builder`
  - The user configures up to four series, date range, transforms, presentation options, and analysis options.
  - Builder state is normalized in [R/chart_helpers.R](/Users/joelfindlay/dev/data_viewer/R/chart_helpers.R), then turned into chart data by `build_chart_data()`.
  - That data is rendered through either a static ggplot path or a Plotly path.
- `Saved Charts`
  - Saves chart state plus data snapshot into local RDS libraries.
  - Supports previews, loading back into the builder, and presentation assembly.
- `About`
  - Static explanatory tab.

The app’s core runtime pattern is:

1. collect UI inputs into a normalized builder state
2. query source data via source-specific functions
3. apply transforms
4. render chart or analysis output
5. optionally save/export/present that output

## 3. Main bottlenecks, weak points, and likely technical debt

### Visible in the code

#### 1. App startup and runtime are still heavily coupled to sourced global scripts

- [app.R](/Users/joelfindlay/dev/data_viewer/app.R)
- [R/bootstrap.R](/Users/joelfindlay/dev/data_viewer/R/bootstrap.R)
- [external_data.R](/Users/joelfindlay/dev/data_viewer/external_data.R)

The app still bootstraps by sourcing many scripts into one global environment. This keeps iteration fast, but it makes startup cost, object lifecycle, and coupling hard to control. It also means import-time side effects matter much more than they would in a modular/package structure.

Example:
- [external_data.R](/Users/joelfindlay/dev/data_viewer/external_data.R) constructs provider metadata objects like `rba_browse_data <- browse_rba_series()` at source time.

Impact:
- startup cost is harder to reason about
- failures in any one provider can compromise app startup
- testing and extension require more full-environment context than they should

#### 2. Main server logic is very large and highly stateful

- [R/main_app.R](/Users/joelfindlay/dev/data_viewer/R/main_app.R#L789)

`build_main_server()` is the orchestration center for nearly everything: lazy tab loading, search, builder state, exports, chart library, presentations, analysis, preview panes, and restore flows. This creates a large reactive dependency graph and raises the risk of loops, fragile restore logic, and “one small fix breaks another workflow” behavior.

Impact:
- responsiveness bugs are more likely
- reasoning about observer order is difficult
- new features will tend to land in the same already-dense file

#### 3. Builder/restore state is powerful but tightly coupled to the UI

- [R/chart_helpers.R](/Users/joelfindlay/dev/data_viewer/R/chart_helpers.R#L1061)
- [R/chart_helpers.R](/Users/joelfindlay/dev/data_viewer/R/chart_helpers.R#L2503)
- [R/main_app.R](/Users/joelfindlay/dev/data_viewer/R/main_app.R#L982)

The normalized builder-state model is a strength. But restore behavior depends on a mix of:
- `restored_state`
- `session$userData$restored_series_specs`
- many source-specific dependency observers
- multiple `observeEvent()` handlers that clear restore state based on later input changes

This is flexible, but brittle. It explains why add-to-builder and source-specific restoration issues are recurring, especially for ABS.

#### 4. Adding a new data source currently cuts across too many files

Visible touch points:
- source access in [external_data.R](/Users/joelfindlay/dev/data_viewer/external_data.R)
- source-specific UI in [R/chart_helpers.R](/Users/joelfindlay/dev/data_viewer/R/chart_helpers.R#L423)
- runtime query dispatch in [R/chart_helpers.R](/Users/joelfindlay/dev/data_viewer/R/chart_helpers.R#L1255)
- search indexing and mapping in [R/data_search.R](/Users/joelfindlay/dev/data_viewer/R/data_search.R)
- source-note formatting in [R/chart_helpers.R](/Users/joelfindlay/dev/data_viewer/R/chart_helpers.R#L42)

This means a “new provider” is not just a data adapter. It is currently:
- provider client
- metadata model
- UI controls
- chart-state serialization
- search integration
- save/export compatibility

That is the biggest extensibility concern in the app.

#### 5. Rendering is in a transitional state

- [R/chart_helpers.R](/Users/joelfindlay/dev/data_viewer/R/chart_helpers.R#L1809)
- [R/chart_helpers.R](/Users/joelfindlay/dev/data_viewer/R/chart_helpers.R#L1994)
- [R/analysis_helpers.R](/Users/joelfindlay/dev/data_viewer/R/analysis_helpers.R#L123)

The builder now has a split between static and Plotly rendering, which is directionally good. But:
- analysis widgets still go through `ggplotly(...)`
- builder and analysis renderers are not yet fully unified
- export behavior still has separate code paths and environment dependencies

Impact:
- parity bugs are expected
- visual consistency is costly to maintain
- export reliability is more fragile than it should be

#### 6. External-data helper design is not very reusable/testable

- [external_data.R](/Users/joelfindlay/dev/data_viewer/external_data.R)

Several helpers still use Shiny `input$...` values as default arguments, for example:
- `fred_data(series = input$fred_series, ...)`
- `db_data(series = input$db_series, ...)`
- `bbg_data(series = input$bloomberg_ticker_1, ...)`

This works in the app, but it is not a good abstraction boundary. It makes helpers harder to reason about outside live Shiny context and increases accidental coupling to UI naming.

#### 7. Search is much improved, but still not cleanly isolated from app concerns

- [R/data_search.R](/Users/joelfindlay/dev/data_viewer/R/data_search.R)

The prebuilt ABS/RBA/CPI search asset approach is a real improvement. But the search layer still mixes:
- index schema
- offline build logic
- runtime cache management
- result classification heuristics
- remote FRED/DBnomics integration
- builder-target mapping

This is workable, but it means the search layer is already doing both search-engine work and application integration work in one place.

#### 8. Persistence model is simple and practical, but could become a scaling limit

- [R/chart_library.R](/Users/joelfindlay/dev/data_viewer/R/chart_library.R)

Using local RDS files for chart and presentation libraries is fine for a single-user/local deployment style. The structure is straightforward and efficient enough for current scale. But writes are coarse-grained, and there is no concurrency or migration discipline beyond best-effort backfilling.

Impact:
- okay for current size
- likely painful if multi-user or larger libraries become important

### Risks / concerns that need validation

#### 1. Import-time external metadata may still be a hidden startup cost

Need validation:
- how expensive is `browse_rba_series()` on cold start in deployment?
- are there other provider metadata loads that hit the network or slow IO at source time?

The code suggests startup sensitivity, but I have not benchmarked each import path.

#### 2. Some analysis paths may still have parity and reactivity issues

- [R/analysis_helpers.R](/Users/joelfindlay/dev/data_viewer/R/analysis_helpers.R)
- [R/main_app.R](/Users/joelfindlay/dev/data_viewer/R/main_app.R)

The code shows analysis features have grown substantially, and some still bridge through legacy rendering patterns. Based on structure alone, I would expect ongoing bugs in:
- renderer parity
- restore/load behavior
- export compatibility

That should be validated by targeted walkthroughs, especially forecast and filter workflows.

#### 3. ABS source-specific restore logic may still be fragile at the edges

The code now clearly tries to preserve restored ABS choices, which is good. But the multi-level dependency chain still looks like a place where edge cases will recur. This should be validated against:
- direct search add-to-builder
- saved-chart load
- changing one upstream ABS selector after restore

## 4. Initial view on scaling / incremental improvement

My first-pass view is:

- **Incremental improvement is still viable.**
- **A full rewrite is not justified yet.**
- **But scaling feature breadth will get painful unless boundaries are improved.**

Why it still supports incremental improvement:
- there is a real normalized builder-state model
- search now has a real offline asset path
- chart-library logic is separated
- analysis logic is separated from main builder transforms
- there is already some movement toward dual renderers

Why scaling will get painful if left as-is:
- source addition is too cross-cutting
- `build_main_server()` is already doing too much
- restore/reactive coordination is complex and fragile
- rendering/export paths are duplicated and partly transitional

So the right first-pass conclusion is:
- **continue incrementally**
- **do not rewrite**
- **but aggressively improve seams where features currently cut across the whole app**

## 5. Short handover notes for another engineer/model

- Start with the current runtime seams, not a rewrite.
- Highest-leverage improvement areas:
  - reduce import-time/provider startup work
  - keep pushing search/index prep offline
  - make provider adapters less UI-coupled
  - make “new source” a clearer contract
  - split main server concerns into smaller units
  - finish the renderer split so Plotly/static responsibilities are explicit
- Most fragile areas:
  - ABS restore/add-to-builder chain
  - analysis render/export parity
  - builder restore state vs live input state
  - PPTX/Plotly export environment dependencies
- Best next validation passes:
  - cold-start profiling by sourced file
  - targeted walkthroughs for ABS add/load
  - renderer parity matrix for builder/analysis/export
  - source-addition spike to see how many files must change today


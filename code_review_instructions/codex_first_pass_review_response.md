# Response to Codex First-Pass Review

Overall this is a well-structured review. Codex correctly identified the three main structural pain points and landed on the right conclusion (incremental improvement, not rewrite). But a few findings need correction or nuance based on what's actually in the code.

## Where Codex got it right

### 1. `build_main_server()` is the biggest structural risk

Confirmed: it starts at line 867 and contains **~108 reactive expressions** (observeEvent, reactive, render* calls) spanning ~3,200 lines to end of file. This is the single largest concentration of complexity. Codex is correct that new features will keep landing here by gravity unless the server is decomposed.

### 2. Source addition is too cross-cutting

Confirmed: adding a new provider requires touching `external_data.R` (client + metadata), `R/chart_helpers.R` (UI controls around line 505, query dispatch around line 1246, source-note at line 46), `R/data_search.R` (search integration), and potentially `R/main_app.R` (restore logic). There's no "add a provider" contract.

### 3. The render split is real and transitional

Confirmed: `build_chart_plot()` (ggplot, line 1801) and `build_chart_widget()` (plotly, line 1986) are parallel implementations sharing `build_chart_data()` and `pretty_axis_breaks()` upstream but diverging completely at render time. Analysis still bridges via `ggplotly()` wrapping at `analysis_helpers.R:124`. Parity drift is structurally inevitable here.

## Where Codex was wrong or overstated

### 4. External data helpers do NOT reference `input$` in defaults

Codex claimed `fred_data(series = input$fred_series, ...)` etc. were default arguments. Checked: the helper functions take explicit parameters, for example `fred_data(series = NULL)` and `db_data(series = NULL)`. This coupling concern is not valid.

### 5. Provider metadata is NOT eagerly loaded at source time

Codex flagged `rba_browse_data <- browse_rba_series()` as an import-time side effect. Checked: the codebase uses `data_viewer_register_active_binding()` (bootstrap.R:91) which creates R active bindings backed by `data_viewer_cache_get()` (bootstrap.R:69) — a lazy cache. Nothing hits the network or does expensive computation until first access. The startup cost concern Codex flagged as "needs validation" is actually already solved.

## Adjusted priority ranking

Given the corrections above, the real priority order for incremental improvement is:

1. **Decompose `build_main_server()`** — this is the load-bearing wall. Split into Shiny modules for search, builder, library, and analysis. The reactive surface area is the primary source of fragility.

2. **Define a provider adapter contract** — make "new source" a pluggable pattern instead of a 5-file scavenger hunt. The fetch functions are already well-isolated; it's the UI controls and query dispatch that need a registry pattern.

3. **Finish the renderer split** — either commit to Plotly everywhere (eliminating `build_chart_plot()` and the `ggplotly()` bridge in analysis) or explicitly maintain both with shared test coverage. The current state guarantees parity drift.

4. ~~Fix input$ coupling in external_data~~ — not a real issue.
5. ~~Fix eager startup loads~~ — already solved via active bindings.

## One thing Codex missed

The **restore logic** (chart_helpers.R:2495 `restore_series_spec()` and line 2593 `restore_chart_state()`) plus `session$userData$restored_series_specs` is a particularly fragile area because it's split between chart_helpers.R and main_app.R with no clear ownership boundary. When the server is decomposed into modules, this restore coordination should become an explicit state-passing contract rather than shared mutable session data.

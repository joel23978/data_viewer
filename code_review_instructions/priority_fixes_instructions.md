# Priority fixes instructions for Codex

## Goal

Do a **targeted first-pass remediation** of the app with a focus on **performance, responsiveness, maintainability, and ease of adding external data sources**.

This is **not** a rewrite. Preserve current behaviour where possible. Prefer the smallest structural changes that create cleaner boundaries and reduce obvious bottlenecks.

## App context

This is a Shiny app with a thin `app.R` entrypoint and a sourced-script architecture. The main runtime currently flows through:

1. collect UI inputs into normalized builder state  
2. fetch source data  
3. apply transforms  
4. render chart/analysis output  
5. optionally save/export/present

Key files:

- `app.R`
- `R/bootstrap.R`
- `R/main_app.R`
- `R/chart_helpers.R`
- `R/data_search.R`
- `R/analysis_helpers.R`
- `R/chart_library.R`
- `external_data.R`
- `scripts/generate_search_index.R`
- `www/app.css`
- `tests/testthat/test_main_app.R`

## What matters most

Prioritise:

1. **startup speed / cold-start cost**
2. **runtime responsiveness in the builder and search flows**
3. **cleaner source/provider boundaries so new external sources are easier to add**
4. **reduced coupling inside the main server and restore flows**
5. **maintainability without breaking existing features**

Do **not** spend time on a full architectural rewrite. Do **not** broaden scope into generic polish unless it supports the priorities above.

## Priority order

### Priority 1 — Reduce startup and import-time work

Investigate and fix expensive work that happens when the app is sourced or booted.

Main concern:
- `external_data.R` appears to build provider metadata at source time, e.g. patterns like `browse_rba_series()` during import.

Tasks:
- Identify all import-time work across sourced files.
- Move expensive provider discovery, metadata loading, or network/IO-dependent work behind lazy functions or cached runtime initialisers.
- Ensure app startup does not fail just because one provider is slow or unavailable.
- Make provider boot failures degrade gracefully where possible.
- Add lightweight timing instrumentation so we can see which sourced files/functions dominate cold start.

Deliverables:
- cold-start profiling notes
- code changes that defer expensive provider setup
- brief explanation of what was moved from import-time to lazy/runtime initialisation

### Priority 2 — Decouple provider adapters from Shiny input state

Some external-data helpers appear to use `input$...` values as default arguments. Remove that pattern.

Tasks:
- Refactor provider helpers so they accept explicit arguments and return predictable objects.
- Prevent data-layer functions from depending on live Shiny `input` names.
- Keep thin UI-to-adapter mapping near the server layer instead.
- Aim for provider helpers to be callable from tests or scripts without a Shiny session.

Deliverables:
- provider helper refactor for the highest-use sources first
- smaller interface contract for each provider adapter
- tests added or updated around the refactored helpers

### Priority 3 — Make “add a new source” require fewer cross-file edits

Right now adding a provider seems to cut across multiple files: provider client, metadata, UI, query dispatch, search integration, notes/formatting, and state restore.

Tasks:
- Identify the current source-extension touchpoints.
- Introduce a clearer provider contract or registry pattern.
- Separate, as much as reasonably possible, these concerns:
  - provider metadata/search config
  - provider query/fetch logic
  - source-specific UI controls
  - result/spec mapping
  - source notes / display formatting
- Reduce hard-coded source branching where possible.

This does **not** need to be a perfect framework. A practical first-pass contract is enough.

Deliverables:
- a short design note describing the new provider seam
- at least one concrete refactor showing fewer touchpoints than before
- identify what still remains cross-cutting after the first pass

### Priority 4 — Split the main server into smaller units

`build_main_server()` is currently doing too much orchestration.

Tasks:
- Extract the most separable concerns into smaller server helpers/modules.
- Focus first on areas with the most reactive density and coordination risk, such as:
  - search flows
  - builder restore/load flows
  - saved chart / presentation flows
  - analysis-related server logic
- Reduce shared mutable state where possible.
- Preserve behaviour; this is not a UI redesign.

Deliverables:
- smaller server sub-functions/modules with clear responsibilities
- reduced complexity in `R/main_app.R`
- brief summary of what was extracted and why

### Priority 5 — Stabilise restore flows, especially ABS

The builder-state model is a strength, but restore logic appears brittle, especially for ABS and source-specific dependent selectors.

Tasks:
- Audit the restore path end-to-end:
  - search result -> add to builder
  - saved chart -> load into builder
  - restored state -> source-specific selectors
- Reduce the number of places where restore state is mutated or cleared indirectly.
- Make restore ordering more explicit.
- Add targeted tests for ABS restore edge cases.

Test these cases explicitly:
- direct search add-to-builder for ABS
- saved-chart load for ABS
- changing an upstream ABS selector after restore
- mixed restored/live input transitions

Deliverables:
- restore-flow fixes
- targeted regression tests
- notes on any remaining edge cases not solved in this pass

### Priority 6 — Clarify rendering boundaries

The app appears mid-transition between static rendering and Plotly rendering, and analysis still relies on legacy `ggplotly(...)` paths.

Tasks:
- Document the current render/export matrix:
  - builder static
  - builder Plotly
  - analysis renderers
  - export paths
- Reduce duplication where practical.
- Make renderer ownership explicit: which code path is authoritative for which outputs.
- Fix obvious parity issues if discovered during refactor, but do not pursue cosmetic perfection.

Deliverables:
- renderer parity matrix
- cleaned-up renderer responsibilities
- small targeted fixes where parity or export issues are obvious

## What not to do

- Do not rewrite the entire app into a package unless a tiny packaging step is needed for one of the priorities.
- Do not rebuild the UI.
- Do not spend time on security review.
- Do not introduce heavy new infrastructure unless clearly justified.
- Do not change user-visible behaviour unnecessarily.

## Expected output format

Please return:

### 1. Findings summary
A concise summary of the most important issues you confirmed in code, grouped by:
- startup/performance
- server/reactivity
- provider extensibility
- restore logic
- rendering/export

### 2. Changes made
List exactly what you changed, file by file.

### 3. Remaining issues
List the important problems that still remain after the first pass.

### 4. Recommended next sequence
Recommend the next 3–5 steps in priority order.

## Working style

- Be pragmatic.
- Prefer high-leverage fixes over broad cleanup.
- Keep diffs readable.
- Add comments only where they clarify a seam or non-obvious choice.
- Where behaviour is uncertain, preserve current behaviour and note the uncertainty.

## Success criteria

A good result will:
- reduce cold-start work
- make provider/data helpers less Shiny-coupled
- reduce the number of places touched when adding a source
- shrink or simplify the main server orchestration surface
- make ABS/restore flows more reliable
- leave the app easier to extend without a rewrite

# Codex first-pass app review brief

## Context
Fill this in before running the review.

- App purpose: A Shiny app for searching, charting, transforming, analysing, saving, and presenting macro and financial time-series data, with especially deep support for Australian CPI and other AU macro data.
- Main user workflows:
  - Search local and live metadata, preview a result, and add it into the chart builder.
  - Build charts from up to four series, apply transforms, and format/export the result.
  - Run analysis workflows such as correlations, regression, forecasting, seasonal adjustment, HP filter, and Kalman filter.
  - Save charts to a local library and assemble/export presentations.
- Current external data sources: ABS CPI/local CPI data, ABS via readabs metadata/data, RBA via readrba, FRED via fredr, DBnomics via rdbnomics, recession shading data from FRED-derived local data.
- Planned future data sources: Not explicitly defined in code comments, but the current architecture implies future additions of further external macro/financial series providers and local metadata-backed sources.
- Known performance pain points:
  - Historically slow Data Search startup and ABS search.
  - Heavy startup caused by global sourcing and metadata loading.
  - Large reactive surface area in the main server and restore logic.
- Known ingestion pain points:
  - Source-specific selection chains, especially ABS.
  - Live provider dependencies and environment requirements (FRED key, Python/Kaleido/PPTX stack).
  - Metadata prep and runtime querying are spread across multiple files.
- Areas that feel hard to extend:
  - Adding a new data source across UI, query, search, save/export, and source-note logic.
  - Keeping static and Plotly rendering paths visually aligned.
  - Managing builder-state restore/update flows without reactive regressions.
- Repo/app path: /Users/joelfindlay/dev/data_viewer
- Entry point(s):
  - app.R
  - R/main_app.R
  - R/chart_helpers.R
  - R/data_search.R
  - R/analysis_helpers.R
  - external_data.R
- Run/test commands:
  - Rscript -e "source('app.R'); cat('app ok\n')"
  - Rscript -e "source('test_app.R'); cat('test app ok\n')"
  - Rscript -e "testthat::test_file('tests/testthat/test_main_app.R', reporter='summary')"
  - Rscript scripts/generate_search_index.R
- Constraints:
  - The app is deployed to shinyapps.io, so runtime/startup costs matter.
  - Some functionality depends on local/external environment setup: FRED API key, Python for Plotly image export and PPTX export.
  - The codebase currently uses sourced scripts rather than Shiny modules/packages.
  - There is active migration tension between static ggplot output and Plotly output.

## Prompt

Please do a first-pass review of this app/codebase.

Assume no prior context. First, understand the app and map the structure. Then identify the main risks to performance, responsiveness, ease of use, external data ingestion, and extensibility to additional data sources.

This is a first pass only. Do not propose a full rewrite plan unless the code strongly justifies it.

Focus on:
- runtime performance and responsiveness
- UX friction
- speed and robustness of external data ingestion
- how easily new external data sources can be added
- architectural or maintenance issues that will block the above

Please return:
1. A concise map of the codebase structure
2. A brief explanation of how the app appears to work
3. The main bottlenecks, weak points, and likely technical debt
4. An initial view on whether the current structure supports incremental improvement or will make scaling painful
5. Short handover notes for another engineer/model

Ground findings in specific files, components, and functions where possible.

Separate clearly between:
- issues directly visible in the code
- risks or concerns that need validation

Flag uncertainty clearly.

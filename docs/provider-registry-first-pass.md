## Provider Registry First Pass

This pass introduces a lightweight provider registry so new sources do not need to thread provider-specific logic through every helper by hand.

### What is in the registry

Each provider entry can supply:

- `id`
- `source`
- `label`
- `controls_ui`
- `register_dependencies`
- `spec_from_input`
- `normalize_spec`
- `query_series_history`
- `restore_controls`
- `default_label`
- `source_note_value`
- `cache_key`
- `search_index_builder`
- `search_remote`
- `search_result_to_spec`
- `search_result_series_id`

### What is migrated now

The registry now owns the live provider seam for:

- `FRED`
- `dbnomics`
- `rba`
- `abs`

That seam now covers, where applicable:

- builder controls
- builder dependency updates
- series spec creation
- spec normalization
- historical data fetch dispatch
- restore-time control updates
- cache key construction
- default legend labels
- source-note values
- local search index construction
- remote search dispatch
- search-result series ID display
- search-result-to-builder restore payloads

### What remains outside the registry for now

The first pass still intentionally leaves these areas in their existing structure:

- `analysis_result`
- presentation/export behavior
- recent search overlay behavior
- search-tab layout and filter-card composition in `R/main_app.R`

### Why this is useful

The registry gives the app one explicit seam for provider behavior without forcing a full package rewrite or Shiny module conversion.

That means future provider work can be more incremental:

- add a provider entry
- wire its fetch and search implementation
- add its source catalog metadata

instead of editing several unrelated files in parallel.

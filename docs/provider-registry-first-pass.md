## Provider Registry First Pass

This pass introduces a lightweight provider registry so new sources do not need to thread provider-specific logic through every helper by hand.

### What is in the registry

Each provider entry can supply:

- `id`
- `label`
- `controls_ui`
- `register_dependencies`
- `spec_from_input`
- `normalize_spec`
- `query_series_history`
- `search_index_builder`
- `search_result_to_spec`

### What is migrated now

`rba` is the proof-of-seam provider in this first pass.

The registry now owns the RBA path for:

- builder controls
- builder dependency updates
- series spec creation
- spec normalization
- historical data fetch dispatch
- local search index construction
- search-result-to-builder restore payloads

### What remains outside the registry for now

The first pass intentionally leaves these areas in their existing structure:

- `ABS CPI`
- `FRED`
- `dbnomics`
- `abs` deep dependent selector logic
- `analysis_result`
- presentation/export behavior
- recent search overlay behavior

### Why this is useful

The registry gives the app one explicit seam for provider behavior without forcing a full package rewrite or Shiny module conversion.

That means future provider work can be more incremental:

- add a provider entry
- wire its fetch and builder helpers
- optionally wire search support

instead of editing several unrelated files in parallel.

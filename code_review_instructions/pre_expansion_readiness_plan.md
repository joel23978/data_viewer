# Pre-Expansion Readiness Plan

## Summary

Against the original refactor plan, the app is not ready to expand data sources yet.

Priorities 1 and 5 are mostly in place, but Priority 3 is still only a first-pass seam and Priority 4 is still incomplete.

The current repo shape makes that clear:

- `R/providers.R` is only `218` lines and only owns `rba`
- `R/main_app.R` is still `4592` lines
- `R/chart_helpers.R` is still `2799` lines
- `R/data_search.R` is still `1127` lines

Before adding more providers, we should finish the seam, clean the search asset path, and harden the acceptance gates.

## What Needs To Happen First

### 1. Finish the provider seam enough that new sources do not require edits across multiple files

Right now the registry in `R/providers.R` is real for `rba`, but `FRED`, `dbnomics`, and much of `abs` still live as ad hoc branches in `R/chart_helpers.R` and `R/data_search.R`.

Before expanding, migrate at least one more non-trivial source, ideally `FRED`, onto the same seam.

Goal:

- a new provider should not require hand-editing builder UI, search indexing, restore payloads, and fetch dispatch in parallel
- `R/providers.R` should become the default place to add provider behavior

### 2. Clean up the search asset path so runtime stays cheap

After CPI removal, the app needed compatibility filtering in `R/data_search.R` so stale CPI rows do not surface from prebuilt search assets.

That works, but before expanding sources we should:

- regenerate the local search assets without CPI
- remove the runtime compatibility shim once the assets are clean
- keep search startup on the prebuilt fast path

Goal:

- local metadata loading should remain a lightweight read, not a rebuild or expensive transformation step

### 3. Make the source catalog a single source of truth

The CPI removal work exposed that source IDs and capabilities still drift across:

- `R/main_app.R`
- `R/chart_helpers.R`
- `R/data_search.R`
- `R/chart_library.R`

Before expansion, source availability should be declared in one place and consumed everywhere else.

Goal:

- source choices, source labels, support status, and search visibility should not be duplicated by hand across the app

### 4. Finish the current server-boundary cleanup enough that new providers do not make `main_app` worse

The extracted helpers in `R/main_app.R` are a good start:

- `init_builder_restore_state()`
- `init_builder_sync_handlers()`
- `init_search_builder_handlers()`
- `init_saved_analysis_restore_handlers()`
- `init_library_preview_handlers()`

But `build_main_server()` is still very large, and expanding sources now would still push more branching into it.

Keep the current approach:

- internal helper extraction
- no Shiny modules in this pass

Goal:

- source-specific builder and search flows should be easier to extend without growing one giant server function

### 5. Keep the test gates dedicated and trustworthy

The right acceptance gates right now are:

- `tests/testthat/test_startup_lazy.R`
- `tests/testthat/test_provider_registry.R`
- `tests/testthat/test_restore_regressions.R`

The broad `tests/testthat/test_main_app.R` file is still too noisy to be the primary signal.

Before expansion:

- continue peeling provider and search regressions out of `test_main_app.R`
- add new-source coverage to the dedicated registry and restore files
- do not let new provider work depend on the legacy all-in-one suite

Goal:

- every new provider should ship with small, trustworthy tests for startup, registry wiring, search mapping, and restore/load behavior

### 6. Decide how unsupported or removed sources should behave in saved charts

CPI removal exposed that this behavior is still not fully explicit.

Before adding more providers, define what should happen when a saved chart references a source that is:

- removed
- unsupported
- unavailable because a package or remote dependency is missing

This decision should be reflected in:

- `R/chart_library.R`
- the load path in `R/main_app.R`

Goal:

- saved-chart behavior should degrade predictably instead of failing silently or surfacing inconsistent UI

## Recommended Sequence

1. Regenerate local search assets without CPI and remove the runtime compatibility shim.
2. Move `FRED` onto the provider seam used by `rba`.
3. Do one more extraction pass in `R/main_app.R` around source-specific builder and search flows.
4. Keep pulling stable provider and search regressions out of `tests/testthat/test_main_app.R`.
5. Define unsupported-source behavior for saved charts before any additional provider is added.

## Bottom Line

Do not expand sources while only `rba` uses the registry and the rest of the app still depends on scattered source-specific branches.

The next data-source pass should begin only after:

- the provider seam is broader than one proof-of-concept provider
- the search asset path is clean and fast
- the current restore and startup gates remain trustworthy

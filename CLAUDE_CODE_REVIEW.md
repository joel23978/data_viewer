# Claude Code Review Instructions

Use this file when reviewing code changes in this repository.

## Goal

Provide a comprehensive review of the change with the primary goal of finding:

- bugs
- regressions
- incorrect assumptions
- missing edge-case handling
- startup or deployment breakages
- performance regressions
- missing or weak test coverage

This is an R/Shiny application. Prioritize whether the app still starts, renders, and behaves correctly over style preferences.

## Review Priorities

Review in this order:

1. App boot and load-order safety
2. User-visible behavior changes or regressions
3. Data/model correctness
4. Server-side reactive correctness
5. Performance and responsiveness
6. Test adequacy
7. Maintainability

## What To Check

### 1. Startup and deployment safety

Look carefully for anything that could break:

- `runApp()`
- `shiny::as.shiny.appobj(".")`
- top-level wrapper files like `app.R`
- bootstrap/source ordering
- path handling from different working directories
- Shiny auto-loading of files in `R/`

Flag any file that executes too much work at source time, especially if it depends on symbols that may not yet be loaded.

### 2. UI regressions

The UI should stay visually and behaviorally the same unless the change explicitly intends otherwise.

Check for regressions in:

- tab structure
- control ids and selected values
- conditional panels
- chart/table output ids
- responsive/mobile behavior
- static assets in `www/`

Call out any renamed input/output ids, changed defaults, or altered labels that could break behavior or downstream references.

### 3. Server/reactive logic

For Shiny server changes, inspect:

- duplicated output ids across files
- undefined variables inside renderers
- invalid reactive dependencies
- missing `req()`/input validation where needed
- code paths that only fail for specific toggle combinations
- inconsistencies between UI choice values and server-side expected values

Be especially careful with:

- custom tax mode
- indexed tax mode
- compare-to-nominal mode
- compare-custom-vs-original mode
- effective tax rate charts
- AWOTE / average earnings multiple paths

### 4. Model and data logic

Check that changes preserve correctness for:

- tax bracket application
- offsets and levies
- CPI indexation
- future assumption handling
- real/nominal conversions
- average earnings multiple calculations
- forecast versus historical segmentation

If the code changes formulas, thresholds, rate logic, or year handling, inspect those carefully and assume regressions are possible.

### 5. Performance

This app is intended to feel responsive on a hosted server.

Call out:

- repeated expensive reactive recomputation
- duplicated dataset building
- unnecessary chart/table recomputation
- eager work at startup
- large inline assets that should be static
- avoidable work inside render functions

### 6. Tests

Check whether the change includes or should include coverage for:

- app boot
- wrapper startup
- changed server branches
- changed UI defaults/ids
- specific regressions being fixed

If a bug fix has no regression test, call that out.

## Expected Review Output

Start with findings, ordered by severity.

For each finding include:

- severity
- file
- exact line or closest relevant lines
- what is wrong
- why it matters
- when possible, a concrete suggestion

After findings, include:

- open questions or assumptions
- residual risks
- testing gaps

If there are no findings, say so explicitly, but still mention any remaining risk areas or untested paths.

## Review Style

- Be direct and specific.
- Prefer concrete behavioral risk over generic style feedback.
- Do not spend time on low-value nits unless they hide a real maintenance or correctness issue.
- Optimize for catching real problems before merge.

## Repo-Specific Commands Worth Considering

If you are able to run checks locally, these are high-value:

```sh
Rscript tests/testthat.R
Rscript -e 'app <- shiny::as.shiny.appobj("."); cat(inherits(app, "shiny.appobj"), "\n")'
```

If reviewing a startup-path change, also verify `app.R` loading behavior.

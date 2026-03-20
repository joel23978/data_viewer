# Data Search Checklist

## Scope

- Build a local metadata index for RBA and ABS.
- Add live FRED metadata search via the FRED API.
- Add live DBnomics metadata search via the `rdbnomics` package.
- Add a dedicated `Data Search` tab to the app.
- Let users push a search result directly into a chart-builder series slot.

## Index Design

- Define a unified schema for every searchable item.
- Store source, title, frequency, start/end dates, summary text, and a builder payload.
- Store a lightweight `type_code` for each result.
- Store a lightweight `location_code` for each result.
- Normalize search text so matching is case-insensitive and token-based.
- Cache the built index in memory so the app does not rebuild it on every interaction.

## Source Coverage

- RBA:
  - Index one row per series description.
  - Store table metadata and frequency from `browse_rba_series()`.
- ABS:
  - Index one row per `series_id`.
  - Store catalogue, table, series, series type, and frequency from `abs_ref`.

## Search UX

- Add a top-level `Data Search` tab.
- Add a search box for free-text queries.
- Support boolean `AND` / `OR` terms.
- Add source and frequency filters.
- Add a broad data-type filter such as `Economic`, `Financial`, and `Other`.
- Add a broad location filter such as `International`, `Australia`, and `States / Cities`.
- Add a FRED search-mode toggle for full-text or series-ID search.
- Add provider and dataset scoping controls for DBnomics search.
- Show results in a sortable table with title, source, frequency, start, end, and summary.
- Show metadata for the selected result in a side panel.
- Add actions to send the selected result to:
  - next empty series
  - Series one
  - Series two
  - Series three
  - Series four

## Builder Integration

- Reuse the existing restore/update flow for series specs.
- Keep payloads source-specific so search results can populate the existing controls directly.
- Switch back to the builder after a result is added.

## Ranking

- Support tokenized keyword search.
- Evaluate boolean query clauses consistently across local and remote results.
- Boost exact title matches.
- Boost exact phrase matches in title and summary.
- Keep ordering deterministic when scores tie.

## Performance

- Build the search index once per app session.
- Avoid remote metadata calls during normal search interactions.
- Cache FRED API search responses by query, mode, and frequency filter.
- Cache DBnomics API search responses by query, provider, and dataset.
- Keep results paged and capped.

## Validation

- Test index construction.
- Test query relevance.
- Test search-result-to-builder wiring.
- Keep app smoke tests green.

## Next Extensions

- Add preview charts for selected search results.
- Add remote metadata adapters for FRED and DBnomics.
- Extend live endpoint search beyond FRED once the pattern is stable.
- Consider richer DBnomics provider and dataset discovery once the scoped search flow is proven.
- Persist the search index to disk for faster startup.
- Add advanced filters for region, units, and transformation family.
- Refine location tagging so ABS state-level series are excluded more precisely.

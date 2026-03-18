# Local Search Index Refresh

Rebuild the precomputed local search assets with:

```bash
Rscript scripts/generate_search_index.R
```

This refreshes:

- `data/local_search_index.rds`
- `data/local_search_token_index.rds`

Use this after updating the ABS, RBA, or CPI metadata inputs that feed the local search index.

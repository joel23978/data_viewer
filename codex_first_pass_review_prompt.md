# Codex first-pass app review brief

## Context
Fill this in before running the review.

- App purpose:
- Main user workflows:
- Current external data sources:
- Planned future data sources:
- Known performance pain points:
- Known ingestion pain points:
- Areas that feel hard to extend:
- Repo/app path:
- Entry point(s):
- Run/test commands:
- Constraints:

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

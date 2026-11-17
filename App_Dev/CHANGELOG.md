# Version tracker — Count on Ewe

Every change to `app.R` gets a version here. Newest first.
MAJOR.MINOR.PATCH: new feature or tab = minor, fix or small change = patch, breaking redesign = major.

How to record a change (in this order):
1. Set `APP_VERSION` in `App_Dev/app.R` (top of file).
2. Add an entry at the top of this file (date, type, what changed and why).
3. Run `Rscript App_Dev/tools/check_version.R` from the project folder; it confirms the two match.

Entry types: Added, Changed, Fixed, Removed.

## 0.1.0 - 2026-11-16
- Added: first working build. Ingests the Jun–Nov 2026 Biomark/P4 handling exports into one
  tidy event table and a per-tag roster; Overview (KPIs, per-session detections, tag × session
  capture heatmap, reported mite results), Tag roster, Capture histories, Map, and a Data Quality
  tab running 13 automated cross-checks.
- Added: four-independent-pens / no-migration model — per-tag pen is provisional from the first
  record and overridable via `data/tag_treatment.csv`; inconsistent session pen labels are flagged
  (Q08) as a recording issue rather than movement.
- Added: release tracking — Pen 1 & Pen 2 recorded as released the evening of 2026-11-13
  (`data/releases.csv`), surfaced in the roster status.

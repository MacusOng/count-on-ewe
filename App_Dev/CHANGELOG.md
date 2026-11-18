# Version tracker — Count on Ewe

Every change to `app.R` gets a version here. Newest first.
MAJOR.MINOR.PATCH: new feature or tab = minor, fix or small change = patch, breaking redesign = major.

How to record a change (in this order):
1. Set `APP_VERSION` in `App_Dev/app.R` (top of file).
2. Add an entry at the top of this file (date, type, what changed and why).
3. Run `Rscript App_Dev/tools/check_version.R` from the project folder; it confirms the two match.

Entry types: Added, Changed, Fixed, Removed.

## 0.9.0 - 2026-11-18
- Changed: the app now starts in Midnight Dark by default. A theme picked in Settings is still remembered
  on that device. The saved-theme key changed, so everyone sees dark once on their next visit.

## 0.8.0 - 2026-11-18
- Added: **mobile beta**. On phones (up to 700 px wide) the page scrolls normally, the tabs become a
  swipeable strip under the header, and a "Filters, clocks & settings" button opens the left panel
  inline, with a "Show results" button at its end. KPI cards sit two across, tables are compact and
  scroll sideways, chart toolbars are hidden so charts do not trap scrolling, and touch targets are larger.
- Changed: the header notice now reads "Mobile version is now available in Beta mode", and the Overview
  shows a short mobile-beta note on phones.
- Changed: charts no longer show the zoom toolbar or capture drags (dragging a chart used to zoom it).

## 0.7.0 - 2026-11-17
- Added: soft synthesized sounds (Web Audio, no files): a gentle tick on clicks and a two-note chime on
  tab changes. A "Soft sounds" switch in Settings turns them off; the choice is remembered per browser.
- Added: "Mobile version not available yet" notice in the header.
- Changed: Levi Oostenbrink is listed as tmixʷ (Wildlife) Biologist, ONA.
- Removed: the "Animate header" switch (the header always animates; reduced-motion settings still pause it).

## 0.6.0 - 2026-11-17
- Added: original bighorn sheep headshot avatars on the Key contacts cards, replacing the initials.
  Each has its own look (curls, glasses, stethoscope, beanie with a fish badge); the eyes blink and
  the ears flick on hover. Drawn as inline SVG, so no image files are needed.

## 0.5.0 - 2026-11-17
- Added: **Overview** tab (first tab): trial at a glance, timeline, three colour-coded biosecurity cards
  (sheep, other wildlife and livestock, staff) with animated ewes and lambs, protective gear by stage,
  and a short "why we do this" note. Content follows the briefing note.
- Added: **Key contacts** tab (names, roles and partners only; no emails or phone numbers).
- Added: trial results stay private. Result cards on the Overview open an "In progress" pop-up. The
  reported mite results table is read only when run locally (never on the public site) and the file is optional.
- Changed: the old Overview is now **Dashboard**. Removed the hard-coded results sentence from the mite card.
- Note: do not copy `reported_mite_results.csv` into `data_public\`.

## 0.4.1 - 2026-11-16
- Added: live clock block at the top of the left panel: UTC date and time to the second, plus live
  clocks for Vancouver, São Paulo, London, Johannesburg, Kuala Lumpur, Sydney and McMurdo (one per
  continent), and a "Developed by Macus Ong" line. Runs in the browser, so no server load.

## 0.4.0 - 2026-11-16
- Added: public mode. On shinyapps.io (or with COE_PUBLIC=1) the CSV download buttons and handlers are
  removed so visitors can view the dashboard but not save the data. Local runs keep the downloads.

## 0.3.0 - 2026-11-16
- Added: two-column left panel (Filters | Settings). Filters: pen location (1-4), mark year, mark month,
  a scan-date range slider that re-fits itself when the mark year or month changes, event type, sessions, reset.
- Added: 8 themes (Midnight Dark, Contrast, Viridis, Viridis Plasma, Desert Sand, Calm, Forest, High Contrast)
  and 10 font faces with a pangram preview; choices are remembered in the browser and applied to plots too.
- Added: 2-inch animated header (ewes and lambs walking and grazing on hill slopes, frolicking lambs, rams
  at the right end, mountains); sun/moon, stars, clouds, fireflies and animation pace follow the theme.
  "Animate header" switch pauses it.
- Changed: raw pen-label filter replaced by the tag-level pen; KPIs, plots, roster, events, map and
  downloads now respect the filters.
- Note: 0.2.0 was not released; version jumps from 0.1.0.

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

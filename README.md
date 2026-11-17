# Count on Ewe

**Status: Beta (v0.1.0)**

A Shiny dashboard for the California bighorn sheep (yilíkʷlxkn) *Psoroptes*
treatment trial at the Okanagan Nation Alliance. It ingests the Biomark/P4
handling-session exports, reconciles them into one mark-recapture history per
animal, and runs automated data-quality checks that surface the real issues in
these files (reused pen labels, re-tagged sheep, a mislabelled export date,
tags that drop out).

Live site: `https://macus-ong.shinyapps.io/count-on-ewe/` *(after first deploy)*

Built following the ONA Shiny App Setup & Shipping Playbook, the same way as the
sockeye-tracker / CSS_FTT / Steelhead dashboards.

## The trial, in one paragraph

Four **independent** captive pens, one treatment arm each, **no migration**
between them: Pen 1 = fluralaner alone, Pen 2 = fluralaner + moxidectin, plus
two further arms. Because animals never move between pens, each PIT tag belongs
to exactly one pen for the whole trial — so where the raw exports record a tag
under different pen labels across sessions, that is a labelling difference, not
movement. Pen 1 and Pen 2 were released to their capture locations on the
evening of **13 Nov 2026**.

## Run it locally

```r
install.packages(c("shiny","bslib","dplyr","tidyr","readr","readxl",
                   "DT","ggplot2","plotly","lubridate","stringr"))
install.packages("leaflet")          # optional — enables the satellite map
# from the project root:
shiny::runApp("App_Dev")
```

No internet needed (system fonts). The app reads `./data` when deployed and
`../data` when you run it from `App_Dev`.

## Folder layout (per the playbook)

```
California Bighorn\            <- project root (your working directory)
├── .gitignore  README.md  LICENSE  VERSION  developer-notes.txt
├── App_Dev\
│   ├── app.R                 <- self-contained: ingest + UI + server
│   ├── CHANGELOG.md
│   ├── count-on-ewe.Rproj
│   ├── tools\                <- check_version.R, deploy_shinyapps.R
│   └── tests\                <- testthat ingest checks
├── data\                     <- full private working data (never committed)
├── data_public\              <- only files approved to go live (empty by default)
├── archive\
└── outputs\  (plots\, tables\)
```

Neither `data\` nor `data_public\` is committed to git (see `.gitignore`).
shinyapps.io gets its copy only through the deploy script's `data_public\` gate.

## Adding a session

1. Drop the Biomark/P4 export (`.csv` or `.xlsx`) into `data\`.
2. Add one row to `data\manifest.csv` (`file, session, session_label, first_day`).

Sessions are ordered by their actual event dates, so order of entry doesn't
matter. A wrong export date goes in `data\corrections.csv`
(`file, from_date, to_date, reason`) — the raw date is kept and the fix is
reported as flag **Q02**.

## Assigning pens + treatment

`data\tag_treatment.csv` is pre-filled with each tag's **provisional** pen
(its first recorded label) in `first_pen_label`. Fill in `trial_pen`,
`treatment`, and `control` with the authoritative assignment; once `trial_pen`
is set the app uses it everywhere instead of the provisional label.

## Data-quality checks (run at every startup — the Data Quality tab)

| ID | Severity | What it catches |
|----|----------|-----------------|
| Q01 | medium | File-name day-of-year ≠ event dates |
| Q02 | high | Dates corrected by rule (the October export) |
| Q03 | low | Records out of chronological order within a file |
| Q04 | low | Release date ≠ event date |
| Q05 | high | Same tag scanned twice in one session |
| Q06 | high | Hex tag code ≠ decimal code in the comment |
| Q07 | medium | Mark events flagged RE-TAGGED (identity chain broken) |
| Q08 | high | Same tag under different pen labels across sessions (labelling, not movement) |
| Q09 | medium | GPS missing or one coordinate reused for several pens |
| Q10 | high | Tags not detected at the latest session and not recorded as released |
| Q11 | medium | Life stage blank for a whole session; length/weight never entered |
| Q12 | info | Tags first seen as "Recapture" (tagged before this dataset) |
| Q13 | medium | Tags without an authoritative pen/treatment assignment |

## Version + deploy

Version lives in three places that must agree: `VERSION`, the `APP_VERSION`
constant in `App_Dev/app.R` (shown in the UI badge), and the top of
`App_Dev/CHANGELOG.md`. `Rscript App_Dev/tools/check_version.R` confirms it.

Deploy with `source("App_Dev/tools/deploy_shinyapps.R")` from the project root —
it uploads only `app.R` plus the files you've placed in `data_public\`, prints
the list, and asks for a typed confirmation first.

## Known data issues (what the checks found)

- **Pen labels are not stable across sessions** (Q08). With independent pens and
  no migration, these are recording differences — e.g. July "Pen 2" = Sep
  "Pen 1". Resolve via `tag_treatment.csv`.
- **Eight June Mark events are re-tags** of sheep whose old tag couldn't be read;
  the previous number isn't recorded, so those individuals can't be linked to
  their earlier history (Q07).
- **The October export date is wrong** and is corrected to 22 Oct (Q02).
- **23 of 35 tags reach the final 13 Nov session** (Pen 1 & 2, then released);
  12 drop out after June/July — likely pens concluded earlier (Q10).
- **Mite results are not in the PIT data** — they come from the trial emails and
  live in `data\reported_mite_results.csv`.

## Next steps

- Fill `tag_treatment.csv` so views can split treatment vs control and show a
  per-pen protection-window timeline.
- Add Pen 1 & 2 release coordinates (and a release map layer) when available.
- The `.xml` P4 exports migrate to P5 in December 2026 — keep exporting CSV/XLSX.


## Left panel (v0.3.0)
Two columns: **Filters** (pen 1-4, mark year, mark month, scan-date slider that re-fits when the
year or month changes, event type, sessions) and **Settings** (8 themes, 10 fonts with a pangram
preview, header animation switch). Theme/font choices are remembered per browser. Fonts load from
Google Fonts when online and fall back to system fonts offline.

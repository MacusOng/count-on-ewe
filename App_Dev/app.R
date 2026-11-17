# =============================================================================
# Count on Ewe  —  California Bighorn Sheep Psoroptes Trial PIT-tag dashboard
# Okanagan Nation Alliance  |  Macus Ong
#
# Self-contained Shiny app (ingest + UI + server in one file, so the
# data_public deploy stages just this file plus the approved data).
#
# Local run:   from the project root -> shiny::runApp("App_Dev")
#              from App_Dev          -> shiny::runApp()
# Data:        reads ./data when deployed, ../data when developing locally.
#              Add a new session: drop its export in data/ and add a row to
#              data/manifest.csv. Nothing else to edit.
# =============================================================================

APP_VERSION <- "0.7.0"          # keep in sync with /VERSION and CHANGELOG.md
APP_STAGE   <- "beta"

# Public mode: true on shinyapps.io (SHINY_PORT / R_CONFIG_ACTIVE=shinyapps) or when COE_PUBLIC=1.
# In public mode the CSV download buttons/handlers are removed so visitors can view
# the dashboard but not save the underlying data. Locally they stay on.
IS_PUBLIC <- nzchar(Sys.getenv("SHINY_PORT")) || identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyapps") ||
  identical(Sys.getenv("COE_PUBLIC"), "1")

suppressPackageStartupMessages({
  library(shiny); library(bslib); library(dplyr); library(tidyr); library(stringr)
  library(readr); library(readxl); library(lubridate); library(DT); library(ggplot2); library(plotly)
})
HAS_LEAFLET <- requireNamespace("leaflet", quietly = TRUE)
if (HAS_LEAFLET) library(leaflet)

TZ <- "America/Vancouver"

# ---- data location ----------------------------------------------------------
resolve_data_dir <- function() {
  for (p in c("data", file.path("..", "data"))) {
    if (dir.exists(p) && file.exists(file.path(p, "manifest.csv"))) return(normalizePath(p))
  }
  stop("Could not find a data/ folder containing manifest.csv (looked in ./data and ../data).")
}

# =============================================================================
# INGEST
# =============================================================================
parse_dt <- function(x) {
  if (inherits(x, "POSIXct")) return(force_tz(x, TZ))
  if (inherits(x, "Date"))    return(as.POSIXct(x, tz = TZ))
  parse_date_time(x, orders = c("mdY IMS p", "mdY HMS", "mdY HM", "Ymd HMS", "Ymd HM"),
                  tz = TZ, quiet = TRUE)
}

hex_to_dec <- function(tag) sprintf("989.%012.0f", strtoi(sub("^.*\\.", "", tag), 16L))

read_export <- function(path) {
  raw <- if (grepl("\\.xlsx$", path, ignore.case = TRUE)) {
    read_excel(path, sheet = 1)
  } else {
    read_csv(path, col_types = cols(.default = "c"), show_col_types = FALSE)
  }
  need <- c("Record#", "PIT Tag", "Event Type", "Text Comments", "Event Date", "Release Date",
            "Raceway/Transect/Tank", "Life Stage", "Latitude", "Longitude", "Length", "Weight")
  for (n in setdiff(need, names(raw))) raw[[n]] <- NA
  raw <- raw[, need]
  keep_dt <- c("Event Date", "Release Date")
  for (n in setdiff(names(raw), keep_dt)) raw[[n]] <- as.character(raw[[n]])
  raw[["Event Date"]]   <- parse_dt(raw[["Event Date"]])
  raw[["Release Date"]] <- parse_dt(raw[["Release Date"]])
  raw
}

load_events <- function(data_dir = resolve_data_dir()) {
  man <- read_csv(file.path(data_dir, "manifest.csv"), col_types = cols(.default = "c"), show_col_types = FALSE)
  cor_path <- file.path(data_dir, "corrections.csv")
  cor <- if (file.exists(cor_path)) read_csv(cor_path, col_types = cols(.default = "c"), show_col_types = FALSE) else
    tibble(file = character(), from_date = character(), to_date = character(), reason = character())

  ev <- bind_rows(lapply(seq_len(nrow(man)), function(i) {
    d <- read_export(file.path(data_dir, man$file[i]))
    d$file <- man$file[i]; d$session <- man$session[i]; d$session_label <- man$session_label[i]
    d
  }))

  ev <- ev |>
    transmute(
      session, session_label, file,
      file_doy = as.integer(str_match(file, "(?i)ona-\\d{4}-(\\d{3})-")[, 2]),
      record = as.integer(`Record#`),
      tag = `PIT Tag`,
      dec_comment = str_extract(`Text Comments`, "989\\.\\d+"),
      event_type = `Event Type`,
      event_dt_raw = `Event Date`,
      release_dt = `Release Date`,
      pen_label = str_extract(`Raceway/Transect/Tank`, "\\d+"),
      life_stage = na_if(`Life Stage`, ""),
      lat = suppressWarnings(as.numeric(Latitude)),
      lon = suppressWarnings(as.numeric(Longitude)),
      comments = `Text Comments`,
      retag = str_detect(coalesce(`Text Comments`, ""), regex("RE-TAGGED", ignore_case = TRUE)),
      pregnant = str_detect(coalesce(`Text Comments`, ""), regex("PREGNANT", ignore_case = TRUE))
    ) |>
    mutate(dec_calc = hex_to_dec(tag), event_dt = event_dt_raw, date_corrected = FALSE)

  for (i in seq_len(nrow(cor))) {
    hit <- ev$file == cor$file[i] & as.Date(ev$event_dt_raw, tz = TZ) == as.Date(cor$from_date[i])
    hit[is.na(hit)] <- FALSE
    shift <- as.integer(as.Date(cor$to_date[i]) - as.Date(cor$from_date[i]))
    ev$event_dt[hit] <- ev$event_dt_raw[hit] + days(shift)
    ev$date_corrected[hit] <- TRUE
  }
  ev$event_date <- as.Date(ev$event_dt, tz = TZ)
  ev |> arrange(event_dt, record)
}

session_order <- function(ev) {
  ev |> group_by(session, session_label) |>
    summarise(first_date = min(event_date), last_date = max(event_date),
              n_records = n(), n_tags = n_distinct(tag),
              n_marked = sum(event_type == "Mark"), n_recaptured = sum(event_type == "Recapture"),
              .groups = "drop") |>
    arrange(first_date) |> mutate(session = factor(session, levels = session))
}

# Four independent pens, no migration: each tag belongs to ONE pen for the
# whole trial, so the per-session pen LABELS (which differ) are a recording
# issue, not movement. We derive a provisional pen from the first record and
# let data/lookup override it (tag_treatment.csv) once the authoritative
# pen/treatment map is supplied.
load_treatment <- function(data_dir, tags) {
  p <- file.path(data_dir, "tag_treatment.csv")
  if (!file.exists(p)) return(NULL)
  read_csv(p, col_types = cols(.default = "c"), show_col_types = FALSE)
}

build_roster <- function(ev, data_dir = resolve_data_dir()) {
  ses <- session_order(ev); lv <- levels(ses$session); last_ses <- tail(lv, 1)
  rel <- {
    p <- file.path(data_dir, "releases.csv")
    if (file.exists(p)) read_csv(p, col_types = cols(.default = "c"), show_col_types = FALSE) else
      tibble(session = character(), release_date = character(), release_time = character(), note = character())
  }
  ev <- ev |> mutate(session = factor(session, levels = lv))
  ord_sess <- function(s) lv[max(as.integer(factor(s, levels = lv)))]

  first <- ev |> arrange(session, event_dt) |> group_by(tag) |>
    summarise(dec = first(dec_calc), first_seen = min(event_date), first_event = first(event_type),
              first_session = as.character(first(session)),
              first_pen_label = first(pen_label[!is.na(pen_label)]),
              retagged_in_dataset = any(retag & event_type == "Mark"),
              ever_pregnant = any(pregnant),
              n_sessions = n_distinct(session), sessions = paste(sort(unique(as.character(session))), collapse = " "),
              last_session = ord_sess(session), last_seen = max(event_date),
              .groups = "drop")
  lab <- ev |> filter(!is.na(pen_label)) |> arrange(session, event_dt) |> group_by(tag) |>
    summarise(last_pen_label = last(pen_label), last_pen_session = as.character(last(session)),
              pen_labels_seen = paste(sort(unique(pen_label)), collapse = "/"),
              n_pen_labels = n_distinct(pen_label), .groups = "drop")

  ro <- first |> left_join(lab, by = "tag")

  # authoritative pen/treatment override, if supplied
  tt <- load_treatment(data_dir, ro$tag)
  if (!is.null(tt) && "trial_pen" %in% names(tt)) {
    ro <- ro |> left_join(tt |> select(any_of(c("tag", "trial_pen", "treatment", "control"))), by = "tag")
  } else {
    ro$trial_pen <- NA_character_; ro$treatment <- NA_character_; ro$control <- NA_character_
  }
  ro <- ro |> mutate(pen_assignment = coalesce(trial_pen, first_pen_label),
                     pen_source = if_else(!is.na(trial_pen), "lookup", "provisional (first record)"))

  # release status
  rel_sess <- rel$session
  ro |> mutate(
    released = last_session %in% rel_sess,
    release_date = rel$release_date[match(last_session, rel$session)],
    status = case_when(
      last_session %in% rel_sess ~ paste0("Released ", release_date),
      last_session == last_ses   ~ paste("Detected at", last_ses),
      TRUE                        ~ paste("Not detected since", last_session)
    )) |>
    arrange(desc(last_session == last_ses), pen_assignment, tag)
}

presence_matrix <- function(ev) {
  lv <- levels(session_order(ev)$session)
  ev |> mutate(session = factor(session, levels = lv)) |>
    group_by(tag, session) |> summarise(event_type = first(event_type), pen_label = first(pen_label), .groups = "drop") |>
    complete(tag, session) |> mutate(event_type = coalesce(event_type, "Not detected"))
}

run_qc <- function(ev, data_dir = resolve_data_dir()) {
  out <- list()
  add <- function(id, severity, check, n, detail) out[[length(out) + 1]] <<- tibble(id, severity, check, n = as.integer(n), detail)

  m <- ev |> group_by(file) |> summarise(doy_name = first(file_doy), first_raw = min(event_dt_raw), last_raw = max(event_dt_raw),
                                         d1 = yday(min(event_dt_raw)), d2 = yday(max(event_dt_raw)), .groups = "drop") |>
    filter(doy_name < d1 | doy_name > d2)
  add("Q01", "medium", "File-name day-of-year does not match event dates", nrow(m),
      if (nrow(m)) paste(sprintf("%s (name day %d, dates %s to %s)", m$file, m$doy_name, as.Date(m$first_raw), as.Date(m$last_raw)), collapse = "; ") else "none")

  fx <- ev |> filter(date_corrected)
  add("Q02", "high", "Event dates corrected by rule (see data/corrections.csv)", nrow(fx),
      if (nrow(fx)) paste0(paste(unique(fx$file), collapse = ", "), " - raw curation date kept in event_dt_raw") else "none")

  ooo <- ev |> arrange(file, record) |> group_by(file) |> filter(event_dt_raw < lag(event_dt_raw)) |> ungroup()
  add("Q03", "low", "Record order not chronological within file", nrow(ooo),
      if (nrow(ooo)) paste(sprintf("%s rec %d (%s)", ooo$file, ooo$record, ooo$tag), collapse = "; ") else "none")

  rel <- ev |> filter(!is.na(release_dt), abs(as.numeric(difftime(release_dt, event_dt_raw, units = "secs"))) > 5)
  add("Q04", "low", "Release date differs from event date", nrow(rel),
      if (nrow(rel)) paste(sprintf("%s rec %d: event %s, release %s", rel$file, rel$record, format(rel$event_dt_raw), format(rel$release_dt)), collapse = "; ") else "none")

  dup <- ev |> count(session, tag) |> filter(n > 1)
  add("Q05", "high", "Tag scanned more than once within a session", nrow(dup), if (nrow(dup)) paste(dup$session, dup$tag, collapse = "; ") else "none")

  hx <- ev |> filter(!is.na(dec_comment), dec_comment != dec_calc)
  add("Q06", "high", "Hex tag code does not convert to the decimal code in comments", nrow(hx), if (nrow(hx)) paste(hx$tag, collapse = "; ") else "none")

  rt <- ev |> filter(retag, event_type == "Mark")
  add("Q07", "medium", "Mark events flagged RE-TAGGED: identity chain broken (old tag ID not recorded)", nrow(rt),
      "Sheep re-tagged because no tag was detected; the previous tag number is unknown, so these may duplicate earlier IDs")

  # Four independent pens, no migration -> inconsistent labels are a recording issue
  drift <- ev |> filter(!is.na(pen_label)) |> group_by(tag) |> filter(n_distinct(pen_label) > 1) |> ungroup()
  add("Q08", "high", "Same tag recorded under different pen labels across sessions", n_distinct(drift$tag),
      "Pens are independent with no migration, so an individual cannot change pen. These are labelling differences (e.g. Jul 'Pen 2' = Sep 'Pen 1'). Assign the true pen per tag in data/tag_treatment.csv")

  nog <- ev |> group_by(session) |> summarise(missing = sum(is.na(lat)), n = n(), .groups = "drop") |> filter(missing > 0)
  add("Q09", "medium", "GPS missing or one coordinate reused for several pens", nrow(nog),
      if (nrow(nog)) paste0(nog$session, " missing ", nog$missing, "/", nog$n, collapse = "; ") else "none")

  ro <- build_roster(ev, data_dir); last_ses <- tail(levels(session_order(ev)$session), 1)
  lost <- ro |> filter(!released, last_session != last_ses)
  add("Q10", "high", "Tags not detected at the latest session and not recorded as released", nrow(lost),
      if (nrow(lost)) paste0(nrow(lost), " tags; last seen: ", paste(names(table(lost$last_session)), table(lost$last_session), sep = " x", collapse = ", "),
             ". With 4 independent pens, these are likely pens concluded earlier; confirm fate per tag") else "none")

  blank <- ev |> group_by(session) |> summarise(ls = mean(is.na(life_stage)), .groups = "drop") |> filter(ls == 1)
  add("Q11", "medium", "Life stage blank for whole session; length and weight empty in every file", nrow(blank),
      paste0("Life stage blank in: ", paste(blank$session, collapse = ", "), ". Weight, length, mark temperature never entered"))

  fr <- ro |> filter(first_event == "Recapture")
  add("Q12", "info", "Tags first seen as 'Recapture' (tagged before the Jun 2026 export)", nrow(fr), "Expected; earlier tagging history is not in this dataset")

  nopen <- ro |> filter(is.na(trial_pen))
  add("Q13", "medium", "Tags without an authoritative pen/treatment assignment", nrow(nopen),
      if (nrow(nopen) == nrow(ro)) "data/tag_treatment.csv not filled yet; app is using provisional first-record pen labels" else paste(nopen$tag, collapse = "; "))

  bind_rows(out)
}

# =============================================================================
# LOAD
# =============================================================================
DATA_DIR  <- resolve_data_dir()
ev        <- load_events(DATA_DIR)
sessions  <- session_order(ev)
roster    <- build_roster(ev, DATA_DIR)
presence  <- presence_matrix(ev)
qc        <- run_qc(ev, DATA_DIR)
# Trial results are private: the mite table is read only when running locally and the file exists.
MITES_FILE <- file.path(DATA_DIR, "reported_mite_results.csv")
HAS_MITES  <- !IS_PUBLIC && file.exists(MITES_FILE)
mites <- if (HAS_MITES) read_csv(MITES_FILE, show_col_types = FALSE) else
  tibble(session = character(), trial_pen = integer(), treatment = character(),
         n_nonctl_live_mites = integer(), mites_present = logical())

pal_event <- c(Mark = "#1F7A3D", Recapture = "#2E6FB0", `Not detected` = "#D9D4CC")
sev_col   <- c(high = "#B23A48", medium = "#C58A2E", low = "#4E8A6B", info = "#5B6B7B")


# =============================================================================
# THEMES, FONTS, HEADER ART
# =============================================================================
`%||%` <- function(a, b) if (is.null(a)) b else a

mk_theme <- function(id, name, mode, sky, ...) list(id = id, name = name, mode = mode, sky = sky, vars = list(...))

THEME_LIST <- list(
  mk_theme("dark", "Midnight Dark", "dark", "night",
    bg = "#10151c", card = "#1a212b", fg = "#e7ebf0", muted = "#97a3b2", border = "#2b3544",
    primary = "#6fb1ff", accent = "#f4c95d", ink = "#ffffff", sky1 = "#070b22", sky2 = "#1d2b5a",
    m1 = "#2a3868", m2 = "#1b2650", snow = "#9fb2e6", hill1 = "#1c3a37", hill2 = "#12292a",
    sheep = "#8d7a68", sheepdark = "#55463a", rump = "#e9e2d4", horn = "#cbbfa6", eye = "#f2efe6",
    sun = "#f4f1de", sunscale = "1", speed = "1.6", hop = "5px",
    mark = "#5fd38d", recap = "#6fb1ff", none = "#333d4b"),
  mk_theme("contrast", "Contrast", "light", "day",
    bg = "#f4f1ea", card = "#ffffff", fg = "#101418", muted = "#4a525c", border = "#2a2f36",
    primary = "#0a3d91", accent = "#c8102e", ink = "#08203a", sky1 = "#4aa8e8", sky2 = "#d6efff",
    m1 = "#5a6f8f", m2 = "#37506e", snow = "#ffffff", hill1 = "#3b8f46", hill2 = "#1f6b30",
    sheep = "#6b4a2e", sheepdark = "#2e1f12", rump = "#fff7e8", horn = "#3b2a1a", eye = "#ffffff",
    sun = "#ffb400", sunscale = "1", speed = "1", hop = "14px",
    mark = "#1b7f3b", recap = "#0a3d91", none = "#c9c4b8"),
  mk_theme("viridis", "Viridis", "light", "day",
    bg = "#f6f8f5", card = "#ffffff", fg = "#1c2b36", muted = "#53646f", border = "#cfd9d4",
    primary = "#31688e", accent = "#35b779", ink = "#ffffff", sky1 = "#26828e", sky2 = "#e9f5a3",
    m1 = "#3b528b", m2 = "#472d7b", snow = "#d8f0b8", hill1 = "#35b779", hill2 = "#1f9e89",
    sheep = "#6a5240", sheepdark = "#3b2c22", rump = "#fbf7ea", horn = "#5a4b3a", eye = "#ffffff",
    sun = "#fde725", sunscale = "1", speed = "1", hop = "14px",
    mark = "#35b779", recap = "#31688e", none = "#d6dad7"),
  mk_theme("plasma", "Viridis Plasma", "dark", "dusk",
    bg = "#170a2a", card = "#240f3d", fg = "#fbeedd", muted = "#c7a9c9", border = "#44205f",
    primary = "#f89540", accent = "#cc4778", ink = "#ffffff", sky1 = "#0d0887", sky2 = "#f89540",
    m1 = "#7e03a8", m2 = "#4a0a6f", snow = "#f0a2c0", hill1 = "#2b0b4a", hill2 = "#1b0733",
    sheep = "#a2705b", sheepdark = "#5d3a3d", rump = "#fde0c5", horn = "#f0c27a", eye = "#fde0c5",
    sun = "#fcce25", sunscale = "1.3", speed = "1.25", hop = "9px",
    mark = "#fca636", recap = "#cc4778", none = "#4b2a66"),
  mk_theme("desert", "Desert Sand", "light", "day",
    bg = "#f3e7d0", card = "#fbf3e1", fg = "#45301c", muted = "#7d6446", border = "#dac49b",
    primary = "#b5601a", accent = "#a23b2a", ink = "#4a2d12", sky1 = "#f2b872", sky2 = "#fbe6bf",
    m1 = "#c8845a", m2 = "#a5643f", snow = "#f3e7d0", hill1 = "#dcbb7c", hill2 = "#c49b55",
    sheep = "#6b4a2f", sheepdark = "#3f2a18", rump = "#fff4de", horn = "#4a3320", eye = "#ffffff",
    sun = "#ffd24a", sunscale = "1.4", speed = "0.9", hop = "16px",
    mark = "#7a8f3a", recap = "#b5601a", none = "#d9c9a8"),
  mk_theme("calm", "Calm", "light", "day",
    bg = "#edf3f6", card = "#ffffff", fg = "#243847", muted = "#5f7684", border = "#cfdde5",
    primary = "#4b86a8", accent = "#7fb7a4", ink = "#26485f", sky1 = "#b5d3e8", sky2 = "#eef6fb",
    m1 = "#9db4c7", m2 = "#7e9bb2", snow = "#ffffff", hill1 = "#a4cfa9", hill2 = "#86bb91",
    sheep = "#8a6f58", sheepdark = "#5c4a3b", rump = "#fffaf0", horn = "#6d5a48", eye = "#ffffff",
    sun = "#ffe7a3", sunscale = "1", speed = "1.5", hop = "10px",
    mark = "#6bb59a", recap = "#4b86a8", none = "#d5e0e6"),
  mk_theme("forest", "Forest", "dark", "day",
    bg = "#16231b", card = "#1f3025", fg = "#e2ebdd", muted = "#9fb5a3", border = "#2f4637",
    primary = "#7ccf8a", accent = "#dcb64a", ink = "#ffffff", sky1 = "#3d7a5a", sky2 = "#bfe0b4",
    m1 = "#2c5c46", m2 = "#1e4333", snow = "#dfeee0", hill1 = "#2f6e3c", hill2 = "#1f5230",
    sheep = "#7a6046", sheepdark = "#46352a", rump = "#f1e9d6", horn = "#c8b894", eye = "#f1e9d6",
    sun = "#fff0a8", sunscale = "0.9", speed = "1.2", hop = "12px",
    mark = "#7ccf8a", recap = "#5fb0d6", none = "#2f4637"),
  mk_theme("hc", "High Contrast", "dark", "night",
    bg = "#000000", card = "#000000", fg = "#ffffff", muted = "#e0e0e0", border = "#ffffff",
    primary = "#ffe600", accent = "#00e5ff", ink = "#ffffff", sky1 = "#000000", sky2 = "#111111",
    m1 = "#2a2a2a", m2 = "#555555", snow = "#ffffff", hill1 = "#1a1a1a", hill2 = "#333333",
    sheep = "#ffffff", sheepdark = "#ffe600", rump = "#ffffff", horn = "#00e5ff", eye = "#000000",
    sun = "#ffffff", sunscale = "1", speed = "1.6", hop = "5px",
    mark = "#ffe600", recap = "#00e5ff", none = "#666666")
)
THEMES        <- setNames(THEME_LIST, vapply(THEME_LIST, `[[`, "", "id"))
DEFAULT_THEME <- "calm"

FONTS <- data.frame(
  id = c("inter","nunito","poppins","atkinson","merriweather","lora","playfair","robotoslab","firacode","patrick"),
  name = c("Inter","Nunito","Poppins","Atkinson Hyperlegible","Merriweather","Lora","Playfair Display",
           "Roboto Slab","Fira Code","Patrick Hand"),
  stack = c("'Inter','Segoe UI',system-ui,-apple-system,Arial,sans-serif",
            "'Nunito','Trebuchet MS','Segoe UI',Arial,sans-serif",
            "'Poppins','Century Gothic','Segoe UI',Arial,sans-serif",
            "'Atkinson Hyperlegible','Verdana',Tahoma,sans-serif",
            "'Merriweather',Georgia,'Times New Roman',serif",
            "'Lora','Palatino Linotype',Palatino,Georgia,serif",
            "'Playfair Display','Bookman Old Style',Georgia,serif",
            "'Roboto Slab','Rockwell','Courier New',serif",
            "'Fira Code','Cascadia Mono',Consolas,'Courier New',monospace",
            "'Patrick Hand','Segoe Print','Comic Sans MS',cursive"),
  stringsAsFactors = FALSE)
DEFAULT_FONT <- "inter"
GOOGLE_FONTS_URL <- paste0("https://fonts.googleapis.com/css2?family=Inter:wght@400;600&family=Nunito:wght@400;700",
  "&family=Poppins:wght@400;600&family=Atkinson+Hyperlegible:wght@400;700&family=Merriweather:wght@400;700",
  "&family=Lora:wght@400;700&family=Playfair+Display:wght@400;700&family=Roboto+Slab:wght@400;700",
  "&family=Fira+Code:wght@400;600&family=Patrick+Hand&display=swap")

PANGRAMS <- c("Sphinx of black quartz, judge my vow.",
              "The quick brown fox jumps over the lazy dog.",
              "Pack my box with five dozen liquor jugs 0123456789")

# ---- header geometry (viewBox 1000 x 192; banner is 2 inches = 192 px tall) --
BANNER_H   <- 192
hill_front <- function(x) 22 + 13 * sin(2 * pi * (1.25 * x + 0.08)) + 5 * sin(2 * pi * 3.2 * x + 1)
hill_back  <- function(x) 52 + 16 * sin(2 * pi * (0.8 * x + 0.5)) + 6 * sin(2 * pi * 2.1 * x)
hill_path  <- function(f, n = 120) {
  x <- seq(0, 1, length.out = n + 1)
  pts <- paste(sprintf("%.1f,%.1f", x * 1000, BANNER_H - f(x)), collapse = " L")
  sprintf("M0,%d L%s L1000,%d Z", BANNER_H, pts, BANNER_H)
}

sheep_svg <- function(kind = "ewe") {
  horn <- if (kind == "ram")
    '<path class="horn" style="stroke-width:3.8" d="M52,12 C43,1 32,10 38,19 C40,23 46,23 47,19"/>'
  else '<path class="horn" style="stroke-width:2.2" d="M53,12 C52,7 49,6 47,8"/>'
  leg <- function(cls, x, ox) sprintf(
    '<g class="leg %s" style="transform-origin:%dpx 30px"><rect x="%d" y="30" width="4" height="15" rx="2"/></g>', cls, ox, x)
  paste0('<svg class="sheep" viewBox="0 0 70 50" xmlns="http://www.w3.org/2000/svg">',
    leg("far lb", 13, 15), leg("far lf", 40, 42),
    '<ellipse class="body" cx="30" cy="25" rx="22" ry="11"/><ellipse class="rump" cx="10" cy="24" rx="6.5" ry="8"/>',
    leg("near lb", 18, 20), leg("near lf", 45, 47),
    '<path class="body" d="M44,22 Q50,12 56,13 L58,22 Q52,30 44,30 Z"/>',
    '<g class="head" style="transform-origin:49px 21px">',
    '<ellipse class="face" cx="59" cy="17" rx="7" ry="4.8" transform="rotate(18 59 17)"/>',
    '<ellipse class="muzzle" cx="64.5" cy="19.5" rx="3" ry="2.4"/><circle class="eye" cx="60" cy="15.5" r="1.1"/>',
    '<ellipse class="ear" cx="54" cy="14" rx="3.2" ry="1.5" transform="rotate(-25 54 14)"/>', horn, '</g></svg>')
}
SHEEP_W <- c(ewe = 66, lamb = 40, ram = 86)

animal <- function(kind, pose, x = NULL, flip = FALSE, delay = 0, dir = 1, id = NULL) {
  s <- SHEEP_W[[kind]] / 70
  style <- sprintf("--d:%ss;--dir:%d;", delay, dir)
  if (!is.null(x)) style <- paste0(style, sprintf("left:%.1f%%;bottom:%.1fpx;", x, hill_front(x / 100) - 4 * s))
  if (!is.null(id)) style <- paste0(style, sprintf("animation-name:%s;animation-delay:calc(var(--ce-speed)*%ss);", id, delay))
  sprintf('<div class="animal %s %s%s%s" style="%s"><div class="inner">%s</div></div>',
          kind, pose, if (flip) " flip" else "", if (!is.null(id)) paste0(" ", id) else "", style, sheep_svg(kind))
}

walker_css <- function(id, kind, x0, x1, dur) {
  s <- SHEEP_W[[kind]] / 70; n <- 10
  fx <- seq(x0, x1, length.out = n + 1)
  fw <- sprintf("%.1f%%{left:%.2f%%;bottom:%.1fpx;transform:scaleX(1)}", seq(0, 48, length.out = n + 1), fx, hill_front(fx / 100) - 4 * s)
  rw <- sprintf("%.1f%%{left:%.2f%%;bottom:%.1fpx;transform:scaleX(-1)}", seq(50, 98, length.out = n + 1), rev(fx), hill_front(rev(fx) / 100) - 4 * s)
  paste0("@keyframes ", id, "{", paste(fw, collapse = ""), paste(rw, collapse = ""),
         sprintf("100%%{left:%.2f%%;bottom:%.1fpx;transform:scaleX(1)}", x0, hill_front(x0 / 100) - 4 * s), "}",
         ".animal.", id, "{animation-duration:calc(", dur, "s*var(--ce-speed));animation-timing-function:linear;animation-iteration-count:infinite}")
}

# animals: ewes + lambs on the left/middle, rams on the right
WALKERS <- list(
  list(id = "wk1", kind = "ewe",  x0 = 3,  x1 = 30, dur = 80, delay = 0),
  list(id = "wk2", kind = "lamb", x0 = 3,  x1 = 30, dur = 80, delay = -3.5),
  list(id = "wk3", kind = "ewe",  x0 = 47, x1 = 63, dur = 70, delay = -35),
  list(id = "wk4", kind = "lamb", x0 = 47, x1 = 63, dur = 70, delay = -38.5))

banner_animals <- function() {
  w <- lapply(WALKERS, function(a) animal(a$kind, "walk", id = a$id, delay = a$delay))
  st <- c(
    animal("ewe",  "graze",  x = 34),
    animal("lamb", "frolic", x = 40, delay = -1),
    animal("lamb", "frolic", x = 44.5, delay = -4.5),
    animal("ram",  "graze",  x = 70),
    animal("ram",  "butt",   x = 79, dir = 1),
    animal("ram",  "butt",   x = 85, flip = TRUE, dir = -1, delay = 0),
    animal("ram",  "stand",  x = 91, flip = TRUE))
  paste(c(unlist(w), st), collapse = "\n")
}

banner_css <- paste(vapply(WALKERS, function(a) walker_css(a$id, a$kind, a$x0, a$x1, a$dur), ""), collapse = "\n")

set.seed(7)
STARS <- paste(sprintf('<i class="star" style="left:%.1f%%;top:%dpx;animation-delay:-%.1fs;--sz:%.1fpx"></i>',
  runif(26, 1, 99), sample(6:105, 26, replace = TRUE), runif(26, 0, 6), sample(c(1.5, 2, 2.5, 3), 26, replace = TRUE)), collapse = "")
FIREFLIES <- paste(sprintf('<i class="ff" style="left:%.1f%%;bottom:%dpx;animation-delay:-%.1fs"></i>',
  runif(9, 3, 97), sample(14:60, 9, replace = TRUE), runif(9, 0, 8)), collapse = "")

banner_html <- function() {
  rays <- paste(sprintf('<line x1="40" y1="40" x2="%.2f" y2="%.2f"/>', 40 + 36 * cos(seq(0, 2*pi, length.out = 13)[-13]),
                        40 + 36 * sin(seq(0, 2*pi, length.out = 13)[-13])), collapse = "")
  HTML(paste0(
  '<div class="ce-banner" role="img" aria-label="Animated header: bighorn ewes, lambs and rams on a mountain slope">',
  '<div class="ce-sky"></div>',
  '<div class="ce-stars">', STARS, '</div>',
  '<div class="ce-clouds"><div class="cloud c1"></div><div class="cloud c2"></div><div class="cloud c3"></div></div>',
  '<div class="ce-orb sun"><svg viewBox="0 0 80 80"><g class="rays">', rays, '</g><circle cx="40" cy="40" r="17"/></svg></div>',
  '<div class="ce-orb moon"><svg viewBox="0 0 80 80"><defs><mask id="ce-moonmask"><rect width="80" height="80" fill="#fff"/>',
  '<circle cx="50" cy="34" r="17" fill="#000"/></mask></defs><circle cx="38" cy="42" r="19" mask="url(#ce-moonmask)"/></svg></div>',
  '<svg class="ce-bg" viewBox="0 0 1000 192" preserveAspectRatio="none" aria-hidden="true">',
  '<path class="m1" d="M0,192 L0,120 L90,70 L150,100 L240,40 L330,105 L420,60 L520,110 L610,50 L700,100 L790,35 L880,95 L1000,60 L1000,192 Z"/>',
  '<g class="snow"><path d="M90,70 L74,86 L84,83 L90,92 L98,82 L106,86 Z"/><path d="M240,40 L220,64 L232,59 L240,70 L250,58 L262,64 Z"/>',
  '<path d="M420,60 L404,78 L414,75 L421,84 L429,74 L438,79 Z"/><path d="M610,50 L591,72 L603,67 L611,78 L620,66 L631,72 Z"/>',
  '<path d="M790,35 L768,62 L781,56 L790,69 L800,55 L813,62 Z"/></g>',
  '<path class="m2" d="M0,192 L0,140 L110,95 L200,130 L300,85 L400,135 L520,100 L640,138 L760,92 L880,130 L1000,105 L1000,192 Z"/>',
  '<path class="h1" d="', hill_path(hill_back), '"/>',
  '<path class="h2" d="', hill_path(hill_front), '"/></svg>',
  '<div class="ce-fireflies">', FIREFLIES, '</div>',
  banner_animals(),
  '<div class="ce-mobile-note">Mobile version not available yet</div>',
  '<div class="ce-title"><h1>Count on Ewe</h1><span>PIT-tag counts &middot; California bighorn Psoroptes trial</span></div>',
  '</div>'))
}

# =============================================================================
# CSS + JS (theme engine runs client-side; the server only needs the ids)
# =============================================================================
CE_CSS <- r"---(
html[data-bs-theme]{--bs-body-bg:var(--ce-bg);--bs-body-color:var(--ce-fg);--bs-border-color:var(--ce-border);
  --bs-primary:var(--ce-primary);--bs-link-color:var(--ce-primary);--bs-secondary-color:var(--ce-muted);
  --bs-body-font-family:var(--ce-font);--bs-emphasis-color:var(--ce-fg);--bs-tertiary-bg:var(--ce-card);}
html,body{background:var(--ce-bg)!important;color:var(--ce-fg);font-family:var(--ce-font);transition:background .4s,color .4s}
button,input,select,textarea,.btn,.form-control,.form-select,.selectize-input,.selectize-dropdown,.nav-link,.dataTables_wrapper{font-family:var(--ce-font)}
.navbar{background:var(--ce-card)!important;border-bottom:1px solid var(--ce-border)}
.navbar .navbar-brand,.navbar .nav-link{color:var(--ce-muted)!important}
.navbar .nav-link.active,.navbar .nav-link:hover{color:var(--ce-primary)!important}
.navbar .navbar-text{color:var(--ce-muted)!important}
.card,.bslib-card{background:var(--ce-card)!important;color:var(--ce-fg);border:1px solid var(--ce-border)}
.card-header{background:transparent!important;border-bottom:1px solid var(--ce-border);color:var(--ce-fg);font-weight:600}
.bslib-sidebar-layout{--_sidebar-bg:var(--ce-card);--_sidebar-fg:var(--ce-fg);--_border-color:var(--ce-border)}
.bslib-sidebar-layout>.sidebar{background:var(--ce-card)!important;color:var(--ce-fg)}
.text-muted,.help-block,.form-text{color:var(--ce-muted)!important}
.ce-kpi{background:var(--ce-card);border:1px solid var(--ce-border);border-radius:.75rem;padding:1rem;height:100%}
.ce-kpi .v{font-size:1.7rem;font-weight:600;color:var(--ce-primary)}.ce-kpi .l{font-size:.85rem;color:var(--ce-muted)}
.form-control,.form-select,.selectize-input,.selectize-dropdown,.selectize-input input{background:var(--ce-bg)!important;color:var(--ce-fg)!important;border-color:var(--ce-border)!important}
.selectize-dropdown .active{background:var(--ce-primary)!important;color:var(--ce-bg)!important}
.selectize-control.multi .selectize-input>div{background:var(--ce-primary);color:var(--ce-bg)}
.form-check-input:checked{background-color:var(--ce-primary);border-color:var(--ce-primary)}
.irs--shiny .irs-line{background:var(--ce-border);border-color:var(--ce-border)}
.irs--shiny .irs-bar{background:var(--ce-primary);border-color:var(--ce-primary)}
.irs--shiny .irs-handle{background:var(--ce-bg);border:2px solid var(--ce-primary);box-shadow:none}
.irs--shiny .irs-from,.irs--shiny .irs-to,.irs--shiny .irs-single{background:var(--ce-primary);color:var(--ce-bg);font-size:10px}
.irs--shiny .irs-min,.irs--shiny .irs-max{background:transparent;color:var(--ce-muted)}
.irs--shiny .irs-grid-text{color:var(--ce-muted)}
.btn-outline-primary,.btn-primary{--bs-btn-color:var(--ce-primary);--bs-btn-border-color:var(--ce-primary);--bs-btn-bg:transparent;
  --bs-btn-hover-bg:var(--ce-primary);--bs-btn-hover-color:var(--ce-bg);--bs-btn-hover-border-color:var(--ce-primary);
  --bs-btn-active-bg:var(--ce-primary);--bs-btn-active-color:var(--ce-bg);--bs-btn-active-border-color:var(--ce-primary)}
.table{--bs-table-bg:transparent;--bs-table-color:var(--ce-fg);--bs-table-border-color:var(--ce-border);--bs-table-striped-bg:rgba(127,127,127,.08);--bs-table-hover-bg:rgba(127,127,127,.14)}
table.dataTable tbody tr,table.dataTable thead th{background:transparent!important;color:var(--ce-fg)!important}
.dataTables_wrapper,.dataTables_wrapper .dataTables_info,.dataTables_wrapper .dataTables_length,.dataTables_wrapper .dataTables_filter{color:var(--ce-fg)!important}
.dataTables_wrapper .paginate_button{color:var(--ce-fg)!important}
.dataTables_wrapper .paginate_button.current{background:var(--ce-primary)!important;color:var(--ce-bg)!important;border-color:var(--ce-primary)!important}
.page-link{background:var(--ce-card);color:var(--ce-fg);border-color:var(--ce-border)}.page-item.active .page-link{background:var(--ce-primary);color:var(--ce-bg);border-color:var(--ce-primary)}

/* ---- sidebar: two columns ------------------------------------------------ */
.ce-two{display:grid;grid-template-columns:1fr 1fr;gap:1rem}
.ce-two h6{font-size:.72rem;letter-spacing:.09em;text-transform:uppercase;color:var(--ce-primary);margin:0 0 .6rem;border-bottom:1px solid var(--ce-border);padding-bottom:.35rem}
.ce-two .form-group,.ce-two .shiny-input-container{margin-bottom:.6rem}
.ce-two label.control-label{font-size:.78rem;color:var(--ce-muted);margin-bottom:.15rem}
.ce-two .irs{font-size:11px}
.ce-sw-grid{display:grid;grid-template-columns:1fr 1fr;gap:.35rem}
.ce-sw{border:2px solid var(--ce-border);background:var(--ce-bg);color:var(--ce-fg);border-radius:.5rem;padding:.3rem .35rem;font-size:.72rem;display:flex;align-items:center;gap:.35rem;cursor:pointer;text-align:left;line-height:1.1}
.ce-sw:hover{border-color:var(--ce-primary)}.ce-sw.on{border-color:var(--ce-primary);box-shadow:0 0 0 1px var(--ce-primary)}
.ce-sw i{display:inline-block;flex:0 0 22px;height:22px;border-radius:50%;border:1px solid rgba(127,127,127,.5)}
.ce-fontsel{width:100%;font-size:.85rem}
.ce-preview{margin-top:.6rem;border:1px dashed var(--ce-border);border-radius:.5rem;padding:.55rem .65rem;background:var(--ce-bg)}
.ce-preview .p1{font-size:1.05rem;font-weight:600;color:var(--ce-primary);line-height:1.25}
.ce-preview .p2{font-size:.88rem;margin-top:.25rem}.ce-preview .p3{font-size:.75rem;color:var(--ce-muted);margin-top:.25rem}
.ce-preview .meta{font-size:.65rem;color:var(--ce-muted);margin-top:.3rem;text-transform:uppercase;letter-spacing:.06em}

/* ---- animated header (2 inches tall) -------------------------------------- */
.ce-banner{position:relative;width:100%;height:2in;min-height:192px;overflow:hidden;border-bottom:1px solid var(--ce-border);isolation:isolate}
.ce-sky{position:absolute;inset:0;background:linear-gradient(var(--ce-sky1),var(--ce-sky2));transition:background .6s}
.ce-bg{position:absolute;left:0;bottom:0;width:100%;height:192px}
.ce-bg .m1{fill:var(--ce-m1)}.ce-bg .m2{fill:var(--ce-m2)}.ce-bg .snow path{fill:var(--ce-snow)}
.ce-bg .h1{fill:var(--ce-hill1);filter:brightness(1.12)}.ce-bg .h2{fill:var(--ce-hill2)}.ce-bg path{transition:fill .6s}
html[data-ce-theme=hc] .ce-bg path{stroke:#fff;stroke-width:1.4;vector-effect:non-scaling-stroke}
.ce-title{position:absolute;left:18px;top:10px;z-index:5;color:var(--ce-ink);text-shadow:0 1px 6px rgba(0,0,0,.25);pointer-events:none}
.ce-title h1{margin:0;font-size:1.7rem;font-weight:700;line-height:1.1;letter-spacing:.01em}
.ce-title span{font-size:.78rem;opacity:.9}
.ce-orb{position:absolute;z-index:1;display:none;width:calc(64px*var(--ce-sunscale))}
.ce-orb svg{width:100%;height:auto;overflow:visible}
.ce-orb.sun{right:15%;top:12px}.ce-orb.sun circle{fill:var(--ce-sun);filter:drop-shadow(0 0 8px var(--ce-sun))}
.ce-orb.sun .rays{stroke:var(--ce-sun);stroke-width:3;stroke-linecap:round;opacity:.85;transform-origin:40px 40px;animation:spin calc(40s*var(--ce-speed)) linear infinite}
.ce-orb.moon{right:17%;top:10px;animation:floaty calc(9s*var(--ce-speed)) ease-in-out infinite}.ce-orb.moon circle{fill:#f6f1d4}.ce-orb.moon svg{filter:drop-shadow(0 0 9px rgba(255,248,200,.8))}
html[data-ce-theme=hc] .ce-orb.moon circle{fill:#fff}
html[data-sky=day] .ce-orb.sun,html[data-sky=dusk] .ce-orb.sun,html[data-sky=night] .ce-orb.moon{display:block}
html[data-sky=dusk] .ce-orb.sun{top:78px;right:22%;animation:sunset calc(30s*var(--ce-speed)) ease-in-out infinite alternate}
.ce-stars,.ce-clouds,.ce-fireflies{position:absolute;inset:0;pointer-events:none;z-index:1}
.ce-stars,.ce-fireflies{display:none}.ce-fireflies{z-index:4}
html[data-sky=night] .ce-stars,html[data-sky=night] .ce-fireflies{display:block}
html[data-sky=night] .ce-clouds{display:none}
.star{position:absolute;width:var(--sz);height:var(--sz);background:#fff;border-radius:50%;animation:twinkle calc(3s*var(--ce-speed)) ease-in-out infinite}
.ff{position:absolute;width:4px;height:4px;border-radius:50%;background:#ffe86b;box-shadow:0 0 8px 3px rgba(255,232,107,.7);animation:ffly calc(9s*var(--ce-speed)) ease-in-out infinite}
.cloud{position:absolute;height:14px;width:64px;border-radius:14px;background:rgba(255,255,255,.85);animation:drift linear infinite}
.cloud:before,.cloud:after{content:"";position:absolute;background:inherit;border-radius:50%}
.cloud:before{width:26px;height:26px;left:10px;top:-13px}.cloud:after{width:20px;height:20px;left:32px;top:-9px}
.cloud.c1{top:26px;animation-duration:calc(120s*var(--ce-speed))}.cloud.c2{top:58px;transform:scale(.7);animation-duration:calc(170s*var(--ce-speed));animation-delay:-60s}
.cloud.c3{top:18px;transform:scale(.55);animation-duration:calc(200s*var(--ce-speed));animation-delay:-120s}
html[data-sky=dusk] .cloud{background:rgba(255,200,170,.45)}
.animal{position:absolute;z-index:3;pointer-events:none}
.animal .inner{transform-origin:50% 100%}
.animal.flip svg{transform:scaleX(-1)}
.animal.ewe svg{width:66px;display:block}.animal.lamb svg{width:40px;display:block}.animal.ram svg{width:86px;display:block}
.sheep .body{fill:var(--ce-sheep)}.sheep .rump{fill:var(--ce-rump)}.sheep .face{fill:var(--ce-sheepdark)}.sheep .ear{fill:var(--ce-sheepdark)}
.sheep .muzzle{fill:var(--ce-rump)}.sheep .eye{fill:var(--ce-eye)}.sheep .horn{fill:none;stroke:var(--ce-horn);stroke-linecap:round}
.sheep .leg rect{fill:var(--ce-sheepdark)}.sheep .leg.far rect{opacity:.7}
html[data-ce-theme=hc] .sheep .body,html[data-ce-theme=hc] .sheep .face{stroke:#000;stroke-width:.6}
.animal.walk .inner{animation:bob calc(.9s*var(--ce-speed)) ease-in-out infinite}
.animal.walk .leg.near.lb,.animal.walk .leg.far.lf,.animal.frolic .leg.near.lb,.animal.frolic .leg.far.lf{animation:swing calc(.9s*var(--ce-speed)) ease-in-out infinite}
.animal.walk .leg.near.lf,.animal.walk .leg.far.lb,.animal.frolic .leg.near.lf,.animal.frolic .leg.far.lb{animation:swing calc(.9s*var(--ce-speed)) ease-in-out infinite;animation-delay:calc(var(--ce-speed)*-.45s)}
.animal.frolic .leg{animation-duration:calc(.5s*var(--ce-speed))!important}
.animal.walk .head{animation:headbob calc(.9s*var(--ce-speed)) ease-in-out infinite}
.animal.graze .head{animation:graze calc(11s*var(--ce-speed)) ease-in-out infinite;animation-delay:var(--d,0s)}
.animal.stand .head{animation:nod calc(6s*var(--ce-speed)) ease-in-out infinite}
.animal.frolic .inner{animation:frolic calc(5s*var(--ce-speed)) ease-in-out infinite;animation-delay:calc(var(--ce-speed)*var(--d,0s))}
.animal.butt .inner{animation:lunge calc(7s*var(--ce-speed)) ease-in-out infinite;animation-delay:calc(var(--ce-speed)*var(--d,0s))}
.animal.butt .head{animation:butthead calc(7s*var(--ce-speed)) ease-in-out infinite}
@keyframes swing{0%,100%{transform:rotate(-24deg)}50%{transform:rotate(24deg)}}
@keyframes bob{0%,100%{transform:translateY(0)}50%{transform:translateY(-1.5px)}}
@keyframes headbob{0%,100%{transform:rotate(-2deg)}50%{transform:rotate(3deg)}}
@keyframes graze{0%,4%{transform:rotate(50deg)}10%{transform:rotate(58deg)}16%{transform:rotate(50deg)}22%{transform:rotate(58deg)}28%{transform:rotate(50deg)}56%{transform:rotate(50deg)}66%,84%{transform:rotate(-4deg)}94%,100%{transform:rotate(50deg)}}
@keyframes nod{0%,100%{transform:rotate(0)}50%{transform:rotate(6deg)}}
@keyframes frolic{0%,100%{transform:translate(0,0) rotate(0)}10%{transform:translate(4px,calc(var(--ce-hop)*-1)) rotate(-14deg)}20%{transform:translate(8px,0) rotate(0)}32%{transform:translate(13px,calc(var(--ce-hop)*-1.3)) rotate(12deg)}44%{transform:translate(18px,0) rotate(0)}60%{transform:translate(18px,0)}70%{transform:translate(10px,calc(var(--ce-hop)*-1)) rotate(-10deg)}82%{transform:translate(2px,0) rotate(0)}}
@keyframes lunge{0%,70%,100%{transform:translateX(0)}78%{transform:translateX(calc(10px*var(--dir,1)))}84%{transform:translateX(0)}}
@keyframes butthead{0%,70%,100%{transform:rotate(0)}74%{transform:rotate(-16deg)}78%{transform:rotate(14deg)}86%{transform:rotate(0)}}
@keyframes spin{to{transform:rotate(360deg)}}
@keyframes floaty{0%,100%{transform:translateY(0)}50%{transform:translateY(7px)}}
@keyframes sunset{from{transform:translateY(0)}to{transform:translateY(22px)}}
@keyframes twinkle{0%,100%{opacity:.2;transform:scale(.8)}50%{opacity:1;transform:scale(1.2)}}
@keyframes ffly{0%,100%{transform:translate(0,0);opacity:.2}25%{transform:translate(16px,-10px);opacity:1}50%{transform:translate(4px,-20px);opacity:.4}75%{transform:translate(-14px,-8px);opacity:1}}
@keyframes drift{from{left:-12%}to{left:108%}}
html[data-anim=off] .ce-banner *{animation-play-state:paused!important}
@media (prefers-reduced-motion:reduce){.ce-banner *{animation-play-state:paused!important}}
@media (max-width:700px){.ce-title h1{font-size:1.25rem}.ce-title span{display:none}.ce-two{grid-template-columns:1fr}}
/* ---- live clocks ------------------------------------------------------------ */
.ce-clock{border:1px solid var(--ce-border);border-radius:.6rem;background:var(--ce-bg);padding:.6rem .75rem;margin-bottom:.9rem}
.ce-utc{display:flex;align-items:baseline;gap:.6rem;flex-wrap:wrap;border-bottom:1px solid var(--ce-border);padding-bottom:.45rem;margin-bottom:.45rem}
.ce-utc .lab{font-size:.7rem;letter-spacing:.12em;color:var(--ce-bg);background:var(--ce-primary);border-radius:.3rem;padding:.05rem .4rem;font-weight:700}
.ce-utc .t{font-size:1.55rem;font-weight:700;color:var(--ce-primary);font-variant-numeric:tabular-nums;letter-spacing:.02em}
.ce-utc .d{font-size:.85rem;color:var(--ce-muted);font-variant-numeric:tabular-nums}
.ce-zones{display:grid;grid-template-columns:repeat(4,1fr);gap:.3rem .6rem}
.ce-zone{line-height:1.15}.ce-zone .c{font-size:.62rem;text-transform:uppercase;letter-spacing:.07em;color:var(--ce-muted)}
.ce-zone .n{font-size:.74rem}.ce-zone .z{font-size:.95rem;font-weight:600;font-variant-numeric:tabular-nums;color:var(--ce-fg)}
.ce-zone .w{font-size:.62rem;color:var(--ce-muted);margin-left:.25rem;font-weight:400}
.ce-dev{margin-top:.5rem;font-size:.72rem;color:var(--ce-muted);text-align:right;font-style:italic}
@media (max-width:700px){.ce-zones{grid-template-columns:repeat(2,1fr)}}
/* ---- overview page ----------------------------------------------------------- */
.ov{max-width:1180px;margin:0 auto;padding:.25rem .25rem 2rem}
.ov h3.sec{font-size:1.05rem;font-weight:700;margin:1.6rem 0 .7rem;display:flex;align-items:center;gap:.5rem}
.ov h3.sec:after{content:"";flex:1;height:1px;background:var(--ce-border)}
.ov .lead{color:var(--ce-muted);font-size:.9rem;margin:-.3rem 0 .8rem}
.ov-stats{display:grid;grid-template-columns:repeat(5,1fr);gap:.7rem}
.ov-stat{background:var(--ce-card);border:1px solid var(--ce-border);border-radius:.75rem;padding:.8rem .9rem}
.ov-stat .v{font-size:1.5rem;font-weight:700;color:var(--ce-primary);line-height:1.1}.ov-stat .l{font-size:.78rem;color:var(--ce-muted);margin-top:.2rem}
.ov-tl{display:flex;align-items:flex-start;gap:0;overflow-x:auto;padding:.4rem 0 .2rem}
.ov-tl .st{flex:1 0 120px;text-align:center;position:relative;padding-top:1.2rem}
.ov-tl .st:before{content:"";position:absolute;left:0;right:0;top:.45rem;height:2px;background:var(--ce-border)}
.ov-tl .st:first-child:before{left:50%}.ov-tl .st:last-child:before{right:50%}
.ov-tl .st:after{content:"";position:absolute;left:calc(50% - 7px);top:0;width:14px;height:14px;border-radius:50%;background:var(--ce-primary);border:3px solid var(--ce-bg)}
.ov-tl .st.fin:after{background:var(--ce-accent)}.ov-tl .st.nxt:after{background:var(--ce-bg);border:3px solid var(--ce-primary)}
.ov-tl .d{font-weight:600;font-size:.85rem}.ov-tl .t{font-size:.74rem;color:var(--ce-muted)}
.bio-grid{display:grid;grid-template-columns:repeat(3,1fr);gap:1rem}
.bio-card{--c:#3aa76d;border:2px solid var(--c);border-radius:1rem;overflow:hidden;background:color-mix(in srgb,var(--c) 11%,var(--ce-card));display:flex;flex-direction:column}
.bio-card.sheep{--c:#3aa76d}.bio-card.wild{--c:#e0a030}.bio-card.staff{--c:#4a90d9}
.bio-card h4{margin:0;padding:.7rem .95rem .1rem;font-size:1rem;font-weight:700;display:flex;align-items:center;gap:.5rem;color:var(--ce-fg)}
.bio-card h4 .badge-ico{display:inline-flex;align-items:center;justify-content:center;width:1.8rem;height:1.8rem;border-radius:50%;background:var(--c);color:#fff;font-size:.85rem}
.bio-card .sub{padding:0 .95rem;font-size:.76rem;color:var(--ce-muted)}
.bio-card ul{margin:.5rem 0 .6rem;padding:0 .95rem 0 2rem;font-size:.84rem;line-height:1.35;flex:1}
.bio-card li{margin-bottom:.3rem}.bio-card li::marker{color:var(--c)}
.bio-scene{position:relative;height:84px;overflow:hidden;background:linear-gradient(color-mix(in srgb,var(--c) 22%,var(--ce-sky2)),color-mix(in srgb,var(--c) 8%,var(--ce-sky2)));border-top:2px solid var(--c)}
.bio-scene:before{content:"";position:absolute;left:-10%;right:-10%;bottom:-26px;height:52px;border-radius:50%;background:var(--ce-hill1)}
.bio-scene .animal{bottom:6px;z-index:2}
.bio-scene .animal.ewe svg{width:54px}.bio-scene .animal.lamb svg{width:34px}.bio-scene .animal.ram svg{width:70px}
.bio-scene .fence{position:absolute;left:50%;bottom:6px;height:34px;width:3px;margin-left:-1px;background:repeating-linear-gradient(to bottom,var(--ce-fg) 0 5px,transparent 5px 9px);opacity:.55;z-index:1}
.bio-scene .spray{position:absolute;right:8px;top:6px;font-size:.62rem;font-weight:700;letter-spacing:.06em;text-transform:uppercase;color:var(--c);background:var(--ce-card);border:1px solid var(--c);border-radius:.6rem;padding:.05rem .45rem;z-index:3}
html[data-ce-theme=hc] .bio-card{border-width:3px}
html[data-anim=off] .bio-scene *,html[data-anim=off] .ce-modal-lamb *{animation-play-state:paused!important}
.ppe{display:grid;grid-template-columns:repeat(5,1fr);gap:.6rem;margin-top:.9rem}
.ppe .p{background:var(--ce-card);border:1px solid var(--ce-border);border-top:4px solid var(--ce-primary);border-radius:.6rem;padding:.55rem .65rem;font-size:.78rem}
.ppe .p b{display:block;font-size:.8rem;margin-bottom:.15rem}.ppe .p span{color:var(--ce-muted)}
.ov-locks{display:grid;grid-template-columns:1fr 1fr;gap:1rem}
.ce-lock{position:relative;cursor:pointer;border:2px dashed var(--ce-border);border-radius:1rem;background:var(--ce-card);padding:1rem 1.1rem 5.2rem;min-height:170px;overflow:hidden;text-align:left;color:var(--ce-fg);width:100%;font-family:inherit}
.ce-lock:hover,.ce-lock:focus-visible{border-color:var(--ce-primary);outline:none}
.ce-lock .ttl{font-weight:700;font-size:.95rem;display:flex;align-items:center;gap:.5rem}
.ce-lock .pill{margin-left:auto;font-size:.66rem;letter-spacing:.07em;text-transform:uppercase;background:var(--ce-accent);color:var(--ce-bg);padding:.1rem .5rem;border-radius:1rem;font-weight:700}
.ce-lock .ghost{position:absolute;left:1.1rem;right:1.1rem;bottom:.9rem;display:flex;align-items:flex-end;gap:.5rem;height:48px;opacity:.28}
.ce-lock .ghost i{flex:1;border-radius:.3rem .3rem 0 0;background:var(--ce-muted)}
.ce-lock .hint{font-size:.78rem;color:var(--ce-muted);margin-top:.35rem}
.ce-lock-sm{min-height:110px;padding-bottom:1rem}
.people{display:grid;grid-template-columns:repeat(4,1fr);gap:1rem}
.person{background:var(--ce-card);border:1px solid var(--ce-border);border-radius:1rem;padding:1rem;text-align:center}
.person .av{width:96px;height:96px;margin:0 auto .5rem;display:block}.person .av svg{width:100%;height:100%;overflow:visible}
.av-bg{fill:color-mix(in srgb,var(--ce-primary) 22%,var(--ce-card));stroke:var(--ce-primary);stroke-width:2.5}
.av .eye{transform-box:fill-box;transform-origin:center;animation:blink 5.5s infinite}
.av .ear{transform-box:fill-box;transform-origin:80% 80%}.av .ear.r{transform-origin:20% 80%}
.person:hover .av .ear.l{animation:earl .6s ease-in-out}.person:hover .av .ear.r{animation:earr .6s ease-in-out .15s}
.person:hover .av svg{transform:translateY(-2px);transition:transform .2s}
@keyframes blink{0%,92%,100%{transform:scaleY(1)}95%{transform:scaleY(.1)}}
@keyframes earl{0%,100%{transform:rotate(0)}40%{transform:rotate(-18deg)}70%{transform:rotate(6deg)}}
@keyframes earr{0%,100%{transform:rotate(0)}40%{transform:rotate(18deg)}70%{transform:rotate(-6deg)}}
html[data-anim=off] .av *{animation-play-state:paused!important}
.person .nm{font-weight:700}.person .rl{font-size:.8rem;color:var(--ce-muted);margin-top:.15rem}
.person .tg{display:inline-block;margin-top:.6rem;font-size:.72rem;border:1px solid var(--ce-primary);color:var(--ce-primary);border-radius:1rem;padding:.1rem .6rem}
.who{display:grid;grid-template-columns:1fr 1fr;gap:.5rem 1rem}
.who div{background:var(--ce-card);border:1px solid var(--ce-border);border-radius:.6rem;padding:.55rem .75rem;font-size:.85rem}
.who b{color:var(--ce-primary)}
.partners{display:flex;flex-wrap:wrap;gap:.45rem}.partners span{background:var(--ce-card);border:1px solid var(--ce-border);border-radius:1rem;padding:.2rem .75rem;font-size:.82rem}
.modal-content{background:var(--ce-card);color:var(--ce-fg);border:2px solid var(--ce-primary)}
.modal-header,.modal-footer{border-color:var(--ce-border)}.btn-close{filter:var(--ce-closefilter,none)}
html[data-bs-theme=dark] .btn-close{filter:invert(1)}
.ce-modal-lamb{position:relative;height:70px;margin:.2rem 0 .6rem;overflow:hidden;border-radius:.6rem;background:linear-gradient(var(--ce-sky1),var(--ce-sky2))}
.ce-modal-lamb:before{content:"";position:absolute;left:-10%;right:-10%;bottom:-24px;height:46px;border-radius:50%;background:var(--ce-hill1)}
.ce-modal-lamb .animal{bottom:5px;z-index:2}.ce-modal-lamb .animal.lamb svg{width:36px}.ce-modal-lamb .animal.ewe svg{width:54px}
@media (max-width:900px){.ov-stats,.ppe{grid-template-columns:repeat(2,1fr)}.bio-grid,.ov-locks,.who{grid-template-columns:1fr}.people{grid-template-columns:repeat(2,1fr)}}
.ce-mobile-note{position:absolute;right:12px;top:8px;z-index:6;font-size:.72rem;font-weight:600;color:var(--ce-fg);background:color-mix(in srgb,var(--ce-card) 88%,transparent);border:1px solid var(--ce-border);border-radius:1rem;padding:.15rem .7rem;pointer-events:none}
.ce-mobile-note:before{content:"\1F4F1\00a0"}
@media (max-width:700px){.ce-mobile-note{left:18px;right:auto;top:auto;bottom:8px}}
)---"

CE_JS <- r"---(
(function(){
  var T = window.CE_THEMES, F = window.CE_FONTS, store = {};
  try { store = window.localStorage; } catch(e) { store = {}; }
  function get(k, d){ try { return store.getItem(k) || d; } catch(e){ return d; } }
  function put(k, v){ try { store.setItem(k, v); } catch(e){} }
  var cur = {theme: get('ce_theme', window.CE_DEFAULT_THEME), font: get('ce_font', window.CE_DEFAULT_FONT), anim: 'on'};
  function byId(arr, id){ for (var i=0;i<arr.length;i++) if (arr[i].id===id) return arr[i]; return arr[0]; }
  function applyTheme(id, fromUser){
    var t = byId(T, id), r = document.documentElement;
    Object.keys(t.vars).forEach(function(k){ r.style.setProperty('--ce-'+k, t.vars[k]); });
    r.setAttribute('data-sky', t.sky); r.setAttribute('data-bs-theme', t.mode); r.setAttribute('data-ce-theme', t.id);
    document.querySelectorAll('.ce-sw').forEach(function(b){ b.classList.toggle('on', b.getAttribute('data-id')===t.id); });
    cur.theme = t.id; put('ce_theme', t.id); sync('theme_id', t.id);
  }
  function applyFont(id){
    var f = byId(F, id), r = document.documentElement;
    r.style.setProperty('--ce-font', f.stack);
    var pv = document.getElementById('ce-preview'); if (pv) { pv.style.fontFamily = f.stack; pv.querySelector('.meta').textContent = f.name; }
    var sel = document.getElementById('ce-fontsel'); if (sel) sel.value = f.id;
    cur.font = f.id; put('ce_font', f.id); sync('font_id', f.id);
  }
  function applyAnim(on){
    document.documentElement.setAttribute('data-anim', on ? 'on' : 'off');
    var cb = document.getElementById('ce-anim'); if (cb) cb.checked = on;
    cur.anim = on ? 'on' : 'off'; put('ce_anim', cur.anim);
  }
  var ready = false, pending = {};
  function sync(name, v){ if (ready && window.Shiny) Shiny.setInputValue(name, v); else pending[name] = v; }
  document.addEventListener('DOMContentLoaded', function(){
    var b = document.querySelector('.ce-banner'), nav = document.querySelector('nav.navbar');
    if (b && nav && nav.parentNode) nav.parentNode.insertBefore(b, nav);
    applyTheme(cur.theme); applyFont(cur.font); applyAnim(cur.anim === 'on');
  });
  document.addEventListener('click', function(e){
    var s = e.target.closest && e.target.closest('.ce-sw'); if (s) applyTheme(s.getAttribute('data-id'), true);
  });
  document.addEventListener('change', function(e){
    if (e.target.id === 'ce-fontsel') applyFont(e.target.value);
    if (e.target.id === 'ce-anim') applyAnim(e.target.checked);
  });
  $(document).on('shiny:connected', function(){
    ready = true; Object.keys(pending).forEach(function(k){ Shiny.setInputValue(k, pending[k]); });
  });
  // apply early (before DOMContentLoaded) to avoid a flash
  applyTheme(cur.theme);
})();
)---"

SOUND_JS <- r"---(
(function(){
  var ctx = null, last = 0, store = {};
  try { store = window.localStorage; } catch(e) { store = {}; }
  function isOn(){ try { return (store.getItem('ce_sound') || 'on') === 'on'; } catch(e) { return true; } }
  function setOn(v){ try { store.setItem('ce_sound', v ? 'on' : 'off'); } catch(e) {} }
  function ac(){
    if (!ctx) { var C = window.AudioContext || window.webkitAudioContext; if (!C) return null; try { ctx = new C(); } catch(e) { return null; } }
    if (ctx.state === 'suspended') { try { ctx.resume(); } catch(e) {} }
    return ctx;
  }
  function tone(freq, at, dur, gain, glide){
    var c = ac(); if (!c) return;
    var o = c.createOscillator(), g = c.createGain(), t = c.currentTime + at;
    o.type = 'sine'; o.frequency.setValueAtTime(freq, t);
    if (glide) o.frequency.exponentialRampToValueAtTime(freq * glide, t + dur);
    g.gain.setValueAtTime(0.0001, t);
    g.gain.exponentialRampToValueAtTime(gain, t + 0.012);
    g.gain.exponentialRampToValueAtTime(0.0001, t + dur);
    o.connect(g); g.connect(c.destination); o.start(t); o.stop(t + dur + 0.05);
  }
  function play(kind){
    if (!isOn()) return;
    var n = Date.now(); if (n - last < 60) return; last = n;
    if (kind === 'chime') { tone(784, 0, 0.55, 0.045); tone(1175, 0.09, 0.65, 0.035); }
    else { tone(660, 0, 0.09, 0.035, 1.25); }
  }
  document.addEventListener('click', function(e){
    var t = e.target; if (!t || !t.closest) return;
    var el = t.closest('a.nav-link,[data-bs-toggle=tab],button,.btn,.ce-sw,.ce-lock,input[type=checkbox],input[type=radio],label.form-check-label,.selectize-input,.selectize-dropdown .option,.irs-handle,.page-link,.paginate_button');
    if (!el) return;
    if (el.id === 'ce-sound' || (el.getAttribute && el.getAttribute('for') === 'ce-sound')) return;
    var tab = el.matches('a.nav-link,[data-bs-toggle=tab]') && el.closest('.navbar');
    play(tab ? 'chime' : 'tick');
  }, true);
  document.addEventListener('change', function(e){
    if (e.target && e.target.id === 'ce-sound') { setOn(e.target.checked); if (e.target.checked) play('tick'); }
  });
  document.addEventListener('DOMContentLoaded', function(){
    var cb = document.getElementById('ce-sound'); if (cb) cb.checked = isOn();
  });
})();
)---"

CLOCK_JS <- r"---(
(function(){
  var zones = Array.prototype.slice.call(document.querySelectorAll('.ce-zone'));
  var fmt = {};
  function f(tz){ return fmt[tz] || (fmt[tz] = new Intl.DateTimeFormat('en-GB', {timeZone: tz, hour: '2-digit', minute: '2-digit', second: '2-digit', hour12: false, weekday: 'short'})); }
  function tick(){
    var now = new Date(), iso = now.toISOString();
    var u = document.getElementById('ce-utc-t'), d = document.getElementById('ce-utc-d');
    if (u) u.textContent = iso.slice(11, 19);
    if (d) d.textContent = iso.slice(0, 10) + ' \u00b7 ' + ['Sun','Mon','Tue','Wed','Thu','Fri','Sat'][now.getUTCDay()];
    document.querySelectorAll('.ce-zone').forEach(function(el){
      var parts = f(el.getAttribute('data-tz')).formatToParts(now), o = {};
      parts.forEach(function(p){ o[p.type] = p.value; });
      el.querySelector('.z').firstChild.nodeValue = o.hour + ':' + o.minute + ':' + o.second;
      el.querySelector('.w').textContent = o.weekday;
    });
  }
  function start(){ tick(); setTimeout(function(){ tick(); setInterval(tick, 1000); }, 1000 - (Date.now() % 1000)); }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start); else start();
})();
)---"

CLOCK_ZONES <- data.frame(
  continent = c("N. America", "S. America", "Europe", "Africa", "Asia", "Oceania", "Antarctica"),
  city      = c("Vancouver", "S\u00e3o Paulo", "London", "Johannesburg", "Kuala Lumpur", "Sydney", "McMurdo"),
  tz        = c("America/Vancouver", "America/Sao_Paulo", "Europe/London", "Africa/Johannesburg",
                "Asia/Kuala_Lumpur", "Australia/Sydney", "Antarctica/McMurdo"),
  stringsAsFactors = FALSE)

clock_ui <- div(class = "ce-clock",
  div(class = "ce-utc", span(class = "lab", "UTC"), span(id = "ce-utc-t", class = "t", "--:--:--"),
      span(id = "ce-utc-d", class = "d")),
  div(class = "ce-zones",
    lapply(seq_len(nrow(CLOCK_ZONES)), function(i) div(class = "ce-zone", `data-tz` = CLOCK_ZONES$tz[i],
      div(class = "c", CLOCK_ZONES$continent[i]), div(class = "n", CLOCK_ZONES$city[i]),
      div(class = "z", "--:--:--", span(class = "w"))))),
  div(class = "ce-dev", "Developed by Macus Ong"))

# =============================================================================
# FILTER LOOKUPS (tag-level pen + mark cohort)
# =============================================================================
tagmeta <- roster |>
  transmute(tag, pen = str_extract(pen_assignment, "[0-9]")) |>
  left_join(
    ev |> filter(event_type == "Mark") |> group_by(tag) |>
      summarise(mark_date = min(event_date), .groups = "drop"),
    by = "tag") |>
  mutate(mark_year = year(mark_date), mark_month = month(mark_date))
ev2 <- ev |> left_join(select(tagmeta, tag, pen, mark_date, mark_year, mark_month), by = "tag")

DATE_MIN <- min(ev2$event_date); DATE_MAX <- max(ev2$event_date)
YEAR_CHOICES <- c("All years" = "All",
                  setNames(as.character(sort(unique(na.omit(tagmeta$mark_year)))),
                           sort(unique(na.omit(tagmeta$mark_year)))),
                  "Not marked in data" = "Unknown")
MONTH_CHOICES <- c("All months" = "all", setNames(as.character(1:12), month.name))

cohort_filter <- function(d, yr, mo) {
  if (identical(yr, "Unknown")) return(filter(d, is.na(mark_year)))
  if (!identical(yr, "All")) d <- filter(d, mark_year == as.integer(yr))
  if (!identical(mo, "all")) d <- filter(d, mark_month == as.integer(mo))
  d
}

# =============================================================================
# UI
# =============================================================================
kpi <- function(value, label) div(class = "ce-kpi", div(class = "v", value), div(class = "l", label))

theme_buttons <- lapply(THEMES, function(t) {
  v <- t$vars
  tags$button(type = "button", class = "ce-sw", `data-id` = t$id,
    tags$i(style = sprintf("background:linear-gradient(135deg,%s 0 50%%,%s 50%% 100%%)", v$sky1, v$primary)),
    t$name)
})

font_select <- tags$select(id = "ce-fontsel", class = "form-select form-select-sm ce-fontsel",
  lapply(seq_len(nrow(FONTS)), function(i)
    tags$option(value = FONTS$id[i], style = paste0("font-family:", FONTS$stack[i]), FONTS$name[i])))

sidebar_ui <- sidebar(
  width = 560, open = "desktop",
  clock_ui,
  div(class = "ce-two",
    div(
      tags$h6("Filters"),
      checkboxGroupInput("pens", "Pen location", choices = 1:4, selected = 1:4, inline = TRUE),
      selectInput("m_year", "Mark year", YEAR_CHOICES, selected = "All"),
      selectInput("m_month", "Mark month", MONTH_CHOICES, selected = "all"),
      sliderInput("dates", "Date range (scan date)", min = DATE_MIN, max = DATE_MAX,
                  value = c(DATE_MIN, DATE_MAX), timeFormat = "%d %b %y", width = "100%"),
      radioButtons("etype", "Event type", c("All", "Mark", "Recapture"), inline = TRUE),
      selectInput("sessions", "Sessions", choices = levels(sessions$session),
                  selected = levels(sessions$session), multiple = TRUE),
      actionButton("reset", "Reset filters", class = "btn-sm btn-outline-primary w-100")
    ),
    div(
      tags$h6("Settings"),
      tags$label(class = "control-label small", "Theme"),
      div(class = "ce-sw-grid mb-2", unname(theme_buttons)),
      tags$label(class = "control-label small", "Font face"),
      font_select,
      div(id = "ce-preview", class = "ce-preview",
          div(class = "p1", PANGRAMS[1]), div(class = "p2", PANGRAMS[2]), div(class = "p3", PANGRAMS[3]),
          div(class = "meta", "Inter")),
      div(class = "form-check form-switch mt-2",
          tags$input(class = "form-check-input", type = "checkbox", id = "ce-sound", checked = NA),
          tags$label(class = "form-check-label small", `for` = "ce-sound", "Soft sounds"))
    )
  ),
  hr(),
  helpText(HTML("Four independent pens, one treatment each, no migration. <b>Pen</b> = the tag's provisional pen (first record) unless set in <code>data/tag_treatment.csv</code>. <b>Mark year/month</b> = when the tag's Mark event was logged; tags first seen as recaptures are under <i>Not marked in data</i>. Choosing a year or month re-fits the date slider.")),
  if (IS_PUBLIC) div(class = "small text-muted", "Data downloads are disabled on the public site.")
  else layout_columns(col_widths = c(6, 6),
    downloadButton("dl_events", "Tidy events (CSV)", class = "btn-sm btn-outline-primary w-100"),
    downloadButton("dl_roster", "Tag roster (CSV)", class = "btn-sm btn-outline-primary w-100"))
)

# =============================================================================
# OVERVIEW + KEY CONTACTS CONTENT (static, public-safe: no trial results here)
# =============================================================================
scene_animal <- function(kind, pose, left, flip = FALSE, delay = 0, dir = 1)
  sprintf('<div class="animal %s %s%s" style="--d:%ss;--dir:%d;left:%s%%;"><div class="inner">%s</div></div>',
          kind, pose, if (flip) " flip" else "", delay, dir, left, sheep_svg(kind))

bio_card <- function(cls, ico, title, sub, items, scene)
  div(class = paste("bio-card", cls),
    tags$h4(span(class = "badge-ico", icon(ico)), title),
    div(class = "sub", sub),
    tags$ul(lapply(items, function(i) tags$li(i))),
    div(class = "bio-scene", `aria-hidden` = "true", HTML(scene)))

lock_card <- function(id, title, hint, bars, small = FALSE)
  tags$button(type = "button", class = paste("ce-lock", if (small) "ce-lock-sm"),
    onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority:'event'})", id),
    div(class = "ttl", icon("lock"), title, span(class = "pill", "In progress")),
    div(class = "hint", hint),
    if (!is.null(bars)) div(class = "ghost", `aria-hidden` = "true",
      lapply(bars, function(h) tags$i(style = sprintf("height:%d%%", h)))))

results_modal <- function() modalDialog(
  title = tagList(icon("hourglass-half"), " Results in progress"),
  div(class = "ce-modal-lamb", `aria-hidden` = "true",
      HTML(paste0(scene_animal("ewe", "graze", 16), scene_animal("lamb", "frolic", 44),
                  scene_animal("lamb", "frolic", 66, delay = -2)))),
  p("The trial results are still being worked through with the project team."),
  p(class = "text-muted small mb-0", "They will appear here once they have been reviewed and approved."),
  easyClose = TRUE, footer = modalButton("Got it"), size = "m")

ov_stat <- function(v, l) div(class = "ov-stat", div(class = "v", v), div(class = "l", l))

ov_timeline <- function() {
  st <- list(div(class = "st", div(class = "d", "1 Apr 2026"), div(class = "t", "Sheep treated")))
  for (i in seq_len(nrow(sessions))) {
    last <- i == nrow(sessions)
    st[[length(st) + 1]] <- div(class = paste("st", if (last) "fin"),
      div(class = "d", sessions$session_label[i]), div(class = "t", if (last) "Final handling" else "Handling session"))
  }
  st[[length(st) + 1]] <- div(class = "st nxt", div(class = "d", "Next"), div(class = "t", "Field trial, PIB herd"))
  div(class = "ov-tl", st)
}

PPE_STAGES <- list(
  c("Moving sheep", "Boot covers"),
  c("Mugging", "Coveralls, gloves, boot covers"),
  c("Carrying", "Gloves, boot covers"),
  c("Processing", "New boot covers, coveralls and gloves for every pen"),
  c("Clean-up", "Dirty laundry bags; boots sprayed with tea tree"))

overview_ui <- div(class = "ov",
  tags$h3(class = "sec", icon("binoculars"), "The trial at a glance"),
  div(class = "ov-stats",
    ov_stat("~36", "Psoroptes-infested sheep in the captive trial"),
    ov_stat("4", "research enclosures (pens)"),
    ov_stat("3", "drug options tested, plus controls"),
    ov_stat(nrow(sessions), "handling sessions in the PIT data"),
    ov_stat(format(max(sessions$last_date), "%d %b %Y"), "latest handling in the data")),

  tags$h3(class = "sec", icon("route"), "Timeline"),
  ov_timeline(),

  tags$h3(class = "sec", icon("flask"), "Trial results"),
  div(class = "lead", "Click a card for the status."),
  div(class = "ov-locks",
    lock_card("lock_ov1", "Protection windows", "Time to first live mites for each treatment.", c(35, 55, 80, 62, 90)),
    lock_card("lock_ov2", "Mite findings by session", "Live mites on treated sheep at each handling.", c(20, 30, 45, 70, 55))),

  tags$h3(class = "sec", icon("shield-halved"), "Biosecurity: how we keep everyone safe"),
  div(class = "lead", "Psoroptes mites can survive up to 3 weeks off the host, so the care we take today protects sheep we have not met yet."),
  div(class = "bio-grid",
    bio_card("sheep", "paw", "Keeping the sheep safe", "Handling is stressful for them",
      c("Stay calm and quiet. If you are not handling, stay about 40 feet back.",
        "No petting: it mimics a predator. Keep contact to a minimum.",
        "Sheep lie on their left side, facing uphill, so they can breathe.",
        "Body temperature every 3 minutes.",
        "At most five blood attempts per animal, then hand off.",
        "Release in open space away from fences, people stepped back. Feed, water and lock gates before leaving."),
      paste0(scene_animal("ewe", "graze", 8), scene_animal("lamb", "graze", 30, delay = -3),
             scene_animal("lamb", "frolic", 56), scene_animal("lamb", "frolic", 74, delay = -2.5))),
    bio_card("wild", "spray-can-sparkles", "Keeping other wildlife and livestock safe", "Stop the mites travelling",
      c("Tea tree spray on boots, tools, blankets, hobbles and anything set down in a pen.",
        "New boot covers for every pen; bin them between pens.",
        "Wash clothes on hot and dry them on hot.",
        "Will you see other sheep, wild or domestic, in the next 3 weeks? Tell the ONA or BC biologists.",
        "Spray the bottom of the cooler and all handling equipment before it leaves site."),
      paste0('<div class="fence"></div><div class="spray">Tea tree</div>', scene_animal("ewe", "graze", 12),
             scene_animal("lamb", "frolic", 30, delay = -1), scene_animal("ram", "stand", 62))),
    bio_card("staff", "helmet-safety", "Keeping staff safe", "The mites are not contagious to humans",
      c("Boot covers for everyone; gloves for anyone handling sheep; coveralls for muggers and for processing.",
        "New coveralls and gloves for each pen; used ones go in the dirty laundry bags.",
        "Bring water and lunch; wear light layers under coveralls.",
        "Read the carrying, chute and restraint protocols beforehand.",
        "Follow the biologists and vets. Blood and drugs only by qualified people."),
      paste0(scene_animal("ram", "stand", 8), scene_animal("ewe", "graze", 42, delay = -4),
             scene_animal("lamb", "frolic", 66, delay = -1.5)))),
  tags$h4(class = "mt-3 mb-0", style = "font-size:.9rem", "Protective gear by stage"),
  div(class = "ppe", lapply(PPE_STAGES, function(x) div(class = "p", tags$b(x[1]), span(x[2])))),

  tags$h3(class = "sec", icon("heart"), "Why we do this"),
  p("Psoroptic mange is a skin disease of California bighorn sheep in the Okanagan, caused by Psoroptes mites. It brings sores, hair loss, crusted ears and poor body condition."),
  p("The yilíkʷlxkn are relatives with whom the Syilx Nation has shared the tmxʷulaxʷ (land) for thousands of years. This trial looks for treatments that could protect whole herds.")
)

# Original bighorn headshot avatars (inline SVG, fixed natural colours so they read in every theme)
avatar_svg <- function(who) {
  horn <- function(d, wo, wi) sprintf(
    '<path d="%s" fill="none" stroke="#8d7a55" stroke-width="%s" stroke-linecap="round"/><path d="%s" fill="none" stroke="#e6d6b0" stroke-width="%s" stroke-linecap="round"/>', d, wo, d, wi)
  both <- function(x) paste0(x, '<g transform="translate(100 0) scale(-1 1)">', x, '</g>')
  big   <- both(horn("M38,42 C21,27 6,38 11,57 C14,69 28,68 32,60 C34,54 30,49 25,52", 9, 6))
  mid   <- both(horn("M38,42 C26,31 13,37 15,50 C16,59 26,59 28,53", 8, 5.2))
  small <- both(horn("M39,41 C34,31 28,29 24,34", 5, 3))
  horns <- switch(who, mackenzie = big, levi = mid, caeley = small, macus = mid)
  acc <- switch(who,
    mackenzie = '<path d="M30,82 L70,82 L50,100 Z" fill="#3aa76d"/><circle cx="42" cy="86" r="1.3" fill="#fff"/><circle cx="50" cy="91" r="1.3" fill="#fff"/><circle cx="58" cy="86" r="1.3" fill="#fff"/>',
    levi = '<circle cx="40" cy="52" r="7.2" fill="#ffffff" fill-opacity=".25" stroke="#3b2c22" stroke-width="1.8"/><circle cx="60" cy="52" r="7.2" fill="#ffffff" fill-opacity=".25" stroke="#3b2c22" stroke-width="1.8"/><path d="M47.2,52 L52.8,52" stroke="#3b2c22" stroke-width="1.8"/>',
    caeley = '<path d="M36,82 C30,97 44,103 50,96 C56,103 70,97 64,82" fill="none" stroke="#4b5563" stroke-width="2.4" stroke-linecap="round"/><circle cx="50" cy="97" r="3.6" fill="#cbd5e1" stroke="#4b5563" stroke-width="1.6"/>',
    macus = '<path d="M31,43 C29,22 71,22 69,43 Z" fill="#4a90d9"/><rect x="30" y="39" width="40" height="7" rx="3.5" fill="#e0a030"/><circle cx="50" cy="21" r="4.6" fill="#e0a030"/><g transform="translate(60 91)"><ellipse rx="7" ry="3.8" fill="#e0a030"/><path d="M6,0 L11,-4 L11,4 Z" fill="#e0a030"/><circle cx="-3.5" cy="-.7" r=".9" fill="#3b2c22"/></g>')
  delay <- c(mackenzie = 0, levi = -1.7, caeley = -3.2, macus = -4.6)[[who]]
  cid <- paste0("avc-", who)
  paste0('<svg viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="Bighorn sheep avatar">',
    '<defs><clipPath id="', cid, '"><circle cx="50" cy="50" r="50"/></clipPath></defs>',
    '<circle class="av-bg" cx="50" cy="50" r="48"/><g clip-path="url(#', cid, ')">',
    '<path d="M12,100 C12,86 30,80 50,80 C70,80 88,86 88,100 Z" fill="#8a6a4f"/><ellipse cx="50" cy="93" rx="19" ry="9" fill="#efe4d0"/>',
    horns,
    '<g class="ear l"><ellipse cx="28" cy="46" rx="9.5" ry="4.6" transform="rotate(-32 28 46)" fill="#8a6a4f"/><ellipse cx="29" cy="46" rx="5.5" ry="2.4" transform="rotate(-32 29 46)" fill="#d6a58f"/></g>',
    '<g class="ear r"><ellipse cx="72" cy="46" rx="9.5" ry="4.6" transform="rotate(32 72 46)" fill="#8a6a4f"/><ellipse cx="71" cy="46" rx="5.5" ry="2.4" transform="rotate(32 71 46)" fill="#d6a58f"/></g>',
    '<path d="M28,46 C28,31 72,31 72,46 C72,60 66,80 50,84 C34,80 28,60 28,46 Z" fill="#a58463"/>',
    '<ellipse cx="33" cy="62" rx="4.2" ry="2.6" fill="#e59f8a" fill-opacity=".55"/><ellipse cx="67" cy="62" rx="4.2" ry="2.6" fill="#e59f8a" fill-opacity=".55"/>',
    '<ellipse cx="50" cy="72" rx="11" ry="9" fill="#efe4d0"/><ellipse cx="50" cy="66" rx="4.2" ry="3" fill="#3b2c22"/>',
    '<path d="M44,76 Q50,80 56,76" fill="none" stroke="#3b2c22" stroke-width="1.6" stroke-linecap="round"/>',
    sprintf('<g class="eye" style="animation-delay:%ss"><ellipse cx="40" cy="52" rx="3.2" ry="3.7" fill="#2a1f18"/><circle cx="41" cy="50.7" r="1" fill="#fff"/></g>', delay),
    sprintf('<g class="eye" style="animation-delay:%ss"><ellipse cx="60" cy="52" rx="3.2" ry="3.7" fill="#2a1f18"/><circle cx="61" cy="50.7" r="1" fill="#fff"/></g>', delay),
    acc, '</g></svg>')
}

contacts_ui <- div(class = "ov",
  tags$h3(class = "sec", icon("address-book"), "Key contacts"),
  div(class = "people",
    lapply(list(
      list("mackenzie", "Mackenzie Clarke", "tmixʷ (Wildlife) Program Lead, ONA", "Project lead"),
      list("levi", "Levi Oostenbrink", "tmix\u02b7 (Wildlife) Biologist, ONA", "Trial coordination and updates"),
      list("caeley", "Caeley", "Provincial Wildlife Veterinarian", "Drug administration"),
      list("macus", "Macus Ong", "Fisheries Biologist, ONA", "PIT tagging and data")),
      function(x) div(class = "person", div(class = "av", HTML(avatar_svg(x[[1]]))), div(class = "nm", x[[2]]),
                      div(class = "rl", x[[3]]), span(class = "tg", x[[4]])))),
  tags$h3(class = "sec", icon("circle-question"), "Who to ask about what"),
  div(class = "who",
    div(tags$b("The trial overall"), " → Mackenzie Clarke"),
    div(tags$b("Updates and coordination"), " → Levi Oostenbrink"),
    div(tags$b("Drug treatment"), " → Caeley, Provincial Wildlife Veterinarian"),
    div(tags$b("PIT tags and this dashboard"), " → Macus Ong")),
  tags$h3(class = "sec", icon("handshake"), "Partners"),
  div(class = "partners", lapply(c("Wild Sheep Society of BC", "BC Ministry staff (WLRS, FOR, ENV)",
    "Penticton Indian Band", "UBC", "Moredun Research Institute"), span)),
  div(class = "small text-muted mt-3", "Contact details are shared by the project team on request.")
)

ui <- page_navbar(
  title = "Count on Ewe", fillable = FALSE, sidebar = sidebar_ui,
  theme = bs_theme(version = 5, primary = "#2E6FB0"),
  header = tagList(
    tags$head(
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "stylesheet", href = GOOGLE_FONTS_URL),
      tags$script(HTML(sprintf("window.CE_THEMES=%s;window.CE_FONTS=%s;window.CE_DEFAULT_THEME='%s';window.CE_DEFAULT_FONT='%s';",
        jsonlite::toJSON(unname(THEMES), auto_unbox = TRUE), jsonlite::toJSON(FONTS, auto_unbox = TRUE),
        DEFAULT_THEME, DEFAULT_FONT))),
      tags$script(HTML(CE_JS)), tags$script(HTML(CLOCK_JS)), tags$script(HTML(SOUND_JS)),
      tags$style(HTML(CE_CSS)), tags$style(HTML(banner_css))),
    banner_html()),
  nav_spacer(),
  nav_item(span(class = "navbar-text small", sprintf("v%s · %s", APP_VERSION, APP_STAGE))),

  nav_panel("Overview", icon = icon("house"), overview_ui),

  nav_panel("Dashboard", icon = icon("chart-line"),
    layout_columns(col_widths = c(3,3,3,3), fill = FALSE,
      uiOutput("k_tags"), uiOutput("k_sessions"), uiOutput("k_latest"), uiOutput("k_qc")),
    card(card_header("Detections per session"), plotlyOutput("p_session", height = 300)),
    layout_columns(col_widths = c(7,5),
      card(card_header("Capture history — tag × session"), plotlyOutput("p_heat", height = 460)),
      if (HAS_MITES)
        card(card_header("Reported mite results (private: shown only when run locally)"),
             DTOutput("t_mites"),
             div(class = "small text-muted p-2", "From the trial-update emails, not the PIT data."))
      else card(card_header("Trial results"),
             lock_card("lock_dash", "Mite results", "Click for the status.", NULL, small = TRUE)))),

  nav_panel("Tag roster", icon = icon("table-list"),
    card(card_header("One row per individual tag (filtered)"), DTOutput("t_roster"))),

  nav_panel("Capture histories", icon = icon("clock-rotate-left"),
    card(card_header("Scan events"), DTOutput("t_events"))),

  nav_panel("Map", icon = icon("location-dot"),
    card(card_header("Recorded pen coordinates"),
         if (HAS_LEAFLET) leafletOutput("map", height = 520) else DTOutput("map_tbl"),
         div(class = "small text-muted p-2",
             "Handling-site coordinates from the exports. Jun/Jul reuse a few points; Sep carries one coordinate; Oct/Nov carry no GPS. Pen 1 & 2 release coordinates to be added."))),

  nav_panel("Data quality", icon = icon("clipboard-check"),
    card(card_header("Automated cross-checks"),
         div(class="p-2 small text-muted",
             "Run against all loaded files at startup. Severity: ",
             span(style=paste0("color:",sev_col['high'],";font-weight:bold"), "high"), ", ",
             span(style=paste0("color:",sev_col['medium'],";font-weight:bold"), "medium"), ", ",
             span(style=paste0("color:",sev_col['low'],";font-weight:bold"), "low"), "."),
         DTOutput("t_qc")),
    card(card_header("Session summary"), DTOutput("t_sessions"))),

  nav_panel("Key contacts", icon = icon("address-book"), contacts_ui)
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {
  th  <- reactive(THEMES[[input$theme_id %||% DEFAULT_THEME]] %||% THEMES[[DEFAULT_THEME]])
  fnt <- reactive({ s <- FONTS$stack[match(input$font_id %||% DEFAULT_FONT, FONTS$id)]; if (is.na(s)) FONTS$stack[1] else s })

  for (id in c("lock_ov1", "lock_ov2", "lock_dash")) local({
    i <- id
    observeEvent(input[[i]], showModal(results_modal()), ignoreInit = TRUE)
  })

  # date slider follows the mark year / month
  observeEvent(list(input$m_year, input$m_month), {
    req(input$m_year, input$m_month)
    d <- cohort_filter(ev2, input$m_year, input$m_month)
    rng <- if (nrow(d)) range(d$event_date) else c(DATE_MIN, DATE_MAX)
    updateSliderInput(session, "dates", min = rng[1], max = rng[2], value = rng)
  })

  observeEvent(input$reset, {
    updateCheckboxGroupInput(session, "pens", selected = 1:4)
    updateSelectInput(session, "m_year", selected = "All")
    updateSelectInput(session, "m_month", selected = "all")
    updateSelectInput(session, "sessions", selected = levels(sessions$session))
    updateRadioButtons(session, "etype", selected = "All")
    updateSliderInput(session, "dates", min = DATE_MIN, max = DATE_MAX, value = c(DATE_MIN, DATE_MAX))
  })

  filt <- reactive({
    req(input$dates, input$m_year, input$m_month)
    d <- ev2 |> filter(session %in% input$sessions, pen %in% input$pens,
                       event_date >= input$dates[1], event_date <= input$dates[2])
    d <- cohort_filter(d, input$m_year, input$m_month)
    if (input$etype != "All") d <- filter(d, event_type == input$etype)
    d
  })
  last_ses <- tail(levels(sessions$session), 1)

  pal <- reactive({ v <- th()$vars; c(Mark = v$mark, Recapture = v$recap, `Not detected` = v$none) })
  gg_theme <- reactive({
    v <- th()$vars
    theme_minimal(base_size = 12) + theme(
      text = element_text(colour = v$fg), axis.text = element_text(colour = v$muted),
      panel.grid = element_line(colour = v$border, linewidth = 0.25),
      plot.background = element_rect(fill = NA, colour = NA), panel.background = element_rect(fill = NA, colour = NA),
      legend.text = element_text(colour = v$fg))
  })
  style_plotly <- function(p) {
    v <- th()$vars
    p |> layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
                font = list(color = v$fg, family = fnt()), legend = list(orientation = "h"))
  }

  output$k_tags     <- renderUI(kpi(n_distinct(filt()$tag), sprintf("Tags in current filter (of %d)", n_distinct(ev$tag))))
  output$k_sessions <- renderUI(kpi(n_distinct(filt()$session), sprintf("Sessions in filter (of %d)", nrow(sessions))))
  output$k_latest   <- renderUI(kpi(sum(roster$last_session == last_ses & roster$tag %in% filt()$tag), paste("Filtered tags detected at", last_ses)))
  output$k_qc       <- renderUI(kpi(sum(qc$severity %in% c("high","medium") & qc$n > 0), "Open data-quality flags"))

  output$p_session <- renderPlotly({
    d <- filt(); validate(need(nrow(d) > 0, "No scans match the current filters."))
    d <- d |> count(session, event_type) |> mutate(session = factor(session, levels = levels(sessions$session)))
    g <- ggplot(d, aes(session, n, fill = event_type, text = paste0(session, "<br>", event_type, ": ", n))) +
      geom_col() + scale_fill_manual(values = pal(), name = NULL) + labs(x = NULL, y = "Scans") + gg_theme()
    style_plotly(ggplotly(g, tooltip = "text"))
  })

  output$p_heat <- renderPlotly({
    keep <- unique(filt()$tag); validate(need(length(keep) > 0, "No tags match the current filters."))
    lv <- levels(sessions$session)
    d <- presence |> filter(tag %in% keep) |>
      mutate(session = factor(session, levels = lv), tag = factor(tag, levels = rev(roster$tag)))
    g <- ggplot(d, aes(session, tag, fill = event_type, text = paste0(tag, "<br>", session, ": ", event_type))) +
      geom_tile(colour = th()$vars$card, linewidth = 0.4) +
      scale_fill_manual(values = pal(), name = NULL) + labs(x = NULL, y = NULL) +
      gg_theme() + theme(axis.text.y = element_text(size = 6))
    style_plotly(ggplotly(g, tooltip = "text"))
  })

  output$t_mites <- renderDT(datatable(
    mites |> select(session, trial_pen, treatment, n_nonctl_live_mites, mites_present),
    rownames = FALSE, options = list(dom = "t", pageLength = 10)))

  output$t_roster <- renderDT({
    keep <- unique(filt()$tag)
    datatable(roster |> filter(tag %in% keep) |>
        select(tag, dec, pen_assignment, pen_source, first_session, last_session, last_seen,
               n_sessions, sessions, pen_labels_seen, status, retagged_in_dataset),
      rownames = FALSE, filter = "top", options = list(pageLength = 15, scrollX = TRUE))
  })

  output$t_events <- renderDT(datatable(
    filt() |> arrange(event_dt) |>
      select(session, event_date, tag, dec = dec_calc, event_type, pen_label, pen, life_stage,
             retagged = retag, pregnant, file),
    rownames = FALSE, filter = "top", options = list(pageLength = 20, scrollX = TRUE)))

  pts <- reactive(filt() |> filter(!is.na(lat)) |>
      group_by(session, pen_label, lat, lon) |> summarise(n = n(), .groups = "drop"))
  if (HAS_LEAFLET) {
    output$map <- renderLeaflet({
      leaflet(pts()) |> addProviderTiles(providers$Esri.WorldImagery) |>
        addCircleMarkers(~lon, ~lat, radius = ~pmax(5, sqrt(n) * 3), color = th()$vars$accent,
                         fillOpacity = 0.7, stroke = TRUE, weight = 1,
                         popup = ~paste0(session, " — Pen ", pen_label, "<br>", n, " scans<br>", round(lat,5), ", ", round(lon,5)))
    })
  } else {
    output$map_tbl <- renderDT(datatable(pts(), rownames = FALSE, options = list(dom = "t", pageLength = 20),
      caption = "leaflet not installed — showing coordinates as a table."))
  }

  output$t_qc <- renderDT(datatable(
    qc |> arrange(factor(severity, c("high","medium","low","info")), id) |> select(id, severity, check, n, detail),
    rownames = FALSE, options = list(pageLength = 20, dom = "t")) |>
      formatStyle("severity", color = styleEqual(names(sev_col), unname(sev_col)), fontWeight = "bold"))

  output$t_sessions <- renderDT(datatable(sessions, rownames = FALSE, options = list(dom = "t")))

  if (!IS_PUBLIC) {
    output$dl_events <- downloadHandler("count_on_ewe_events.csv",
      function(f) write_csv(filt() |> select(session, event_date, tag, dec_calc, event_type, pen_label, pen, life_stage, retag, pregnant, lat, lon, file), f))
    output$dl_roster <- downloadHandler("count_on_ewe_roster.csv",
      function(f) write_csv(roster |> filter(tag %in% filt()$tag), f))
  }
}

shinyApp(ui, server)

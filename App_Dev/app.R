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

APP_VERSION <- "0.1.0"          # keep in sync with /VERSION and CHANGELOG.md
APP_STAGE   <- "beta"

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
mites     <- read_csv(file.path(DATA_DIR, "reported_mite_results.csv"), show_col_types = FALSE)

pal_event <- c(Mark = "#1F7A3D", Recapture = "#2E6FB0", `Not detected` = "#D9D4CC")
sev_col   <- c(high = "#B23A48", medium = "#C58A2E", low = "#4E8A6B", info = "#5B6B7B")

# =============================================================================
# UI
# =============================================================================
kpi <- function(value, label) div(class = "border rounded-3 p-3 h-100 bg-white",
  div(class = "fs-3 fw-semibold", value), div(class = "text-muted small", label))

ui <- page_navbar(
  title = "Count on Ewe", theme = bs_theme(version = 5, primary = "#2E6FB0"), fillable = FALSE,
  nav_spacer(),
  nav_item(span(class = "navbar-text small text-muted", sprintf("v%s · %s", APP_VERSION, APP_STAGE))),
  sidebar = sidebar(
    width = 270,
    selectInput("sessions", "Sessions", choices = levels(sessions$session),
                selected = levels(sessions$session), multiple = TRUE),
    selectInput("pen", "Pen label (raw)", choices = c("All", sort(unique(na.omit(ev$pen_label)))), selected = "All"),
    radioButtons("etype", "Event type", c("All", "Mark", "Recapture"), inline = TRUE),
    hr(),
    helpText(HTML("Four independent pens, one treatment each, no migration. Raw pen labels are reused across sessions &mdash; see <b>Data Quality</b>. Confirm the true pen per tag in <code>data/tag_treatment.csv</code>.")),
    downloadButton("dl_events", "Tidy events (CSV)", class = "btn-sm btn-outline-primary w-100 mb-2"),
    downloadButton("dl_roster", "Tag roster (CSV)", class = "btn-sm btn-outline-primary w-100")
  ),

  nav_panel("Overview", icon = icon("chart-line"),
    layout_columns(col_widths = c(3,3,3,3), fill = FALSE,
      uiOutput("k_tags"), uiOutput("k_sessions"), uiOutput("k_latest"), uiOutput("k_qc")),
    card(card_header("Detections per session"), plotlyOutput("p_session", height = 300)),
    layout_columns(col_widths = c(7,5),
      card(card_header("Capture history — tag × session"), plotlyOutput("p_heat", height = 460)),
      card(card_header("Reported mite results (from trial-update emails, not the PIT data)"),
           DTOutput("t_mites"),
           div(class = "small text-muted p-2",
               "Pen 1 (Fluralaner alone): protection ~3 months. Pen 2 (Fluralaner + moxidectin): mite-free 5.5 months, mites by 22 Oct. Pen 1 & 2 released the evening of 13 Nov 2026.")))),

  nav_panel("Tag roster", icon = icon("table-list"),
    card(card_header("One row per individual tag"), DTOutput("t_roster"))),

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
    card(card_header("Session summary"), DTOutput("t_sessions")))
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {
  filt <- reactive({
    d <- ev |> filter(session %in% input$sessions)
    if (input$pen != "All")   d <- filter(d, pen_label == input$pen)
    if (input$etype != "All") d <- filter(d, event_type == input$etype)
    d
  })
  last_ses <- tail(levels(sessions$session), 1)

  output$k_tags     <- renderUI(kpi(n_distinct(ev$tag), "Distinct tags (all sessions)"))
  output$k_sessions <- renderUI(kpi(nrow(sessions), "Handling sessions"))
  output$k_latest   <- renderUI(kpi(sum(roster$last_session == last_ses), paste("Detected at", last_ses)))
  output$k_qc       <- renderUI(kpi(sum(qc$severity %in% c("high","medium") & qc$n > 0), "Open data-quality flags"))

  output$p_session <- renderPlotly({
    d <- filt() |> count(session, event_type) |>
      mutate(session = factor(session, levels = levels(sessions$session)))
    g <- ggplot(d, aes(session, n, fill = event_type, text = paste0(session, "<br>", event_type, ": ", n))) +
      geom_col() + scale_fill_manual(values = pal_event, name = NULL) +
      labs(x = NULL, y = "Scans") + theme_minimal(base_size = 12)
    ggplotly(g, tooltip = "text") |> layout(legend = list(orientation = "h"))
  })

  output$p_heat <- renderPlotly({
    lv <- levels(sessions$session)
    d <- presence |> filter(tag %in% filt()$tag) |>
      mutate(session = factor(session, levels = lv), tag = factor(tag, levels = rev(roster$tag)))
    g <- ggplot(d, aes(session, tag, fill = event_type, text = paste0(tag, "<br>", session, ": ", event_type))) +
      geom_tile(colour = "white", linewidth = 0.4) +
      scale_fill_manual(values = pal_event, name = NULL) +
      labs(x = NULL, y = NULL) + theme_minimal(base_size = 9) + theme(axis.text.y = element_text(size = 6))
    ggplotly(g, tooltip = "text")
  })

  output$t_mites <- renderDT(datatable(
    mites |> select(session, trial_pen, treatment, n_nonctl_live_mites, mites_present),
    rownames = FALSE, options = list(dom = "t", pageLength = 10)))

  output$t_roster <- renderDT(datatable(
    roster |> select(tag, dec, pen_assignment, pen_source, first_session, last_session, last_seen,
                     n_sessions, sessions, pen_labels_seen, status, retagged_in_dataset),
    rownames = FALSE, filter = "top", options = list(pageLength = 15, scrollX = TRUE)))

  output$t_events <- renderDT(datatable(
    filt() |> arrange(event_dt) |>
      select(session, event_date, tag, dec = dec_calc, event_type, pen_label, life_stage,
             retagged = retag, pregnant, file),
    rownames = FALSE, filter = "top", options = list(pageLength = 20, scrollX = TRUE)))

  pts <- reactive(ev |> filter(!is.na(lat)) |>
      group_by(session, pen_label, lat, lon) |> summarise(n = n(), .groups = "drop"))
  if (HAS_LEAFLET) {
    output$map <- renderLeaflet({
      leaflet(pts()) |> addProviderTiles(providers$Esri.WorldImagery) |>
        addCircleMarkers(~lon, ~lat, radius = ~pmax(5, sqrt(n) * 3), color = "#F4C430",
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

  output$dl_events <- downloadHandler("count_on_ewe_events.csv",
    function(f) write_csv(ev |> select(session, event_date, tag, dec_calc, event_type, pen_label, life_stage, retag, pregnant, lat, lon, file), f))
  output$dl_roster <- downloadHandler("count_on_ewe_roster.csv",
    function(f) write_csv(roster, f))
}

shinyApp(ui, server)

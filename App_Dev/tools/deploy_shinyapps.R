# Publish Count on Ewe to shinyapps.io.
# Run from the PROJECT ROOT (the folder containing App_Dev and data_public):
#   source("App_Dev/tools/deploy_shinyapps.R")
# Uploads ONLY App_Dev/app.R and whatever is in data_public/.
#
# First time on a new machine, run once (token from shinyapps.io -> Account -> Tokens):
#   rsconnect::setAccountInfo(name="macus-ong", token="...", secret="...")
# Never paste that token into a chat or commit it anywhere.

APP_NAME  <- "count-on-ewe"                 # shinyapps.io slug -> macus-ong.shinyapps.io/count-on-ewe
APP_TITLE <- "Count on Ewe — Bighorn Psoroptes PIT Tags"
SRC_APP   <- file.path("App_Dev", "app.R")
PUB_DATA  <- "data_public"

if (!requireNamespace("rsconnect", quietly = TRUE))
  stop("Install rsconnect first: install.packages(\"rsconnect\")")
if (!file.exists(SRC_APP))
  stop("Run this from the project folder (App_Dev/app.R was not found).")
if (!dir.exists(PUB_DATA))
  stop("Create a folder named data_public next to App_Dev and copy into it ",
       "ONLY the data files approved for the public website.")

pub_files <- list.files(PUB_DATA, pattern = "\\.(csv|xlsx)$", full.names = TRUE, ignore.case = TRUE)
if (!length(pub_files)) stop("data_public has no csv or xlsx files.")
if (!"manifest.csv" %in% basename(pub_files))
  stop("data_public must include manifest.csv (the app reads it to know which exports to load).")

src_lines <- readLines(SRC_APP, warn = FALSE)
ver_line  <- grep("^APP_VERSION\\s*<-", src_lines, value = TRUE)[1]
app_ver   <- sub('.*"([0-9.]+)".*', "\\1", ver_line)

cat("\nAbout to publish version", app_ver, "\n")
cat("Public data files (", length(pub_files), "):\n", sep = "")
cat(paste0("  ", basename(pub_files), collapse = "\n"), "\n\n")
ans <- readline("These files become publicly viewable through the website. Continue? (y/n) ")
if (!tolower(trimws(ans)) %in% c("y", "yes")) stop("Cancelled. Nothing was uploaded.")

stage <- file.path(tempdir(), "count_on_ewe_stage")
unlink(stage, recursive = TRUE)
dir.create(file.path(stage, "data"), recursive = TRUE)
file.copy(SRC_APP, file.path(stage, "app.R"))
file.copy(pub_files, file.path(stage, "data"))

rsconnect::deployApp(appDir = stage, appName = APP_NAME, appTitle = APP_TITLE,
                     forceUpdate = TRUE, launch.browser = TRUE)
cat("\nDone. Version", app_ver, "is live.\n")

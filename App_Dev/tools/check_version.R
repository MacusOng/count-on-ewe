# Confirms APP_VERSION in app.R, the VERSION file, and the top of
# CHANGELOG.md all agree. Run from the project root:
#   Rscript App_Dev/tools/check_version.R
app_lines <- readLines("App_Dev/app.R", warn = FALSE)
ver_line  <- grep('^APP_VERSION\\s*<-', app_lines, value = TRUE)[1]
app_ver   <- sub('.*"([0-9.]+)".*', "\\1", ver_line)

file_ver  <- trimws(readLines("VERSION", warn = FALSE)[1])

cl_lines  <- readLines("App_Dev/CHANGELOG.md", warn = FALSE)
cl_line   <- grep("^## ", cl_lines, value = TRUE)[1]
cl_ver    <- sub("^## ([0-9.]+).*", "\\1", cl_line)

ok <- app_ver == file_ver && app_ver == cl_ver
cat(sprintf("app.R: %s | VERSION: %s | CHANGELOG top: %s -> %s\n",
            app_ver, file_ver, cl_ver, if (ok) "OK" else "MISMATCH - fix before committing"))
if (!ok) stop("Version mismatch.")

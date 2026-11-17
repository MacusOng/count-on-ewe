# Run from the PROJECT ROOT:  Rscript App_Dev/tests/run_tests.R
# Sources the app (which defines the ingest functions and loads `ev`/`DATA_DIR`),
# then runs the ingest tests. shinyApp() at the end of app.R just builds the
# object; it does not launch a server here.
suppressWarnings(suppressMessages(invisible(source("App_Dev/app.R"))))
testthat::test_dir("App_Dev/tests/testthat", stop_on_failure = TRUE)

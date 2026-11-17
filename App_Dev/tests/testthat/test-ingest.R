# Run from the PROJECT ROOT:  Rscript App_Dev/tests/run_tests.R
library(testthat)

test_that("all sessions load with the expected totals", {
  s <- session_order(ev)
  expect_equal(nrow(s), 5)                 # Jun, Jul, Sep, Oct, Nov
  expect_equal(sum(s$n_records), 133)
  expect_equal(dplyr::n_distinct(ev$tag), 35)
})

test_that("hex tag codes convert to the decimal codes in the comments", {
  chk <- ev[!is.na(ev$dec_comment), ]
  expect_true(all(chk$dec_comment == chk$dec_calc))
})

test_that("no tag is scanned twice within one session", {
  expect_equal(max(dplyr::count(ev, session, tag)$n), 1)
})

test_that("October date correction applied, raw date retained", {
  oct <- ev[ev$session == "Oct", ]
  expect_true(all(oct$event_date == as.Date("2026-10-22")))
  expect_true(any(oct$event_dt_raw > as.Date("2026-11-01")))
})

test_that("roster has one row per tag; 23 reach the final session and are released", {
  r <- build_roster(ev, DATA_DIR)
  expect_equal(nrow(r), 35)
  expect_equal(sum(r$last_session == "Nov"), 23)
  expect_equal(sum(r$released), 23)
})

test_that("QC returns all checks; no dup/hex errors; pen-label inconsistency flagged", {
  q <- run_qc(ev, DATA_DIR)
  expect_equal(nrow(q), 13)
  expect_equal(q$n[q$id == "Q05"], 0L)    # no within-session duplicates
  expect_equal(q$n[q$id == "Q06"], 0L)    # hex/dec consistent
  expect_gt(q$n[q$id == "Q08"], 0L)       # pen labels inconsistent across sessions
})

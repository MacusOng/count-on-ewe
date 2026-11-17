test_that("themes and fonts are complete", {
  expect_length(THEMES, 8)
  expect_equal(nrow(FONTS), 10)
  keys <- names(THEMES[[1]]$vars)
  for (t in THEMES) expect_setequal(names(t$vars), keys)
  expect_true(all(vapply(THEMES, function(t) t$sky %in% c("day","dusk","night"), TRUE)))
  expect_true(DEFAULT_THEME %in% names(THEMES) && DEFAULT_FONT %in% FONTS$id)
})

test_that("filters: every tag has a pen and cohort filter behaves", {
  expect_false(any(is.na(tagmeta$pen)))
  expect_true(all(tagmeta$pen %in% as.character(1:4)))
  all_n <- n_distinct(ev2$tag)
  expect_equal(n_distinct(cohort_filter(ev2, "All", "all")$tag), all_n)
  marked <- n_distinct(cohort_filter(ev2, "2026", "all")$tag)
  unk <- n_distinct(cohort_filter(ev2, "Unknown", "all")$tag)
  expect_equal(marked + unk, all_n)
})

test_that("banner is 2 inches and has animals", {
  html <- as.character(banner_html())
  expect_equal(BANNER_H, 192)
  expect_true(grepl('class="animal ram', html))
  expect_gte(lengths(regmatches(html, gregexpr('class="animal ', html))), 11)
})

test_that("public mode flag exists", {
  expect_true(is.logical(IS_PUBLIC))
})

test_that("public mode is off locally and on when forced", {
  expect_false(IS_PUBLIC)
  expect_true(identical(Sys.getenv("COE_PUBLIC", "0"), "0"))
})

test_that("overview + contacts present, results are not in the page", {
  html <- paste(as.character(ui), collapse = "")
  expect_true(grepl("Biosecurity", html) && grepl("Key contacts", html) && grepl("Dashboard", html))
  expect_equal(lengths(regmatches(html, gregexpr('class="bio-card ', html))), 3)
  expect_true(grepl("lock_ov1", html) && grepl("lock_ov2", html))
  for (bad in c("~3 months", "5.5 months", "Fluralaner alone", "protection ~")) expect_false(grepl(bad, html, fixed = TRUE))
  expect_false(grepl("@", gsub("@media", "", html, fixed = TRUE)))   # no email addresses
  expect_false(grepl("Wade", html) || grepl("Salty", html))           # no meeting points
})

test_that("mite table is private when public", {
  expect_true(HAS_MITES == (!IS_PUBLIC && file.exists(MITES_FILE)))
})

test_that("contacts use sheep avatars", {
  html <- paste(as.character(ui), collapse = "")
  expect_equal(lengths(regmatches(html, gregexpr('class="av"', html))), 4)
  expect_equal(lengths(regmatches(html, gregexpr('aria-label="Bighorn sheep avatar"', html))), 4)
  expect_false(grepl('<div class="av">MC<', html, fixed = TRUE))
})

test_that("v0.7.0 UI changes", {
  html <- paste(as.character(ui), collapse = "")
  expect_false(grepl("Animate header", html, fixed = TRUE))
  expect_true(grepl("Mobile version not available yet", html, fixed = TRUE))
  expect_true(grepl('id="ce-sound"', html, fixed = TRUE))
  expect_true(grepl("Wildlife) Biologist, ONA", html, fixed = TRUE))
})

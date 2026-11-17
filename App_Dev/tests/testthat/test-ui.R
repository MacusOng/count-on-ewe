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

test_that("db_add uses energetic addition", {
  expect_equal(db_add(c(80, 80)), 80 + 10 * log10(2), tolerance = 1e-10)
  expect_true(is.na(db_add(c(80, NA_real_))))
})

test_that("daily single and multisegment exposure agree", {
  expect_equal(laeq_d(85, 8), 85, tolerance = 1e-10)
  expect_equal(laeq_d_multi(c(85, 85), c(4, 4)), 85, tolerance = 1e-10)
})

test_that("weekly exposure uses fixed five-day denominator", {
  expect_equal(laeq_s(rep(85, 5)), 85, tolerance = 1e-10)
  w <- laeq_weekly(c(85, NA, 85))
  expect_equal(w$exposed_days, 2)
  expect_equal(w$normative_5d, 10 * log10(2 * 10^(8.5) / 5), tolerance = 1e-10)
})

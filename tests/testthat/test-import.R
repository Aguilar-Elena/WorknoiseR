test_that("numeric_flexible handles list columns and comma decimals", {
  x <- list("1,5", "2.5", NULL)
  expect_equal(numeric_flexible(x)[1:2], c(1.5, 2.5))
  expect_true(is.na(numeric_flexible(x)[3]))
})

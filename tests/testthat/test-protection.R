test_that("HML and SNR return finite protected levels", {
  h <- hml_protection(90, 94, 30, 25, 20)
  expect_true(is.finite(h$laeq_t_protected))
  expect_equal(snr_protection(95, 25), 70)
})

test_that("APV uses mf minus sf", {
  r <- apv_protection(c(80, 80), c("63", "125"), c(20, 20), c(2, 2), c("63", "125"))
  expect_equal(r$table$APV_dB, c(18, 18))
  expect_equal(r$laeq_t_protected, db_add(c(62, 62)))
})

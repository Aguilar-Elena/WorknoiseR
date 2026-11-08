test_that("free-field propagation is consistent", {
  expect_equal(level_at_distance_free_field(100, 10), 80, tolerance = 1e-10)
  expect_equal(distance_for_level_free_field(100, 80), 10, tolerance = 1e-10)
})

test_that("receiver combines multiple sources energetically", {
  s <- data.frame(x = c(0, 0), y = c(0, 0), Li = c(90, 90))
  expect_equal(receiver_level(1, 0, s), db_add(c(90, 90)), tolerance = 1e-8)
})

test_that("grid and inverse estimation are available", {
  s <- data.frame(nombre = "S1", x = 0, y = 0, Li = 90)
  g <- noise_grid(s, domain = c(0, 2, 0, 2), resolution = 1)
  expect_true(all(c("x", "y", "Lp") %in% names(g)))
  m <- data.frame(x = c(1, 2), y = c(0, 0), Lp = c(90, 90 - 20 * log10(2)))
  est <- estimate_source_levels(m, s, start = 85)
  expect_equal(unname(est$levels), 90, tolerance = 0.05)
})

test_that("language preference supports Spanish and English", {
  old <- worknoiser_language()
  on.exit(set_worknoiser_language(old), add = TRUE)

  set_worknoiser_language("es")
  expect_equal(worknoiser_language(), "es")
  expect_equal(worknoiser_translate("not_evaluated"), "No evaluado")

  set_worknoiser_language("en")
  expect_equal(worknoiser_language(), "en")
  expect_equal(worknoiser_translate("not_evaluated"), "Not evaluated")
})

test_that("automatic legal framework follows application language", {
  es <- evaluate_noise_compliance(85, 137, language = "es")
  en <- evaluate_noise_compliance(85, 137, language = "en")
  expect_equal(attr(es, "framework"), "rd286")
  expect_equal(attr(en, "framework"), "eu2003_10")
  expect_true(any(grepl("Real Decreto 286/2006", es[[2]], fixed = TRUE)))
  expect_true(any(grepl("Directive 2003/10/EC", en[[2]], fixed = TRUE)))
  expect_true(any(grepl("85.00 dB\\(A\\)", es[[2]])))
  expect_true(any(grepl("85.00 dB\\(A\\)", en[[2]])))
  expect_true(any(grepl("140 Pa", en[[2]], fixed = TRUE)))
})

test_that("evaluate_rd286 remains a Spanish-framework compatibility wrapper", {
  en <- evaluate_rd286(85, 137, language = "en")
  expect_equal(attr(en, "framework"), "rd286")
  expect_true(any(grepl("Spanish Royal Decree 286/2006", en[[2]], fixed = TRUE)))
})

test_that("Directive 2003/10/EC thresholds are represented with EU terminology", {
  low <- evaluate_noise_compliance(80, 135, language = "en")
  upper <- evaluate_noise_compliance(85, 137, language = "en")
  limit <- evaluate_noise_compliance(87.1, 141, language = "en")

  expect_true(any(grepl("Lower action value", low[[2]], fixed = TRUE)))
  expect_true(any(grepl("112 Pa", low[[2]], fixed = TRUE)))
  expect_true(any(grepl("Upper action value", upper[[2]], fixed = TRUE)))
  expect_true(any(grepl("140 Pa", upper[[2]], fixed = TRUE)))
  expect_true(any(grepl("200 Pa", limit[[1]], fixed = TRUE)))
  expect_true(any(grepl("Exceeds 87 dB(A)", limit[[2]], fixed = TRUE)))
  expect_true(any(grepl("Exceeds 140 dB(C)", limit[[2]], fixed = TRUE)))
})

test_that("language does not alter acoustic calculations", {
  old <- worknoiser_language()
  on.exit(set_worknoiser_language(old), add = TRUE)
  set_worknoiser_language("es")
  es <- c(
    db_add(c(80, 80)),
    laeq_d_multi(c(90, 85, 80), c(2, 3, 1)),
    laeq_s(c(80, 82, 84, 86, 88)),
    noise_dose(85, 4),
    receiver_level(10, 0, data.frame(x = 0, y = 0, level = 100))
  )
  set_worknoiser_language("en")
  en <- c(
    db_add(c(80, 80)),
    laeq_d_multi(c(90, 85, 80), c(2, 3, 1)),
    laeq_s(c(80, 82, 84, 86, 88)),
    noise_dose(85, 4),
    receiver_level(10, 0, data.frame(x = 0, y = 0, level = 100))
  )
  expect_equal(es, en, tolerance = 1e-12)
})

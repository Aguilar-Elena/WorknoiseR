cat("\nWorknoiseR bilingual release validation / Validación bilingüe de release\n")
cat("=====================================================================\n")

stopifnot(file.exists("DESCRIPTION"))
d <- read.dcf("DESCRIPTION")
stopifnot(d[1, "Package"] == "WorknoiseR")
stopifnot(d[1, "Version"] == "0.5.0")

cat("Package / Paquete:", d[1, "Package"], "\n")
cat("Version / Versión:", d[1, "Version"], "\n")

r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
for (f in r_files) source(f, local = .GlobalEnv)

required <- c(
  "db_add", "laeq_d", "laeq_d_multi", "laeq_s", "laeq_weekly",
  "noise_dose", "max_exposure_time", "evaluate_rd286",
  "apv_protection", "hml_protection", "snr_protection",
  "level_at_distance_free_field", "distance_for_level_free_field",
  "receiver_level", "noise_grid", "noise_contours",
  "estimate_source_levels", "read_noise_csv", "run_noise_app",
  "worknoiser_language", "set_worknoiser_language", "worknoiser_translate"
)
missing <- required[!vapply(required, exists, logical(1), mode = "function")]
if (length(missing)) stop("Missing functions / Funciones ausentes: ", paste(missing, collapse = ", "))

stopifnot(abs(db_add(c(80, 80)) - 83.0102999566) < 1e-6)
stopifnot(abs(laeq_d(85, 8) - 85) < 1e-12)
stopifnot(abs(laeq_d_multi(c(90,85,80), c(2,3,1)) - 85.8067) < 0.01)
stopifnot(abs(laeq_s(c(80,82,84,86,88)) - 84.8769) < 0.01)
stopifnot(abs(noise_dose(85, 4) - 50) < 1e-12)
stopifnot(abs(max_exposure_time(88) - 4) < 1e-12)
stopifnot(abs(level_at_distance_free_field(100, 10) - 80) < 1e-12)

src <- data.frame(x = c(0,20), y = c(0,0), level = c(100,100))
stopifnot(abs(receiver_level(10,0,src) - 83.0102999566) < 1e-6)
cat("Core regression checks / Regresión del núcleo: PASS\n")

set_worknoiser_language("es")
es <- evaluate_rd286(85, 137)
set_worknoiser_language("en")
en <- evaluate_rd286(85, 137)
stopifnot(nrow(es) == nrow(en))
stopifnot(any(grepl("Valor superior", es[[2]], fixed = TRUE)))
stopifnot(any(grepl("Upper action", en[[2]], fixed = TRUE)))
stopifnot(worknoiser_translate("not_evaluated", "es") == "No evaluado")
stopifnot(worknoiser_translate("not_evaluated", "en") == "Not evaluated")
set_worknoiser_language("es")
cat("Bilingual checks / Comprobaciones bilingües: PASS\n")

if (requireNamespace("testthat", quietly = TRUE)) {
  cat("Running testthat suite / Ejecutando testthat...\n")
  testthat::test_dir("tests/testthat", reporter = "summary")
} else {
  cat("testthat not installed / testthat no instalado: unit tests not run.\n")
}

cat("\nWorknoiseR 0.5.0 bilingual release validation / validación bilingüe: PASS\n")

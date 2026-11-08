# Validación rápida / Quick validation of the WorknoiseR computational core.
for (f in c("00_constants.R", "05_i18n.R", "10_exposure.R", "20_protection.R", "30_spatial.R", "40_import.R")) {
  source(file.path("R", f), local = FALSE)
}
stopifnot(abs(db_add(c(80,80)) - 83.0102999566) < 1e-8)
stopifnot(abs(laeq_d(85,8) - 85) < 1e-10)
stopifnot(abs(laeq_d_multi(c(85,85), c(4,4)) - 85) < 1e-10)
stopifnot(abs(laeq_s(rep(85,5)) - 85) < 1e-10)
stopifnot(abs(level_at_distance_free_field(100,10) - 80) < 1e-10)
s <- data.frame(nombre="S1", x=0, y=0, Li=90)
stopifnot(abs(receiver_level(1,0,s) - 90) < 1e-8)
g <- noise_grid(s, domain=c(0,2,0,2), resolution=1)
stopifnot(nrow(g) >= 9, all(c("x","y","Lp") %in% names(g)))
stopifnot(worknoiser_translate("not_evaluated", "es") == "No evaluado")
stopifnot(worknoiser_translate("not_evaluated", "en") == "Not evaluated")
cat("WorknoiseR core: VALIDACIÓN BÁSICA / BASIC VALIDATION PASSED\n")

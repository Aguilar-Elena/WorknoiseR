# WorknoiseR core constants and low-level utilities

.worknoiser_octave_bands <- c("31.5", "63", "125", "250", "500", "1000", "2000", "4000", "8000")
.worknoiser_weighting_a <- c(-39.4, -26.2, -16.1, -8.6, -3.2, 0.0, 1.2, 1.0, -1.1)
.worknoiser_weighting_c <- c(-3.0, -0.8, -0.2, 0.0, 0.0, 0.0, -0.2, -0.8, -3.0)
.worknoiser_apv_bands <- c("63", "125", "250", "500", "1000", "2000", "4000", "8000")

#' Energetic sum of sound levels
#'
#' @param x Numeric vector of levels in dB.
#' @param na.rm Logical. Remove missing values before calculation.
#' @return Energetic sum in dB, or NA_real_ when the input is invalid.
#' @export
db_add <- function(x, na.rm = FALSE) {
  x <- suppressWarnings(as.numeric(x))
  if (na.rm) x <- x[is.finite(x)]
  if (length(x) == 0L || any(!is.finite(x))) return(NA_real_)
  xmax <- max(x)
  # Numerically stable form of 10*log10(sum(10^(x/10))).
  xmax + 10 * log10(sum(10^((x - xmax) / 10)))
}

# Backward-compatible internal alias used by the original Shiny modules.
db_sum <- function(x) db_add(x)

.worknoiser_scalar <- function(x, name, positive = FALSE, nonnegative = FALSE) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1L || !is.finite(x)) stop(name, " debe ser un n\u00famero finito.", call. = FALSE)
  if (positive && x <= 0) stop(name, " debe ser > 0.", call. = FALSE)
  if (nonnegative && x < 0) stop(name, " debe ser >= 0.", call. = FALSE)
  x
}

.worknoiser_numeric <- function(x, name, allow_empty = FALSE) {
  x <- suppressWarnings(as.numeric(x))
  if (!allow_empty && length(x) == 0L) stop(name, " no puede estar vac\u00edo.", call. = FALSE)
  if (length(x) && any(!is.finite(x))) stop(name, " contiene valores no finitos.", call. = FALSE)
  x
}

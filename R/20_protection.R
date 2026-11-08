# Hearing protector calculations

#' HML hearing protector method
#' @export
hml_protection <- function(laeq_t, lceq_t, H, M, L) {
  vals <- suppressWarnings(as.numeric(c(laeq_t, lceq_t, H, M, L)))
  if (length(vals) != 5L || any(!is.finite(vals))) return(NULL)
  laeq_t <- vals[1]; lceq_t <- vals[2]; H <- vals[3]; M <- vals[4]; L <- vals[5]
  delta_ca <- lceq_t - laeq_t
  if (delta_ca <= 2) {
    pnr <- M - ((H - M) / 4) * (delta_ca - 2)
  } else {
    pnr <- M - ((M - L) / 8) * (delta_ca - 2)
  }
  list(delta_ca = delta_ca, pnr = pnr, laeq_t_protected = laeq_t - pnr,
       laeq_t_protegido = laeq_t - pnr)
}

#' SNR hearing protector method
#' @export
snr_protection <- function(lceq_t, snr, derating = 0) {
  vals <- suppressWarnings(as.numeric(c(lceq_t, snr, derating)))
  if (length(vals) != 3L || any(!is.finite(vals)) || vals[3] < 0) return(NA_real_)
  vals[1] - (vals[2] - vals[3])
}

#' Octave-band APV hearing protector method
#'
#' The implementation preserves the current WorknoiseR convention for 84%
#' assumed protection: APV = mf - sf. The input band levels must already be
#' A-weighted, as in the existing Shiny calculation module.
#'
#' @param levels_a A-weighted environmental levels by octave band.
#' @param frequencies Frequencies corresponding to levels_a.
#' @param mf Mean attenuation values declared for the protector.
#' @param sf Standard deviations declared for the protector.
#' @param protector_frequencies Frequencies corresponding to mf/sf.
#' @param alpha Multiplier applied to sf; alpha=1 reproduces APV=mf-sf.
#' @return A list containing the matched band table and protected LAeq,T.
#' @export
apv_protection <- function(levels_a, frequencies, mf, sf,
                           protector_frequencies = .worknoiser_apv_bands,
                           alpha = 1) {
  levels_a <- suppressWarnings(as.numeric(levels_a))
  frequencies <- as.character(frequencies)
  mf <- suppressWarnings(as.numeric(mf)); sf <- suppressWarnings(as.numeric(sf))
  protector_frequencies <- as.character(protector_frequencies)
  alpha <- suppressWarnings(as.numeric(alpha))
  if (length(levels_a) != length(frequencies) || length(mf) != length(sf) ||
      length(mf) != length(protector_frequencies) || length(alpha) != 1L ||
      !is.finite(alpha) || alpha < 0 || any(!is.finite(levels_a)) ||
      any(!is.finite(mf)) || any(!is.finite(sf))) return(NULL)
  keep <- frequencies %in% protector_frequencies
  if (!any(keep)) return(NULL)
  idx <- match(frequencies[keep], protector_frequencies)
  if (any(is.na(idx))) return(NULL)
  apv <- mf[idx] - alpha * sf[idx]
  protected <- levels_a[keep] - apv
  tab <- data.frame(
    Frecuencia_Hz = frequencies[keep],
    LpA_dB = levels_a[keep],
    mf_dB = mf[idx],
    sf_dB = sf[idx],
    APV_dB = apv,
    LpA_protegido_dB = protected,
    stringsAsFactors = FALSE
  )
  list(table = tab, laeq_t_protected = db_add(protected),
       laeq_t_protegido = db_add(protected))
}

# Backward-compatible names from the original Shiny module.
calc_hml <- function(laeq_t, lceq_t, H, M, L) hml_protection(laeq_t, lceq_t, H, M, L)
calc_snr <- function(lceq_t, snr, derating = 0) snr_protection(lceq_t, snr, derating)

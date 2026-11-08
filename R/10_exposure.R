# Occupational noise exposure calculations

#' Daily noise exposure level LAeq,d
#'
#' @param laeq_t Equivalent continuous A-weighted sound level for the exposure period.
#' @param duration_h Exposure duration in hours.
#' @param reference_h Reference working day in hours; 8 by default.
#' @return LAeq,d in dB(A).
#' @export
laeq_d <- function(laeq_t, duration_h, reference_h = 8) {
  laeq_t <- suppressWarnings(as.numeric(laeq_t))
  duration_h <- suppressWarnings(as.numeric(duration_h))
  reference_h <- suppressWarnings(as.numeric(reference_h))
  if (length(laeq_t) != 1L || length(duration_h) != 1L || length(reference_h) != 1L ||
      any(!is.finite(c(laeq_t, duration_h, reference_h))) || duration_h <= 0 ||
      duration_h > 24 || reference_h <= 0) return(NA_real_)
  laeq_t + 10 * log10(duration_h / reference_h)
}

#' Daily noise exposure from multiple tasks or periods
#'
#' @param levels Vector of LAeq,T values in dB(A).
#' @param durations_h Matching durations in hours.
#' @param reference_h Reference working day in hours; 8 by default.
#' @return LAeq,d in dB(A).
#' @export
laeq_d_multi <- function(levels, durations_h, reference_h = 8) {
  levels <- suppressWarnings(as.numeric(levels))
  durations_h <- suppressWarnings(as.numeric(durations_h))
  reference_h <- suppressWarnings(as.numeric(reference_h))
  if (length(levels) == 0L || length(levels) != length(durations_h) ||
      length(reference_h) != 1L || any(!is.finite(levels)) || any(!is.finite(durations_h)) ||
      !is.finite(reference_h) || any(durations_h <= 0) || reference_h <= 0) return(NA_real_)
  10 * log10(sum(durations_h * 10^(levels / 10)) / reference_h)
}

#' Weekly noise exposure summary
#'
#' Uses the fixed five-day normalization used for LAeq,s. Missing/non-finite
#' daily values are ignored, preserving the behaviour of the existing app.
#'
#' @param daily_laeq_d Daily LAeq,d values.
#' @return A list with normative_5d, actual_days_mean and exposed_days.
#' @export
laeq_weekly <- function(daily_laeq_d) {
  x <- suppressWarnings(as.numeric(daily_laeq_d))
  x <- x[is.finite(x)]
  if (length(x) == 0L) {
    return(list(normative_5d = NA_real_, actual_days_mean = NA_real_, exposed_days = 0L))
  }
  energy <- sum(10^(x / 10))
  list(
    normative_5d = 10 * log10(energy / 5),
    actual_days_mean = 10 * log10(energy / length(x)),
    exposed_days = length(x)
  )
}

#' Weekly noise exposure level LAeq,s
#'
#' @param daily_laeq_d Daily LAeq,d values.
#' @return Weekly exposure level normalized to five working days.
#' @export
laeq_s <- function(daily_laeq_d) laeq_weekly(daily_laeq_d)$normative_5d

#' Apply A, C or Z frequency weighting to octave-band levels
#'
#' @param levels Numeric octave-band levels.
#' @param frequencies Character/numeric frequencies matching the supported bands.
#' @param weighting One of A, C or Z.
#' @return Weighted band levels.
#' @export
frequency_weighting <- function(levels, frequencies = .worknoiser_octave_bands,
                                weighting = c("A", "C", "Z")) {
  weighting <- match.arg(weighting)
  levels <- suppressWarnings(as.numeric(levels))
  frequencies <- as.character(frequencies)
  idx <- match(frequencies, .worknoiser_octave_bands)
  if (any(is.na(idx)) || length(levels) != length(frequencies) || any(!is.finite(levels))) {
    return(rep(NA_real_, length(levels)))
  }
  correction <- switch(
    weighting,
    A = .worknoiser_weighting_a[idx],
    C = .worknoiser_weighting_c[idx],
    Z = rep(0, length(idx))
  )
  levels + correction
}

#' Criterion parameters for noise dose calculations
#' @export
criterion_params <- function(standard = c("NIOSH", "OSHA", "CUSTOM"),
                             criterion_level = NULL, exchange_rate = NULL,
                             reference_time_h = 8, threshold = NULL) {
  standard <- match.arg(standard)
  if (standard == "NIOSH") {
    return(list(name = "NIOSH REL", criterion_level = 85, exchange_rate = 3,
                reference_time_h = 8, threshold = 80))
  }
  if (standard == "OSHA") {
    return(list(name = "OSHA PEL", criterion_level = 90, exchange_rate = 5,
                reference_time_h = 8, threshold = 90))
  }
  vals <- suppressWarnings(as.numeric(c(criterion_level, exchange_rate, reference_time_h)))
  if (length(vals) != 3L || any(!is.finite(vals)) || vals[2] <= 0 || vals[3] <= 0) return(NULL)
  list(name = worknoiser_translate("custom"), criterion_level = vals[1], exchange_rate = vals[2],
       reference_time_h = vals[3],
       threshold = if (is.null(threshold) || length(threshold) != 1L || !is.finite(threshold)) -Inf else as.numeric(threshold))
}

#' Maximum allowable exposure time for a criterion/exchange-rate model
#' @export
max_exposure_time <- function(level, criterion_level = 85, exchange_rate = 3,
                              reference_time_h = 8) {
  level <- suppressWarnings(as.numeric(level))
  vals <- suppressWarnings(as.numeric(c(criterion_level, exchange_rate, reference_time_h)))
  if (any(!is.finite(level)) || length(vals) != 3L || any(!is.finite(vals)) ||
      vals[2] <= 0 || vals[3] <= 0) return(rep(NA_real_, length(level)))
  vals[3] * 2^((vals[1] - level) / vals[2])
}

#' Noise dose for multiple exposure periods
#' @export
noise_dose <- function(levels, durations_h, criterion_level = 85, exchange_rate = 3,
                       reference_time_h = 8, threshold = -Inf) {
  levels <- suppressWarnings(as.numeric(levels))
  durations_h <- suppressWarnings(as.numeric(durations_h))
  if (length(levels) == 0L || length(levels) != length(durations_h) ||
      any(!is.finite(levels)) || any(!is.finite(durations_h)) || any(durations_h < 0) ||
      length(threshold) != 1L || is.na(threshold)) return(NA_real_)
  use <- levels >= threshold & durations_h > 0
  if (!any(use)) return(0)
  allowed <- max_exposure_time(levels[use], criterion_level, exchange_rate, reference_time_h)
  if (any(!is.finite(allowed)) || any(allowed <= 0)) return(NA_real_)
  100 * sum(durations_h[use] / allowed)
}

#' Assess occupational-noise exposure against the applicable legal framework
#'
#' In Spanish (`language = "es"`) the automatic framework is Spanish Royal
#' Decree 286/2006. In English (`language = "en"`) it is Directive 2003/10/EC.
#' Both frameworks use equivalent daily A-weighted exposure thresholds. The EU
#' Directive expresses peak action/limit values as peak sound pressure (Pa);
#' WorknoiseR also reports their approximate C-weighted dB equivalents because
#' the application accepts peak input in dB(C).
#'
#' @param laeq_d Daily A-weighted noise exposure level in dB(A), normalised to 8 h.
#' @param lcpeak Peak C-weighted sound pressure level in dB(C).
#' @param protected_laeq_d Daily exposure after hearing-protector attenuation,
#'   when a valid protected value is available.
#' @param protector_used Logical indicating whether protected exposure should be
#'   considered for the exposure-limit check.
#' @param language `"es"` or `"en"`.
#' @param framework One of `"auto"`, `"rd286"` or `"eu2003_10"`.
#' @return A translated data frame summarising action values and exposure limits.
#' @export
evaluate_noise_compliance <- function(laeq_d, lcpeak = NA_real_, protected_laeq_d = NA_real_,
                                      protector_used = FALSE,
                                      language = worknoiser_language(),
                                      framework = c("auto", "rd286", "eu2003_10")) {
  language <- .worknoiser_lang(language)
  framework <- match.arg(framework)
  if (framework == "auto") framework <- if (identical(language, "en")) "eu2003_10" else "rd286"
  if (length(laeq_d) != 1L || !is.finite(laeq_d)) return(NULL)
  tr <- function(key) worknoiser_translate(key, language)

  exposure_band <- if (laeq_d >= 85) {
    tr("upper_action_a")
  } else if (laeq_d >= 80) {
    tr("lower_action_a")
  } else {
    tr("below_action_a")
  }

  peak_band <- if (!is.finite(lcpeak)) {
    tr("not_evaluated")
  } else if (framework == "eu2003_10") {
    if (lcpeak >= 137) tr("upper_action_c_eu") else if (lcpeak >= 135) tr("lower_action_c_eu") else tr("below_action_c_eu")
  } else {
    if (lcpeak >= 137) tr("upper_action_c") else if (lcpeak >= 135) tr("lower_action_c") else tr("below_action_c")
  }

  effective_daily <- if (isTRUE(protector_used) && is.finite(protected_laeq_d)) protected_laeq_d else laeq_d
  limit_daily <- if (effective_daily > 87) tr("exceeds_87") else tr("not_exceeds_87")
  limit_peak <- if (!is.finite(lcpeak)) {
    tr("not_evaluated")
  } else if (!isTRUE(protector_used)) {
    if (lcpeak > 140) tr("exceeds_140") else tr("not_exceeds_140")
  } else if (lcpeak <= 140) {
    tr("not_exceeds_140_unprotected")
  } else {
    tr("peak_not_evaluable")
  }

  peak_magnitude <- if (framework == "eu2003_10") tr("peak_limit_eu") else tr("peak_limit")
  framework_label <- if (framework == "eu2003_10") tr("framework_eu") else tr("framework_rd286")

  out <- data.frame(
    magnitude = c(tr("legal_framework"), tr("ambient_daily"), tr("ambient_peak"), tr("daily_limit"), peak_magnitude),
    result = c(
      framework_label,
      sprintf("%.2f dB(A) \u2014 %s", laeq_d, exposure_band),
      if (is.finite(lcpeak)) sprintf("%.2f dB(C) \u2014 %s", lcpeak, peak_band) else peak_band,
      sprintf(tr("actual_exposure"), effective_daily, limit_daily),
      limit_peak
    ),
    stringsAsFactors = FALSE
  )
  names(out) <- c(tr("magnitude"), tr("result"))
  attr(out, "framework") <- framework
  out
}

#' Summary against Spanish RD 286/2006 action and limit values
#'
#' Backward-compatible Spanish-regulation wrapper. For bilingual applications,
#' prefer [evaluate_noise_compliance()], which automatically uses Directive
#' 2003/10/EC in English.
#'
#' @inheritParams evaluate_noise_compliance
#' @return A translated data frame summarising Spanish RD 286/2006 values.
#' @export
evaluate_rd286 <- function(laeq_d, lcpeak = NA_real_, protected_laeq_d = NA_real_,
                           protector_used = FALSE, language = worknoiser_language()) {
  evaluate_noise_compliance(
    laeq_d = laeq_d,
    lcpeak = lcpeak,
    protected_laeq_d = protected_laeq_d,
    protector_used = protector_used,
    language = language,
    framework = "rd286"
  )
}

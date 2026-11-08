# WorknoiseR internationalisation helpers ------------------------------------

.worknoiser_languages <- c("es", "en")

#' Get or set the WorknoiseR language
#'
#' WorknoiseR keeps calculation objects language-neutral where possible. This
#' preference controls human-readable messages, summaries and the initial
#' language of the included Shiny application.
#'
#' @param language Language code: `"es"` (Spanish) or `"en"` (English).
#' @return `worknoiser_language()` returns the active language invisibly as a
#'   character scalar. `set_worknoiser_language()` returns the previous language
#'   invisibly.
#' @export
worknoiser_language <- function() {
  lang <- getOption("worknoiser.language", Sys.getenv("WORKNOISER_LANG", "es"))
  lang <- tolower(as.character(lang)[1])
  if (!lang %in% .worknoiser_languages) lang <- "es"
  lang
}

#' @rdname worknoiser_language
#' @export
set_worknoiser_language <- function(language = c("es", "en")) {
  language <- match.arg(language)
  old <- worknoiser_language()
  options(worknoiser.language = language)
  invisible(old)
}

.worknoiser_text <- list(
  es = c(
    custom = "Personalizado",
    file_missing = "No existe el archivo CSV indicado.",
    csv_read_error = "No se pudo leer el CSV.",
    source_required = "Se requiere al menos una fuente.",
    grid_required = "grid debe contener x, y y la columna de nivel.",
    not_evaluated = "No evaluado",
    upper_action_a = "Valor superior de acci\u00f3n alcanzado/superado (>= 85 dB(A))",
    lower_action_a = "Valor inferior de acci\u00f3n alcanzado/superado (>= 80 dB(A))",
    below_action_a = "Por debajo del valor inferior de acci\u00f3n (< 80 dB(A))",
    upper_action_c = "Valor superior de acci\u00f3n alcanzado/superado (>= 137 dB(C))",
    lower_action_c = "Valor inferior de acci\u00f3n alcanzado/superado (>= 135 dB(C))",
    below_action_c = "Por debajo del valor inferior de acci\u00f3n (< 135 dB(C))",
    exceeds_87 = "Supera 87 dB(A)",
    not_exceeds_87 = "No supera 87 dB(A)",
    exceeds_140 = "Supera 140 dB(C)",
    not_exceeds_140 = "No supera 140 dB(C)",
    not_exceeds_140_unprotected = "No supera 140 dB(C) incluso sin descontar el protector",
    peak_not_evaluable = "No evaluable con los datos actuales: se requiere una estimaci\u00f3n v\u00e1lida de la exposici\u00f3n real al pico con el protector",
    magnitude = "Magnitud",
    result = "Resultado",
    ambient_daily = "LAeq,d sin descontar protector",
    ambient_peak = "Lpico,C sin descontar protector",
    daily_limit = "Valor l\u00edmite LAeq,d",
    peak_limit = "Valor l\u00edmite Lpico,C",
    actual_exposure = "Exposici\u00f3n real usada: %.2f dB(A) \u2014 %s",
    legal_framework = "Marco normativo",
    framework_rd286 = "Real Decreto 286/2006 (Espa\u00f1a)",
    framework_eu = "Directiva 2003/10/CE",
    upper_action_c_eu = "Valor superior de exposici\u00f3n que da lugar a una acci\u00f3n: 140 Pa (aprox. 137 dB(C))",
    lower_action_c_eu = "Valor inferior de exposici\u00f3n que da lugar a una acci\u00f3n: 112 Pa (aprox. 135 dB(C))",
    below_action_c_eu = "Por debajo del valor inferior de exposici\u00f3n que da lugar a una acci\u00f3n: 112 Pa (aprox. 135 dB(C))",
    peak_limit_eu = "Valor l\u00edmite p_peak: 200 Pa (aprox. 140 dB(C))"
  ),
  en = c(
    custom = "Custom",
    file_missing = "The specified CSV file does not exist.",
    csv_read_error = "The CSV file could not be read.",
    source_required = "At least one source is required.",
    grid_required = "grid must contain x, y and the level column.",
    not_evaluated = "Not evaluated",
    upper_action_a = "Upper action value reached/exceeded (>= 85 dB(A))",
    lower_action_a = "Lower action value reached/exceeded (>= 80 dB(A))",
    below_action_a = "Below the lower action value (< 80 dB(A))",
    upper_action_c = "Upper action value reached/exceeded (>= 137 dB(C))",
    lower_action_c = "Lower action value reached/exceeded (>= 135 dB(C))",
    below_action_c = "Below the lower action value (< 135 dB(C))",
    exceeds_87 = "Exceeds 87 dB(A)",
    not_exceeds_87 = "Does not exceed 87 dB(A)",
    exceeds_140 = "Exceeds 140 dB(C)",
    not_exceeds_140 = "Does not exceed 140 dB(C)",
    not_exceeds_140_unprotected = "Does not exceed 140 dB(C), even before accounting for the protector",
    peak_not_evaluable = "Not evaluable with the current data: a valid estimate of the actual protected peak exposure is required",
    magnitude = "Magnitude",
    result = "Result",
    ambient_daily = "Daily noise exposure LEX,8h before hearing-protector attenuation",
    ambient_peak = "Peak input before hearing-protector attenuation",
    daily_limit = "Exposure limit value LEX,8h",
    peak_limit = "Lpeak,C limit value",
    actual_exposure = "Actual exposure used: %.2f dB(A) \u2014 %s",
    legal_framework = "Legal framework",
    framework_rd286 = "Spanish Royal Decree 286/2006",
    framework_eu = "Directive 2003/10/EC",
    upper_action_c_eu = "Upper exposure action value: 140 Pa (approx. 137 dB(C))",
    lower_action_c_eu = "Lower exposure action value: 112 Pa (approx. 135 dB(C))",
    below_action_c_eu = "Below the lower exposure action value: 112 Pa (approx. 135 dB(C))",
    peak_limit_eu = "Peak exposure limit value p_peak: 200 Pa (approx. 140 dB(C))"
  )
)

#' Translate a WorknoiseR user-facing message key
#'
#' @param key Translation key.
#' @param language Language code; defaults to the current WorknoiseR language.
#' @param ... Values passed to `sprintf()` when the translated string contains
#'   formatting placeholders.
#' @return A translated character scalar.
#' @export
worknoiser_translate <- function(key, language = worknoiser_language(), ...) {
  language <- match.arg(language, .worknoiser_languages)
  key <- as.character(key)[1]
  val <- .worknoiser_text[[language]][[key]]
  if (is.null(val)) val <- key
  dots <- list(...)
  if (length(dots)) do.call(sprintf, c(list(val), dots)) else unname(val)
}

.worknoiser_lang <- function(language = NULL) {
  if (is.null(language)) worknoiser_language() else match.arg(language, .worknoiser_languages)
}

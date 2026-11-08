#' Run the WorknoiseR Shiny application
#'
#' Launches the bilingual WorknoiseR Shiny interface. The language can be
#' changed at any time from the application header.
#'
#' @param language Initial interface language: `"es"` or `"en"`.
#' @param launch.browser Passed to `shiny::runApp()`.
#' @param ... Additional arguments passed to `shiny::runApp()`.
#' @export
run_noise_app <- function(language = worknoiser_language(), launch.browser = interactive(), ...) {
  language <- .worknoiser_lang(language)
  required <- c("shiny", "rhandsontable", "ggplot2", "plotly", "reshape2",
                "viridisLite", "scales", "sf")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    msg <- if (language == "en") "Missing packages required to run the app: " else "Faltan paquetes para ejecutar la app: "
    stop(msg, paste(missing, collapse = ", "), call. = FALSE)
  }
  app_dir <- system.file("shiny", "WorknoiseR", package = "WorknoiseR")
  if (!nzchar(app_dir)) {
    stop(if (language == "en") "The Shiny application installed with WorknoiseR could not be found." else "No se encuentra la aplicaci\u00f3n Shiny instalada con WorknoiseR.", call. = FALSE)
  }
  old <- getOption("worknoiser.language")
  on.exit(options(worknoiser.language = old), add = TRUE)
  options(worknoiser.language = language)
  shiny::runApp(app_dir, launch.browser = launch.browser, ...)
}

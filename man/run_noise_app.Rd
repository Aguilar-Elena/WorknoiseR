\name{run_noise_app}
\alias{run_noise_app}
\title{Run the bilingual WorknoiseR Shiny application / Ejecutar la aplicación bilingüe WorknoiseR}
\usage{
run_noise_app(language = worknoiser_language(), launch.browser = interactive(), ...)
}
\arguments{
  \item{language}{Initial language: \code{"es"} (Spanish) or \code{"en"} (English). / Idioma inicial: \code{"es"} (español) o \code{"en"} (inglés).}
  \item{launch.browser}{Passed to \code{shiny::runApp()}.}
  \item{...}{Additional arguments passed to \code{shiny::runApp()}.}
}
\description{
Launches the WorknoiseR interactive application. The interface language can be changed at any time from the header selector. / Ejecuta la aplicación interactiva WorknoiseR. El idioma de la interfaz puede cambiarse en cualquier momento desde el selector de la cabecera.
}

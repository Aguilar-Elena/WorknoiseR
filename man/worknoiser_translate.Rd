\name{worknoiser_translate}
\alias{worknoiser_translate}
\title{Translate WorknoiseR messages / Traducir mensajes de WorknoiseR}
\usage{worknoiser_translate(key, language = worknoiser_language(), ...)}
\arguments{
  \item{key}{Translation key. / Clave de traducción.}
  \item{language}{\code{"es"} or \code{"en"}. / \code{"es"} o \code{"en"}.}
  \item{...}{Values passed to \code{sprintf()} when required. / Valores pasados a \code{sprintf()} cuando sean necesarios.}
}
\description{Internal/public helper for the shared bilingual message catalogue. / Función auxiliar para el catálogo bilingüe compartido de mensajes.}
\value{A translated character scalar. / Una cadena de texto traducida.}

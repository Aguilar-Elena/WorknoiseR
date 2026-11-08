\name{frequency_weighting}
\alias{frequency_weighting}
\title{Apply frequency weighting to octave-band levels / Aplicar ponderación frecuencial a bandas de octava}
\usage{frequency_weighting(levels, frequencies = .worknoiser_octave_bands,
  weighting = c("A", "C", "Z"))}
\arguments{
  \item{levels}{Numeric octave-band levels.}
  \item{frequencies}{Frequencies corresponding to \code{levels}.}
  \item{weighting}{One of \code{"A"}, \code{"C"} or \code{"Z"}.}
}
\value{A numeric vector of weighted band levels.}
\description{Applies the WorknoiseR octave-band A, C or Z weighting corrections. / Aplica las correcciones de ponderación A, C o Z por bandas de octava de WorknoiseR.}

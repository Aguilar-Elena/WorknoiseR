\name{noise_dose}
\alias{noise_dose}
\alias{max_exposure_time}
\alias{criterion_params}
\title{Noise dose and criterion-based exposure time / Dosis de ruido y tiempo de exposición según criterio}
\usage{
criterion_params(standard = c("NIOSH", "OSHA", "CUSTOM"),
  criterion_level = NULL, exchange_rate = NULL, reference_time_h = 8,
  threshold = NULL)

max_exposure_time(level, criterion_level = 85, exchange_rate = 3,
  reference_time_h = 8)

noise_dose(levels, durations_h, criterion_level = 85, exchange_rate = 3,
  reference_time_h = 8, threshold = -Inf)
}
\arguments{
  \item{standard}{Criterion model.}
  \item{criterion_level}{Criterion level in dB.}
  \item{exchange_rate}{Exchange rate in dB.}
  \item{reference_time_h}{Reference duration in hours.}
  \item{threshold}{Threshold below which periods are omitted from dose.}
  \item{level}{One or more sound levels.}
  \item{levels}{Sound levels for multiple exposure periods.}
  \item{durations_h}{Durations corresponding to \code{levels}.}
}
\value{Criterion parameters, allowable time, or dose percentage depending on the function used.}
\description{Helpers for criterion/exchange-rate based occupational noise calculations. / Herramientas para cálculos de ruido laboral basados en nivel criterio y tasa de intercambio.}

\name{evaluate_rd286}
\alias{evaluate_rd286}
\title{Assess occupational-noise exposure under Spanish RD 286/2006}
\usage{
evaluate_rd286(
  laeq_d,
  lcpeak = NA_real_,
  protected_laeq_d = NA_real_,
  protector_used = FALSE,
  language = worknoiser_language()
)
}
\arguments{
\item{laeq_d}{Daily A-weighted noise exposure level in dB(A), normalised to 8 h.}
\item{lcpeak}{Peak C-weighted sound pressure level in dB(C).}
\item{protected_laeq_d}{Daily exposure after hearing-protector attenuation, when available.}
\item{protector_used}{Logical indicating whether protected exposure should be considered for the exposure-limit check.}
\item{language}{Output language, \code{"es"} or \code{"en"}.}
}
\value{A translated data frame summarising Spanish RD 286/2006 values.}
\description{Backward-compatible wrapper fixed to the Spanish legal framework. For bilingual applications use \code{evaluate_noise_compliance()}, which automatically selects Directive 2003/10/EC in English.}

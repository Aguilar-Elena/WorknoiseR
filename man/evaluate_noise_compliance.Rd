\name{evaluate_noise_compliance}
\alias{evaluate_noise_compliance}
\title{Assess occupational-noise exposure against the applicable legal framework}
\usage{
evaluate_noise_compliance(
  laeq_d,
  lcpeak = NA_real_,
  protected_laeq_d = NA_real_,
  protector_used = FALSE,
  language = worknoiser_language(),
  framework = c("auto", "rd286", "eu2003_10")
)
}
\arguments{
\item{laeq_d}{Daily A-weighted noise exposure level in dB(A), normalised to 8 h.}
\item{lcpeak}{Peak C-weighted sound pressure level in dB(C).}
\item{protected_laeq_d}{Daily exposure after hearing-protector attenuation, when available.}
\item{protector_used}{Logical indicating whether protected exposure should be considered for the exposure-limit check.}
\item{language}{\code{"es"} or \code{"en"}.}
\item{framework}{\code{"auto"}, \code{"rd286"}, or \code{"eu2003_10"}.}
}
\value{A translated data frame summarising exposure action values and exposure limits.}
\description{Uses Spanish Royal Decree 286/2006 automatically in Spanish and Directive 2003/10/EC automatically in English. The calculation thresholds are equivalent; EU peak values are additionally identified in pascals.}

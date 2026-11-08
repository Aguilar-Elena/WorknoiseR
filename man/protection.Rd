\name{apv_protection}
\alias{apv_protection}
\alias{hml_protection}
\alias{snr_protection}
\title{Hearing protector calculations / Cálculos de protectores auditivos}
\usage{
apv_protection(levels_a, frequencies, mf, sf,
  protector_frequencies = .worknoiser_apv_bands, alpha = 1)

hml_protection(laeq_t, lceq_t, H, M, L)

snr_protection(lceq_t, snr, derating = 0)
}
\arguments{
  \item{levels_a}{A-weighted octave-band environmental levels.}
  \item{frequencies}{Frequencies corresponding to \code{levels_a}.}
  \item{mf}{Mean attenuation by protector band.}
  \item{sf}{Standard deviation by protector band.}
  \item{protector_frequencies}{Frequencies corresponding to \code{mf} and \code{sf}.}
  \item{alpha}{Multiplier applied to \code{sf}; 1 preserves the current APV = mf - sf convention.}
  \item{laeq_t}{A-weighted equivalent level.}
  \item{lceq_t}{C-weighted equivalent level.}
  \item{H}{High-frequency HML value.}
  \item{M}{Medium-frequency HML value.}
  \item{L}{Low-frequency HML value.}
  \item{snr}{Single Number Rating.}
  \item{derating}{Optional reduction applied to SNR before subtraction.}
}
\value{APV and HML return lists with intermediate and protected levels; SNR returns the protected level.}
\description{Implements the hearing-protector calculation methods exposed by WorknoiseR. / Implementa los métodos de cálculo de protectores auditivos disponibles en WorknoiseR.}

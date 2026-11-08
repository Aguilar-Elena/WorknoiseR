\name{laeq_d}
\alias{laeq_d}
\alias{laeq_d_multi}
\alias{laeq_s}
\alias{laeq_weekly}
\title{Daily and weekly occupational noise exposure / Exposición diaria y semanal al ruido laboral}
\usage{
laeq_d(laeq_t, duration_h, reference_h = 8)

laeq_d_multi(levels, durations_h, reference_h = 8)

laeq_s(daily_laeq_d)

laeq_weekly(daily_laeq_d)
}
\arguments{
  \item{laeq_t}{Equivalent continuous A-weighted sound level for the exposure period.}
  \item{duration_h}{Exposure duration in hours.}
  \item{reference_h}{Reference duration in hours; 8 by default.}
  \item{levels}{Vector of LAeq,T levels for multiple tasks or periods.}
  \item{durations_h}{Vector of durations corresponding to \code{levels}.}
  \item{daily_laeq_d}{Vector of daily LAeq,d values.}
}
\value{\code{laeq_d()}, \code{laeq_d_multi()} and \code{laeq_s()} return a numeric level. \code{laeq_weekly()} returns a list with the normative five-day level, the energetic mean of valid exposure days and the number of valid days.}
\description{Functions for daily and weekly occupational noise exposure calculations. / Funciones para el cálculo de la exposición diaria y semanal al ruido laboral.}

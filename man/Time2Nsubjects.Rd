% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exports.R
\name{Time2Nsubjects}
\alias{Time2Nsubjects}
\title{Simulate number of weeks needed to recruit a given number of subjects}
\usage{
Time2Nsubjects(
  nSub = 50L,
  nSim = 10000L,
  fillGaps = FALSE,
  cauchyWt = FALSE,
  coeff = 1
)
}
\arguments{
\item{nSub}{Number of subjects to recruit (default = 50L)}

\item{nSim}{Number of simulations to run (default = 1e4L)}

\item{fillGaps}{Whether to fill gaps in the data (default = FALSE)}

\item{cauchyWt}{Whether to use Cauchy weights for sampling.
If FALSE (default),\if{html}{\out{<br>}}
binomial weights are used.}

\item{coeff}{A coefficient to apply to the recruitment rate (default = 1)}
}
\value{
An object of \code{RCTNWeeks} class with four elements.
\enumerate{
\item \code{weeks} is an integer vector with length equal to \code{nSim} containing the
simulation results.
\item \code{CI} shows the median and the 95\% CI.
\item \code{call.}: The call (deparsed) that created the object
\item \code{cargs}: A list with the arguments of the call that created the object\if{html}{\out{<br>}}
including the default arguments
}
}
\description{
Simulate number of weeks needed to recruit a given number of subjects
}
\examples{
LoadData(gripsYR1, ScreenDt, Enrolled)
(res <- Time2Nsubjects())
str(res)
}
\seealso{
Other Links: 
\code{\link{GetDistance}()},
\code{\link{GetWeekPredCI}()},
\code{\link{LoadData}()},
\code{\link{RCTRecruit-package}},
\code{\link{gripsYR1}},
\code{\link{gripsYR2}},
\code{\link{gripsYR2Weekly}}
}
\concept{Links}

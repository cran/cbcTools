% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/choices.R
\name{randLN}
\alias{randLN}
\title{Define prior (assumed) model parameter as log-normally-distributed.}
\usage{
randLN(mean = 0, sd = 1)
}
\arguments{
\item{mean}{Mean of the distribution on the log scale, defaults to \code{0}.}

\item{sd}{Standard deviation of the distribution on the log scale,
defaults to \code{1}.}
}
\value{
A list defining log-normally-distributed parameters of the prior
(assumed) utility model used to simulate choices in the \code{cbc_choices()}
function.
}
\description{
Define prior (assumed) model parameter as log-normally-distributed.
Used in the \code{cbc_choices()} function.
}
\examples{
# Insert example
}

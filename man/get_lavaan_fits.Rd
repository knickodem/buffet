% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_lavaan_fits.R
\name{get_lavaan_fits}
\alias{get_lavaan_fits}
\title{Fit measures for a lavaan model.}
\usage{
get_lavaan_fits(object, measures = "scaled")
}
\arguments{
\item{object}{A \code{lavaan} object (e.g., \code{\link[lavaan]{cfa}},
\code{\link[lavaan]{sem}}).}

\item{measures}{If "all", all fit measures available are returned.
A single quoted fit measure or character vector of fit measures can also be specified.
There are also sets of pre-selected fit measures: "naive" for measures using the naive chi-square test statistic,
"scaled" for measures using the scaled chi-square test statistic (default), or
"robust for measures calculated with population-consistent formulas.}
}
\value{
a single row data.frame with columns for:
\itemize{

\item{"ntotal"}{Number of observations included}
\item{"ngroups"}{Number of groups in model}
\item{"measures"}{The fit measures specified in the \code{measures} argument}
\item{"AIC, BIC"}{Akaike and Bayesian information criterion, if a maximum likelihood estimator was used}
}
}
\description{
Extracts number of observations and groups along with user-specified fit measures
from a \code{lavaan} object into a \code{data.frame}.
}
\details{
Similar to \code{\link[broom]{glance.lavaan}} but with more flexibility in which fit measures to output.
}
\examples{
library(lavaan)
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit <- cfa(HS.model, data = HolzingerSwineford1939)

get_lavaan_fits(fit)
get_lavaan_fits(fit, measures = "robust")
get_lavaan_fits(fit, measures = c("cfi","rmsea", "srmr"))

}

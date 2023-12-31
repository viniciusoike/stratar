% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_census_weights.R
\name{get_census_weights}
\alias{get_census_weights}
\title{Compute Census Weights}
\usage{
get_census_weights(state, dir = NULL, export = FALSE, variable = "all")
}
\arguments{
\item{state}{Two letter abbreviation of the state. This option assumes the Census
microdata has been downloaded and unziped in the `dir` directory. Either a single
state or 'all'.}

\item{dir}{Path to the Census microdata directory. Defaults to `NULL` assuming
standard file names.}

\item{export}{Logical indicating if results should be exported locally. Defaults
to `FALSE`.}

\item{variable}{One of `count`, `income`, or `all` (default).}
}
\value{
A `data.table` with Census totals on columns by weighting area.
}
\description{
Uses 2010 Census household microdata to estimate strata weights. Currently
supports income and count weighting by number of bedrooms.
}

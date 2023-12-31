% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_census.R
\name{download_census}
\alias{download_census}
\title{Download and optionally unzip Census data}
\usage{
download_census(
  type = "universo",
  year = 2010,
  download = TRUE,
  dir = NULL,
  unzip = TRUE
)
}
\arguments{
\item{type}{One of `'universo'` or `'amostra'`}

\item{year}{Only accepts 2010}

\item{download}{Logical indicating if data should be downloaded locally. Defaults
to TRUE}

\item{dir}{Directory where the data should be downloaded. Defaults to `"data-raw"`}

\item{unzip}{Logical indicating if downloaded zip files should be unzipped. Defaults
to `TRUE`.}
}
\description{
Download and optionally unzip Census data
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grabclean.R
\name{newgrabclean}
\alias{newgrabclean}
\title{Cleaning grab data from 2024 on}
\usage{
newgrabclean(yearmon, tofile = FALSE, fdir = getOption("fdir"))
}
\arguments{
\item{yearmon}{numeric survey date in yyyymm format}

\item{tofile}{logical save output to file}

\item{fdir}{character file path to local data directory}
}
\description{
if column names do not match up make sure there is only one "date" column
}
\details{
If streaming data does not exist for a particular data/time pull averages for the previous minute (if data exists).
}

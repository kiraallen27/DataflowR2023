% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/streamingclean.R
\name{streamqa}
\alias{streamqa}
\title{Supervised quality control of streaming datasets}
\usage{
streamqa(
  yearmon,
  parset = NA,
  setthresh = TRUE,
  trimends = FALSE,
  paired = TRUE,
  fdir = getOption("fdir")
)
}
\arguments{
\item{yearmon}{numeric date in yyyymm format}

\item{parset}{character vector of column names to QA}

\item{setthresh}{logical set parameter thresholds}

\item{trimends}{logical look to trim ends of data stream? NOT IMPLEMENTED YET}

\item{paired}{logical examine relationships between paried parameters?}

\item{fdir}{file.path to data directory}
}
\value{
a matrix of the same size/shape of the fulldataset, with entries specifying where to set to NA, saved to DF_FullDataSets/Raw/IntrumentOutput
}
\description{
Supervised quality control of streaming datasets
}
\details{
loop through parameters giving the opportunity to trim measurement ends, set entire variables to NA, remove variables above/below a threshold
}
\examples{
\dontrun{
dt<-streamqa(yearmon=201410)
}
}

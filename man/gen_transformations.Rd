% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_transformations.R
\name{gen_transformations}
\alias{gen_transformations}
\title{Create dataframe including data transformations}
\usage{
gen_transformations(data)
}
\arguments{
\item{data}{dataframe created by \emph{get_matching}}
}
\value{
seasonally adjusted time series as vector
}
\description{
\emph{gen_transformations} creates a dataframe including multiple transformations. Takes about 20-30 min.
}
\examples{
\dontrun{
gen_dataframe_level()
}
}

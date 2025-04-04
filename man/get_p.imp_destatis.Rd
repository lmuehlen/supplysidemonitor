% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_p.imp_destatis.R
\name{get_p.imp_destatis}
\alias{get_p.imp_destatis}
\title{Load and format import prices destatis}
\usage{
get_p.imp_destatis()
}
\value{
A dataframe
}
\description{
\emph{get_p.indu_destatis()} loads data on import prices from destatis and formats them. Before downloading the data, the user has the use \emph{restatis::gen_auth_save()}, see \url{https://github.com/CorrelAid/restatis}.
}
\examples{
\dontrun{
get_p.imp_destatis()
}
}

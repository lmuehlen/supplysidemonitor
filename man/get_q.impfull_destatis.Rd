% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_q.imp_functions.R
\name{get_q.impfull_destatis}
\alias{get_q.impfull_destatis}
\title{Load and format disagregated import volume destatis}
\usage{
get_q.impfull_destatis()
}
\value{
A dataframe
}
\description{
\emph{get_q.impfull_destatis()} loads data on import volumes by country from destatis and formats them. Before downloading the data, the user has the use \emph{restatis::gen_auth_save()}, see \url{https://github.com/CorrelAid/restatis}.
}
\examples{
\dontrun{
get_q.impfull_destatis()
}
}

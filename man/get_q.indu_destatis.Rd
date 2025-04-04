% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_q.indu_destatis.R
\name{get_q.indu_destatis}
\alias{get_q.indu_destatis}
\title{Load and format production industry destatis}
\usage{
get_q.indu_destatis()
}
\value{
A dataframe
}
\description{
\emph{get_p.indu_destatis()} loads data on production from destatis and formats them. Before downloading the data, the user has the use \emph{restatis::gen_auth_save()}, see \url{https://github.com/CorrelAid/restatis}.
}
\examples{
\dontrun{
get_p.indu_destatis()
}
}

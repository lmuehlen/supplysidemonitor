% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table_helper_functions.R
\name{get_cols}
\alias{get_cols}
\title{Create Lineplot in reactable}
\usage{
get_cols(
  sts_transf = c("c12m_nsa"),
  sts_series_transf = "level_sa",
  ifo_transf = "level_nsa",
  ifo_series_transf = "level_nsa",
  output_labels = c("ΔProd", "ΔImp"),
  price_labels = c("ΔProd", "ΔImp"),
  ifo_labels = c("Material", "Orders")
)
}
\value{
plot
}
\description{
Create Lineplot in reactable
}
\examples{
\dontrun{
gen_transformations()
}
}

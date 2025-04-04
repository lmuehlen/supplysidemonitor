% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_table.R
\name{get_table}
\alias{get_table}
\title{Create dataframe including data transformations}
\usage{
get_table(
  data,
  width_col_sec = 150,
  width_col_ppi = 65,
  width_col_q.p = 60,
  width_col_ifo = 70,
  width_plot = 150,
  ppi_name = "PPI",
  abbr_ppi = "Producer Price Index weight",
  abbr_price =
    c("ΔProd:\\n YoY Change Rate Producer Prices\\n with rolling averages\\n yₜ / (yₜ₋₁₁ + yₜ₋₁₂ + yₜ₋₁₃)",
    "ΔImp:\\n YoY Change Rate Import Price\\n with rolling averages\\n yₜ / (yₜ₋₁₁ + yₜ₋₁₂ + yₜ₋₁₃)"),
  abbr_output =
    c("ΔProd:\\n YoY Change Rate Production Volume\\n with rolling averages\\n yₜ / (yₜ₋₁₁ + yₜ₋₁₂ + yₜ₋₁₃)",
    "ΔImp:\\n YoY Change Rate Import Volume\\n with rolling averages\\n yₜ / (yₜ₋₁₁ + yₜ₋₁₂ + yₜ₋₁₃)"),
  abbr_ifo =
    c("Material:\\n Percentage of firms naming 'shortage of material'\\n as limiting factor for production",
    "Orders:\\n Percentage of firms naming 'lack of orders'\\n as limiting factor for production"),
  expanded = FALSE,
  sorting = list(ppi.weight = "desc"),
  pagination = FALSE,
  filterable = TRUE,
  searchable = FALSE,
  page_size = 7,
  backgroundColor = "#cdcadc4d",
  ...
)
}
\arguments{
\item{data}{dataframe created by \emph{get_transformation}}

\item{width_col_sec}{width of sector column}

\item{width_col_ppi}{width of ppi column}

\item{width_col_q.p}{width of output and price numerical columns}

\item{width_col_ifo}{width of ifo numerical columns}

\item{abbr_ppi}{Full text header ppi}
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

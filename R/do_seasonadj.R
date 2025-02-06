#' Helper function for seasonal adjustment
#'
#' *do_seasonadj()* is used within *gen_transformations*
#'
#' @return seasonally adjusted time series as vector
#'
#' @import restatis
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import lubridate
#' @import RJDemetra
#'
#' @examples
#' \dontrun{
#' get_matching()
#' }
do_seasonadj<-function(x,date){



  vals <- x
  dates <- date

  # Identify the indices of non-NA values
  non_na_idx <- which(!is.na(vals))

  # need 2 or more non-NA
  if (length(non_na_idx) < 2) {
    return(vals*NA)
  }

  # Extract the non-NA segment
  vals_sub <- vals[non_na_idx]
  dates_sub <- dates[non_na_idx]

  # differ between monthly and quarterly series
  if(mean(diff(non_na_idx))-1<1e-6){
    ts_data <- ts(vals_sub, start = c(year(dates_sub[1]), month(dates_sub[1])), frequency = 12)
  }else if(mean(diff(non_na_idx))-3<1e-6){
    ts_data <- ts(vals_sub, start = c(year(dates_sub[1]), month(dates_sub[1])), frequency = 4)
  }else{
    return(vals*NA)
  }

  # Try running x13; if it fails, return the original NA vector
  sa_result <- tryCatch({
    x13(ts_data)
  }, error = function(e) {
    return(NULL)
  })

  if (is.null(sa_result)) {
    # If x13 fails, return NAs
    return(vals * NA)
  } else {
    # Extract the final seasonally adjusted series
    sa_vals_sub <- sa_result$final$series[,2]

    res <- rep(NA_real_, length(vals))
    res[non_na_idx] <- sa_vals_sub
    return(res)
  }
}

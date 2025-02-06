#' Combining dataframes
#'
#' *get_matching()* combines all datasets.
#'
#' @return A dataframe
#' @export
#'
#' @import restatis
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import lubridate
#'
#' @examples
#' \dontrun{
#' get_matching()
#' }
get_matching<-function(matching_base,p.indu,q.indu,p.serv,q.serv,pimp,qimp,limiting.indu){

  dates <-  seq(as.Date("1991-01-01"), floor_date(Sys.Date(), unit = "month"), by = "month")
  expanded_dataset <- expand.grid(nace2 = matching_base$nace2, date = dates)

  data<-expanded_dataset%>%
    left_join(matching_base,by="nace2")%>%
    #Production
    left_join(q.indu,by=c("nace2","date"))%>%
    left_join(q.serv,by=c("nace2","date"))%>%
    mutate(production=case_when(!is.na(production.x)~production.x, # get production in one
                                !is.na(production.y)~production.y,
                                TRUE~NA_real_))%>%
    #Price
    left_join(p.indu,by=c("nace2","date"))%>%
    left_join(p.serv,by=c("nace2","date"))%>%
    mutate(producerprices=case_when(!is.na(producerprices.x)~producerprices.x, #get prices in one
                                    !is.na(producerprices.y)~producerprices.y,
                                    TRUE~NA_real_))%>%
    select(-c(production.x,production.y,producerprices.x,producerprices.y))%>%
    # Imports
    left_join(pimp%>%select(-level),by=c("nace2","date"))%>%
    left_join(qimp,by=c("nace2","date"))%>%
    left_join(limiting.indu,by=c("nace2","date"))

  return(data)
}

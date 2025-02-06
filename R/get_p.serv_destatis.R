#' Load and format producer prices service destatis
#'
#' *get_p.indu_destatis()* loads data on producer prices in the service sector from destatis and formats them. Before downloading the data, the user has the use *restatis::gen_auth_save()*, see <https://github.com/CorrelAid/restatis>.
#'
#'
#' @return A dataframe
#' @export
#'
#' @import restatis
#' @import dplyr
#' @import tidyr
#' @import lubridate
#'
#' @examples
#' \dontrun{
#' get_p.serv_destatis()
#' }
get_p.serv_destatis<-function(){
  data<-gen_cube("61311BV002", database = "genesis")%>%
    #  filter(WZ08D6!="WZ08-501-01")%>% #getting rid of one composed thing
    mutate(nace2=sub("WZ08-(.*)","\\1",WZ08D6),
           # level=2,
           ym=paste0(as.character(JAHR),as.character( as.numeric(sub("QUART(.*)","\\1",QUARTG))*3)),
           date=ym(ym),
           producerprices=PRE023_WERT)%>%
    select(nace2,date,producerprices)

  data%>%
    mutate(producerprices=ifelse(producerprices==0,NA_real_,producerprices))
}

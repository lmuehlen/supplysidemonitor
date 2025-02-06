#' Load and format production service destatis
#'
#' *get_p.indu_destatis()* loads data on production in the service sector from destatis and formats them. Before downloading the data, the user has the use *restatis::gen_auth_save()*, see <https://github.com/CorrelAid/restatis>.
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
#' get_q.serv_destatis()
#' }
get_q.serv_destatis<-function(){
  data<-gen_cube("47414BM004", database = "genesis")%>%
    filter(WERT01=="WERTORG")%>%
    #  filter(WZ08D6!="WZ08-501-01")%>% #getting rid of one composed thing
    mutate(nace2=sub("WZ08-(.*)","\\1",WZ08N8),
           # level=2,
           ym=paste0(as.character(JAHR),sub("MONAT(.*)","\\1",MONAT)),
           date=ym(ym),
           production=PRO102_WERT)%>%
    select(nace2,date,production)

  data%>%
    mutate(production=ifelse(production==0,NA_real_,production))
}

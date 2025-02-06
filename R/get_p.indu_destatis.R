#' Load and format producer prices industry destatis
#'
#' *get_p.indu_destatis()* loads data on producer prices from destatis and formats them. Before downloading the data, the user has the use *restatis::gen_auth_save()*, see <https://github.com/CorrelAid/restatis>.
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
#' get_p.indu_destatis()
#' }
get_p.indu_destatis<-function(){
  data_2d<-gen_cube("61241BM002", database = "genesis")%>%
    mutate(nace2=sub("GP19-(.*)","\\1",GP19M2),
           ym=paste0(as.character(JAHR),sub("MONAT(.*)","\\1",MONAT)),
           date=ym(ym),
           producerprices=PRE001_WERT)%>%
    select(nace2,date,producerprices)

  data_3d<-gen_cube("61241BM003", database = "genesis")%>%
    mutate(nace2=sub("GP19-(.*)","\\1",GP19M3),
           ym=paste0(as.character(JAHR),sub("MONAT(.*)","\\1",MONAT)),
           date=ym(ym),
           producerprices=PRE001_WERT)%>%
    select(nace2,date,producerprices)

  data_4d<-gen_cube("61241BM004", database = "genesis")%>%
    mutate(
      nace2=sub("GP19-(.*)","\\1",GP19M4),
      level=4,
      ym=paste0(as.character(JAHR),sub("MONAT(.*)","\\1",MONAT)),
      date=ym(ym),
      producerprices=PRE001_WERT)%>%
    select(nace2,date,producerprices)

  rbind(data_2d,data_3d,data_4d)%>%
    mutate(producerprices=ifelse(producerprices==0,NA_real_,producerprices))
}

#' Load and format production industry destatis
#'
#' *get_p.indu_destatis()* loads data on production from destatis and formats them. Before downloading the data, the user has the use *restatis::gen_auth_save()*, see <https://github.com/CorrelAid/restatis>.
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
get_q.indu_destatis<-function(){
  data_2d<-gen_cube("42153BM002", database = "genesis")%>%
    filter(WERT03=="WERTORG")%>%
    mutate(nace2=sub("WZ08-(.*)","\\1",WZ08V2),
           ym=paste0(as.character(JAHR),sub("MONAT(.*)","\\1",MONAT)),
           date=ym(ym),
           production=PRO101_WERT)%>%
    select(nace2,date,production)

  data_3d<-gen_cube("42153BM003", database = "genesis")%>%
    filter(WERT03=="WERTORG")%>%
    mutate(nace2=sub("WZ08-(.*)","\\1",WZ08V3),
           ym=paste0(as.character(JAHR),sub("MONAT(.*)","\\1",MONAT)),
           date=ym(ym),
           production=PRO101_WERT)%>%
    select(nace2,date,production)

  data_4d<-gen_cube("42153BM004", database = "genesis")%>%
    filter(WERT03=="WERTORG")%>%
    mutate(nace2=sub("WZ08-(.*)","\\1",WZ08V4),
           ym=paste0(as.character(JAHR),sub("MONAT(.*)","\\1",MONAT)),
           date=ym(ym),
           production=PRO101_WERT)%>%
    select(nace2,date,production)

  rbind(data_2d,data_3d,data_4d)%>%
    mutate(production=ifelse(production==0,NA_real_,production))
}

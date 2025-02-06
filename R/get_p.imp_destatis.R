#' Load and format import prices destatis
#'
#' *get_p.indu_destatis()* loads data on import prices from destatis and formats them. Before downloading the data, the user has the use *restatis::gen_auth_save()*, see <https://github.com/CorrelAid/restatis>.
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
#' get_p.imp_destatis()
#' }
get_p.imp_destatis<-function(){
  data_2d<-gen_cube("61411BM002", database = "genesis")%>%
    mutate(nace2=sub("GP19-(.*)","\\1",GP19X2),
           level=2,
           ym=paste0(as.character(JAHR),sub("MONAT(.*)","\\1",MONAT)),
           date=ym(ym),
           importprices=PRE004_WERT)%>%
    select(nace2,level,date,importprices)

  data_3d<-gen_cube("61411BM003", database = "genesis")%>%
    mutate(nace2=sub("GP19-(.*)","\\1",GP19X3),
           level=3,
           ym=paste0(as.character(JAHR),sub("MONAT(.*)","\\1",MONAT)),
           date=ym(ym),
           importprices=PRE004_WERT)%>%
    select(nace2,level,date,importprices)


  data_4d<-gen_cube("61411BM004", database = "genesis")%>%
    mutate(nace2=sub("GP19-(.*)","\\1",GP19X4),
           level=4,
           ym=paste0(as.character(JAHR),sub("MONAT(.*)","\\1",MONAT)),
           date=ym(ym),
           importprices=PRE004_WERT)%>%
    select(nace2,level,date,importprices)

  rbind(data_2d,data_3d,data_4d)%>%
    mutate(importprices=ifelse(importprices==0,NA_real_,importprices))
}

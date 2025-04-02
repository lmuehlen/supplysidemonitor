#' Create dataframe for reactable table
#'
#'  *gen_datatable_level()* is a subfunction of *gen_datatable()*
#' @param data dataframe created by *get_matching*
#' @return seasonally adjusted time series as vector
#'
#' @import restatis
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import lubridate
#'
#' @examples
#' \dontrun{
#' gen_transformations()
#' }
get_datatable_level<- function(data,lev=2,sts_transf=c("c12m_nsa"),sts_series_transf="level_sa",ifo_transf=c("level_nsa"),ifo_series_transf="level_nsa",startdate="2015-01-01",enddate="2024-12-01",select_full=FALSE){

  #dataseries sts


  data_series_sts<-data%>%
    filter(adj==sub("(.*)_(.*)","\\2",sts_series_transf),transf==sub("(.*)_(.*)","\\1",sts_series_transf),level==lev,date>=startdate,date<=enddate)%>%
    {if(select_full==TRUE) {filter(.,include_full==TRUE)}else{filter(.,include_selected==TRUE)}}%>%
    group_by(nace2,full_name,short_name,ppi.weight) %>%
    arrange(date) %>%
    summarize(
      output_series = list(tibble(date,
                                  production=round(production,1),
                                  importvolume=round(importTOT,1))),
      price_series = list(tibble(date,
                                 producerprices=round(producerprices,1),
                                 importprices=round(importprices,1)))
    )%>%
    ungroup()

  # dataseries ifo


  data_series_ifo<-data%>%
    filter(adj==sub("(.*)_(.*)","\\2",ifo_series_transf),transf==sub("(.*)_(.*)","\\1",ifo_series_transf),level==lev,date>=startdate,date<=enddate)%>%
    {if(select_full==TRUE) {filter(.,include_full==TRUE)}else{filter(.,include_selected==TRUE)}}%>%
    group_by(nace2,full_name,short_name,ppi.weight) %>%
    arrange(date) %>%
    summarize(
      ifo_series = list(tibble(date,
                               # obstproduction=round(ifo.obstruction.production,1),
                               shortmaterial=round(ifo.shortage.material,1),
                               lackorders=round(ifo.lack.orders,1)))
    )%>%
    ungroup()

  #sts data transf
  transf<-sub("(.*)_(.*)","\\1",sts_transf)
  sa<-sub("(.*)_(.*)","\\2",sts_transf)

  sts<-map2(transf,sa,function(x,y){

    data%>%
      filter(adj==y,transf==x,level==lev,date>=startdate,date<=enddate)%>%
      {if(select_full==TRUE) {filter(.,include_full==TRUE)}else{filter(.,include_selected==TRUE)}}%>%
      group_by(nace2,full_name,short_name,ppi.weight) %>%
      arrange(date) %>%
      summarize(
        production = round(last(production[!is.na(production)]),1),
        producerprices = round(last(producerprices[!is.na(producerprices)]),1),
        importTOT = round(last(importTOT[!is.na(importTOT)]),1),
        importprices = round(last(importprices[!is.na(importprices)]),1)
      )%>%
      ungroup()
  })



  sts2<-map2(sts,sts_transf,function(x,y){
    x%>%
      rename_with(~paste0(.x,"_", y), .cols = c("production","producerprices","importTOT","importprices"))
  })


  #ifo data

  transf<-sub("(.*)_(.*)","\\1",ifo_transf)
  sa<-sub("(.*)_(.*)","\\2",ifo_transf)

  ifo<-map2(transf,sa,function(x,y){

    data%>%
      filter(adj==y,transf==x,level==lev,date>=startdate,date<=enddate)%>%
      {if(select_full==TRUE) {filter(.,include_full==TRUE)}else{filter(.,include_selected==TRUE)}}%>%
      group_by(nace2,full_name,short_name,ppi.weight) %>%
      arrange(date) %>%
      summarize(
        #  obstproduction = round(last(ifo.obstruction.production[!is.na(ifo.obstruction.production)]),1),
        shortagematerial = round(last(ifo.shortage.material[!is.na(ifo.shortage.material)]),1),
        lackorders = round(last(ifo.lack.orders[!is.na(ifo.lack.orders)]),1)
      )%>%
      ungroup()
  })



  ifo2<-map2(ifo,ifo_transf,function(x,y){
    x%>%
      rename_with(~paste0(.x,"_", y), .cols = c("shortagematerial","lackorders"))
  })


  list3<-c(sts2,ifo2,list(data_series_sts,data_series_ifo))


  cols<-get_cols(sts_transf,sts_series_transf,ifo_transf,ifo_series_transf)

  var_order<-c(cols[[2]],"price_series",cols[[1]],"output_series",cols[[3]],"ifo_series")

  data_table<-list3%>%reduce(left_join,by=c("full_name","short_name","nace2","ppi.weight"))%>%select(full_name,short_name,nace2,ppi.weight,all_of(var_order))

  return(data_table)
}



#' Create dataframe for reactable table
#'
#'  *gen_datatable* creates a datatable with the format of the final reactable.
#' @param data dataframe created by *get_matching*
#' @param sectors Sectors to be included. Default: all
#'
#'
#'
#' @return seasonally adjusted time series as vector
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
#' gen_dataframe_level()
#' }
get_datatable<-function(data,sectors=c(as.character(10:35),"06"),...){
  data_table2<-get_datatable_level(data=data,lev=2,...)%>%
    mutate(level2=sub("(\\d{2}).","\\1",nace2),
           level2=factor(level2,levels=sectors))%>%
    filter(level2%in%sectors)%>%
    arrange(level2)%>%
    select(-level2)


  data_table3<-get_datatable_level(data=data_transf,lev=3,...)%>%filter(sub("(\\d{2}).*","\\1",full_name)%in%sectors)%>%
    mutate(level2=sub("(\\d{2}).","\\1",nace2))%>%arrange(-ppi.weight)

  data_table<-list(data_table2,data_table3)
  return(data_table)
}

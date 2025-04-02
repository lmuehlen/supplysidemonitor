#' Get import electricity
#'
#' @returns dataframe
#' @export
#'
#'
get_q.impelect<-function(){

  data_qimpelect4<-gen_cube("43312BM001",database="genesis",contents="EKT102")%>%
    filter(JAHR>=1991)%>%
    mutate(nace2="3511",
           ym=paste0(as.character(JAHR),sub("MONAT(.*)","\\1",MONAT)),
           date=ym(ym),
           importEU=NA,
           importROW=NA,
           importTOT=100*EKT102_WERT/mean(EKT102_WERT[year(date) == 2021]))%>%
    select(nace2,date,importEU,importROW,importTOT)


  data_qimpelect3<-data_qimpelect4%>%
    mutate(nace2="351")

  data_qimpelect<-rbind(data_qimpelect4,data_qimpelect3)

  return(data_qimpelect)
}



#' Get production electricity
#'
#' @returns dataframe
#' @export
#'
#'
get_q.elect<-function(){
  data_qelect4<-gen_cube("43312BM003",database="genesis",contents="SPE001")%>%
    mutate(nace2="3511",
           ym=paste0(as.character(JAHR),sub("MONAT(.*)","\\1",MONAT)),
           date=ym(ym),
           production=100*SPE001_WERT/mean(SPE001_WERT[year(date) == 2021]))%>%
    select(nace2,date,production)


  data_qelect3<-data_qelect4%>%
    mutate(nace2="351")

  data_qelect<-rbind(data_qelect4,data_qelect3)
  return(data_qelect)
}


#' Get eurostat gas imports
#'
#' @returns dataframe
#' @export
#'
#'
get_eurostatgasimport<-function(){
  data_gasimport<-get_eurostat("nrg_cb_gasm")%>%
    filter(geo=="DE",nrg_bal=="IC_CAL_MG",unit=="MIO_M3")%>%
    select(date=TIME_PERIOD,importTOT=values)%>%
    mutate(nace2="062",importEU=NA,importROW=NA)%>%
    select(nace2,date,importEU,importROW,importTOT)

  data_gasimport<-data_gasimport%>%
    mutate(
      importTOT=100*importTOT/(data_gasimport%>%filter(year(date)==2021)%>%pull(importTOT)%>%mean()))

  return(data_gasimport)
}

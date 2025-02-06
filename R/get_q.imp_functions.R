#' Load and format disagregated import volume destatis
#'
#' *get_q.impfull_destatis()* loads data on import volumes by country from destatis and formats them. Before downloading the data, the user has the use *restatis::gen_auth_save()*, see <https://github.com/CorrelAid/restatis>.
#'
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
#' get_q.impfull_destatis()
#' }
get_q.impfull_destatis<-function(){

  data_import_2d<-map_df(2008:2024,function(x){
    gen_cube("51000BM221", database = "genesis",startyear=x,endyear=x,contents="GEWE")%>%
      mutate(nace2=sub("GP19-(.*)","\\1",GP19B2),
             level=2,
             countrycode=STLAH,
             ym=paste0(as.character(JAHR),sub("MONAT(.*)","\\1",MONAT)),
             date=ym(ym),
             importvolume=GEWE_WERT)%>%
      select(nace2,countrycode,level,date,importvolume)
  })


  data_import_4d1<-map_df(2008:2014,function(x){
    gen_cube("51000BM241", database = "genesis",startyear=x,endyear=x,contents="GEWE")%>%
      mutate(nace2=sub("GP19-(.*)","\\1",GP19B4),
             level=4,
             countrycode=STLAH,
             ym=paste0(as.character(JAHR),sub("MONAT(.*)","\\1",MONAT)),
             date=ym(ym),
             importvolume=GEWE_WERT)%>%
      select(nace2,countrycode,level,date,importvolume)
  })

  data_import_4d2<-map_df(2015:2024,function(x){
    gen_cube("51000BM241", database = "genesis",startyear=x,endyear=x,contents="GEWE")%>%
      mutate(nace2=sub("GP19-(.*)","\\1",GP19B4),
             level=4,
             countrycode=STLAH,
             ym=paste0(as.character(JAHR),sub("MONAT(.*)","\\1",MONAT)),
             date=ym(ym),
             importvolume=GEWE_WERT)%>%
      select(nace2,countrycode,level,date,importvolume)
  })

  data_import<-rbind(data_import_2d,data_import_4d1,data_import_4d2)

  data_import2<-left_join(data_import,countrycodes,by="countrycode")

  return(data_import2)
}

#' Aggregate import volume data
#'
#' *get_q.imp_agg()* aggregates data downloaded via *get_q.impfull_destatis*.
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
#' get_q.imp_agg()
#' }
gen_q.imp_agg<-function(data){



  df<-data%>%
    filter(!nace2%in%c("32UU","89AW","89BF","89KE"))%>%
    select(nace2,date,country_group,importvolume)%>%
    mutate(year=year(date))%>%# for scaling (mean 2021 = 100)
    group_by(nace2,date,country_group)%>%
    summarise(
      import_sum=sum(importvolume,na.rm=T))%>% # importvolume
    ungroup()%>%
    # gen sum world
    pivot_wider(names_from = country_group,values_from = import_sum)%>%
    mutate(TOT=case_when(!is.na(EU)&!is.na(ROW)~EU+ROW,
                         is.na(ROW)~ROW,
                         is.na(EU)~EU,
                         TRUE~NA_real_
    ))

  #create nace2 level3 by summing over values of level 4 (short sanity check: fit)
  df2<-df%>%
    filter(nchar(nace2)==4)%>%
    mutate(nace2=sub("(\\d{3}).","\\1",nace2))%>%
    group_by(nace2,date)%>%
    summarise(across(where(is.numeric),~sum(.,na.rm=TRUE)))

  df3<-rbind(df,df2)%>%
    pivot_longer(-c(nace2,date),names_to = "country_group",values_to = "import_sum")%>%
    group_by(nace2,country_group)%>%
    mutate(
      import_avg_2021=mean(import_sum[year(date) == 2021], na.rm = TRUE),
      import_avg_2021=case_when(import_avg_2021==0~NA_real_,
                                TRUE~import_avg_2021))%>%
    ungroup()%>%
    mutate(import=100*import_sum/import_avg_2021)%>%
    select(-c(import_sum,import_avg_2021))%>%
    mutate(import=ifelse(import==0,NA_real_,import))%>%
    pivot_wider(values_from = import,names_from = country_group,names_prefix = "import")

  return(df3)
}

#' Create dataframe including data transformations
#'
#'  *gen_transformations* creates a dataframe including multiple transformations. Takes about 20-30 min.
#' @param data dataframe created by *get_matching*
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
gen_transformations<-function(data){
  data%>%
    group_by(nace2)%>%
    arrange(date)%>%
    mutate(
      across(c(production,producerprices,importprices,importEU,importROW,importTOT,matches("ifo")),~do_seasonadj(.x,date),.names = "{.col}_sa"))%>%
    rename(production_nsa=production,producerprices_nsa=producerprices,importprices_nsa=importprices,importEU_nsa=importEU,importROW_nsa=importROW,importTOT_nsa=importTOT,
           "ifo.capacity.bottleneck_nsa"="ifo.capacity.bottleneck",
           "ifo.shortage.material_nsa"="ifo.shortage.material",
           "ifo.financial.bottleneck_nsa"="ifo.financial.bottleneck",
           "ifo.lack.orders_nsa"="ifo.lack.orders",
           "ifo.labour.bottleneck_nsa"="ifo.labour.bottleneck",
           "ifo.other.obstructive.factors_nsa"="ifo.other.obstructive.factors",
           "ifo.obstruction.production_nsa"="ifo.obstruction.production")%>%
    mutate(
      #growth year on year
      across(c(production_nsa,producerprices_nsa,importprices_nsa,importEU_nsa,importROW_nsa,importTOT_nsa,matches("ifo.*_nsa$")),~round(100*(./lag(.,12)-1),1),.names = "{.col}_c12m"),
      #growth month on month
      across(c(production_nsa,producerprices_nsa,importprices_nsa,importEU_nsa,importROW_nsa,importTOT_nsa,matches("ifo.*_nsa$")),~round(100*(./lag(.,1)-1),1),.names = "{.col}_c1m"),
      #growth month on month sa
      across(c(production_sa,producerprices_sa,importprices_sa,importEU_sa,importROW_sa,importTOT_sa,matches("ifo.*_sa$")),~round(100*(./lag(.,1)-1),1),.names = "{.col}_c1m"),
      #growth year on year, sa, 3 month rolling average both sides
      across(c(production_sa,producerprices_sa,importprices_sa,importEU_sa,importROW_sa,importTOT_sa,matches("ifo.*_sa$")),~round(100*((.+lag(.,1)+lag(.,2))/(lag(.,12)+lag(.,12+1)+lag(.,12+2))-1),1),.names = "{.col}_c12mr3"),
      #growth year on year, sa, 3 month rolling average only base
      across(c(production_sa,producerprices_sa,importprices_sa,importEU_sa,importROW_sa,importTOT_sa,matches("ifo.*_sa$")),
             ~round(100*((.)/((lag(.,12)+lag(.,12+1)+lag(.,12+2))/3)-1),1),.names = "{.col}_c12mr3f")
    )%>%
    ungroup()%>%
    #renaming to have base values (and sa values) labeled as levels
    rename(production_nsa_level=production_nsa,
           producerprices_nsa_level=producerprices_nsa,
           importprices_nsa_level=importprices_nsa,
           importEU_nsa_level=importEU_nsa,
           importROW_nsa_level=importROW_nsa,
           importTOT_nsa_level=importTOT_nsa,
           ifo.shortage.material_nsa_level=ifo.shortage.material_nsa,
           ifo.lack.orders_nsa_level=ifo.lack.orders_nsa,
           ifo.labour.bottleneck_nsa_level=ifo.labour.bottleneck_nsa,
           ifo.capacity.bottleneck_nsa_level=ifo.capacity.bottleneck_nsa,
           ifo.obstruction.production_nsa_level=ifo.obstruction.production_nsa,
           ifo.financial.bottleneck_nsa_level=ifo.financial.bottleneck_nsa,
           ifo.other.obstructive.factors_nsa_level=ifo.other.obstructive.factors_nsa,
           production_sa_level=production_sa,
           producerprices_sa_level=producerprices_sa,
           importprices_sa_level=importprices_sa,
           importEU_sa_level=importEU_sa,
           importROW_sa_level=importROW_sa,
           importTOT_sa_level=importTOT_sa,
           ifo.shortage.material_sa_level=ifo.shortage.material_sa,
           ifo.lack.orders_sa_level=ifo.lack.orders_sa,
           ifo.labour.bottleneck_sa_level=ifo.labour.bottleneck_sa,
           ifo.capacity.bottleneck_sa_level=ifo.capacity.bottleneck_sa,
           ifo.obstruction.production_sa_level=ifo.obstruction.production_sa,
           ifo.financial.bottleneck_sa_level=ifo.financial.bottleneck_sa,
           ifo.other.obstructive.factors_sa_level=ifo.other.obstructive.factors_sa)%>%
    pivot_longer(-c(nace2,date,category,nace2_main_code,nace2_main_name,level,nace2_sub_name,include_full,include_selected,Weber_name,Weber_weight),names_pattern = "(.*)_(.*)_(.*)",names_to = c("vars","adj","transf"),values_to = "values")%>%
    pivot_wider(names_from = "vars",values_from = "values")
}

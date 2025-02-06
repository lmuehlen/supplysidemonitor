

#' Load and format ifo data
#'
#' *get_ifo_indu()* loads data on limiting factors for production in the industry originally provided by the ifo Institut.
#' The data are downloaded from Macrobond using the package MacrobondAPI (which needs specific rights) and formated to be easily combined with further data.
#'
#' @return A dataframe
#' @export
#'
#' @import MacrobondAPI
#' @import purrr
#' @importFrom zoo fortify.zoo
#' @importFrom xts as.xts
#' @import dplyr
#' @import tidyr
#' @examples
#' \dontrun{
#' get_ifo_indu()
#' }
get_ifo_indu<-function(){
  #Downloading from Macrobond####
  #creating query
  query <- CreateSearchQuery()
  addAttributeValueFilter(query,"Release","rel_deifodr")
  setSearchText(query, "Manufacturing by Sector Yes-Answers")
  entities<-c(query)%>%SearchEntities()%>%getEntities()
  release_tickers<-c(map_chr(entities,getName))

  y<-release_tickers%>%FetchTimeSeries()

  #transform time series to dataframe
  df<-y%>%map(as.xts)%>%do.call("cbind",.)%>%fortify.zoo(name="date")%>%as_tibble()%>%
    pivot_longer(-date,names_to = "code",values_to = "values")

  #get Metadata and combine with time series
  m<-map(y,function(x)
  {cbind(title=getTitle(x),
         code=getMetadataValues(getMetadata(x),"PrimName"),
         countrycode=getMetadataValues(getMetadata(x),"Region"))
  })

  m2<-do.call("rbind",m)%>%as_tibble()
  data<-left_join(df,m2,by="code")

  #Transforming data####
  data.indu<-data%>%
    #filter only time series we are interested in
    filter(grepl("deifo_c(\\d{4}000)_(tkj|maj|fej|afj|arj|auj|bhj)_bdu",code))%>%
    # bring vars in right format and pivot
    mutate(
      nace2=sub(".*deifo_.(\\d{4}).*", "\\1", code),
      level=case_when(grepl("00$",nace2)~2,
                      grepl("0$",nace2)~3,
                      TRUE~4),
      nace2=gsub("0{1,2}$", "", nace2), #delete last two zeros (if exist)
      var=sub(".*deifo_(.*)_(.*)_bdu","\\2",code)
    )%>%
    select(-c(code,title,level,countrycode))%>%
    filter(date>="2015-01-01",nace2!="00")%>%
    pivot_wider(values_from = values,names_from = var)%>%
    rename("ifo.capacity.bottleneck"=tkj,
           "ifo.shortage.material"=maj,
           "ifo.financial.bottleneck"=fej,
           "ifo.lack.orders"=afj,
           "ifo.labour.bottleneck"=arj,
           "ifo.other.obstructive.factors"=auj,
           "ifo.obstruction.production"=bhj)

  return(data.indu)
}

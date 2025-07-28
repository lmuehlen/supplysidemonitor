#' Create dataframe including data transformations
#'
#'  *gen_transformations* creates a dataframe including multiple transformations. Takes about 20-30 min.
#' @param data dataframe created by *get_transformation*
#' @param width_col_sec width of sector column
#' @param width_col_ppi width of ppi column
#' @param width_col_q.p width of output and price numerical columns
#' @param width_col_ifo width of ifo numerical columns
#' @param abbr_ppi Full text header ppi
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
#' @import reactable
#' @import htmlwidgets
#' @import htmltools
#' @import magrittr
#'
#' @examples
#' \dontrun{
#' gen_dataframe_level()
#' }
get_table<-function(data,width_col_sec=150,width_col_ppi=65,width_col_q.p=60,width_col_ifo=70,width_plot=150,
                    ppi_name="PPI",
                    abbr_ppi="Producer Price Index weight",
                    abbr_price=c("ΔProd:\n YoY Change Rate Producer Prices\n with rolling averages\n (yₜ / (yₜ₋₁₁ + yₜ₋₁₂ + yₜ₋₁₃)-1)*100","ΔImp:\n YoY Change Rate Import Price\n with rolling averages\n (yₜ / (yₜ₋₁₁ + yₜ₋₁₂ + yₜ₋₁₃)-1)*100"),
                    abbr_output=c("ΔProd:\n YoY Change Rate Production Volume\n with rolling averages\n (yₜ / (yₜ₋₁₁ + yₜ₋₁₂ + yₜ₋₁₃)-1)*100","ΔImp:\n YoY Change Rate Import Volume\n with rolling averages\n (yₜ / (yₜ₋₁₁ + yₜ₋₁₂ + yₜ₋₁₃)-1)*100"),
                    abbr_ifo=c("Material:\n Percentage of firms naming 'shortage of material'\n as limiting factor for production","Orders:\n Percentage of firms naming 'lack of orders'\n as limiting factor for production"),

                    expanded=FALSE,sorting=list(ppi.weight="desc"),pagination=FALSE,filterable=TRUE,searchable=FALSE,page_size=7,backgroundColor="#cdcadc4d",...){

  ##preparation####
  #specification arguments for functions
  dots<-list(...)
  args_dffunc<-dots[names(dots)%in%c("sts_transf","sts_series_transf","ifo_transf","ifo_series_transf","startdate","enddate","sectors","select_full")]
  args<-c(list("data"=data),args_dffunc)
  args_cols<-dots[names(dots)%in%c("sts_transf","sts_series_transf","ifo_transf","ifo_series_transf","output_labels","price_labels","ifo_labels")]

  # loading data
  data_tables<-do.call(get_datatable,args)


  #col specifications
  cols<-do.call(get_cols,args_cols)

  output_cols <- make_colblock(
    vars       = cols[[1]],
    var_labels = cols[[4]],
    abbr=abbr_output,
    width_col  = width_col_q.p
  )
  price_cols<-make_colblock(
    vars       = cols[[2]],
    var_labels = cols[[5]],
    abbr=abbr_price,
    width_col  = width_col_q.p,
    reverse=TRUE
  )

  ifo_cols<-make_colblock(
    vars       = cols[[3]],
    var_labels = cols[[6]],
    abbr=abbr_ifo,
    width_col  = width_col_ifo,
    ifo=TRUE
  )

  #further specifics for table
  theme_level1<-reactableTheme(
    style = list(
      fontFamily="OpenSans, sans-serif",
      marginTop="0px"
    ),
    headerStyle = list(fontSize = "14px"),
    cellStyle = list(fontSize = "14px",display = "flex", flexDirection = "column", justifyContent = "center")
  )

  theme_level2 <- reactableTheme(
    headerStyle = list(display = "none"),
    backgroundColor = backgroundColor,
    style = list(
      fontFamily="OpenSans, sans-serif",
      overflow= "visible"
    ),
    cellStyle = list(fontSize = "14px",display = "flex", flexDirection = "column", justifyContent = "center")
  )

  width_total<-width_col_sec+width_col_ppi+(length(output_cols)+length(price_cols))*width_col_q.p+length(ifo_cols)*width_col_ifo+3*width_plot+20

  #table####
  table<-reactable(

    #Level2
    data=data_tables[[1]],
    width=width_total,
    columns = c(output_cols,price_cols,ifo_cols,list(
      nace2 = colDef(show=F),
      short_name= colDef(name="click for subsector",
                         width=width_col_sec,
                         cell = function(value) {
                           full <- data_tables[[1]]%>%filter(short_name==value)%>%pull(full_name)
                           tags$abbr(title = full, value,style = "text-decoration: none;")
                         }
      ),
      full_name=colDef(show=F),
      ppi.weight=colDef(name=ppi_name,
                        align="center",
                        format = colFormat(suffix = "%"),
                        header = function(colName) {
                          tags$abbr(
                            title = abbr_ppi,
                            colName  # The default column name, e.g., "mpg"
                          )},
                        width=width_col_ppi,filterable = FALSE),

      output_series = get_col_series(y_vars=c("production","importvolume"),y_labels = c(production="Production",importvolume="Import Volume"),width = width_plot),
      price_series = get_col_series(y_vars=c("producerprices","importprices"),y_labels = c(producerprices="Producer prices",importprices="Import prices"),width = width_plot),
      ifo_series = get_col_series(y_vars=c("shortmaterial","lackorders"),y_labels = c(shortmaterial="Shortage of material",lackorders="Lack of orders"),width = width_plot,quarterly=T,ifo=T)


    )),
    columnGroups = list(
      colGroup(name="Sector",columns=c("short_name","ppi.weight")),
      colGroup(name = "Output",columns = c(cols[[1]],"output_series")),#headerStyle = list(textAlign = "left")
      colGroup(name = "Prices",columns =c(cols[[2]],"price_series")),
      colGroup(name = "Limiting factors of production",columns =c(cols[[3]],"ifo_series"))
    ),

    #Main table specs
    bordered = FALSE,
    defaultSorted = sorting,
    highlight = FALSE,
    striped = FALSE,
    resizable = FALSE,
    pagination=pagination,
    defaultPageSize = page_size,
    filterable=filterable,
    searchable=searchable,
    defaultExpanded = expanded,
    theme=theme_level1,

    #Level 3
    onClick = "expand",
    details = colDef(width=0,details = function(index) {
      # data_sub <- data_table4[data_table4$level2 == data_table2$nace2[index], ]
      data_sub <- data_tables[[2]][data_tables[[2]]$level2 == data_tables[[1]]$nace2[index], ]%>%
        select(-level2)
      reactable(
        data_sub,
        width=width_total,
        theme = theme_level2,
        columns  = c(output_cols,price_cols,ifo_cols,list(
          nace2 = colDef(show=F),
          short_name= colDef(name="Sector",width=width_col_sec,
                             cell = function(value) {
                               full <- data_tables[[2]]%>%filter(short_name==value)%>%pull(full_name)
                               tags$abbr(title = full, value,style = "text-decoration: none;")
                             },
          ),
          full_name=colDef(show=F),
          ppi.weight=colDef(name=ppi_name,
                            align="center",
                            format = colFormat(suffix = "%"),
                            width=width_col_ppi),

          output_series = get_col_series(y_vars=c("production","importvolume"),y_labels = c(production="Production",importvolume="Import Volume"),width = width_plot),
          price_series = get_col_series(y_vars=c("producerprices","importprices"),y_labels = c(producerprices="Producer prices",importprices="Import prices"),width = width_plot),
          ifo_series = get_col_series(y_vars=c("shortmaterial","lackorders"),y_labels = c(shortmaterial="Shortage of material",lackorders="Lack of orders"),width = width_plot,quarterly=T,ifo=T)


        )),
        bordered = FALSE,
        highlight = TRUE,
        striped = FALSE,
        resizable = FALSE,
        pagination=FALSE,
        filterable=FALSE

      )
    })


  )

  ##return & save####
  return(table)
}


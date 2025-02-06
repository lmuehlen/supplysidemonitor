#' Create Lineplot in reactable
#'
#'
#' @return plot
#'
#' @import restatis
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import lubridate
#' @import echarts4r
#'
#' @examples
#' \dontrun{
#' gen_transformations()
#' }
get_lineplot<-function(x,y_vars=c("production","importvolume"),y_labels=NULL,width=150,quarterly=F,ifo=F){

  all_na <- all(sapply(x[y_vars], function(col) all(is.na(col))))

  # 2) If yes, return NA:
  if (all_na) {
    return("")
  }

  min<-x$date%>%year()%>%min()
  max<-x$date%>%year()%>%max()+1


  if(quarterly){
    x<-x%>%filter(month(date)%in%c(1,4,7,10))
  }
  # x=test[[10]][[1]]
  # y_vars=c("production","importvolume")
  # width=150
  # min=2015
  # max=2026
  #Prepping

  if (is.null(y_labels)) {
    # Create a named vector from the y_vars themselves
    y_labels <- setNames(y_vars, y_vars)
  }

  missing_labels <- setdiff(y_vars, names(y_labels))
  if (length(missing_labels) > 0) {
    stop("You must provide labels for all y_vars. Missing: ",
         paste(missing_labels, collapse = ", "))
  }

  x$date_num<-year(x$date)+month(x$date)/13


  min_inter<-50
  if(ifo==FALSE){
    all_values <- x %>%
      select(all_of(y_vars)) %>%
      unlist()

    y_min <- min(all_values, na.rm = TRUE)
    y_max <- max(all_values, na.rm = TRUE)

    #dist <- max(abs(100 - y_min), abs(y_max - 100),50)
    if(y_max<200){
      dist<-50*max(
        ceiling(abs(2-y_min/50)),
        ceiling(abs(2-y_max/50))
      )
      cust_min<-100-dist
      cust_max<-100+dist

    }else if(y_max<400){
      dist<-100*ceiling(abs(1-y_max/100))

      cust_min<-0
      cust_max<-100+dist
      min_inter<-100
    }else{
      cust_max<-600
      cust_min<-0
      min_inter<-200
    }


  }else{
    cust_min<-0
    cust_max<-100
  }

  #styling
  e_common(
    font_family = "Open Sans"
  )

  ## chart
  p<-x%>%
    e_charts(date_num,height = "80px", width = paste0(width,"px"))%>%
    e_color(c("#ee6174","#51275f", "#36a9e1"))

  #### Changing ####
  for(col in y_vars){
    p<-p%>%e_line_(col,symbol ="none",name = y_labels[[col]])
  }

  # Tooltip####
  p%>% e_tooltip(trigger="axis",
                 confine = FALSE,
                 position = JS("
      function(pos, params, dom, rect, size) {
        // pos is [mouseX, mouseY]
        // We'll keep the same y as the mouse
        var tooltipY = 0.4*pos[1]-20;

        // Choose a fixed x. For example, 10px from the left edge
        var tooltipX = pos[0]-size.contentSize[0]-40;

        // Return [x, y] coordinates (in pixels) relative to the chart container
        return [tooltipX, tooltipY];
      }
    "),
                 formatter = htmlwidgets::JS("
    function(params) {
      // If no data points, return empty tooltip
      if (params.length === 0) return '';

      // Extract the numeric date from the first series.
      // e.g. [date_num, value, ...]
      var rawValue = params[0].value[0];
      var year = Math.floor(rawValue);
      var fraction = rawValue - year;

      // Convert fraction to a month index: 1..12
      var monthIndex = Math.round(fraction * 13);
      var monthNames = [
        'Jan','Feb','Mar','Apr','May','Jun',
        'Jul','Aug','Sep','Oct','Nov','Dec'
      ];

      // monthIndex goes from 1..12, but array is 0..11
      var dateLabel = monthNames[monthIndex - 1] + ' ' + year;

      // Build tooltip text:
      //  - First line: Date: ...
      //  - Then each variable: Production: X, Imports: Y, etc.
      var tooltipText = 'Date: ' + dateLabel;
      for (var i = 0; i < params.length; i++) {
        var seriesName = params[i].seriesName; // e_line(name = ...)
        var yValue     = params[i].value[1];   // The second array entry is the y-value
        tooltipText += '<br/>' + seriesName + ': ' + yValue;
      }

      return tooltipText;
    }
  ")
  )%>%

    ##### non changing#####
  e_grid(left="25px")%>%
    e_axis_labels(x=NA,y=NA)%>%
    # y axis
    e_y_axis(
      show = TRUE,
      min=cust_min,
      max=cust_max,
      minInterval=min_inter,
      axisTick=list(show=FALSE),
      axisLine = list(show = FALSE,onZero=FALSE),
      splitLine=list(
        showMinLine=FALSE,
        showMaxLabel=FALSE,
        lineStyle=list(type="dotted",
                       color="#181c44")
      ),
      axisLabel = list(
        color="#181c44",
        fontFamily = "Open Sans",
        fontSize = 10,
        formatter=JS("function(value){return value;}")
      ))%>%
    #x axis
    e_x_axis(
      position = "bottom",
      #  type="time",
      min=min,
      max=max,
      minInterval=200,
      axisTick=list(show=FALSE),
      axisLine = list(show = FALSE,onZero=FALSE),
      splitLine = list(show = FALSE),
      axisLabel = list(
        formatter = JS("
        function(value) {
          return Number(value);
        }
      "),
        color="#181c44",
        fontFamily = "Open Sans",
        fontSize = 10
      )
    )%>%
    e_legend(show=FALSE)%>%

    htmlwidgets::onRender(
      htmlwidgets::JS("
        function(el, x) {
          el.parentElement.style.overflow = 'visible';
        }
      ")
    )
}



#' Create Lineplot in reactable
#'
#'
#' @return plot
#'
#' @import restatis
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import lubridate
#' @import echarts4r
#'
#' @examples
#' \dontrun{
#' gen_transformations()
#' }
get_cols<-function(sts_transf=c("c12m_nsa"),sts_series_transf="level_sa",ifo_transf=c("level_nsa"),ifo_series_transf="level_nsa",output_labels=NULL,price_labels=NULL,ifo_labels=NULL){
  output_vars<-expand_grid(names=c("production","importTOT"), transformations= sts_transf) %>%
    mutate(var = paste(names, transformations, sep = "_")) %>%
    pull(var)
  if(is.null(output_labels)|length(output_labels)!=length(output_vars)){
    output_labels<-output_vars
  }

  price_vars<-expand_grid(names=c("producerprices","importprices"), transformations=sts_transf) %>%
    mutate(var = paste(names, transformations, sep = "_")) %>%
    pull(var)
  if(is.null(price_labels)|length(price_labels)!=length(price_vars)){
    price_labels<-price_vars
  }

  ifo_vars<-expand_grid(names=c("shortagematerial","lackorders"), transformations=ifo_transf) %>%
    mutate(var = paste(names, transformations, sep = "_")) %>%
    pull(var)
  if(is.null(ifo_labels)|length(ifo_labels)!=length(ifo_vars)){
    ifo_labels<-ifo_vars
  }


  cols<-list(output_vars,price_vars,ifo_vars,output_labels,price_labels,ifo_labels)
  return(cols)
}


#' Create Lineplot in reactable
#'
#'
#' @return plot
#'
#' @import restatis
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import lubridate
#' @import echarts4r
#'
#' @examples
#' \dontrun{
#' gen_transformations()
#' }
get_col_num<-function(name,width=100,reverse=FALSE,transf_name="12m_ra",tooltip=NA){

  #hier werden die farben definiert
  colors<-colorRampPalette(c("#ee6174","grey95","#36a9e1"))(13)[-7]
  if(reverse){
    colors<-rev(colors)
  }

  col_num<-colDef(name = name,width = width,
                  align="center",
                  filterable = FALSE,
                  style = function(val) {
                    idx <- cut(
                      val,
                      breaks = c(-Inf, seq(-10, 10, by = 2), Inf),
                      labels = FALSE,
                      right = TRUE
                    )

                    background_color<-list(
                      background = colors[idx]
                    )

                    return(background_color)
                  },
                  header = function(colName) {
                    if(!is.na(tooltip)){
                      tags$abbr(
                        title = tooltip,
                        colName  # The default column name, e.g., "mpg"
                      )}
                  }

  )

  return(col_num)
}



#' Create Lineplot in reactable
#'
#'
#' @return plot
#'
#' @import restatis
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import lubridate
#' @import echarts4r
#'
#' @examples
#' \dontrun{
#' gen_transformations()
#' }
get_col_num_ifo<-function(name,width=100,reverse=FALSE,transf_name="12m_ra",tooltip=NA){


  col_num_ifo<-colDef(name = name,width = width,
                      align="center",
                      filterable = FALSE,
                      style = function(val) {
                        idx <- cut(
                          val,
                          breaks =  c(-1,seq(10,100,by=10)),
                          labels = FALSE,
                          right = TRUE
                        )
                        #hier Farbe für ifo
                        colors_ifo<-colorRampPalette(c("white","#ee6174"))(10)

                        background_color<-list(
                          background = colors_ifo[idx]
                        )
                        return(background_color)
                      },
                      header = function(colName) {
                        if(!is.na(tooltip)){
                          tags$abbr(
                            title = tooltip,
                            colName  # The default column name, e.g., "mpg"
                          )}
                      }
  )

  return(col_num_ifo)
}


#' Create Lineplot in reactable
#'
#'
#' @return plot
#'
#' @import restatis
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import lubridate
#' @import echarts4r
#'
#' @examples
#' \dontrun{
#' gen_transformations()
#' }
get_col_series<-function(y_vars,y_labels,name="2021=100",width,quarterly=F,ifo=F){
  col_series<-colDef(
    name = name,
    style = "overflow: visible;",
    filterable = FALSE,
    width=width+5,
    align="center",
    cell = function(x){
      x%>%get_lineplot(y_vars=y_vars,y_labels=y_labels,width = width,quarterly=quarterly,ifo=ifo)
    }
  )
}


make_colblock <- function(vars, var_labels = NULL,abbr=NULL, width_col = 100,reverse=F,ifo=FALSE) {

  if (is.null(var_labels)|length(vars) != length(var_labels)) {
    var_labels <- vars
  }
  if (is.null(abbr)|(length(vars) != length(abbr))) {
    abbr<-rep(NA,length(vars))
  }

  # For each var in `vars`, create a named colDef entry
  if(ifo){
    cols_list <- map2(var_labels,abbr, function(x,y) {
      get_col_num_ifo(name = x, width = width_col, reverse = reverse,tooltip=y)
    })
  }else{
    cols_list <- map2(var_labels,abbr, function(x,y) {
      get_col_num(name = x, width = width_col, reverse = reverse,tooltip=y)
    })
  }
  names(cols_list) <- vars

  return(cols_list)
}

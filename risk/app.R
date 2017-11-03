rm(list=ls())

library(shiny)
library(highcharter)
library(purrr)
library(lubridate)
library(dplyr)
library(reshape2)

###RISK REPORT####
baseDir <- ifelse(as.character(.Platform$OS.type) == "windows",
                  "J:\\Misc\\Shane\\mqa\\",
                  "/srv/shiny-server/utilities/")

source(paste0(baseDir,"query_function.R"))

options(shiny.trace=T)

riskGroup <- "Total"

managers <- qry(" SELECT distinct MANAGER FROM
                ((SELECT MAX(DATE) as DATE from barra) as a
                INNER JOIN
                (select DATE, MANAGER 
                from barra
                order by MANAGER) as b
                on a.DATE = b.DATE)")

{
    
    external_javascript <- "<link href='https://gitcdn.github.io/bootstrap-toggle/2.2.2/css/bootstrap-toggle.min.css' rel='stylesheet'>
    <script src='https://gitcdn.github.io/bootstrap-toggle/2.2.2/js/bootstrap-toggle.min.js'></script>
    <link rel='stylesheet' href='https://cdn.rawgit.com/maxazan/jquery-treegrid/master/css/jquery.treegrid.css'>
    <script type='text/javascript' src='https://cdn.rawgit.com/maxazan/jquery-treegrid/master/js/jquery.treegrid.js'></script>
    <script type='text/javascript' src='https://cdn.rawgit.com/maxazan/jquery-treegrid/master/js/jquery.treegrid.bootstrap3.js'></script>
    "
}

result <- lapply(managers, 
                 function(x){
                   paste0("<option value='",trimws(x),"'>",trimws(x),"</option>",collapse="")
                 })

bucket <- qry("SELECT DISTINCT bucket as Buckets
              FROM allmanagers
              WHERE bucket != ''
              ORDER BY bucket")
max_date <- as.character(qry("select max(date) from barra"))
overview_dates <- qry("select distinct DATE from barra
                      order by Date DESC")

selectTheme <- function(theme){
  switch(theme,
         null = hc_theme_null(),
         darkunica = hc_theme_darkunica(),
         gridlight = hc_theme_gridlight(),
         fintimes = hc_theme_ft(),
         dotabuff = hc_theme_db(),
         google = hc_theme_google(),
         flat = hc_theme_flat(),
         flatdark = hc_theme_flatdark(),
         simple = hc_theme_smpl(),
         sandsignika = hc_theme_sandsignika(),
         fivethirtyeight = hc_theme_538(),
         economist = hc_theme_economist())
}

ui <- fluidPage(
  tags$head(HTML(paste0(external_javascript,
                        "<script src='https://cdnjs.cloudflare.com/ajax/libs/bootstrap-select/1.10.0/js/bootstrap-select.min.js'></script>
                        <script src='https://gitcdn.github.io/troolee/gridstack.js'></script>
                        <link rel='stylesheet' href='https://cdnjs.cloudflare.com/ajax/libs/bootstrap-select/1.10.0/css/bootstrap-select.min.css'>
                        <script>
                        (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
                        (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
                        m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
                        })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');
                        
                        ga('create', 'UA-88914395-1', 'auto');
                        ga('send', 'pageview');
                        
                        </script>"))),
#  HTML(nav_button("Risk Report")),
  titlePanel("Risk Report"),
  tabsetPanel(
    tabPanel(
      "Risk Overview", br(),
      fluidRow(
        column(width=3, selectInput("overview_manager","Select Portfolio:",choices=managers)),
        column(width=2, selectInput("overview_date","Select Date:",choices=overview_dates)),br(),
        #column(width=1, actionButton("to_timeseries","",icon=icon('share-alt',lib='glyphicon'))),
        column(width=2, HTML("<div style='margin-top: -11%'><label> Arrange by:</label>
                             <input id='grouping' class='btn-lg' type='checkbox' data-width='150'
                             checked data-toggle='toggle' data-on='Risk Contribution' data-off='Active Exposure' 
                             data-onstyle='default'></div>
                             "))
        ),
      fluidRow(
        column(width=6, highchartOutput("overview_tl")),
        column(width=6, highchartOutput("overview_tr"))
      ),
      fluidRow(
        column(width=6, highchartOutput("overview_bl")),
        column(width=6, highchartOutput("overview_br"))
      ),
      fluidRow(
        column(width=6, highchartOutput("overview_bbl")),
        column(width=6, highchartOutput("overview_bbr"))
      )
        ),
    tabPanel("Time-Series",br(),
             fluidRow(
               column(width = 9,
                      htmlOutput("hcontainer",height = "500px")
               ),
               column(width = 3, class = "panel",
                      HTML(paste0("
                                  <label for='manager'>Select Manager:</label>
                                  <select data-width='100%' class='selectpicker'  data-actions-box='true'
                                  data-live-search='true' multiple id='manager'
                                  title='Select Managers'>",result,"</select>
                                  </br>")),br(),
                      selectInput("bucketselect", label = "Bucket:",  width = "100%",
                                  choices = c("Select Bucket", bucket)),
                      selectInput("graphType", label = "Risk or Exposure?",
                                  choices = c("Risk", "Exposure"), 
                                  selected = "Contribution"),
                      conditionalPanel(
                        condition = "input.graphType == 'Risk'",
                        selectInput("risk", label = "Risk Type:",  width = "100%",
                                    choices = c("Contribution", "Percent_Contribution"))),
                      conditionalPanel(
                        condition = "input.graphType == 'Exposure'",
                        selectInput("exposure", label = "Exposure Type:",  width = "100%",
                                    choices = c("Time-Series", "Range"))),
                      selectInput("riskGroup", label = "Grouping:",  width = "100%",
                                  choices = c("Total", "Country", "Industry", "Style")),
                      conditionalPanel(
                        condition = "input.riskGroup != 'Total'",
                        HTML(paste0("
                                    <label for='factorSelect'>Select Factor:</label>
                                    <select data-width='100%' class='selectpicker'  data-actions-box='true'
                                    data-live-search='true' multiple id='factorSelect'
                                    title='Select Factors'>Test</select>
                                    </br>")),br()),
                      selectInput("theme", label = "Theme:",  width = "100%",
                                  choices = c("None" = FALSE, "fivethirtyeight", "economist",
                                              "darkunica", "gridlight", "sandsignika",
                                              "simple","fintimes","dotabuff","google",
                                              "flat", "flatdark")
                      ),
                      sliderInput("dateRange", width="80%",
                                  label="Date Range:", timeFormat="%b %Y",
                                  min = as.Date("2014-02-01"), max = as.Date(max_date), 
                                  value = c(as.Date("2014-02-01"), as.Date(max_date))
                      ),
                      downloadButton('downloadBarra', 'Download')
                        )
                      ))),
  HTML("<script>
       $(document).ready(function(){
       
       $('#manager').selectpicker('deselectAll');
       $('#manager').selectpicker('refresh');
       
       
       })
       
       $(function () {
       var options = {
       cell_height: 80,
       vertical_margin: 10
       };
       $('.grid-stack').gridstack(options);
       });
       
       var bucketList;
       var manArr;
       
       Shiny.addCustomMessageHandler('bucket',
       function(result){
       bucketList = result
       
       if(bucketList != 'Select Bucket'){
       $('#manager option').removeAttr('selected');
       
       selectors = bucketList;
       select = []
       
       $.each(selectors, function(i, d){
       select.push(d);
       });
       
       manArr = select[0];
       
       $('#manager').val(select[0]);
       $('#manager').change();
       $('#manager').selectpicker('refresh');				
       }
       });
       
       Shiny.addCustomMessageHandler('factor',
       function(result){
       
       var z = JSON.stringify(result);
       var select = $('#factorSelect');
       
       select.selectpicker('deselectAll');
       select.empty().append(z);
       select.selectpicker('refresh');  			
       
       });
       
       </script>
       ")
  )

server = function(session, input, output) {
  
  output$hcontainer <- renderUI({
    
    start <- input$dateRange[1]
    end <- input$dateRange[2]
    
    riskType <- switch(input$risk,
                       Contribution = 'ACT_RISK_CONT',
                       Percent_Contribution = 'ACT_RISK_PERC')
    
    factorCount <- length(input$factorSelect)
    factorSelect <- sqlClean(input$factorSelect)
    start <- input$dateRange[1]
    end <- input$dateRange[2]
    
    
    if(length(input$manager) > 1){
      
      multiMan <- input$manager
      
      if(input$graphType == 'Risk'){
        
        parentNode <- switch(input$riskGroup,
                             Style = "Risk Indices",
                             Country = "Country",
                             Industry = "Industry",
                             Total = "Total")
        
        
        if(parentNode == "Total" & month(start) == month(end) & year(start) == year(end)){
          
          data <- qry(paste0("select MANAGER, DATE, RISK_SOURCE, ACT_RISK_PERC, ACT_RISK_CONT, PARENT_NODE
                             FROM barra
                             where Manager in (",sqlClean(multiMan),")
                             AND EXTRACT(YEAR_MONTH FROM DATE) Between EXTRACT(YEAR_MONTH FROM '",start,"')
                             AND EXTRACT(YEAR_MONTH FROM '",end,"')
                             AND RISK_SOURCE NOT REGEXP 'Local Excess|Common Factor'
                             AND EXTRACT(YEAR_MONTH FROM Date) = EXTRACT(YEAR_MONTH FROM '",end,"')
                             ORDER BY MANAGER, DATE, RISK_SOURCE"))
          
          parentNode <- switch(input$riskGroup,
                               Style = unique(data[data$PARENT_NODE == '/Total/Local Excess/Residual/Common Factor/Risk Indices',"RISK_SOURCE"]),
                               Country = unique(data[data$PARENT_NODE == '/Total/Local Excess/Residual/Common Factor/Country',"RISK_SOURCE"]),
                               Industry = unique(data[data$PARENT_NODE == '/Total/Local Excess/Residual/Common Factor/Industry',"RISK_SOURCE"]),
                               Total = unique(data[data$PARENT_NODE == '/Total/Local Excess/Residual/Common Factor' |
                                                     data$PARENT_NODE == '/Total/Local Excess/Residual' |
                                                     data$PARENT_NODE == '/Total' |
                                                     data$PARENT_NODE == '/Total/Local Excess/Residual/Common Factor/World' &
                                                     data$RISK_SOURCE == 'World',
                                                   "RISK_SOURCE"]))
          
          #LABEL DRILLDOWN BUCKETS
          vlookup <- data.frame(PARENT_NODE = c('/Total/Local Excess/Residual/Common Factor/Industry',
                                                '/Total/Local Excess/Residual/Common Factor/Country', 
                                                '/Total/Local Excess/Residual/Common Factor/Risk Indices'), 
                                drilldown = c('Industry', 'Country', 'Style'))
          
          vlookup2 <- data.frame(RISK_SOURCE = c('Industry', 'Country', 'Risk Indices'),
                                 drilldown = c('Industry', 'Country', 'Style'))
          
          graph2 <- merge(data, vlookup, on="PARENT_NODE", all.x=TRUE) %>%
            arrange(MANAGER, RISK_SOURCE)
          
          downloadData <<- graph2[,-ncol(graph2)]
          
          hc <- map( unique(riskType), function(xx){
            
            hc <- highchart() %>% 
              hc_chart(type = "column") %>% 
              hc_title(text = "Contribution to Tracking Error") %>% 
              hc_subtitle(text=paste0("As of ", unique(graph2$DATE))) %>%
              hc_xAxis(categories = unique(graph2$MANAGER)) %>% 
              hc_plotOptions( area = list(
                marker = list(
                  enabled = FALSE
                )
              ),
              spline = list(
                series = list(
                  marker = list(
                    enabled = FALSE
                  )
                ),
                dataLabels = list(
                  enabled = TRUE
                )
              ),
              series = list(
                stacking = "normal")) %>%
              hc_exporting(enabled = TRUE, filename = "custom-file-name") %>%
              hc_legend(maxHeight = 80)
            
            if(riskType == "ACT_RISK_CONT"){
              hc <- hc %>%
                hc_yAxis( stackLabels = list(
                  enabled=TRUE, 
                  formatter=JS("function () {
                               return Highcharts.numberFormat(this.total,2);
            }")),
                  plotLines = list(
                    list(color = "black",
                         width = 2,
                         value = 0))) 
          } else {
            hc <- hc %>%
              hc_yAxis(plotLines = list(
                list(color = "black",
                     width = 2,
                     value = 0)))
          }
            
            for(x in parentNode){
              x <- as.character(x)
              
              hc <- hc %>%
                hc_add_series(data = graph2[graph2$RISK_SOURCE == x, xx],
                              name = x)
            }
            
            if (input$theme != FALSE) {
              theme <- selectTheme(input$theme)
              hc <- hc %>% hc_add_theme(theme)
            }
            
            hc
            
          }) %>% hw_grid(rowheight = 500, ncol = 1)
          
          return(hc)
          
        }
        
        
        if(is.null(factorSelect)){
          z <- qry(paste0(
            "select * from barra 
            where Manager in (",sqlClean(multiMan),") 
            and PARENT_NODE IN 
            ('/Total','/Common Factor','/Total/Local Excess/Residual/Common Factor',
            '/Total/Local Excess/Residual','/')
            and RISK_SOURCE NOT REGEXP 'Excess|Common'
            AND EXTRACT(YEAR_MONTH FROM DATE) Between EXTRACT(YEAR_MONTH FROM '",start,"')
            AND EXTRACT(YEAR_MONTH FROM '",end,"')
            ORDER BY DATE"
          ))
          
          z <- z %>% filter(RISK_SOURCE == parentNode)
          
        } else {
          
          z <- qry(paste0(
            "select * from barra
            where Manager in (",sqlClean(multiMan),") 
            and RISK_SOURCE IN (",factorSelect,")
            AND EXTRACT(YEAR_MONTH FROM DATE) Between EXTRACT(YEAR_MONTH FROM '",start,"')
            AND EXTRACT(YEAR_MONTH FROM '",end,"')
            ORDER BY DATE"
          ))
          
        }
        z <- z[,c("MANAGER","DATE",riskType, "RISK_SOURCE")]
      } else {
        if(is.null(factorSelect)){return("Select a Factor")}
        
        z <- qry(paste0(
          "select * from barraexposure
          where Manager in (",sqlClean(multiMan),") 
          and FACTOR IN (",factorSelect,")
          AND EXTRACT(YEAR_MONTH FROM DATE) Between EXTRACT(YEAR_MONTH FROM '",start,"')
          AND EXTRACT(YEAR_MONTH FROM '",end,"')
          ORDER BY DATE, MANAGER, FACTOR"
        ))
        
        z <- z[,c("MANAGER","DATE", "ACT_EXP" ,"FACTOR")]
        
      }
      names(z) <- c("MANAGER","x","y", "FACTOR")
      downloadData <<- z
      
      z$x<- as.numeric(as.POSIXct( as.Date(z$x, format='%Y-%m-%d')))*1000
      z$y<- as.numeric(as.character(z$y))
      
      #number of columns, height of graph for grid
      graphArgs <- ifelse(factorCount > 1, c(2,300), c(1,600) )
      
      if(input$graphType == "Exposure" & input$exposure == "Range"){
        z <- z[z$x == max(z$x),]
        
        hc <- map(c(1), function(xx){
          xx <- xx  
          hc <- highchart() %>% 
            hc_exporting(enabled = TRUE, filename = "custom-file-name") %>%
            hc_add_series_boxplot(x = z$y, by = z$FACTOR, 
                                  name = "Style", outliers = FALSE,
                                  enableMouseTracking = FALSE ) %>%
            hc_yAxis(title=list(text="Active Tilt")) %>%
            hc_plotOptions(boxplot=list(tooltip=list(enabled=FALSE)))
          
          
          for(man in multiMan){
            zz <- z[z$MANAGER == man, ]
            hc <- hc %>% 
              hc_add_series_scatter(x = c(0:(length(zz$FACTOR)-1)), 
                                    y = zz$y, name = man, showInLegend=TRUE)  
          }
          
          if (input$theme != FALSE) {
            theme <- selectTheme(input$theme)
            hc <- hc %>% hc_add_theme(theme)
          }
          
          hc 
        }) %>% hw_grid(rowheight = 500, ncol = 1)
        
        hc
      }else if(month(start) == month(end) & year(start) == year(end)){
        
        data <- z
        names(data) <- c("MANAGER","DATE", "ACT_EXP" ,"FACTOR")
        
        data$COLOR <- NULL
        
        if(input$riskGroup == "Style"){
          
          #           data[data$FACTOR %in% c("Book-to-Price","Dividend Yield", "Earnings Yield"),"COLOR"] <- "blue"
          #           data[data$FACTOR %in% c("Growth"),"COLOR"] <- "green"
          #           data[data$FACTOR %in% c("Non-linear Size", "Size", "Liquidity"),"COLOR"] <- "red"
          #           data[data$FACTOR %in% c("Momentum"),"COLOR"] <- "black"
          #           data[data$FACTOR %in% c("Leverage"),"COLOR"] <- "yellow"
          #           data[data$FACTOR %in% c("Residual Volatility","Beta"),"COLOR"] <- "purple"
          #           
          factorOrder <- c("Book-to-Price","Dividend Yield", "Earnings Yield", "Growth",
                           "Non-linear Size", "Size", "Liquidity", "Momentum", "Leverage",
                           "Residual Volatility","Beta")
          
          z$FACTOR <- factor(z$FACTOR, levels = factorOrder)
          z <- z[order(z$FACTOR),]
        }
        
        hc <- map(c(1), function(xx){
          hc <- hchart(z, "column", hcaes(x=FACTOR, y=y, group=MANAGER))
          hc 
        }) %>% hw_grid(rowheight = 500, ncol = 1)
        
      }else{
        
        
        hc <- map(unique(z$FACTOR), function(xx){
          hc <- highchart() %>%
            hc_chart(type="spline") %>%
            hc_title(align="left", text=paste0(xx," ",input$graphType," ",
                                               ifelse(input$graphType == "Risk",input$risk,""))) %>%
            hc_exporting(enabled = TRUE, filename = "custom-file-name") %>%
            hc_xAxis(type='datetime',
                     labels = list(
                       format = '{value:%Y-%m-%d}',
                       rotation = -45
                     )) %>% 
            hc_plotOptions( 
              spline=list(marker=list(enabled=FALSE)),
              series = list(
                point = list(
                  events = list(
                    click = JS(
                      "function(event){
                      var seriesName = this.series.name;
                      
                      $('#manager').val(seriesName);
                      $('#manager').change();
                      $('#manager').selectpicker('refresh');
                      $('#bucketselect').selectize()[0].selectize.setValue('Select Bucket')
                      
        }"
                  )
                    )
                    )
                    )) 
          
          
          for(man in multiMan){
            zz <- z %>% filter(MANAGER == man, FACTOR == xx)  
            hc <- hc %>% 
              hc_add_series(data=list.parse3(zz[,c("x","y")]), name=man)
          }
          
          if (input$theme != FALSE) {
            theme <- selectTheme(input$theme)
            hc <- hc %>% hc_add_theme(theme)
          }
          
          hc
      }) %>% hw_grid(rowheight = graphArgs[2], ncol = graphArgs[1])
        
        hc
      }
      
    } else {
      
      if(input$graphType == "Risk"){
        
        data <- qry(paste0("select MANAGER, DATE, RISK_SOURCE, ACT_RISK_PERC, ACT_RISK_CONT, PARENT_NODE
                           FROM barra
                           WHERE MANAGER = '",input$manager,"'
                           AND RISK_SOURCE NOT REGEXP 'Local Excess|Common Factor'
                           AND EXTRACT(YEAR_MONTH FROM DATE) Between EXTRACT(YEAR_MONTH FROM '",start,"')
                           AND EXTRACT(YEAR_MONTH FROM '",end,"')
                           ORDER BY MANAGER, DATE, RISK_SOURCE"))
        
        parentNode <- switch(input$riskGroup,
                             Style = unique(data[data$PARENT_NODE == '/Total/Local Excess/Residual/Common Factor/Risk Indices',"RISK_SOURCE"]),
                             Country = unique(data[data$PARENT_NODE == '/Total/Local Excess/Residual/Common Factor/Country',"RISK_SOURCE"]),
                             Industry = unique(data[data$PARENT_NODE == '/Total/Local Excess/Residual/Common Factor/Industry',"RISK_SOURCE"]),
                             Total = unique(data[data$PARENT_NODE == '/Total/Local Excess/Residual/Common Factor' |
                                                   data$PARENT_NODE == '/Total/Local Excess/Residual' |
                                                   data$PARENT_NODE == '/Total' |
                                                   data$PARENT_NODE == '/Total/Local Excess/Residual/Common Factor/World' &
                                                   data$RISK_SOURCE == 'World',
                                                 "RISK_SOURCE"]))
        
        #LABEL DRILLDOWN BUCKETS
        vlookup <- data.frame(PARENT_NODE = c('/Total/Local Excess/Residual/Common Factor/Industry',
                                              '/Total/Local Excess/Residual/Common Factor/Country', 
                                              '/Total/Local Excess/Residual/Common Factor/Risk Indices'), 
                              drilldown = c('Industry', 'Country', 'Style'))
        
        vlookup2 <- data.frame(RISK_SOURCE = c('Industry', 'Country', 'Risk Indices'),
                               drilldown = c('Industry', 'Country', 'Style'))
        
        graph2 <- merge(data, vlookup, on="PARENT_NODE", all.x=TRUE) %>%
          arrange(DATE, RISK_SOURCE)
        
        man <- as.character(input$manager)
        
        graph2 <- graph2 %>% 
          filter(MANAGER == man, 
                 graph2$DATE >= start,
                 graph2$DATE <= end)
        
        downloadData <<- graph2[,-ncol(graph2)]
        
        hc <- map( unique(riskType), function(xx){
          
          hc <- highchart() %>% 
            hc_chart(type = "column") %>% 
            hc_title(text = "Contribution to Tracking Error") %>% 
            hc_xAxis(categories = unique(graph2$DATE)) %>% 
            hc_yAxis(plotLines = list(
              list(color = "black",
                   width = 2,
                   value = 0))) %>% 
            hc_plotOptions( area = list(
              marker = list(
                enabled = FALSE
              )
            ),
            spline = list(
              series = list(
                marker = list(
                  enabled = FALSE
                )
              ),
              dataLabels = list(
                enabled = TRUE
              )
            ),
            series = list(
              stacking = "normal",
              #               events = list(
              #                 legendItemClick = JS(
              #                   "function(event) {
              #                   
              #                   var selected = this.index;
              #                   var allSeries = this.chart.series;
              #                   
              #                   $.each(allSeries, function(index, series) {
              #                   if(selected == index){
              #                   series.show()
              #                   }else{
              #                   series.visible == true ? series.hide() : series.show();
              #                   };
              #                   });
              #                   
              #                   return false;
              #                   
              #         }"
              #                             )
              #                 ),
              point = list(
                events = list(
                  click = JS(
                    "function(event){
                    var drillGroup = this.series.name
                    
                    if (['Country','Risk Indices','Industry'].indexOf(drillGroup) > -1){
                    if(drillGroup == 'Risk Indices'){drillGroup = 'Style'}
                    
                    $('#riskGroup').selectize()[0].selectize.setValue(drillGroup)
                    } else {
                    
                    if($('#riskGroup').selectize()[0].selectize.getValue() != 'Total'){
                    
                    event.ctrlKey == true ? 
                    $('#graphType').selectize()[0].selectize.setValue('Exposure') :
                    $('#riskGroup').selectize()[0].selectize.setValue('Total')
                    
                    }
                    }
        }"
                                )
                  )
                  )
                  )) %>%
            hc_exporting(enabled = TRUE, filename = "custom-file-name") %>%
            hc_legend(maxHeight = 80)
          
          
          #parentNode <- ifelse(input$riskGroup == "Total", parentNode, input$factorSelect)
          
          for(x in parentNode){
            x <- as.character(x)
            
            hc <- hc %>%
              hc_add_series(data = graph2[graph2$RISK_SOURCE == x, xx],
                            name = x)
          }
          
          #if(input$risk == "Contribution" & input$riskGroup == "Total"){
          groupLine <- ifelse(input$riskGroup == "Style", "Risk Indices", input$riskGroup)
          hc <- hc %>%
            hc_add_series(name = groupLine, type = "spline",
                          data = round(graph2[graph2$RISK_SOURCE == groupLine, xx],2))
          #}
          
          if (input$theme != FALSE) {
            theme <- selectTheme(input$theme)
            hc <- hc %>% hc_add_theme(theme)
          }
          
          hc
          
    }) %>% hw_grid(rowheight = 500, ncol = 1)
        
        hc
        
}
      
      if(input$graphType == "Exposure"){
        
        validate(
          need(input$riskGroup!="Total","Select A Grouping")
        )
        
        validate(
          need(input$factorSelect!="Select a Factor","Select A Factor")
        )
        
        exposure <- qry(paste0("select MANAGER, DATE, FACTOR, PARENT_NODE, ACT_EXP
                               from barraexposure
                               WHERE MANAGER = '",input$manager,"'
                               and FACTOR IN (",factorSelect,")
                               ORDER BY DATE, MANAGER")) 
        
        validate(
          need(nrow(exposure)>0,"Select A Factor")
        )
        
        parentNode <- switch(input$riskGroup,
                             Style = unique(exposure[exposure$PARENT_NODE == '/Risk Indices',"FACTOR"]),
                             Country = unique(exposure[exposure$PARENT_NODE == '/Country',"FACTOR"]),
                             Industry = unique(exposure[exposure$PARENT_NODE == '/Industry',"FACTOR"]),
                             Total = c())
        
        graph3 <- exposure
        
        man <- as.character(input$manager)
        
        start <- input$dateRange[1]
        end <- input$dateRange[2]
        
        
        graph3 <- graph3 %>% 
          filter(MANAGER == man, 
                 graph3$DATE >= start,
                 graph3$DATE <= end)
        
        # if(input$factorSelect != "All"){
        #   graph3 <- graph3 %>% filter(FACTOR == input$factorSelect)
        # }
        
        if(month(start) == month(end) & year(start) == year(end)){
          
          data <- graph3[graph3$FACTOR %in% parentNode, c("FACTOR","ACT_EXP")] %>% arrange(FACTOR)
          
          data$COLOR <- NULL
          
          if(input$riskGroup == "Style"){
            
            
            
            data[data$FACTOR %in% c("Book-to-Price","Dividend Yield", "Earnings Yield"),"COLOR"] <- "blue"
            data[data$FACTOR %in% c("Growth"),"COLOR"] <- "green"
            data[data$FACTOR %in% c("Non-linear Size", "Size", "Liquidity", "Mid Capitalization"),"COLOR"] <- "red"
            data[data$FACTOR %in% c("Momentum"),"COLOR"] <- "black"
            data[data$FACTOR %in% c("Leverage", "Earnings Quality", "Investment Quality",
                                    "Earnings Variability", "Profitability"),"COLOR"] <- "yellow"
            data[data$FACTOR %in% c("Residual Volatility","Beta"),"COLOR"] <- "purple"
            
            factorOrder <- c("Book-to-Price","Dividend Yield", "Earnings Yield", "Growth",
                             "Non-linear Size", "Size", "Liquidity", "Momentum", "Leverage",
                             "Residual Volatility","Beta")
            
            data <- data %>% slice(match(factorOrder, FACTOR))
            
          }else{
            
            data$COLOR <- "#800000"
            
          }
          
          downloadData <<-  data
          
          hc <- map(c(1),function(xx){
            hc <- hchart(data, "column", hcaes(x= FACTOR, y= ACT_EXP, color=COLOR ))
          }) %>% hw_grid(rowheight=500, ncol=1) 
          
          hc
          
        }
        else if(input$exposure == "Time-Series"){
          
          downloadData <<-  graph3
          
          hc <- map(c(1),function(xx){
            hc <- highchart() %>% 
              hc_chart(type = "spline") %>% 
              hc_title(text = "Manager Active Exposure") %>%
              hc_xAxis(categories = unique(graph3$DATE)) %>%
              hc_yAxis(plotLines = list(
                list(color = "black",
                     width = 2,
                     value = 0))) %>% 
              hc_exporting(enabled = TRUE, filename = "custom-file-name") %>%
              hc_legend(maxHeight = 80) %>%
              hc_plotOptions( series = list(
                events = list(
                  legendItemClick = JS(
                    "function(event) {
                    
                    var selected = this.index;
                    var allSeries = this.chart.series;
                    
                    $.each(allSeries, function(index, series) {
                    if(selected == index){
                    series.show()
                    }else{
                    series.visible == true ? series.hide() : series.show();
                    };
                    });
                    
                    return false;
                    
          }"
                            )
                  ),
                point = list(
                  events = list(
                    click = JS(
                      "function(event){
                      if(event.ctrlKey == true){
                      $('#graphType').selectize()[0].selectize.setValue('Risk')
                      } else {
                      $('#exposure').selectize()[0].selectize.setValue('Range')
                      }
        }"
                                )
                    )
                    )
                    )) 
            
            for(x in parentNode){
              x <- as.character(x)
              
              hc <- hc %>%
                hc_add_series(data = graph3[graph3$FACTOR == x, "ACT_EXP"],
                              name = x)
            }
            
            if (input$theme != FALSE) {
              theme <- selectTheme(input$theme)
              hc <- hc %>% hc_add_theme(theme)
            }
            
            hc
  }) %>% hw_grid(rowheight=500, ncol=1)    
          
          hc
  }
        
        else if(input$exposure == "Range"){
          
          parentNode <- switch(input$riskGroup,
                               Style = '/Risk Indices',
                               Country = '/Country',
                               Industry = '/Industry',
                               Total = c())
          
          graphEx <- exposure %>%
            filter(PARENT_NODE == parentNode, 
                   DATE >= start, DATE <= end) %>%
            group_by(FACTOR) %>%
            summarise(low=min(ACT_EXP), 
                      high=max(ACT_EXP)) %>%
            arrange(FACTOR)
          
          maxDate <- max(exposure$DATE)
          minDate <- min(exposure$DATE)
          
          graphCurr <- exposure %>%
            filter(PARENT_NODE == parentNode, DATE== maxDate )
          
          graphEx <- merge(graphEx, graphCurr, 
                           on="FACTOR", all.y=FALSE, all.x=TRUE) %>%
            mutate(ACT_EXP = ifelse(is.na(ACT_EXP),0,ACT_EXP))
          
          rng <- (maxDate != minDate)
          
          hc <- map(c(1),function(xx){  
            hc <- highchart() %>% 
              hc_chart(type = ifelse(rng, "columnrange", "column")) %>% 
              hc_title(text = "Manager Active Exposure") %>% 
              hc_xAxis(categories = graphEx$FACTOR) %>% 
              hc_yAxis(plotLines = list(
                list(color = "#2C3539",
                     width = 2,
                     value = 0)),
                title=list(text="Active Tilt")) %>% 
              hc_exporting(enabled = TRUE, filename = "custom-file-name") %>%
              hc_legend(maxHeight = 80)
            
            
            if(rng){
              hc <- hc %>%
                hc_add_series(data=list.parse2(graphEx[,c("low", "high")]),
                              name = "Range", showInLegend = FALSE, 
                              colorByPoint= TRUE) %>%
                hc_add_series_scatter(y=graphEx$ACT_EXP,x=c(1:nrow(graphEx))-1,
                                      name="Current", showInLegend = TRUE, color=rep("#ccd1d1", nrow(graphEx)))
              
            } else {
              hc <- hc %>% 
                hc_add_series(data= graphEx$ACT_EXP,
                              name = "Current", showInLegend = FALSE, 
                              colorByPoint= TRUE)
            }
            
            hc <- hc %>% 
              hc_plotOptions( series = list(
                point = list(
                  events = list(
                    click = JS(
                      "function(event){
                      $('#exposure').selectize()[0].selectize.setValue('Time-Series')
          }"
                )
                    )
                    )
                  )) 
            hc
          }) %>% hw_grid(rowheight=500, ncol=1)
          hc
        }
        
        }
      hc
      }
    })
  
  observe({  
    
    if(input$bucketselect != "Select Bucket"){
      bucket <- paste0("'",input$bucketselect,"'")
      
      results <- qry(paste0("SELECT BarraID 
                            from allmanagers 
                            where Bucket IN (",bucket,")
                            and Bucket != ''"))
      
      results <- lapply(results,trimws)
      session$sendCustomMessage(type="bucket", message=results)  
    }
    
  })
  
  observe({
    
    z <- switch(input$riskGroup,
                Total = NULL,
                Country='/Country',
                Industry='/Industry',
                Style='/Risk Indices')
    
    man <- input$manager
    
    if(!is.null(z) & !is.null(man)){
      result <- qry(paste0("select distinct FACTOR from barraexposure 
                           WHERE PARENT_NODE = '",z,"'
                           AND Manager in (",sqlClean(man),") 
                           ORDER BY FACTOR"))
      
      
      results <- lapply(result, function(x){
        paste0("<option>",trimws(x),"</option>",collapse="")
      })
      
      session$sendCustomMessage(type='factor', message=as.character(results))
      
    }
  })
  
  #   observe({
  #     
  #     graphType <- input$graphType
  #     riskGroup <- input$riskGroup
  #     
  #     ifelse(graphType == "Exposure",
  #            updateSelectInput(session, "riskGroup", choices = c("Style", "Country", "Industry")),#,
  #                              #selected = ifelse(riskGroup != "Total", riskGroup, NULL)),
  #            updateSelectInput(session, "riskGroup", 
  #                              choices = c("Total", "Style", "Country", "Industry"))#, 
  #                              #selected = riskGroup)
  #            )
  #   })
  
  timeStamp <- function(){return(gsub("[[:punct:]|[:space:]]", "" ,Sys.time()))}
  
  output$downloadBarra <- downloadHandler(
    filename = function(){paste0("export_file_",timeStamp(),".csv")},
    content = function(file) {
      write.csv(downloadData, file,row.names=F)
    }
  )
  
  output$overview_tl <- renderHighchart({
    
    man <- input$overview_manager
    sel_date <- input$overview_date
    # # 
         # man <- "Neptune"
         # sel_date <- '2017-01-31'
    
    te <- qry(paste0("select round(ACT_RISK_CONT,2) as result
                     from barra
                     where MANAGER = '",man,"'
                     and DATE = '",sel_date,"'
                     and RISK_SOURCE = 'Total'"))$result
    
    overview_data <- qry(paste0("(select * from barra 
                                where MANAGER = '",man,"'
                                and DATE = '",sel_date,"'
                                and PARENT_NODE NOT IN 
                                ('/Total','/Common Factor','/Total/Local Excess/Residual/Common Factor',
                                '/Total/Local Excess/Residual','/')
                                AND RISK_SOURCE NOT IN ('Total', 'Local Excess','Common Factor','Residual')
                                AND ACT_RISK_PERC > 0
                                ORDER BY PARENT_NODE)
                                UNION 
                                (select * from barra 
                                where MANAGER = '",man,"'
                                and DATE = '",sel_date,"'
                                AND RISK_SOURCE in ('Specific','World'))")) %>% 
      mutate(PARENT_NODE= replace(PARENT_NODE,PARENT_NODE=="/Total/Currency","Currency")) %>%
      mutate(PARENT_NODE= replace(PARENT_NODE,PARENT_NODE=="/Total/Local Excess/Residual","Specific")) %>%
      mutate(PARENT_NODE= replace(PARENT_NODE,PARENT_NODE=="/Total/Local Excess/Residual/Common Factor","Cash")) %>%
      mutate(PARENT_NODE= replace(PARENT_NODE,PARENT_NODE=="/Total/Local Excess/Residual/Common Factor/Country","Country")) %>%
      mutate(PARENT_NODE= replace(PARENT_NODE,PARENT_NODE=="/Total/Local Excess/Residual/Common Factor/Industry","Industry")) %>%
      mutate(PARENT_NODE= replace(PARENT_NODE,PARENT_NODE=="/Total/Local Excess/Residual/Common Factor/Risk Indices","Style")) %>%
      arrange(PARENT_NODE, ACT_RISK_PERC)
    
    overview_data2 <- overview_data %>% group_by(PARENT_NODE) %>% summarise(PARENT_SUM = sum(ACT_RISK_PERC))
    
    risk_groups <- c('Currency', 'Specific','Country', 'Industry', 'Style', 'Cash')
    risk_colors <- c('#7cb5ec', '#434348', '#90ed7d', '#f7a35c', '#8085e9', 
                     '#f15c80', '#e4d354', '#2b908f', '#f45b5b', '#91e8e1')
    
    overview_data$colour <- as.character(sapply(overview_data$PARENT_NODE,function(x){risk_colors[match(x,risk_groups)]}))
    overview_data2$colour <- as.character(sapply(overview_data2$PARENT_NODE,function(x){risk_colors[match(x,risk_groups)]}))
    
    overview_data <- overview_data %>% filter(!is.na(colour))
    overview_data2 <- overview_data2 %>% filter(!is.na(colour))
    
    hc <- hchart(overview_data2, type="pie", hcaes(x="PARENT_NODE", y="PARENT_SUM", color="colour"),
                 innerSize='50%',size='80%', showInLegend=TRUE,
                 borderColor= '#000000',
                 borderWidth= .5) %>% #, dataLabels=list(enabled=TRUE)) %>%
      hc_chart(borderColor= '#000000', borderWidth= .5, type='line') %>%
      hc_exporting(enabled = TRUE, filename = "custom-file-name") %>%
      hc_plotOptions(pie=list(dataLabels=list(enabled=FALSE)),
                     series=list(point=list(events=list(
                       mouseOver="function(){
                       var chart = this.series.chart;
                       chart.setTitle({text:'New title'});
  }"
                           )))) %>%
      hc_tooltip(pointFormat='{series.name}: <b>{point.percentage:.2f}%</b>') %>%
      hc_add_series(overview_data, type='pie', hcaes(x="RISK_SOURCE", y="ACT_RISK_PERC", color='colour'),
                    innerSize='70%', borderColor= '#000000', borderWidth= .5) %>%
      hc_title(text=paste0(te,'%'), verticalAlign='middle', floating=TRUE, y=-10) %>%
      hc_subtitle(text='Tracking Error:', verticalAlign='middle', floating=TRUE, y=-30)
    
    return(hc)
    
    })
  
  output$overview_tr <- renderHighchart({
    
    man <- input$overview_manager
    sel_date <- input$overview_date
    
    # man <- "ABERDEEN ASSET MGMT"
    # sel_date <- '2017-03-31'
    # 
    
    overview_data <- qry(paste0("(select * from barra 
                                where MANAGER = '",man,"'
                                and DATE = '",sel_date,"'
                                and PARENT_NODE NOT IN 
                                ('/Total','/Common Factor','/Total/Local Excess/Residual/Common Factor',
                                '/Total/Local Excess/Residual','/')
                                AND RISK_SOURCE NOT IN ('Total', 'Local Excess','Common Factor','Residual')
                                AND ACT_RISK_PERC > 0
                                ORDER BY PARENT_NODE)
                                UNION 
                                (select * from barra 
                                where MANAGER = '",man,"'
                                and DATE = '",sel_date,"'
                                AND RISK_SOURCE in ('Specific','World'))")) %>% 
      mutate(PARENT_NODE= replace(PARENT_NODE,PARENT_NODE=="/Total/Currency","Currency")) %>%
      mutate(PARENT_NODE= replace(PARENT_NODE,PARENT_NODE=="/Total/Local Excess/Residual","Specific")) %>%
      mutate(PARENT_NODE= replace(PARENT_NODE,PARENT_NODE=="/Total/Local Excess/Residual/Common Factor","Cash")) %>%
      mutate(PARENT_NODE= replace(PARENT_NODE,PARENT_NODE=="/Total/Local Excess/Residual/Common Factor/Country","Country")) %>%
      mutate(PARENT_NODE= replace(PARENT_NODE,PARENT_NODE=="/Total/Local Excess/Residual/Common Factor/Industry","Industry")) %>%
      mutate(PARENT_NODE= replace(PARENT_NODE,PARENT_NODE=="/Total/Local Excess/Residual/Common Factor/Risk Indices","Style")) %>%
      arrange(PARENT_NODE, ACT_RISK_PERC) %>% filter(RISK_SOURCE != 'Specific') %>% arrange(desc(ACT_RISK_PERC)) %>%slice(1:10) 
    
    risk_groups <- c('Currency', 'Specific','Country', 'Industry', 'Style', 'Cash')
    risk_colors <- c('#7cb5ec', '#434348', '#90ed7d', '#f7a35c', '#8085e9', 
                     '#f15c80', '#e4d354', '#2b908f', '#f45b5b', '#91e8e1')
    
    overview_data$colour <- as.character(sapply(overview_data$PARENT_NODE,function(x){risk_colors[match(x,risk_groups)]}))
    
    hc <- hchart(overview_data, type="bar", 
                 hcaes(x='RISK_SOURCE', y='ACT_RISK_PERC', color='colour'),
                 showInLegend=F,
                 borderColor= '#000000',
                 borderWidth= 1) %>%
      hc_exporting(enabled = TRUE, filename = "custom-file-name") %>%
      hc_chart(borderColor= '#000000', borderWidth= .5, type='line') %>%
      hc_title(text="Top 10 Risk Contributors") %>%
      hc_yAxis(title=list(text="Total Risk Contribution (%)"))%>%
      hc_xAxis(title=list(text=""))#%>%
    # hc_exporting(enabled = TRUE, filename = "custom-file-name") %>%
    # hc_xAxis(title=list(text=NULL),
    #          min= 0,
    #          max= 4,
    #          scrollbar=list(
    #            enabled= 'true',
    #            tickLength= 0)) %>%
    # hc_yAxis(title=list(text=NULL)) #%>%
    # hc_tooltip(pointFormat='{series.name}: <b>{point.percentage:.2f}%</b>')
    
    hc
    
  })
  
  output$overview_bl <- renderHighchart({
    
    man <- input$overview_manager
    sel_date <- input$overview_date
    
    country_exposure <- qry(paste0("select FACTOR, PORT_EXP, BMK_EXP, ACT_EXP
                                   from barraexposure 
                                   where MANAGER = '",man,"'
                                   and DATE = '",sel_date,"'
                                   and PARENT_NODE IN 
                                   ('/Country')
                                   ORDER BY ACT_EXP DESC"))
    
    country_risk<- qry(paste0("select RISK_SOURCE as FACTOR, ACT_RISK_PERC
                              from barra 
                              where MANAGER = '",man,"'
                              and DATE = '",sel_date,"'
                              and PARENT_NODE IN 
                              ('/Total/Local Excess/Residual/Common Factor/Country')
                              ORDER BY ACT_RISK_PERC DESC"))
    
    
    country_data <- country_exposure %>% left_join(country_risk, by='FACTOR')
    
    if(input$grouping){
      country_data <- country_data %>% arrange(desc(ACT_RISK_PERC))
    }else{
      country_data <- country_data %>% arrange(desc(ACT_EXP))
    }
    
    country_data <- melt(country_data)
    
    hc <- hchart(country_data, type="bar", hcaes(x='FACTOR', y='value', group='variable'),
                 borderColor= '#000000',
                 borderWidth= 1) %>%
      hc_chart(borderColor= '#000000', borderWidth= .5, type='line') %>%
      hc_title(text="Active Risk, Country Contributors") %>%
      hc_exporting(enabled = TRUE, filename = "custom-file-name") %>%
      hc_xAxis(title=list(text=NULL),
               min= 0,
               max= 4,
               scrollbar=list(
                 enabled= 'true',
                 tickLength= 0)) %>%
      hc_yAxis(title=list(text="")) #%>%
    # hc_tooltip(pointFormat='{series.name}: <b>{point.percentage:.2f}%</b>')
    
    hc
    
  })
  
  output$overview_br <- renderHighchart({
    
    man <- input$overview_manager
    sel_date <- input$overview_date
    
    country_exposure <- qry(paste0("select FACTOR, PORT_EXP, BMK_EXP, ACT_EXP
                                   from barraexposure 
                                   where MANAGER = '",man,"'
                                   and DATE = '",sel_date,"'
                                   and PARENT_NODE IN 
                                   ('/Industry')
                                   ORDER BY ACT_EXP DESC"))
    
    country_risk<- qry(paste0("select RISK_SOURCE as FACTOR, ACT_RISK_PERC
                              from barra 
                              where MANAGER = '",man,"'
                              and DATE = '",sel_date,"'
                              and PARENT_NODE IN 
                              ('/Total/Local Excess/Residual/Common Factor/Industry')
                              ORDER BY ACT_RISK_PERC DESC"))
    
    
    country_data <- country_exposure %>% left_join(country_risk, by='FACTOR')
    
    if(input$grouping){
      country_data <- country_data %>% arrange(desc(ACT_RISK_PERC))
    }else{
      country_data <- country_data %>% arrange(desc(ACT_EXP))
    }
    
    country_data <- melt(country_data)
    
    hc <- hchart(country_data, type="bar", hcaes(x='FACTOR', y='value', group='variable'),
                 borderColor= '#000000',
                 borderWidth= 1) %>%
      hc_chart(borderColor= '#000000', borderWidth= .5, type='line') %>%
      hc_title(text="Active Risk, Industry Contributors") %>%
      hc_exporting(enabled = TRUE, filename = "custom-file-name") %>%
      hc_xAxis(title=list(text=NULL),
               min= 0,
               max= 4,
               scrollbar=list(
                 enabled= 'true',
                 tickLength= 0)) %>%
      hc_yAxis(title=list(text="")) #%>%
    # hc_tooltip(pointFormat='{series.name}: <b>{point.percentage:.2f}%</b>')
    
    hc
    
  })
  
  output$overview_bbl <- renderHighchart({
    
    man <- input$overview_manager
    sel_date <- input$overview_date
    
    country_exposure <- qry(paste0("select FACTOR, PORT_EXP, BMK_EXP, ACT_EXP
                                   from barraexposure 
                                   where MANAGER = '",man,"'
                                   and DATE = '",sel_date,"'
                                   and PARENT_NODE IN 
                                   ('/Risk Indices')
                                   ORDER BY ACT_EXP DESC"))
    
    country_risk<- qry(paste0("select RISK_SOURCE as FACTOR, ACT_RISK_PERC
                              from barra 
                              where MANAGER = '",man,"'
                              and DATE = '",sel_date,"'
                              and PARENT_NODE IN 
                              ('/Total/Local Excess/Residual/Common Factor/Risk Indices')
                              ORDER BY ACT_RISK_PERC DESC"))
    
    
    country_data <- country_exposure %>% left_join(country_risk, by='FACTOR') 
    
    if(input$grouping){
      country_data <- country_data %>% arrange(desc(ACT_RISK_PERC))
    }else{
      country_data <- country_data %>% arrange(desc(ACT_EXP))
    }
    
    country_data <- melt(country_data)
    
    perc_range <- max(abs(min(country_data$value[country_data$variable == 'ACT_RISK_PERC'], na.rm=T)),
                      max(country_data$value[country_data$variable == 'ACT_RISK_PERC'], na.rm=T)) * 1.25
    
    exp_range <- max(abs(min(country_data$value[country_data$variable != 'ACT_RISK_PERC'], na.rm=T)),
                     max(country_data$value[country_data$variable != 'ACT_RISK_PERC'], na.rm=T)) * 1.25
    
    
    hc <- hchart(country_data[country_data$variable != 'ACT_RISK_PERC',],
                 type="bar", hcaes(x='FACTOR', y='value', group='variable'),
                 borderColor= '#000000', borderWidth= 1) %>%
      hc_chart(borderColor= '#000000', borderWidth= .5, type='line') %>%
      hc_title(text="Active Risk, Style Contributors") %>%
      hc_exporting(enabled = TRUE, filename = "custom-file-name") %>%
      hc_xAxis(title=list(text=NULL),
               min= 0,
               max= 4,
               scrollbar=list(
                 enabled= 'true',
                 tickLength= 0)) %>%
      hc_yAxis(title=list(text="")) %>%
      hc_yAxis_multiples(
        list(
          title = list(text = "Active Exposure"),
          min=-exp_range, max=exp_range,
          labels=list(format='{value:.1f}')
        ),
        list(
          title = list(text = "Risk Contribution (%)"),
          opposite = TRUE, min=-perc_range,max=perc_range,
          labels=list(format='{value:.1f}')
        )
      ) %>%
      hc_add_series(country_data[country_data$variable == 'ACT_RISK_PERC',], 
                    type="bar", hcaes(x='FACTOR', y='value'), name='ACT_RISK_PERC',
                    borderColor= '#000000', borderWidth= 1, yAxis=1)
    # hc_tooltip(pointFormat='{series.name}: <b>{point.percentage:.2f}%</b>')
    
    hc
    
  })
  
  output$overview_bbr <- renderHighchart({
    
    man <- input$overview_manager
    sel_date <- input$overview_date
    
    country_exposure <- qry(paste0("select FACTOR, PORT_EXP, BMK_EXP, ACT_EXP
                                   from barraexposure 
                                   where MANAGER = '",man,"'
                                   and DATE = '",sel_date,"'
                                   and PARENT_NODE IN 
                                   ('/Currency')
                                   ORDER BY ACT_EXP DESC"))
    
    country_risk<- qry(paste0("select RISK_SOURCE as FACTOR, ACT_RISK_PERC
                              from barra 
                              where MANAGER = '",man,"'
                              and DATE = '",sel_date,"'
                              and PARENT_NODE IN 
                              ('/Total/Currency')
                              ORDER BY ACT_RISK_PERC DESC"))
    
    
    country_data <- country_exposure %>% left_join(country_risk, by='FACTOR')
    
    if(input$grouping){
      country_data <- country_data %>% arrange(desc(ACT_RISK_PERC))
    }else{
      country_data <- country_data %>% arrange(desc(ACT_EXP))
    }
    
    country_data <- melt(country_data)
    
    hc <- hchart(country_data, type="bar", hcaes(x='FACTOR', y='value', group='variable'),
                 borderColor= '#000000',
                 borderWidth= 1) %>%
      hc_chart(borderColor= '#000000', borderWidth= .5, type='line') %>%
      hc_title(text="Active Risk, Currency Contributors") %>%
      hc_exporting(enabled = TRUE, filename = "custom-file-name") %>%
      hc_xAxis(title=list(text=NULL),
               min= 0,
               max= 4,
               scrollbar=list(
                 enabled= 'true',
                 tickLength= 0)) %>%
      hc_yAxis(title=list(text="")) #%>%
    # hc_tooltip(pointFormat='{series.name}: <b>{point.percentage:.2f}%</b>')
    
    hc
    
  })
  
  
  
  # output$overview_tr <- renderHighchart({
  #   
  #   man <- input$overview_manager
  #   sel_date <- input$overview_date
  #   
  #   country_data <- qry(paste0("select RISK_SOURCE, PORT_RISK_PERC, BMK_RISK_PERC, ACT_RISK_PERC
  #                              from barra 
  #                              where MANAGER = '",man,"'
  #                              and DATE = '",sel_date,"'
  #                              and PARENT_NODE IN 
  #                              ('/Total/Local Excess/Residual/Common Factor/Industry')
  #                              ORDER BY ACT_RISK_PERC DESC")) %>% 
  #                               melt() %>% 
  #     left_join(qry("select Sector, Industry as RISK_SOURCE 
  #                   from industrysectormap"), by="RISK_SOURCE") %>%
  #     group_by(Sector, variable) %>% summarise(value = sum(value)) %>%
  #     as.data.frame() %>% arrange(variable, desc(value))
  #   
  #   hc <- hchart(country_data, type="bar", hcaes(x='Sector', y='value', group='variable')) %>%
  #     hc_title(text="Top 10 Industry Risk Contributors") %>%
  #     hc_exporting(enabled = TRUE, filename = "custom-file-name") %>%
  #     hc_xAxis(title=list(text=NULL),
  #              min= 0,
  #              max= 4,
  #              scrollbar=list(
  #                enabled= 'true',
  #              tickLength= 0)) %>%
  #     hc_yAxis(title=list(text=NULL)) %>%
  #     hc_tooltip(pointFormat='{series.name}: <b>{point.percentage:.2f}%</b>')
  #   
  #   hc
  #   
  # })
  
  }

shinyApp(ui = ui, server = server)

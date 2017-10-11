rm(list=ls())

library(shiny)
library(highcharter)
library(dplyr)
library(purrr)
library(lubridate)

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


result <- lapply(managers, 
                 function(x){
                   paste0("<option value='",trimws(x),"'>",trimws(x),"</option>",collapse="")
                 })

bucket <- qry("SELECT DISTINCT bucket as Buckets
              FROM allmanagers
              WHERE bucket != ''
              ORDER BY bucket")

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
  tags$head(HTML("
                 <script src='https://cdnjs.cloudflare.com/ajax/libs/bootstrap-select/1.10.0/js/bootstrap-select.min.js'></script>
                 <link rel='stylesheet' href='https://cdnjs.cloudflare.com/ajax/libs/bootstrap-select/1.10.0/css/bootstrap-select.min.css'>
                 <script>
                 (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
                 (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
                 m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
                 })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');
                 
                 ga('create', 'UA-88914395-1', 'auto');
                 ga('send', 'pageview');
                 
                 </script>
                 ")),
  titlePanel("Risk Report"),
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
                       </br>")
           ),br(),
           selectInput("bucketselect", label = "Bucket:",  width = "100%",
                       choices = c("Select Bucket", bucket)
           ),
           selectInput("graphType", label = "Risk or Exposure?",
                       choices = c("Risk", "Exposure"), 
                       selected = "Contribution"),
           conditionalPanel(
             condition = "input.graphType == 'Risk'",
             selectInput("risk", label = "Risk Type:",  width = "100%",
                         choices = c("Contribution", "Percent_Contribution"))
           ),
           conditionalPanel(
             condition = "input.graphType == 'Exposure'",
             selectInput("exposure", label = "Exposure Type:",  width = "100%",
                         choices = c("Time-Series", "Range"))
           ),
           selectInput("riskGroup", label = "Grouping:",  width = "100%",
                       choices = c("Total", "Country", "Industry", "Style")
           ),
           conditionalPanel(
             condition = "input.riskGroup != 'Total'",
             HTML(paste0("
                         <label for='factorSelect'>Select Factor:</label>
                         <select data-width='100%' class='selectpicker'  data-actions-box='true'
                         data-live-search='true' multiple id='factorSelect'
                         title='Select Factors'>Test</select>
                         </br>")
             ),br()),
           #            selectInput("type", label = "Graph Type:", width = "100%",
           #                        choices = c("column", "line", "bar", "area")), 
           selectInput("theme", label = "Theme:",  width = "100%",
                       choices = c("None" = FALSE, "fivethirtyeight", "economist",
                                   "darkunica", "gridlight", "sandsignika",
                                   "simple","fintimes","dotabuff","google",
                                   "flat", "flatdark")
           ),
           sliderInput("dateRange", width="80%",
                       label="Date Range:", timeFormat="%b %Y",
                       min = as.Date("2014-02-01"), max = as.Date("2017-01-31"), 
                       value = c(as.Date("2014-02-01"), as.Date("2017-01-31"))
           ),
           downloadButton('downloadBarra', 'Download')
             )
             ),
  HTML("<script>
       $(document).ready(function(){
       
       $('#manager').selectpicker('deselectAll');
       $('#manager').selectpicker('refresh');  
       })
       
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
        
        
        exposure <- qry(paste0("select MANAGER, DATE, FACTOR, PARENT_NODE, ACT_EXP
                               from barraexposure
                               WHERE MANAGER = '",input$manager,"'
                               ORDER BY DATE, MANAGER")) 
        
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
        
        if(month(start) == month(end) & year(start) == year(end)){
          
          data <- graph3[graph3$FACTOR %in% parentNode, c("FACTOR","ACT_EXP")] %>% arrange(FACTOR)
          
          data$COLOR <- NULL
          
          if(input$riskGroup == "Style"){
            
            data[data$FACTOR %in% c("Book-to-Price","Dividend Yield", "Earnings Yield"),"COLOR"] <- "blue"
            data[data$FACTOR %in% c("Growth"),"COLOR"] <- "green"
            data[data$FACTOR %in% c("Non-linear Size", "Size", "Liquidity"),"COLOR"] <- "red"
            data[data$FACTOR %in% c("Momentum"),"COLOR"] <- "black"
            data[data$FACTOR %in% c("Leverage"),"COLOR"] <- "yellow"
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
  
  }

shinyApp(ui = ui, server = server)

